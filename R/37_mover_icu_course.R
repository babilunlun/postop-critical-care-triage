#!/usr/bin/env Rscript
# 37_mover_icu_course.R — extract ICU-course interventions for the MOVER cohort
# Sources (LOG_ID-keyed unless noted):
#   patient_lda.csv:         airway records -> postop mechanical ventilation
#                            (ETT removal > AN_STOP + 6h, OR new airway placed after
#                             AN_STOP, OR surgical airway/tracheostomy placed postop)
#   mover_vaso_rows.csv:     MAR rows pre-filtered for vasopressors/inotropes;
#                            keep MAR_ACTION_NM=="Given", MED_ACTION_TIME in
#                            (AN_STOP, HOSP_DISCH_TIME]
#   patient_coding.csv:      ICD-10-PCS ventilation 5A1935Z/45Z/55Z, dialysis 5A1D*,
#                            ECMO 5A1522* — PATIENT-level (MRN), used only for
#                            single-operation patients (sensitivity/corroboration)
# Quality gate: >=50% of ICU_ADMIN_FLAG=="Yes" cohort ops have >=1 postop LDA/MAR row.
# Output: /workspace/ext_out/mover_icu_course.rds

suppressPackageStartupMessages({library(data.table); library(lubridate)})
setDTthreads(8)
EMR <- "/workspace/mover_check/EPIC_EMR/EMR"
OUT <- "/workspace/ext_out"

parse_dt <- function(x) {
  x <- trimws(x)
  suppressWarnings(parse_date_time(x, orders = c("mdy HM", "ymd HMS", "mdy HMS", "mdy")))
  as.POSIXct(suppressWarnings(parse_date_time(x, orders = c("mdy HM", "ymd HMS", "mdy HMS", "mdy"))))
}

# ---------- cohort + encounter anchors ----------
info <- fread(file.path(EMR, "patient_information.csv"))
info[, `:=`(an_stop = parse_dt(AN_STOP_DATETIME),
            hosp_disch = parse_dt(HOSP_DISCH_TIME),
            hosp_adm = parse_dt(HOSP_ADMSN_TIME))]
am <- readRDS("/mnt/shared-workspace/shared/analysis_mover.rds")
keep <- unique(am$LOG_ID)
info <- info[LOG_ID %in% keep]
cat("cohort ops:", nrow(info), "| ICU_ADMIN_FLAG=Yes:",
    sum(info$ICU_ADMIN_FLAG == "Yes"), "\n")

# ---------- 1. LDA airways -> postop ventilation ----------
lda <- fread(file.path(EMR, "patient_lda.csv"))
lda <- lda[LOG_ID %in% keep]
air <- lda[grepl("AIRWAY", toupper(Line_Group_Name)) | grepl("Airway", description)]
cat("airway rows in cohort:", nrow(air), "\n")
air[, `:=`(pl = parse_dt(placement_instant), rm = parse_dt(removal_instant))]
air <- merge(air, info[, .(LOG_ID, an_stop, hosp_disch)], by = "LOG_ID")
# postop ventilation definitions
air[, vent_6h := !is.na(rm) & !is.na(an_stop) & rm > an_stop + 6 * 3600 &
      rm <= hosp_disch + 86400]
air[, vent_newpost := !is.na(pl) & !is.na(an_stop) & pl > an_stop &
      pl <= hosp_disch + 86400]
air[, trach_post := grepl("Surgical Airway", description) & vent_newpost]
vent_flags <- air[, .(postop_vent = as.integer(any(vent_6h | vent_newpost, na.rm = TRUE)),
                      postop_vent_6h_only = as.integer(any(vent_6h, na.rm = TRUE)),
                      postop_trach = as.integer(any(trach_post, na.rm = TRUE)),
                      n_airway = .N), by = LOG_ID]
cat("ops with postop ventilation flag:", sum(vent_flags$postop_vent), "\n")

# ---------- 2. MAR vasopressors/inotropes ----------
vr <- fread("/workspace/mover_check/mover_vaso_rows.csv")
names(vr) <- tolower(names(vr))
vr <- vr[log_id %in% keep]
cat("vaso MAR rows in cohort:", nrow(vr), "\n")
print(vr[, .N, by = .(medication_nm = tolower(medication_nm))][order(-N)][1:12])
vr[, med_t := parse_dt(med_action_time)]
vr <- merge(vr, info[, .(LOG_ID, an_stop, hosp_disch)], by.x = "log_id", by.y = "LOG_ID")
vr[, postop := !is.na(med_t) & !is.na(an_stop) & med_t > an_stop &
     med_t <= hosp_disch + 86400]
# administration actually given (keep NA action as given if RECORD_TYPE says POST-OP? no: require Given/Taken/Infusion verbs)
vr[, given := mar_action_nm %in% c("Given", "Infusion Started", "Infusion Completed",
                                   "Bolus", "Given by Other", "Patient's Own Med") |
       is.na(mar_action_nm)]
vaso_flags <- vr[postop == TRUE & given == TRUE,
                 .(postop_vaso = 1L, n_vaso_rows = .N,
                   postop_vaso_ne = as.integer(any(grepl("norepinephrine|levophed",
                                                         tolower(medication_nm))))),
                 by = log_id]
setnames(vaso_flags, "log_id", "LOG_ID")
cat("ops with postop vasopressor flag:", nrow(vaso_flags), "\n")

# ---------- 3. ICD-10-PCS (patient-level; single-op patients only) ----------
cod <- fread(file.path(EMR, "patient_coding.csv"))
pcs <- cod[REF_BILL_CODE_SET_NAME == "ICD-10-PCS"]
pcs[, vent_pcs := grepl("^5A1935Z|^5A1945Z|^5A1955Z", REF_BILL_CODE)]
pcs[, dial_pcs := grepl("^5A1D", REF_BILL_CODE)]
pcs[, ecmo_pcs := grepl("^5A1522", REF_BILL_CODE)]
pcs_mrn <- pcs[, .(vent_pcs = as.integer(any(vent_pcs)),
                   dial_pcs = as.integer(any(dial_pcs)),
                   ecmo_pcs = as.integer(any(ecmo_pcs))), by = MRN]
nops_per_mrn <- info[, .N, by = MRN]
single_op_mrns <- nops_per_mrn[N == 1, MRN]
pcs_mrn <- pcs_mrn[MRN %in% single_op_mrns]
pcs_flags <- merge(info[, .(LOG_ID, MRN)], pcs_mrn, by = "MRN", all.x = TRUE)
pcs_flags <- pcs_flags[!is.na(vent_pcs) | !is.na(dial_pcs) | !is.na(ecmo_pcs)]
cat("single-op patients with PCS vent/dialysis/ecmo:", nrow(pcs_flags), "\n")

# ---------- 4. assemble ----------
out <- info[, .(LOG_ID, MRN, icu_flag = as.integer(ICU_ADMIN_FLAG == "Yes"),
                expired = as.integer(DISCH_DISP == "Expired"),
                an_stop, hosp_disch)]
out <- merge(out, vent_flags, by = "LOG_ID", all.x = TRUE)
out <- merge(out, vaso_flags, by = "LOG_ID", all.x = TRUE)
out <- merge(out, pcs_flags[, .(LOG_ID, vent_pcs, dial_pcs, ecmo_pcs)],
             by = "LOG_ID", all.x = TRUE)
for (cc in c("postop_vent", "postop_vent_6h_only", "postop_trach", "postop_vaso",
             "postop_vaso_ne", "vent_pcs", "dial_pcs", "ecmo_pcs"))
  set(out, which(is.na(out[[cc]])), cc, 0L)
out[, icu_intervention := as.integer(postop_vent | postop_vaso)]
out[, icu_intervention_pluspcs := as.integer(postop_vent | postop_vaso |
                                             vent_pcs | dial_pcs | ecmo_pcs)]
saveRDS(out, file.path(OUT, "mover_icu_course.rds"))

# ---------- 5. quality gate + sanity ----------
cat("\n===== QUALITY GATE =====\n")
icu_ops <- out[icu_flag == 1]
# any postop LDA or MAR row at all (data presence, not just interventions)
any_lda <- lda[, .N, by = LOG_ID]
any_mar <- vr[given == TRUE & postop == TRUE, .N, by = log_id]
cov <- mean(icu_ops$LOG_ID %in% c(any_lda$LOG_ID, any_mar$log_id))
cat(sprintf("ICU-flag ops with any postop LDA/MAR row: %.1f%% (gate: >=50%%)\n", 100 * cov))
cat("ICU-flag ops:", nrow(icu_ops), "\n")
cat("  with intervention (vent|vaso):", sum(icu_ops$icu_intervention),
    sprintf("(%.1f%%)\n", 100 * mean(icu_ops$icu_intervention)))
cat("  vent:", sum(icu_ops$postop_vent), sprintf("(%.1f%%)", 100 * mean(icu_ops$postop_vent)),
    "| vaso:", sum(icu_ops$postop_vaso), sprintf("(%.1f%%)\n", 100 * mean(icu_ops$postop_vaso)))
cat("  with intervention+PCS:", sum(icu_ops$icu_intervention_pluspcs),
    sprintf("(%.1f%%)\n", 100 * mean(icu_ops$icu_intervention_pluspcs)))
cat("expired ops:", sum(out$expired), "\n")
cat("== 37 done ==\n")
