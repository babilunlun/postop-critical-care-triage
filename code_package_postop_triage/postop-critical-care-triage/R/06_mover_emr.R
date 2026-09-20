#!/usr/bin/env Rscript
# MOVER EPIC -> VitalDB-harmonized clinical features (EMR part)
# Cohort: adult, general anesthesia, non-cardiac surgery
# Outcome: ICU_ADMIN_FLAG == "Yes" | DISCH_DISP == "Expired"
# Units harmonized to VitalDB conventions (mg/mcg/mL).

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

EMR <- "/workspace/mover/EPIC_EMR/EMR"
OUT <- "/workspace/mover"
setDTthreads(8)

# ---------- 1. master table: cohort + outcome + static features ----------
info <- fread(file.path(EMR, "patient_information.csv"))
cat("raw rows:", nrow(info), "\n")

# parse datetimes (formats differ: "12/20/18 12:27" vs "2021-01-14 20:58:00")
parse_dt <- function(x) {
  x <- trimws(x)
  out <- suppressWarnings(parse_date_time(x, orders = c("mdy HM", "ymd HMS", "mdy HMS")))
  as.POSIXct(out)
}
info[, `:=`(
  an_start = parse_dt(AN_START_DATETIME),
  an_stop  = parse_dt(AN_STOP_DATETIME),
  surg_dt  = parse_dt(SURGERY_DATE)
)]
info[, ane_dur_min := as.numeric(difftime(an_stop, an_start, units = "mins"))]

# age is stored in BIRTH_DATE column (deidentified); height ft'in; weight oz
info[, age := as.numeric(BIRTH_DATE)]
parse_height_cm <- function(h) {
  m <- regmatches(h, regexec("^(\\d+)'\\s*(\\d+)?", h))
  sapply(m, function(v) {
    if (length(v) < 2) return(NA_real_)
    ft <- as.numeric(v[2]); inch <- ifelse(length(v) >= 3 & !is.na(v[3]), as.numeric(v[3]), 0)
    (ft * 12 + inch) * 2.54
  })
}
info[, height_cm := parse_height_cm(HEIGHT)]
info[, weight_kg := as.numeric(WEIGHT) * 0.0283495]
info[, bmi := weight_kg / (height_cm / 100)^2]
info[bmi < 10 | bmi > 80, bmi := NA_real_]

# cardiac exclusion (align with VitalDB: no cardiac surgery)
cardiac_re <- paste(c("HEART", "CABG", "CORONARY", "CARDIAC", "VALVE",
                      "MITRAL", "TRICUSPID VALVE", "AORTIC VALVE",
                      "BYPASS GRAFT", "CARDIOPULMONARY"), collapse = "|")
info[, cardiac := grepl(cardiac_re, PRIMARY_PROCEDURE_NM, ignore.case = TRUE)]

# department mapping from procedure name (VitalDB levels: General/Thoracic/Gyn/Urol)
map_dept <- function(p) {
  p <- toupper(p)
  fifelse(grepl("LUNG|THORAC|LOBECTOMY|ESOPHAGECTOMY|MEDIASTIN|PNEUMONECTOMY|WEDGE RESECTION|BRONCHOSCOPY", p), "Thoracic surgery",
  fifelse(grepl("UTERUS|HYSTERECTOMY|OVAR|CERVIX|CESAREAN|VULV|PELVIC EXENTERATION|TUBAL", p), "Gynecology",
  fifelse(grepl("PROSTATE|CYSTO|NEPHRECTOMY|NEPHRO|URETER|TURP|TURBT|BLADDER|URETHR|KIDNEY|RENAL|PYELO|LITHOTRIPSY|ORCHI", p), "Urology",
          "General surgery")))
}
info[, department := map_dept(PRIMARY_PROCEDURE_NM)]

# outcome
info[, outcome := as.integer(ICU_ADMIN_FLAG == "Yes" | DISCH_DISP == "Expired")]

# cohort filter
cohort <- info[PRIMARY_ANES_TYPE_NM == "General" & age >= 18 & !cardiac &
               !is.na(ane_dur_min) & ane_dur_min >= 30 & ane_dur_min < 24 * 60]
cat("cohort (adult GA non-cardiac, dur>=30min):", nrow(cohort),
    " events:", sum(cohort$outcome), sprintf("(%.1f%%)", 100 * mean(cohort$outcome)), "\n")
print(cohort[, .N, by = department][order(-N)])

# emergency flag: not available in MOVER EPIC -> set 0, documented limitation
cohort[, emop := 0L]
cohort[, sex := fifelse(SEX == "Male", "M", "F")]
cohort[, asa := as.numeric(ASA_RATING_C)]
cohort[asa < 1 | asa > 5, asa := NA_real_]

# ---------- 2. comorbidities from history (ICD-9/10 mixed) ----------
hist <- fread(file.path(EMR, "patient_history.csv"))
mrn_keep <- unique(cohort$MRN)
hist <- hist[mrn %in% mrn_keep]
dx <- toupper(hist$diagnosis_code)
htn_mrn <- unique(hist$mrn[grepl("^(I1[0-6]|401|402|403|404|405)", dx)])
dm_mrn  <- unique(hist$mrn[grepl("^(E1[0-4]|250)", dx)])
cohort[, `:=`(preop_htn = as.integer(MRN %in% htn_mrn),
              preop_dm  = as.integer(MRN %in% dm_mrn))]
cat("htn:", mean(cohort$preop_htn), " dm:", mean(cohort$preop_dm), "\n")

# ---------- 3. preop labs: last value within 30 d before anesthesia start ----------
labs <- fread(file.path(EMR, "patient_labs.csv"),
              select = c("LOG_ID", "Lab Name", "Observation Value", "Collection Datetime"))
lab_map <- c("Hemoglobin" = "preop_hb", "Platelets" = "preop_plt",
             "Sodium" = "preop_na", "Potassium" = "preop_k",
             "Glucose" = "preop_gluc", "Albumin" = "preop_alb",
             "Aspartate aminotransferase" = "preop_ast",
             "Alanine aminotransferase" = "preop_alt",
             "Urea nitrogen" = "preop_bun", "Creatinine" = "preop_cr")
labs <- labs[`Lab Name` %in% names(lab_map)]
labs[, `:=`(val = suppressWarnings(as.numeric(`Observation Value`)),
            coll = parse_dt(`Collection Datetime`))]
labs <- labs[!is.na(val)]
setnames(labs, "Lab Name", "lab")
labs[, lab := lab_map[lab]]

an_times <- cohort[, .(LOG_ID, an_start)]
labs <- merge(labs, an_times, by = "LOG_ID", all = FALSE)
labs[, dt_days := as.numeric(difftime(an_start, coll, units = "days"))]
preop <- labs[dt_days >= 0 & dt_days <= 30]
setorder(preop, LOG_ID, lab, dt_days)
preop_last <- preop[, .SD[1], by = .(LOG_ID, lab)]  # smallest dt_days = closest to anesthesia
preop_w <- dcast(preop_last, LOG_ID ~ lab, value.var = "val")
# physiological plausibility caps (Epic placeholder values like 9999999 -> NA)
lab_rng <- list(preop_hb = c(2, 25), preop_plt = c(5, 1500), preop_na = c(100, 180),
                preop_k = c(1.5, 9), preop_gluc = c(20, 1500), preop_alb = c(0.5, 7),
                preop_ast = c(1, 20000), preop_alt = c(1, 20000),
                preop_bun = c(1, 250), preop_cr = c(0.05, 20))
for (lb in names(lab_rng)) {
  if (lb %in% names(preop_w)) {
    r <- lab_rng[[lb]]
    preop_w[[lb]][preop_w[[lb]] < r[1] | preop_w[[lb]] > r[2]] <- NA_real_
  }
}
cat("preop lab coverage (of", nrow(cohort), "):\n")
print(preop_w[, lapply(.SD, function(x) mean(!is.na(x))), .SDcols = -1])

cohort <- merge(cohort, preop_w, by = "LOG_ID", all.x = TRUE)

# ---------- 4. intraop medications (INTRA-OP record type, Given only, IV route) ----------
meds <- fread(file.path(EMR, "patient_medications.csv"),
              select = c("LOG_ID", "MEDICATION_NM", "RECORD_TYPE", "MAR_ACTION_NM",
                         "ADMIN_SIG", "DOSE_UNIT_NM", "MED_ROUTE_NM", "MED_ACTION_TIME"))
meds <- meds[RECORD_TYPE == "INTRA-OP" & MAR_ACTION_NM == "Given" &
             MED_ROUTE_NM == "IntraVENOUS" & LOG_ID %in% cohort$LOG_ID]
meds[, dose := suppressWarnings(as.numeric(ADMIN_SIG))]
meds <- meds[!is.na(dose) & dose >= 0]

drug_group <- function(nm) {
  nm <- toupper(nm)
  fifelse(grepl("^PROPOFOL", nm), "intraop_ppf",
  fifelse(grepl("^MIDAZOLAM", nm), "intraop_mdz",
  fifelse(grepl("^FENTANYL", nm), "intraop_ftn",
  fifelse(grepl("^ROCURONIUM", nm), "intraop_rocu",
  fifelse(grepl("^EPHEDRINE", nm), "intraop_eph",
  fifelse(grepl("^PHENYLEPHRINE", nm), "intraop_phe",
  fifelse(grepl("^EPINEPHRINE(?!.*LIDOCAINE)", nm, perl = TRUE), "intraop_epi",
          NA_character_)))))))
}
meds[, drug := drug_group(MEDICATION_NM)]
meds <- meds[!is.na(drug)]
meds[, unit := toupper(trimws(DOSE_UNIT_NM))]

# bolus rows: dose units -> harmonize to VitalDB (ppf/mdz/rocu/eph mg; ftn/phe/epi mcg)
bolus <- meds[!grepl("/MIN|/HR|/HOUR|/KG/", unit)]
bolus[, dose_h := fcase(
  drug %in% c("intraop_ppf", "intraop_mdz", "intraop_rocu", "intraop_eph") &
    grepl("MCG", unit), dose / 1000,
  drug %in% c("intraop_ftn", "intraop_phe", "intraop_epi") &
    grepl("^MG", unit), dose * 1000,
  default = dose
)]
# unit-label errors: >50 "mg" IV epinephrine bolus is impossible; value is actually mcg
bolus[drug == "intraop_epi" & unit == "MG" & dose > 50, dose_h := dose]
bolus_tot <- bolus[, .(tot = sum(dose_h)), by = .(LOG_ID, drug)]

# infusion rows: rate units -> approximate total = mean rate x infusion span
# (span = last - first charted rate; single record -> assume 15 min). mcg/kg/min uses weight.
inf <- meds[grepl("/MIN|/HR|/HOUR", unit)]
if (nrow(inf) > 0) {
  inf[, rate_mcg_min := fcase(
    grepl("MCG/KG/MIN", unit), dose * 70,           # placeholder, weight joined below
    grepl("MCG/MIN", unit), dose,
    grepl("MG/MIN", unit), dose * 1000,
    grepl("MG/HR|MG/HOUR", unit), dose * 1000 / 60,
    grepl("MCG/HR|MCG/HOUR", unit), dose / 60,
    default = NA_real_
  )]
  w <- cohort[, .(LOG_ID, weight_kg)]
  inf <- merge(inf, w, by = "LOG_ID", all.x = TRUE)
  inf[grepl("MCG/KG/MIN", unit) & !is.na(weight_kg), rate_mcg_min := dose * weight_kg]
  inf <- inf[!is.na(rate_mcg_min)]
  inf[, act_t := parse_dt(MED_ACTION_TIME)]
  inf_agg <- inf[, .(mean_rate = mean(rate_mcg_min),
                     span_min = max(1, as.numeric(difftime(max(act_t), min(act_t), units = "mins"))),
                     n_rec = .N), by = .(LOG_ID, drug)]
  inf_agg[n_rec == 1, span_min := 15]
  # clip span to anesthesia duration (guards against cross-encounter time mixing)
  inf_agg <- merge(inf_agg, cohort[, .(LOG_ID, ane_dur_min)], by = "LOG_ID")
  inf_agg[, span_min := pmin(span_min, ane_dur_min)]
  # convert to drug target unit (mg for ppf; mcg for phe/epi)
  inf_agg[, tot := fifelse(drug == "intraop_ppf",
                           mean_rate * span_min / 1000,
                           mean_rate * span_min)]
  inf_tot <- inf_agg[, .(LOG_ID, drug, tot)]
  med_tot <- rbindlist(list(bolus_tot, inf_tot))[, .(tot = sum(tot)), by = .(LOG_ID, drug)]
} else {
  med_tot <- bolus_tot
}
# physiological per-case caps (resuscitation-level upper bounds; VitalDB units)
phys_cap <- c(intraop_ppf = 3000, intraop_mdz = 50, intraop_ftn = 3000,
              intraop_rocu = 300, intraop_eph = 500, intraop_phe = 20000,
              intraop_epi = 10000)
med_tot[, tot := pmin(tot, phys_cap[drug])]
# also cap at 99.9th pct (residual charting errors)
caps <- med_tot[, .(cap = quantile(tot, 0.999, na.rm = TRUE)), by = drug]
print(caps)
med_tot <- merge(med_tot, caps, by = "drug")
med_tot[tot > cap, tot := cap]
med_w <- dcast(med_tot, LOG_ID ~ drug, value.var = "tot", fill = 0)
cohort <- merge(cohort, med_w, by = "LOG_ID", all.x = TRUE)
for (dg in c("intraop_ppf", "intraop_mdz", "intraop_ftn", "intraop_rocu",
            "intraop_eph", "intraop_phe", "intraop_epi")) {
  if (!dg %in% names(cohort)) cohort[, (dg) := 0]
  cohort[is.na(get(dg)), (dg) := 0]
}

# ---------- 5. transfusion from billing codes (ICD-10-PCS 3023xY1) ----------
# N=RBC, L=FFP fresh plasma, K=plasma; count rows as unit approximation.
# Keyed by MRN (encounter-level): assign only when patient has 1 cohort surgery.
coding <- fread(file.path(EMR, "patient_coding.csv"))
tx <- coding[grepl("^3023", REF_BILL_CODE)]
tx[, product := fcase(substr(REF_BILL_CODE, 6, 6) == "N", "intraop_rbc",
                      substr(REF_BILL_CODE, 6, 6) %in% c("L", "K"), "intraop_ffp",
                      default = NA_character_)]
tx <- tx[!is.na(product)]
tx_n <- tx[, .N, by = .(MRN, product)]
surg_per_mrn <- cohort[, .N, by = MRN]
single_surg <- surg_per_mrn[N == 1, MRN]
tx_n <- tx_n[MRN %in% single_surg]
tx_w <- dcast(tx_n, MRN ~ product, value.var = "N", fill = 0)
cohort <- merge(cohort, tx_w, by = "MRN", all.x = TRUE)
for (dg in c("intraop_rbc", "intraop_ffp")) {
  if (!dg %in% names(cohort)) cohort[, (dg) := 0]
  cohort[is.na(get(dg)), (dg) := 0]
}
cat("RBC>0:", mean(cohort$intraop_rbc > 0), " FFP>0:", mean(cohort$intraop_ffp > 0), "\n")

# ---------- 6. colloid: albumin 5% bags x 250 mL ----------
alb <- meds[grepl("^ALBUMIN", toupper(MEDICATION_NM))]
if (nrow(alb) > 0) {
  alb_n <- alb[, .N, by = LOG_ID]
  alb_n[, intraop_colloid := pmin(N * 250, 5000)]
  cohort <- merge(cohort, alb_n[, .(LOG_ID, intraop_colloid)], by = "LOG_ID", all.x = TRUE)
}
if (!"intraop_colloid" %in% names(cohort)) cohort[, intraop_colloid := 0]
cohort[is.na(intraop_colloid), intraop_colloid := 0]

# ---------- save ----------
keep <- c("LOG_ID", "MRN", "outcome", "age", "sex", "bmi", "asa", "emop",
          "department", "preop_htn", "preop_dm",
          "preop_hb", "preop_plt", "preop_na", "preop_k", "preop_gluc",
          "preop_alb", "preop_ast", "preop_alt", "preop_bun", "preop_cr",
          "intraop_rbc", "intraop_ffp", "intraop_colloid",
          "intraop_ppf", "intraop_mdz", "intraop_ftn", "intraop_rocu",
          "intraop_eph", "intraop_phe", "intraop_epi",
          "ane_dur_min", "an_start", "an_stop")
mover_emr <- cohort[, ..keep]
saveRDS(mover_emr, file.path(OUT, "mover_emr.rds"))
cat("saved mover_emr.rds:", nrow(mover_emr), "rows\n")
print(summary(mover_emr[, .(age, bmi, asa, ane_dur_min, intraop_ppf, intraop_ftn)]))
