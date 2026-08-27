#!/usr/bin/env Rscript
# 25_inspire_cohort.R — INSPIRE cohort + clinical features (VitalDB/MOVER-harmonized)
# Cohort: adult (all INSPIRE 18-90), general anesthesia, non-cardiac, ane duration >= 30 min
# Outcome: ICU admission within 24h postop (icuin_time) | in-hospital death (inhosp_death_time)
# Comorbidities: preop ICD-10 dx OR preop medication (claims undercapture chronic disease
#   vs VitalDB preop assessment: HTN dx-only 6.8% vs VitalDB 31%; dx+med -> ~20%)
# Input: /workspace/inspire/{operations,labs,diagnosis,medications}.csv.gz
# Output: /workspace/inspire/inspire_cohort.rds (clinical part; TS/IO features added by 26)

suppressPackageStartupMessages(library(data.table))
setDTthreads(8)
DIR <- "/workspace/inspire"

# ---------- 1. operations: cohort + static features ----------
op <- fread(file.path(DIR, "operations.csv.gz"))
cat("operations raw:", nrow(op), "\n")

op[, `:=`(
  ane_dur_min = anend_time - anstart_time,
  bmi = weight / (height / 100)^2
)]
op[bmi < 10 | bmi > 80, bmi := NA_real_]
op[asa < 1 | asa > 5, asa := NA_real_]

# cardiac exclusion: CPB used OR ICD-10-PCS 02* (heart & great vessels)
op[, cardiac := !is.na(cpbon_time) | substr(icd10_pcs, 1, 2) == "02"]

# outcome: postop ICU within 24h | in-hospital death
op[, outcome := as.integer(!is.na(icuin_time) | !is.na(inhosp_death_time))]

# department mapping to VitalDB levels (MOVER-consistent: default General surgery)
op[, department := fcase(
  department == "CTS", "Thoracic surgery",
  department == "OG",  "Gynecology",
  department == "UR",  "Urology",
  default = "General surgery"
)]

cohort <- op[age >= 18 & antype == "General" & !cardiac &
             !is.na(ane_dur_min) & ane_dur_min >= 30]
cat("cohort (adult GA non-cardiac, dur>=30min):", nrow(cohort),
    " events:", sum(cohort$outcome),
    sprintf("(%.1f%%)", 100 * mean(cohort$outcome)), "\n")
print(cohort[, .N, by = department][order(-N)])
cat("subjects:", uniqueN(cohort$subject_id), " ops/subject:",
    round(nrow(cohort) / uniqueN(cohort$subject_id), 3), "\n")

# ---------- 2. comorbidities: preop diagnosis OR preop medication ----------
# HTN: I10-I16 preop  OR  preop antihypertensive (ATC C02/C03/C07/C08/C09)
# DM:  E10-E14 preop  OR  preop antidiabetic (ATC A10)
ops_t <- cohort[, .(op_id, subject_id, anstart_time)]
dx <- fread(file.path(DIR, "diagnosis.csv.gz"))
dx <- dx[subject_id %in% cohort$subject_id]
dx <- merge(dx, ops_t, by = "subject_id", allow.cartesian = TRUE)
dx <- dx[chart_time < anstart_time]   # preoperative diagnoses only
htn_dx <- unique(dx[grepl("^I1[0-6]", icd10_cm), op_id])
dm_dx  <- unique(dx[grepl("^E1[0-4]", icd10_cm), op_id])

med <- fread(file.path(DIR, "medications.csv.gz"))
med <- med[subject_id %in% cohort$subject_id]
med <- merge(med, ops_t, by = "subject_id", allow.cartesian = TRUE)
pre_med <- med[chart_time < anstart_time & !is.na(atc_code)]
htn_med <- unique(pre_med[grepl("^C0[23789]", atc_code), op_id])
dm_med  <- unique(pre_med[grepl("^A10", atc_code), op_id])

htn_op <- union(htn_dx, htn_med)
dm_op  <- union(dm_dx, dm_med)
cohort[, `:=`(preop_htn = as.integer(op_id %in% htn_op),
              preop_dm  = as.integer(op_id %in% dm_op))]
cat("htn:", round(mean(cohort$preop_htn), 3),
    "(dx-only:", round(length(htn_dx) / nrow(cohort), 3), ")",
    " dm:", round(mean(cohort$preop_dm), 3),
    "(dx-only:", round(length(dm_dx) / nrow(cohort), 3), ")\n")

# ---------- 3. preop labs: last value within 30 d before anesthesia start ----------
labs <- fread(file.path(DIR, "labs.csv.gz"))
lab_map <- c(hb = "preop_hb", platelet = "preop_plt", sodium = "preop_na",
             potassium = "preop_k", glucose = "preop_gluc", albumin = "preop_alb",
             ast = "preop_ast", alt = "preop_alt", bun = "preop_bun",
             creatinine = "preop_cr")
labs <- labs[item_name %in% names(lab_map) & subject_id %in% cohort$subject_id]
labs[, lab := lab_map[item_name]]
labs <- merge(labs[, .(subject_id, chart_time, lab, value)],
              cohort[, .(op_id, subject_id, anstart_time)],
              by = "subject_id", allow.cartesian = TRUE)
labs[, dt_min := anstart_time - chart_time]
preop <- labs[dt_min >= 0 & dt_min <= 30 * 1440 & !is.na(value)]
setorder(preop, op_id, lab, dt_min)
preop_last <- preop[, .SD[1], by = .(op_id, lab)]   # closest to anesthesia start
preop_w <- dcast(preop_last, op_id ~ lab, value.var = "value")

# physiological plausibility caps (identical to MOVER pipeline)
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
cat("preop lab coverage (of", nrow(cohort), "ops):\n")
print(preop_w[, lapply(.SD, function(x) round(mean(!is.na(x)), 3)), .SDcols = -1])

cohort <- merge(cohort, preop_w, by = "op_id", all.x = TRUE)

# ---------- 4. save ----------
keep <- c("op_id", "subject_id", "hadm_id", "case_id", "opdate", "outcome",
          "age", "sex", "bmi", "asa", "emop", "department", "icd10_pcs",
          "preop_htn", "preop_dm",
          "preop_hb", "preop_plt", "preop_na", "preop_k", "preop_gluc",
          "preop_alb", "preop_ast", "preop_alt", "preop_bun", "preop_cr",
          "ane_dur_min", "anstart_time", "anend_time",
          "icuin_time", "icuout_time", "inhosp_death_time")
for (cc in setdiff(keep, names(cohort))) cohort[, (cc) := NA_real_]
out <- cohort[, ..keep]
saveRDS(out, file.path(DIR, "inspire_cohort.rds"))
cat("saved inspire_cohort.rds:", nrow(out), "ops\n")
