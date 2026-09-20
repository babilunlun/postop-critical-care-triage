#!/usr/bin/env Rscript
# 40_sensitivity_b5.R — B5 sensitivity analyses for the ICU-course–validated outcomes
#   S-a INSPIRE: intervention window ICU-only (primary) vs postop-48h union
#   S-b INSPIRE: vasopressor definition any-vs-norepinephrine-only
#   S-c both:    middle band absolute 5-30% (primary) vs percentile P10-P90
#   S-d MOVER:   exclude routine-ICU procedures (S1-90% list from v3 sensitivity)
#   S-e INSPIRE: icu_dependent with vent-only intervention (vaso under-capture check)
#   S-f MOVER:   intervention definition + patient-level PCS codes (single-op patients)
# Output: /mnt/results/06_icu_course/table_s_b5_sensitivity.csv

suppressPackageStartupMessages({library(data.table); library(pROC)})
set.seed(42)
OUT <- "/mnt/results/06_icu_course"
DIR <- "/workspace/inspire"

auc_ci <- function(y, p) {
  if (length(unique(y)) < 2) return("NA")
  ci <- ci.auc(roc(y, p, quiet = TRUE), method = "delong")
  sprintf("%.3f (%.3f-%.3f)", ci[2], ci[1], ci[3])
}
res <- list()
add <- function(cohort, variant, metric, value)
  res[[length(res) + 1]] <<- data.table(cohort = cohort, variant = variant,
                                        metric = metric, value = value)

ins <- fread(file.path(OUT, "refined_categories_inspire.csv"))
mov <- fread(file.path(OUT, "refined_categories_mover.csv"))

# ---------- S-a/S-b/S-e: INSPIRE re-extraction with variant windows ----------
co <- readRDS(file.path(DIR, "inspire_cohort.rds"))
prim_ids <- ins$LOG_ID
ops <- co[op_id %in% prim_ids, .(op_id, subject_id, anstart_time, anend_time,
                                 icuin_time, icuout_time, discharge_time,
                                 early_icu, delayed_icu, death_idx)]

# 48h-union window flags from ward_vitals + medications
wv <- fread(file.path(DIR, "ward_vitals.csv.gz"))
wv <- wv[item_name %in% c("vent", "crrt", "ecmo", "iabp") & value == 1 &
         subject_id %in% ops$subject_id]
setnames(wv, "chart_time", "t")
m48 <- merge(wv, ops, by = "subject_id", allow.cartesian = TRUE)
m48[, in48 := t >= anend_time & t <= pmin(discharge_time, anend_time + 48 * 60)]
m48[, inicu := !is.na(icuin_time) & t >= icuin_time & t <= icuout_time]
w48 <- m48[, .(flag48 = as.integer(any(in48 | inicu)),
               flagicu = as.integer(any(inicu))), by = op_id]

vaso_drugs <- c("norepinephrine", "epinephrine", "dopamine", "dobutamine",
                "phenylephrine", "vasopressin", "terlipressin", "milrinone")
meds <- fread(file.path(DIR, "medications.csv.gz"))
meds[, drug_name := tolower(drug_name)]
meds <- meds[drug_name %in% vaso_drugs & route == "iv" & subject_id %in% ops$subject_id]
setnames(meds, "chart_time", "t")
mm <- merge(meds, ops, by = "subject_id", allow.cartesian = TRUE)
mm <- mm[!(t >= anstart_time & t <= anend_time)]  # exclude intraop
mm[, in48 := t >= anend_time & t <= pmin(discharge_time, anend_time + 48 * 60)]
mm[, inicu := !is.na(icuin_time) & t >= icuin_time & t <= icuout_time]
v48 <- mm[, .(vaso48 = as.integer(any(in48 | inicu)),
              vasoicu = as.integer(any(inicu)),
              vaso48_ne = as.integer(any((in48 | inicu) & drug_name == "norepinephrine")),
              vasoicu_ne = as.integer(any(inicu & drug_name == "norepinephrine"))),
          by = op_id]

var <- merge(ops, w48, by = "op_id", all.x = TRUE)
var <- merge(var, v48, by = "op_id", all.x = TRUE)
for (cc in c("flag48", "flagicu", "vaso48", "vasoicu", "vaso48_ne", "vasoicu_ne"))
  set(var, which(is.na(var[[cc]])), cc, 0L)
var[, interv_icu := as.integer(flagicu | vasoicu)]          # primary (matches 36)
var[, interv_48 := as.integer(flag48 | vaso48)]             # S-a
# vent-only (S-e): ward_vitals vent flag within ICU window
vent_icu <- m48[item_name == "vent", .(venticu = as.integer(any(inicu))), by = op_id]
var <- merge(var, vent_icu, by = "op_id", all.x = TRUE)
set(var, which(is.na(var$venticu)), "venticu", 0L)
var[, interv_ne := as.integer(venticu | vasoicu_ne)]        # S-b: vent + NE-only vaso
var[, interv_ventonly := venticu]                           # S-e

classify <- function(d, interv_col) {
  iv <- d[[interv_col]]
  fcase(d$early_icu & (iv == 1 | d$death_idx), "icu_dependent",
        d$early_icu, "observational_icu",
        !d$early_icu & (d$delayed_icu | d$death_idx), "missed_escalation",
        default = "uncomplicated_ward")
}
base <- merge(ins[, .(LOG_ID, XGB_full, SASA, category)],
              var[, .(op_id, interv_icu, interv_48, interv_ne, interv_ventonly,
                      early_icu, delayed_icu, death_idx)],
              by.x = "LOG_ID", by.y = "op_id")
chk <- base$category == classify(base, "interv_icu")
cat("consistency w/ 36:", sum(chk), "/", length(chk), "matched\n")
if (!all(chk)) {
  mm <- base[!chk]
  cat("mismatch examples:\n"); print(head(mm[, .(LOG_ID, category, early_icu, delayed_icu,
                                               death_idx, interv_icu)], 10))
}

for (v in c("interv_48", "interv_ne", "interv_ventonly")) {
  cat_v <- classify(base, v)
  y_dep <- as.integer(cat_v == "icu_dependent")
  d3 <- cat_v %in% c("icu_dependent", "observational_icu")
  add("INSPIRE", v, "n_icu_dependent", sum(y_dep))
  add("INSPIRE", v, "AUROC_icu_dependent", auc_ci(y_dep, base$XGB_full))
  add("INSPIRE", v, "AUROC_dep_vs_obs_among_admitted",
      auc_ci(as.integer(cat_v[d3] == "icu_dependent"), base$XGB_full[d3]))
}
add("INSPIRE", "interv_icu (primary)", "n_icu_dependent", sum(base$category == "icu_dependent"))
add("INSPIRE", "interv_icu (primary)", "AUROC_icu_dependent",
    auc_ci(as.integer(base$category == "icu_dependent"), base$XGB_full))
d3p <- base$category %in% c("icu_dependent", "observational_icu")
add("INSPIRE", "interv_icu (primary)", "AUROC_dep_vs_obs_among_admitted",
    auc_ci(as.integer(base$category[d3p] == "icu_dependent"), base$XGB_full[d3p]))

# ---------- S-f: MOVER +PCS intervention definition ----------
mov2 <- copy(mov)
cat_f <- fcase(mov2$icu_flag == 1 & (mov2$icu_intervention_pluspcs == 1 | mov2$expired == 1),
               "icu_dependent",
               mov2$icu_flag == 1, "observational_icu",
               mov2$icu_flag == 0 & mov2$expired == 1, "missed_escalation",
               default = "uncomplicated_ward")
add("MOVER", "intervention+PCS", "n_icu_dependent", sum(cat_f == "icu_dependent"))
add("MOVER", "intervention+PCS", "AUROC_icu_dependent",
    auc_ci(as.integer(cat_f == "icu_dependent"), mov2$XGB_full))
d3m <- cat_f %in% c("icu_dependent", "observational_icu")
add("MOVER", "intervention+PCS", "AUROC_dep_vs_obs_among_admitted",
    auc_ci(as.integer(cat_f[d3m] == "icu_dependent"), mov2$XGB_full[d3m]))
add("MOVER", "intervention (primary)", "n_icu_dependent", sum(mov2$category == "icu_dependent"))
add("MOVER", "intervention (primary)", "AUROC_icu_dependent",
    auc_ci(as.integer(mov2$category == "icu_dependent"), mov2$XGB_full))
d3mp <- mov2$category %in% c("icu_dependent", "observational_icu")
add("MOVER", "intervention (primary)", "AUROC_dep_vs_obs_among_admitted",
    auc_ci(as.integer(mov2$category[d3mp] == "icu_dependent"), mov2$XGB_full[d3mp]))

# ---------- S-d: MOVER exclude routine-ICU procedures (S1-90) ----------
info <- fread("/workspace/mover_check/EPIC_EMR/EMR/patient_information.csv",
              select = c("LOG_ID", "PRIMARY_PROCEDURE_NM"))
info <- info[!duplicated(LOG_ID)]
rates <- fread("/workspace/sens_out/procedure_icu_rates.csv")
# rebuild exclusion list at 90% threshold (>=20 cases)
excl_procs <- rates[icu_rate >= 0.9 & n >= 20, PRIMARY_PROCEDURE_NM]
cat("routine-ICU procedures excluded (S1-90):", length(excl_procs), "\n")
mov3 <- merge(mov2, info, by = "LOG_ID", all.x = TRUE)
mov3 <- mov3[!PRIMARY_PROCEDURE_NM %in% excl_procs]
add("MOVER", "exclude routine-ICU procedures", "n", nrow(mov3))
add("MOVER", "exclude routine-ICU procedures", "AUROC_composite",
    auc_ci(as.integer(mov3$outcome == 1), mov3$XGB_full))
add("MOVER", "exclude routine-ICU procedures", "AUROC_icu_dependent",
    auc_ci(as.integer(mov3$category == "icu_dependent"), mov3$XGB_full))
add("MOVER", "exclude routine-ICU procedures", "AUROC_any_true_need",
    auc_ci(as.integer(mov3$category %in% c("icu_dependent", "missed_escalation")),
           mov3$XGB_full))

# ---------- S-c: percentile middle band (P10-P90 of need-recalibrated risk) ----------
for (nm in c("inspire", "mover")) {
  d <- fread(file.path(OUT, sprintf("preds_need_recal_%s.csv", nm)))
  lo <- quantile(d$xgb_need, 0.10); hi <- quantile(d$xgb_need, 0.90)
  mid <- d[xgb_need >= lo & xgb_need < hi]
  adm <- mid[category %in% c("icu_dependent", "observational_icu")]
  notadm <- mid[!category %in% c("icu_dependent", "observational_icu")]
  add(toupper(nm), "middle band P10-P90", "pct_of_cohort",
      round(100 * nrow(mid) / nrow(d), 1))
  add(toupper(nm), "middle band P10-P90", "admitted_pct",
      round(100 * nrow(adm) / nrow(mid), 1))
  add(toupper(nm), "middle band P10-P90", "dep_among_admitted_pct",
      round(100 * mean(adm$category == "icu_dependent"), 1))
  add(toupper(nm), "middle band P10-P90", "missed_among_notadm_pct",
      round(100 * mean(notadm$category == "missed_escalation"), 2))
}

s <- rbindlist(res)
fwrite(s, file.path(OUT, "table_s_b5_sensitivity.csv"))
print(s)
cat("== 40 done ==\n")
