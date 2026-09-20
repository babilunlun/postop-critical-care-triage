#!/usr/bin/env Rscript
# 38_refined_outcome.R — four-category ICU-course–validated outcome classification
# (plan Part A3) + B1 decomposition table + B2 model discrimination vs refined outcomes.
#
# Categories (mutually exclusive, first match wins):
#   1 ICU-dependent (true need):  early ICU + >=1 ICU-level intervention, or early ICU + death
#        INSPIRE: early_icu & (icu_intervention | death_idx)
#        MOVER:   icu_flag  & (icu_intervention | expired)
#   2 Observational ICU:          early ICU, no intervention, alive
#   3 Missed escalation:          no early ICU, but delayed ICU (INSPIRE only) or death
#        MOVER has no ICU timestamps -> death-only (disclosed asymmetry)
#   4 Uncomplicated ward:         none of the above
#
# Outputs: /mnt/results/06_icu_course/refined_categories_{inspire,mover}.csv (per-op),
#          table3_outcome_decomposition.csv, table_b2_refined_auroc.csv

suppressPackageStartupMessages({library(data.table); library(pROC)})
DIR <- "/workspace/inspire"
OUT <- "/mnt/results/06_icu_course"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

cat4 <- c("icu_dependent", "observational_icu", "missed_escalation", "uncomplicated_ward")

# ================= INSPIRE =================
ic <- readRDS(file.path(DIR, "inspire_icu_course.rds"))
pr <- readRDS(file.path(DIR, "inspire_cv_preds.rds"))   # primary cohort only
ins <- merge(pr, ic[, .(op_id, early_icu, delayed_icu, death_idx, icu_intervention,
                        icu_vent, icu_vaso, icu_crrt, icu_ecmo, icu_iabp, icu_los_h)],
             by.x = "LOG_ID", by.y = "op_id", all.x = TRUE, sort = FALSE)
stopifnot(!any(is.na(ins$early_icu)))
ins[, category := fcase(
  early_icu & (icu_intervention == 1 | death_idx), "icu_dependent",
  early_icu, "observational_icu",
  !early_icu & (delayed_icu | death_idx), "missed_escalation",
  default = "uncomplicated_ward")]
ins[, category := factor(category, levels = cat4)]
cat("=== INSPIRE primary cohort ===\n"); print(ins[, .N, by = category][order(category)])

# ================= MOVER =================
mc <- readRDS("/workspace/ext_out/mover_icu_course.rds")
pm <- readRDS("/workspace/ext_out/preds_mover_cvrecal.rds")
mov <- merge(pm, mc[, .(LOG_ID, icu_flag, expired, icu_intervention, postop_vent,
                        postop_vaso, icu_intervention_pluspcs)],
             by = "LOG_ID", all.x = TRUE, sort = FALSE)
stopifnot(!any(is.na(mov$icu_flag)))
# MOVER patient_information contains 1,024 duplicate LOG_ID rows (administrative
# double-entries; outcomes verified 100% concordant). Collapse to unique operations.
setorder(mov, LOG_ID)
mov <- mov[!duplicated(LOG_ID)]
cat("MOVER after LOG_ID dedup:", nrow(mov), "\n")
mov[, category := fcase(
  icu_flag == 1 & (icu_intervention == 1 | expired == 1), "icu_dependent",
  icu_flag == 1, "observational_icu",
  icu_flag == 0 & expired == 1, "missed_escalation",
  default = "uncomplicated_ward")]
mov[, category := factor(category, levels = cat4)]
cat("=== MOVER cohort ===\n"); print(mov[, .N, by = category][order(category)])

# ================= B1: decomposition table =================
ins[, died := as.integer(death_idx)]
mov[, died := as.integer(expired == 1)]
decomp <- function(d, cohort) {
  n <- nrow(d)
  rbindlist(lapply(cat4, function(k) {
    sub <- d[category == k]
    data.table(cohort = cohort, category = k, n = nrow(sub),
      pct = round(100 * nrow(sub) / n, 2),
      pct_of_composite = round(100 * nrow(sub) / sum(d$outcome), 1),
      died = sum(sub$died == 1, na.rm = TRUE),
      median_pred_risk = round(median(sub$XGB_full), 3),
      median_pred_risk_recal = round(median(sub$XGB_full_platt_cv), 3))
  }))
}
b1 <- rbind(decomp(ins, "INSPIRE"), decomp(mov, "MOVER"))
b1[, composite_event := category %in% c("icu_dependent", "observational_icu", "missed_escalation")]
fwrite(b1, file.path(OUT, "table3_outcome_decomposition.csv"))
print(b1)

# ================= B2: discrimination vs refined outcomes =================
auc_ci <- function(y, p) {
  if (length(unique(y)) < 2) return(c(NA, NA, NA))
  ci <- ci.auc(roc(y, p, quiet = TRUE), method = "delong")
  c(AUROC = as.numeric(ci[2]), lo = as.numeric(ci[1]), hi = as.numeric(ci[3]))
}
b2_one <- function(d, cohort) {
  d[, `:=`(
    y_composite = as.integer(outcome == 1),
    y_icu_dep = as.integer(category == "icu_dependent"),
    y_any_need = as.integer(category %in% c("icu_dependent", "missed_escalation")),
    y_obs_vs_ward = NA_integer_
  )]
  d2 <- d[category %in% c("observational_icu", "uncomplicated_ward")]
  d2_y <- as.integer(d2$category == "observational_icu")
  d3 <- d[category %in% c("icu_dependent", "observational_icu")]
  d3_y <- as.integer(d3$category == "icu_dependent")
  rbindlist(lapply(c("XGB_full", "SASA"), function(m) {
    a1 <- auc_ci(d$y_composite, d[[m]]); a2 <- auc_ci(d$y_icu_dep, d[[m]])
    a3 <- auc_ci(d$y_any_need, d[[m]]); a4 <- auc_ci(d2_y, d2[[m]])
    a5 <- auc_ci(d3_y, d3[[m]])
    data.table(cohort = cohort, model = m,
      composite = sprintf("%.3f (%.3f-%.3f)", a1[1], a1[2], a1[3]),
      icu_dependent = sprintf("%.3f (%.3f-%.3f)", a2[1], a2[2], a2[3]),
      any_true_need = sprintf("%.3f (%.3f-%.3f)", a3[1], a3[2], a3[3]),
      obsICU_vs_ward = sprintf("%.3f (%.3f-%.3f)", a4[1], a4[2], a4[3]),
      dep_vs_obs_among_admitted = sprintf("%.3f (%.3f-%.3f)", a5[1], a5[2], a5[3]))
  }))
}
b2 <- rbind(b2_one(ins, "INSPIRE"), b2_one(mov, "MOVER"))
fwrite(b2, file.path(OUT, "table_b2_refined_auroc.csv"))
print(b2)

# save per-op category files for downstream B3-B5
fwrite(ins[, .(LOG_ID, outcome, category, early_icu, delayed_icu, death_idx,
               icu_intervention, icu_vent, icu_vaso, icu_crrt, icu_ecmo, icu_iabp,
               icu_los_h, SASA, XGB_full, XGB_full_platt_cv, XGB_noArt,
               XGB_noArt_platt_cv, LR_full, LR_full_platt_cv)],
       file.path(OUT, "refined_categories_inspire.csv"))
fwrite(mov[, .(LOG_ID, outcome, category, icu_flag, expired, icu_intervention,
               postop_vent, postop_vaso, icu_intervention_pluspcs,
               SASA, XGB_full, XGB_full_platt_cv, LR_full, LR_full_platt_cv)],
       file.path(OUT, "refined_categories_mover.csv"))
cat("== 38 done ==\n")
