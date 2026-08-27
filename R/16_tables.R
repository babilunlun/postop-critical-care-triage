#!/usr/bin/env Rscript
# Assemble manuscript Table 1 (cohort characteristics, modeling cohorts) and
# Table 2 (model performance with 95% CI). All numbers from verified artifacts.
suppressPackageStartupMessages({
  library(data.table); library(tableone); library(pROC); library(tidymodels)
})

OUT <- "/workspace/tables"
dir.create(OUT, showWarnings = FALSE)

# ================= Table 1: VitalDB side on modeling cohort (n = 5,987) =================
v <- readRDS("/mnt/shared-workspace/shared/analysis_vitaldb.rds")
g <- readRDS("/workspace/gru/gru_data.rds")
v <- v[caseid %in% g$caseid]                       # drop caseid 2274, 3066
stopifnot(nrow(v) == 5987, sum(v$outcome) == 1184)

tab1_vars <- c("age", "sex", "bmi", "asa", "emop", "department", "preop_htn",
               "preop_dm", "preop_hb", "preop_plt", "preop_na", "preop_k",
               "preop_gluc", "preop_alb", "preop_ast", "preop_alt",
               "preop_bun", "preop_cr", "ane_dur_min",
               "intraop_ebl", "intraop_crystalloid", "intraop_rbc",
               "map_mean", "min_map65", "area_map65", "hr_mean", "spo2_min",
               "bis_mean")
tab1 <- CreateTableOne(vars = tab1_vars, strata = "outcome",
                       data = as.data.frame(v),
                       factorVars = c("sex", "asa", "emop", "department",
                                      "preop_htn", "preop_dm"),
                       addOverall = TRUE)
tab1_mat <- print(tab1, printToggle = FALSE, nonnormal = c("intraop_ebl",
                  "intraop_crystalloid", "min_map65", "area_map65", "ane_dur_min"))
write.csv(tab1_mat, file.path(OUT, "table1_vitaldb_modeling.csv"))
cat("VitalDB table1 regenerated on n =", nrow(v), "\n")

# ================= AUROC 95% CI (DeLong) for models lacking them =================
ci_row <- function(y, p, cohort, model) {
  r <- roc(y, p, quiet = TRUE)
  ci <- as.numeric(ci.auc(r, method = "delong"))
  data.frame(cohort = cohort, model = model,
             AUROC = round(ci[2], 3), CI_lo = round(ci[1], 3), CI_hi = round(ci[3], 3))
}

tp  <- fread("/mnt/shared-workspace/shared/model_out/test_predictions.csv")
pg  <- fread("/mnt/shared-workspace/shared/preds_gru.csv")
pm  <- as.data.table(readRDS("/mnt/shared-workspace/shared/preds_mover.rds"))

# XGB_noArt: predictions were saved inside model_noart.rds
noart <- readRDS("/workspace/gru/out/model_noart.rds")
p_noart_int <- noart$pred_test
p_noart_ext <- noart$pred_mover
setorder(v, caseid); ntr <- floor(0.7 * nrow(v))
test <- v[(ntr + 1):nrow(v)]
stopifnot(nrow(test) == 1797, length(p_noart_int) == 1797)
ext <- as.data.table(readRDS("/mnt/shared-workspace/shared/analysis_mover.rds"))
stopifnot(nrow(ext) == 49394, length(p_noart_ext) == 49394)

ci_new <- rbind(
  ci_row(tp$outcome, tp$XGB_full, "VitalDB (internal test)", "XGB_full"),  # sanity vs existing
  ci_row(test$outcome, p_noart_int, "VitalDB (internal test)", "XGB_noArt"),
  ci_row(pg$outcome, pg$gru_seq_ens, "VitalDB (internal test)", "GRU_seq_ens"),
  ci_row(pg$outcome, pg$gru_full_ens, "VitalDB (internal test)", "GRU_full_ens"),
  ci_row(pm$outcome, pm$XGB_full, "MOVER (external)", "XGB_full"),          # sanity
  ci_row(ext$outcome, p_noart_ext, "MOVER (external)", "XGB_noArt")
)
print(ci_new)
fwrite(ci_new, file.path(OUT, "auroc_ci_new.csv"))

# ================= Table 2: model performance =================
m_int   <- fread("/mnt/results/01_internal_validation/metrics_test.csv")
m_gru   <- fread("/mnt/results/03_gru_comparator/metrics_gru.csv")
m_ext   <- fread("/mnt/results/02_external_validation/metrics_mover.csv")
m_cvr   <- fread("/mnt/results/02_external_validation/metrics_mover_recal_cv.csv")
ci_old  <- fread("/mnt/results/02_external_validation/auroc_ci.csv")

ci_all <- rbindlist(list(
  ci_old[, .(cohort, model, AUROC, CI_lo, CI_hi)],
  { dt <- as.data.table(ci_new)[model %in% c("XGB_noArt", "GRU_seq_ens", "GRU_full_ens")]
    rec <- c(GRU_seq_ens = "gru_seq_ens", GRU_full_ens = "gru_full_ens")
    dt[, model := ifelse(model %in% names(rec), rec[model], model)]
    dt[, .(cohort, model, AUROC, CI_lo, CI_hi)] }
), fill = TRUE)
ci_all[, ci_str := sprintf("%.3f (%.3f\u2013%.3f)", AUROC, CI_lo, CI_hi)]

fmt <- function(x, d = 3) ifelse(is.na(x), NA, formatC(x, format = "f", digits = d))

# --- Panel A: internal test ---
gru_int <- m_gru[cohort == "VitalDB_test" & model %in% c("XGB_noArt", "gru_seq_ens", "gru_full_ens")]
panelA <- rbindlist(list(
  m_int[, .(model, AUROC, AUPRC, Brier, cal_intercept, cal_slope)],
  gru_int[, .(model, AUROC, AUPRC, Brier, cal_intercept, cal_slope)]
), fill = TRUE)
panelA[, cohort := "VitalDB internal test (n = 1,797)"]

# --- Panel B: external, no recalibration ---
panelB <- m_ext[recalibration == "none",
                .(model, AUROC, AUPRC, Brier, cal_intercept, cal_slope)]
panelB[, cohort := "MOVER external (n = 49,394)"]
# add XGB_noArt external
noart_ext <- m_gru[model == "XGB_noArt_mover",
                   .(model = "XGB_noArt", AUROC, AUPRC, Brier, cal_intercept, cal_slope)]
noart_ext[, cohort := "MOVER external (n = 49,394)"]
panelB <- rbindlist(list(panelB, noart_ext), fill = TRUE)

# --- Panel C: external after Platt recalibration (500x split-sample validated) ---
panelC <- m_cvr[model %in% c("LR_clinical", "LR_full", "XGB_full") & recalibration == "platt",
                .(model, Brier_mean, Brier_lo, Brier_hi,
                  cal_slope_mean, cal_slope_lo, cal_slope_hi)]
panelC[, `:=`(cohort = "MOVER external, Platt-recalibrated (500\u00d7 split-sample)",
              AUROC = NA_real_, AUPRC = NA_real_)]
# AUROC unchanged by monotone recalibration; attach from panelB
auc_map <- panelB[, .(model, AUROC)]
panelC <- merge(panelC, auc_map, by = "model")

mk_row <- function(cohort, model, auroc_ci, auprc, brier, cal_int, cal_slope, note = "") {
  data.table(Cohort = cohort, Model = model, `AUROC (95% CI)` = auroc_ci,
             AUPRC = auprc, Brier = brier, `Cal intercept` = cal_int,
             `Cal slope` = cal_slope, Note = note)
}

rows <- list()
ordA <- c("SASA", "LR_clinical", "LR_full", "XGB_full", "XGB_noArt",
          "gru_seq_ens", "gru_full_ens")
lbl <- c(SASA = "SASA (reference)", LR_clinical = "LR (clinical)",
         LR_full = "LR (full)", XGB_full = "XGBoost (full, primary)",
         XGB_noArt = "XGBoost (no arterial-line flag)",
         gru_seq_ens = "GRU (sequences only, 3-seed ensemble)",
         gru_full_ens = "GRU (sequences + static, 3-seed ensemble)")
for (mdl in ordA) {
  r <- panelA[model == mdl]
  ci_str <- ci_all[cohort == "VitalDB (internal test)" & model == mdl, ci_str]
  if (length(ci_str) == 0) ci_str <- fmt(r$AUROC)
  rows[[length(rows) + 1]] <- mk_row("VitalDB internal test", lbl[[mdl]], ci_str,
                                     fmt(r$AUPRC), fmt(r$Brier),
                                     fmt(r$cal_intercept), fmt(r$cal_slope))
}
ordB <- c("SASA", "LR_clinical", "LR_full", "XGB_full", "XGB_noArt")
for (mdl in ordB) {
  r <- panelB[model == mdl]
  ci_str <- ci_all[cohort == "MOVER (external)" & model == mdl, ci_str]
  if (length(ci_str) == 0) ci_str <- fmt(r$AUROC)
  rows[[length(rows) + 1]] <- mk_row("MOVER external (no recalibration)", lbl[[mdl]],
                                     ci_str, fmt(r$AUPRC), fmt(r$Brier),
                                     fmt(r$cal_intercept), fmt(r$cal_slope))
}
for (mdl in c("LR_clinical", "LR_full", "XGB_full")) {
  r <- panelC[model == mdl]
  ci_str <- ci_all[cohort == "MOVER (external)" & model == mdl, ci_str]
  rows[[length(rows) + 1]] <- mk_row(
    "MOVER external (Platt recalibrated)", lbl[[mdl]], ci_str, "\u2014",
    sprintf("%.3f (%.3f\u2013%.3f)", r$Brier_mean, r$Brier_lo, r$Brier_hi),
    "0.00 (by design)",
    sprintf("%.3f (%.3f\u2013%.3f)", r$cal_slope_mean, r$cal_slope_lo, r$cal_slope_hi),
    "split-sample validated (500\u00d7)")
}
table2 <- rbindlist(rows)
print(table2, nrows = 30)
fwrite(table2, file.path(OUT, "table2_performance.csv"))

# ================= Combined Table 1 (VitalDB modeling + MOVER) =================
t1v <- fread(file.path(OUT, "table1_vitaldb_modeling.csv"), header = TRUE)
t1m <- fread("/mnt/results/02_external_validation/table1_mover.csv", header = TRUE)
cat("t1v cols:", paste(names(t1v), collapse = " | "), "\n")
cat("t1m cols:", paste(names(t1m), collapse = " | "), "\n")
fwrite(t1v, file.path(OUT, "table1_vitaldb_side.csv"))
cat("ALL DONE\n")
