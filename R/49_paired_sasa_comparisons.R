# 49_paired_sasa_comparisons.R — paired (complete-case) SASA vs XGBoost comparisons
# SASA requires estimated blood loss, which is informatively missing (~40% in both
# validation cohorts); pROC::roc() silently drops NA rows, so all SASA AUROCs are
# complete-case estimates. This script recomputes every SASA-vs-model contrast on the
# SASA-evaluable subset with paired DeLong tests, recomputes XGBoost on the same subset
# (selection-effect check), runs LR_full vs XGB_full DeLong tests, and rebuilds the
# SASA triage tiers with the evaluable denominator.
suppressPackageStartupMessages({library(data.table); library(pROC)})

OUT <- "/mnt/results/06_icu_course"

auc_ci <- function(truth, pred) {
  r <- pROC::roc(truth, pred, quiet = TRUE)
  ci <- as.numeric(pROC::ci.auc(r))
  sprintf("%.3f (%.3f-%.3f)", ci[2], ci[1], ci[3])
}

delong_p <- function(truth, p1, p2) {
  r1 <- pROC::roc(truth, p1, quiet = TRUE)
  r2 <- pROC::roc(truth, p2, quiet = TRUE)
  pROC::roc.test(r1, r2, method = "delong")$p.value
}

res <- list()
inband <- list()

for (cc in c("inspire", "mover")) {
  d <- fread(file.path(OUT, sprintf("preds_need_recal_%s.csv", cc)))
  cohort <- toupper(cc)
  ev <- d[!is.na(SASA)]
  cat(sprintf("\n== %s: cohort %s ops; SASA-evaluable %s (%.1f%%) ==\n",
              cohort, format(nrow(d), big.mark = ","), format(nrow(ev), big.mark = ","),
              100 * nrow(ev) / nrow(d)))

  # event rate by SASA evaluability (informative-missingness check)
  cat(sprintf("  composite rate: SASA-evaluable %.2f%% vs SASA-missing %.2f%%\n",
              100 * ev[, mean(outcome)], 100 * d[is.na(SASA), mean(outcome)]))

  # --- paired contrasts on the SASA-evaluable subset ---
  add <- function(contrast, sub_all, sub_ev, truth_expr, p_xgb = "XGB_full", p_sasa = "SASA") {
    y_all <- sub_all[[truth_expr]]; y_ev <- sub_ev[[truth_expr]]
    res[[length(res) + 1]] <<- data.table(
      cohort = cohort, contrast = contrast,
      n_full = nrow(sub_all), n_evaluable = nrow(sub_ev),
      xgb_full_cohort = auc_ci(y_all, sub_all[[p_xgb]]),
      xgb_evaluable = auc_ci(y_ev, sub_ev[[p_xgb]]),
      sasa_evaluable = auc_ci(y_ev, sub_ev[[p_sasa]]),
      delong_p_xgb_vs_sasa = signif(delong_p(y_ev, sub_ev[[p_xgb]], sub_ev[[p_sasa]]), 3))
  }

  add("composite", d, ev, "outcome")
  add("icu_dependent_vs_rest", d[, .(outcome, SASA, XGB_full, dep = as.integer(category == "icu_dependent"))],
      ev[, .(SASA, XGB_full, dep = as.integer(category == "icu_dependent"))], "dep")
  add("any_true_need", d[, .(SASA, XGB_full, an = as.integer(category %in% c("icu_dependent", "missed_escalation")))],
      ev[, .(SASA, XGB_full, an = as.integer(category %in% c("icu_dependent", "missed_escalation")))], "an")
  ow_all <- d[category %in% c("observational_icu", "uncomplicated_ward")]
  ow_ev  <- ev[category %in% c("observational_icu", "uncomplicated_ward")]
  add("obsICU_vs_ward", ow_all[, .(SASA, XGB_full, y = as.integer(category == "observational_icu"))],
      ow_ev[, .(SASA, XGB_full, y = as.integer(category == "observational_icu"))], "y")
  ad_all <- d[category %in% c("icu_dependent", "observational_icu")]
  ad_ev  <- ev[category %in% c("icu_dependent", "observational_icu")]
  add("dep_vs_obs_among_admitted", ad_all[, .(SASA, XGB_full, y = as.integer(category == "icu_dependent"))],
      ad_ev[, .(SASA, XGB_full, y = as.integer(category == "icu_dependent"))], "y")

  # --- LR_full vs XGB_full, full cohort (both fully predicted) ---
  p_lr <- delong_p(d$outcome, d$LR_full, d$XGB_full)
  cat(sprintf("  LR_full vs XGB_full composite DeLong p = %.3g (LR %s | XGB %s)\n",
              p_lr, auc_ci(d$outcome, d$LR_full), auc_ci(d$outcome, d$XGB_full)))
  res[[length(res) + 1]] <- data.table(
    cohort = cohort, contrast = "composite_LRfull_vs_XGBfull", n_full = nrow(d), n_evaluable = nrow(d),
    xgb_full_cohort = auc_ci(d$outcome, d$XGB_full), xgb_evaluable = auc_ci(d$outcome, d$LR_full),
    sasa_evaluable = NA_character_, delong_p_xgb_vs_sasa = signif(p_lr, 3))

  # --- in-band paired comparison (middle tier by xgb_need; SASA-evaluable members) ---
  band <- d[tier == "middle_5-30%"]
  band_ev <- band[!is.na(SASA)]
  p_in <- delong_p(band_ev$any_need, band_ev$xgb_need, band_ev$sasa_need)
  inband[[length(inband) + 1]] <- data.table(
    cohort = cohort, band_n = nrow(band), band_evaluable_n = nrow(band_ev),
    auroc_xgb_all_band = auc_ci(band$any_need, band$xgb_need),
    auroc_xgb_evaluable = auc_ci(band_ev$any_need, band_ev$xgb_need),
    auroc_sasa_evaluable = auc_ci(band_ev$any_need, band_ev$sasa_need),
    delong_p = signif(p_in, 3))
  cat(sprintf("  in-band paired: XGB %s (all) / %s (evaluable) vs SASA %s, DeLong p=%.3g\n",
              inband[[length(inband)]]$auroc_xgb_all_band, inband[[length(inband)]]$auroc_xgb_evaluable,
              inband[[length(inband)]]$auroc_sasa_evaluable, p_in))

  # --- SASA tiers with evaluable denominator ---
  st <- ev[, .N, by = sasa_tier][order(sasa_tier)]
  print(st)
}

paired <- rbindlist(res, fill = TRUE)
fwrite(paired, file.path(OUT, "table_s_paired_sasa.csv"))
inband_dt <- rbindlist(inband)
fwrite(inband_dt, file.path(OUT, "table_s_inband_paired.csv"))
cat("\n== saved table_s_paired_sasa.csv and table_s_inband_paired.csv ==\n")
print(paired)
print(inband_dt)
