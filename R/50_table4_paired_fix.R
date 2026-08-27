# 50_table4_paired_fix.R — rebuild Table 4 with SASA-evaluable denominators and
# paired tier comparison, and rebuild Table S8 as a paired complete-case table.
# Also fixes DeLong direction for the dep_vs_obs contrast (XGB AUROC < 0.5 in INSPIRE).
suppressPackageStartupMessages({library(data.table); library(pROC)})

OUT <- "/mnt/results/06_icu_course"

auc_ci <- function(truth, pred) {
  r <- pROC::roc(truth, pred, quiet = TRUE, direction = "<")
  ci <- as.numeric(pROC::ci.auc(r))
  sprintf("%.3f (%.3f-%.3f)", ci[2], ci[1], ci[3])
}
delong_p <- function(truth, p1, p2) {
  r1 <- pROC::roc(truth, p1, quiet = TRUE, direction = "<")
  r2 <- pROC::roc(truth, p2, quiet = TRUE, direction = "<")
  pROC::roc.test(r1, r2, method = "delong")$p.value
}

t4 <- fread(file.path(OUT, "table4_triage_pathway.csv"))
t4[, `:=`(pct_of_evaluable = NA_real_, denominator = NA_character_)]
s8 <- list()

for (cc in c("inspire", "mover")) {
  d <- fread(file.path(OUT, sprintf("preds_need_recal_%s.csv", cc)))
  cohort <- toupper(cc)
  ev <- d[!is.na(SASA)]
  n_ev <- nrow(ev)
  cat(sprintf("\n== %s: SASA-evaluable %s / %s (%.1f%%); composite rate evaluable %.2f%% vs missing %.2f%% ==\n",
              cohort, format(n_ev, big.mark = ","), format(nrow(d), big.mark = ","),
              100 * n_ev / nrow(d), 100 * ev[, mean(outcome)], 100 * d[is.na(SASA), mean(outcome)]))

  # --- Table 4: SASA rows get evaluable-denominator percentages ---
  for (tr in c("low_<5%", "middle_5-30%", "high_>30%")) {
    n_tr <- ev[sasa_tier == tr, .N]
    t4[cohort == cohort & stratifier == "SASA (need-recal)" & tier == tr,
       `:=`(pct_of_evaluable = round(100 * n_tr / n_ev, 1),
            denominator = sprintf("%s SASA-evaluable ops (%.1f%% of cohort)", format(n_ev, big.mark = ","), 100 * n_ev / nrow(d)))]
  }
  t4[cohort == cohort & stratifier == "XGB_full (need-recal)",
     denominator := sprintf("all %s ops", format(nrow(d), big.mark = ","))]

  # --- paired tier comparison on the SASA-evaluable subset: XGB tiers vs SASA tiers ---
  cat("  -- on SASA-evaluable subset --\n")
  for (st in c("tier", "sasa_tier")) {
    tab <- ev[, .(n = .N,
                  any_need = mean(any_need) * 100,
                  died = mean(died) * 100,
                  npv = (1 - mean(any_need)) * 100), by = get(st)][order(get)]
    print(tab)
  }

  # --- Table S8 paired: refined AUROCs, XGB full-cohort + XGB/SASA evaluable + DeLong p ---
  mk <- function(contrast, sub_all, sub_ev, yvar) {
    s8[[length(s8) + 1]] <<- data.table(
      cohort = cohort, contrast = contrast,
      n_full = nrow(sub_all), n_evaluable = nrow(sub_ev),
      XGB_full_cohort = auc_ci(sub_all[[yvar]], sub_all$XGB_full),
      XGB_evaluable = auc_ci(sub_ev[[yvar]], sub_ev$XGB_full),
      SASA_evaluable = auc_ci(sub_ev[[yvar]], sub_ev$SASA),
      DeLong_p_XGB_vs_SASA = signif(delong_p(sub_ev[[yvar]], sub_ev$XGB_full, sub_ev$SASA), 3))
  }
  d[, `:=`(dep = as.integer(category == "icu_dependent"),
           an = as.integer(category %in% c("icu_dependent", "missed_escalation")))]
  ev[, `:=`(dep = as.integer(category == "icu_dependent"),
            an = as.integer(category %in% c("icu_dependent", "missed_escalation")))]
  mk("composite", d, ev, "outcome")
  mk("icu_dependent", d, ev, "dep")
  mk("any_true_need", d, ev, "an")
  ow_a <- d[category %in% c("observational_icu", "uncomplicated_ward")][, y := as.integer(category == "observational_icu")]
  ow_e <- ev[category %in% c("observational_icu", "uncomplicated_ward")][, y := as.integer(category == "observational_icu")]
  mk("obsICU_vs_ward", ow_a, ow_e, "y")
  ad_a <- d[category %in% c("icu_dependent", "observational_icu")][, y := as.integer(category == "icu_dependent")]
  ad_e <- ev[category %in% c("icu_dependent", "observational_icu")][, y := as.integer(category == "icu_dependent")]
  mk("dep_vs_obs_among_admitted", ad_a, ad_e, "y")
}

fwrite(t4, file.path(OUT, "table4_triage_pathway.csv"))
s8dt <- rbindlist(s8)
fwrite(s8dt, file.path(OUT, "table_b2_refined_auroc.csv"))
cat("\n== saved table4_triage_pathway.csv and table_b2_refined_auroc.csv (paired) ==\n")
print(s8dt)
