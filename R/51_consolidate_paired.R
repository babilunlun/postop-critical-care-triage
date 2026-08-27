# 51_consolidate_paired.R — fix Table 4 denominators (per-cohort), build consolidated
# Supplementary Table S11 (paired SASA-evaluable analyses), and add paired in-band
# columns to Table S9 (middle band).
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
s11 <- list()
b3 <- fread(file.path(OUT, "table_b3_middle_band.csv"))
b3[, `:=`(band_evaluable_n = NA_integer_, auroc_any_need_inband_xgb_evaluable = NA_character_,
          delong_p_inband_xgb_vs_sasa = NA_character_)]

for (cc in c("inspire", "mover")) {
  cc2 <- toupper(cc)
  d <- fread(file.path(OUT, sprintf("preds_need_recal_%s.csv", cc)))
  ev <- d[!is.na(SASA)]
  n_all <- nrow(d); n_ev <- nrow(ev)

  # --- Table 4 denominators (per cohort) ---
  t4[cohort == cc2 & stratifier == "XGB_full (need-recal)",
     denominator := sprintf("all %s ops", format(n_all, big.mark = ","))]
  for (tr in c("low_<5%", "middle_5-30%", "high_>30%")) {
    n_tr <- ev[sasa_tier == tr, .N]
    t4[cohort == cc2 & stratifier == "SASA (need-recal)" & tier == tr,
       `:=`(pct_of_evaluable = round(100 * n_tr / n_ev, 1),
            denominator = sprintf("%s SASA-evaluable ops (%.1f%% of cohort)",
                                  format(n_ev, big.mark = ","), 100 * n_ev / n_all))]
  }

  # --- S11 block 1: informative missingness ---
  s11[[length(s11) + 1]] <- data.table(
    block = "sasa_evaluability", cohort = cc2, metric = "SASA evaluable",
    value = sprintf("%s / %s (%.1f%%)", format(n_ev, big.mark = ","), format(n_all, big.mark = ","),
                    100 * n_ev / n_all))
  s11[[length(s11) + 1]] <- data.table(
    block = "sasa_evaluability", cohort = cc2, metric = "composite rate, SASA-evaluable vs SASA-missing",
    value = sprintf("%.2f%% vs %.2f%%", 100 * ev[, mean(outcome)], 100 * d[is.na(SASA), mean(outcome)]))

  # --- S11 block 2: paired AUROCs (from table_b2, already paired) ---
  s8 <- fread(file.path(OUT, "table_b2_refined_auroc.csv"))[cohort == cc2]
  for (j in seq_len(nrow(s8))) {
    s11[[length(s11) + 1]] <- data.table(
      block = "paired_auroc", cohort = cc2,
      metric = paste0(s8$contrast[j], " (XGB full cohort | XGB evaluable | SASA evaluable; DeLong p)"),
      value = sprintf("%s | %s | %s; p=%.3g", s8$XGB_full_cohort[j], s8$XGB_evaluable[j],
                      s8$SASA_evaluable[j], s8$DeLong_p_XGB_vs_SASA[j]))
  }

  # --- S11 block 3: tier comparison on the SASA-evaluable subset ---
  for (st in c("tier", "sasa_tier")) {
    tab <- ev[, .(n = .N, any_need = mean(any_need) * 100, died = mean(died) * 100,
                  npv = (1 - mean(any_need)) * 100), by = get(st)]
    setnames(tab, "get", "tier")
    for (j in seq_len(nrow(tab))) {
      s11[[length(s11) + 1]] <- data.table(
        block = "tiers_on_sasa_evaluable", cohort = cc2,
        metric = sprintf("%s | %s", ifelse(st == "tier", "XGB_full (need-recal)", "SASA (need-recal)"), tab$tier[j]),
        value = sprintf("n=%s (%.1f%% of evaluable); any-need %.2f%%; died %.2f%%; NPV %.2f%%",
                        format(tab$n[j], big.mark = ","), 100 * tab$n[j] / n_ev,
                        tab$any_need[j], tab$died[j], tab$npv[j]))
    }
  }

  # --- S9: paired in-band columns ---
  band <- d[tier == "middle_5-30%"]; band_ev <- band[!is.na(SASA)]
  b3[cohort == cc2,
     `:=`(band_evaluable_n = nrow(band_ev),
          auroc_any_need_inband_xgb_evaluable = auc_ci(band_ev$any_need, band_ev$xgb_need),
          delong_p_inband_xgb_vs_sasa = sprintf("%.3g", delong_p(band_ev$any_need, band_ev$xgb_need, band_ev$sasa_need)))]
}

fwrite(t4, file.path(OUT, "table4_triage_pathway.csv"))
fwrite(rbindlist(s11), file.path(OUT, "table_s11_paired_sasa.csv"))
fwrite(b3, file.path(OUT, "table_b3_middle_band.csv"))
cat("== saved table4, table_s11_paired_sasa.csv, table_b3_middle_band.csv ==\n")
print(t4[cohort == "INSPIRE", .(stratifier, tier, n, pct, pct_of_evaluable, denominator)])
print(b3[, .(cohort, n, band_evaluable_n, auroc_any_need_inband_xgb, auroc_any_need_inband_xgb_evaluable,
             auroc_any_need_inband_sasa, delong_p_inband_xgb_vs_sasa)])
