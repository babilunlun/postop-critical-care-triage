#!/usr/bin/env Rscript
# ============================================================================
# B6 — GRU-full vs XGBoost decision-curve comparison (internal test set)
# ============================================================================
# Revision analysis B6 (manuscript Results; Supplementary Table S6).
# Completes the pre-specified GRU comparator: discrimination + calibration +
# net benefit. Net benefit of the sequence-plus-static GRU ensemble versus
# XGBoost on the VitalDB temporal hold-out test set, thresholds 1-50%.
#
# Inputs:
#   - gru/out/preds_gru.csv (GRU ensemble predictions; step 14)
#   - gru/test_predictions.csv (XGBoost test predictions; step 03/14)
# Outputs:
#   - table_b6_gru_dca_internal.csv
#
# Paths refer to the original analysis workspace; adapt to your local layout.
# ============================================================================
suppressPackageStartupMessages(library(data.table))

OUT <- "/mnt/results/07_revision_analyses"
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)

pg <- fread("/workspace/gru/out/preds_gru.csv")          # caseid, outcome, gru_*_ens
tp <- fread("/workspace/gru/test_predictions.csv")       # caseid, outcome, XGB_full ...
stopifnot(identical(pg$caseid, tp$caseid), identical(pg$outcome, tp$outcome))
y <- pg$outcome; n <- length(y)
cat("test n =", n, " prevalence =", round(mean(y), 4), "\n")

nb <- function(p, pt) {  # net benefit per operation at threshold pt
  pred <- p >= pt
  tp_ <- sum(pred & y == 1); fp <- sum(pred & y == 0)
  tp_/n - fp/n * pt/(1 - pt)
}
pts <- seq(0.01, 0.50, by = 0.01)
dca <- rbindlist(lapply(pts, function(pt) data.table(
  pt = pt,
  XGB = nb(tp$XGB_full, pt),
  GRU_full = nb(pg$gru_full_ens, pt),
  treat_all = mean(y) - (1 - mean(y)) * pt/(1 - pt))))
dca[, gru_minus_xgb_per1000 := round((GRU_full - XGB) * 1000, 2)]
cat("\nNB per 1,000 operations at key thresholds:\n")
print(dca[pt %in% c(0.05, 0.10, 0.20, 0.30, 0.50),
          .(pt, XGB = round(XGB*1000,1), GRU_full = round(GRU_full*1000,1),
            treat_all = round(treat_all*1000,1), gru_minus_xgb_per1000)])
# where does GRU materially beat XGB (>2 net true positives per 1,000)?
rg <- dca[abs(gru_minus_xgb_per1000) > 2, range(pt)]
cat("\nthreshold range where |GRU-XGB| > 2 per 1,000:", ifelse(is.finite(rg[1]), paste(rg, collapse = "-"), "none"), "\n")
cat("max |diff| per 1,000:", max(abs(dca$gru_minus_xgb_per1000)), "at pt =", dca$pt[which.max(abs(dca$gru_minus_xgb_per1000))], "\n")
fwrite(dca, file.path(OUT, "tables", "table_b6_gru_dca_internal.csv"))
cat("DONE b6\n")
