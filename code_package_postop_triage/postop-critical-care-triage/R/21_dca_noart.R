#!/usr/bin/env Rscript
# Ablation DCA: clinical utility of XGB_full vs XGB_noArt (arterial-catheter variable removed)
#   Internal: VitalDB test set (n=1,797), raw probabilities (model well calibrated internally)
#   External: MOVER (n=49,394), raw AND 10-fold-CV Platt-recalibrated probabilities
# Threshold range 0.05-0.50. Net benefit via dcurves.

suppressPackageStartupMessages({
  library(data.table)
  library(pROC)
  library(dcurves)
})

OUT <- "/workspace/sens_out"
dir.create(OUT, showWarnings = FALSE)

# ---------- internal test set ----------
tp <- fread("/mnt/shared-workspace/shared/model_out/test_predictions.csv")
noart_test <- readRDS("/mnt/shared-workspace/shared/model_noart.rds")$pred_test
stopifnot(nrow(tp) == length(noart_test))
tp[, XGB_noArt := noart_test]

gate <- function(y, p, expect, tag, tol = 0.002) {
  a <- as.numeric(auc(roc(y, p, quiet = TRUE)))
  cat(sprintf("alignment %s: %.3f (expect %.3f)\n", tag, a, expect))
  stopifnot(abs(a - expect) < tol)
}
gate(tp$outcome, tp$XGB_full, 0.932, "internal XGB_full")
gate(tp$outcome, tp$XGB_noArt, 0.928, "internal XGB_noArt")

# ---------- external (MOVER) ----------
cv <- readRDS(file.path(OUT, "sens_cv_preds.rds"))$full  # full-cohort CV Platt preds
gate(cv$outcome, cv$XGB_full, 0.794, "external XGB_full raw")
gate(cv$outcome, cv$XGB_noArt, 0.754, "external XGB_noArt raw")
# sanity: CV-recalibrated probabilities should be well calibrated (slope ~ 1)
chk <- glm(cv$outcome ~ qlogis(pmin(pmax(cv$XGB_full_cvplatt, 1e-6), 1 - 1e-6)),
           family = binomial)
cat("external XGB_full CV-Platt slope:", round(coef(chk)[2], 3), "\n")

# ---------- DCA ----------
ths <- seq(0.05, 0.50, by = 0.01)

run_dca <- function(df, cohort_label) {
  d <- dca(outcome ~ XGB_full + XGB_noArt, data = as.data.frame(df),
           thresholds = ths)
  out <- as.data.table(d$dca)
  out[, cohort := cohort_label]
  out
}

dca_internal <- run_dca(tp[, .(outcome, XGB_full, XGB_noArt)], "internal")
dca_ext_raw  <- run_dca(cv[, .(outcome, XGB_full = XGB_full,
                               XGB_noArt = XGB_noArt)], "external_raw")
dca_ext_cal  <- run_dca(cv[, .(outcome, XGB_full = XGB_full_cvplatt,
                               XGB_noArt = XGB_noArt_cvplatt)], "external_recalibrated")

dca_all <- rbindlist(list(dca_internal, dca_ext_raw, dca_ext_cal))
fwrite(dca_all, file.path(OUT, "dca_noart_curves.csv"))

# ---------- net benefit at key thresholds (model rows only) ----------
for (cc in c("internal", "external_raw", "external_recalibrated")) {
  sub <- dca_all[cohort == cc & variable %in% c("XGB_full", "XGB_noArt") &
                 abs(round(threshold, 2) %% 0.1) < 1e-9 &
                 round(threshold, 2) %in% c(0.10, 0.20, 0.30)]
  cat("\n== ", cc, " ==\n")
  print(sub[, .(variable, threshold, net_benefit = round(net_benefit, 4))])
}
cat("DONE\n")
