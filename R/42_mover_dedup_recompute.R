#!/usr/bin/env Rscript
# 42_mover_dedup_recompute.R — v4: deduplicate MOVER by LOG_ID (keep first) and
# recompute all MOVER evaluation products on the deduped cohort (n = 48,370):
#   metrics (none/intercept/platt) for SASA, LR_clinical, LR_full, XGB_full, XGB_noArt
#   10-fold CV intercept/Platt predictions (LR_clinical, LR_full, XGB_full)
#   500x split-sample validation of the Platt procedure (LR_clinical, LR_full, XGB_full)
# Mirrors 05_external_eval.R / 10_recal_cv.R / 11_sensitivity_noart.R conventions.
suppressPackageStartupMessages({
  library(data.table); library(pROC); library(PRROC)
})

EXT <- "/workspace/ext_out"
set.seed(20250825)

# ---------- dedup ----------
a  <- as.data.table(readRDS("/mnt/shared-workspace/shared/analysis_mover.rds"))
p  <- as.data.table(readRDS(file.path(EXT, "preds_mover.rds")))
stopifnot(nrow(a) == 49394, nrow(p) == 49394)
keep <- !duplicated(a$LOG_ID)
ad <- a[keep]
pd <- p[match(ad$LOG_ID, p$LOG_ID)]   # align preds to deduped analysis rows
stopifnot(nrow(ad) == 48370, nrow(pd) == 48370,
          identical(ad$LOG_ID, pd$LOG_ID),
          all(ad$outcome == pd$outcome))
cat("dedup:", nrow(ad), "ops,", sum(ad$outcome), "events (",
    sprintf("%.1f", 100 * mean(ad$outcome)), "%),",
    length(unique(ad$MRN)), "patients\n")
cat("usable TS (!is.na coverage):", sum(!is.na(ad$coverage)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(ad$coverage))), "\n")
saveRDS(ad, file.path(EXT, "analysis_mover_dedup.rds"))
saveRDS(pd, file.path(EXT, "preds_mover_dedup.rds"))

# ---------- XGB_noArt predictions on deduped cohort ----------
# model_noart.rds (from 11_sensitivity_noart.R) stores pred_mover aligned to the
# original analysis_mover.rds row order -> subset with the same dedup mask.
fit_noart <- readRDS("/workspace/model_noart.rds")
stopifnot(length(fit_noart$pred_mover) == 49394)
pd[, XGB_noArt := fit_noart$pred_mover[keep]]
saveRDS(pd, file.path(EXT, "preds_mover_dedup.rds"))  # re-save with noArt column

# ---------- metrics ----------
eval_binary <- function(y01, p) {
  ok <- !is.na(p); y2 <- y01[ok]; p2 <- p[ok]
  r <- roc(y2, p2, quiet = TRUE)
  ci <- as.numeric(ci.auc(r, method = "delong"))
  pr <- pr.curve(scores.class0 = p2[y2 == 1], scores.class1 = p2[y2 == 0], curve = FALSE)
  lp <- qlogis(pmin(pmax(p2, 1e-6), 1 - 1e-6))
  list(AUROC = ci[2], lo = ci[1], hi = ci[3], AUPRC = pr$auc.integral,
       Brier = mean((p2 - y2)^2),
       cal_intercept = unname(coef(glm(y2 ~ offset(lp), family = binomial))[1]),
       cal_slope = unname(coef(glm(y2 ~ lp, family = binomial))[2]),
       n = length(y2), events = sum(y2))
}
recal_fun <- function(p_tr, y_tr, p_te, mode) {
  lp_tr <- qlogis(pmin(pmax(p_tr, 1e-6), 1 - 1e-6))
  lp_te <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  if (mode == "intercept") {
    a0 <- unname(coef(glm(y_tr ~ offset(lp_tr), family = binomial))[1])
    return(plogis(lp_te + a0))
  }
  cf <- unname(coef(glm(y_tr ~ lp_tr, family = binomial)))
  plogis(cf[1] + cf[2] * lp_te)
}

y <- pd$outcome
models <- c("SASA", "LR_clinical", "LR_full", "XGB_full", "XGB_noArt")
met <- list()
for (m in models) {
  pr <- pd[[m]]
  e <- eval_binary(y, pr)
  met[[length(met) + 1]] <- cbind(data.table(cohort = "MOVER", model = m,
                                             recalibration = "none"), as.data.table(e))
  for (mode in c("intercept", "platt")) {
    pr2 <- recal_fun(pr, y, pr, mode)   # in-sample recal for point metrics (as before)
    e2 <- eval_binary(y, pr2)
    met[[length(met) + 1]] <- cbind(data.table(cohort = "MOVER", model = m,
                                               recalibration = mode), as.data.table(e2))
  }
  cat("metrics:", m, "done\n")
}
met <- rbindlist(met)
fwrite(met, file.path(EXT, "metrics_mover_dedup.csv"))

# ---------- 10-fold CV recalibrated probabilities (seed 42, as 10_recal_cv.R) ----------
set.seed(42)
K <- 10
fold_id <- sample(rep(1:K, length.out = length(y)))
cvp <- copy(pd)
for (m in c("LR_clinical", "LR_full", "XGB_full")) {
  pr <- pd[[m]]
  for (mode in c("intercept", "platt")) {
    pcv <- numeric(length(y))
    for (k in 1:K) {
      te <- which(fold_id == k); tr <- which(fold_id != k)
      pcv[te] <- recal_fun(pr[tr], y[tr], pr[te], mode)
    }
    cvp[, paste0(m, "_", mode, "_cv") := pcv]
  }
}
saveRDS(cvp, file.path(EXT, "preds_mover_cvrecal_dedup.rds"))
cat("CV recal preds saved\n")

# ---------- 500x split-sample validation of Platt (seed 123, as 10_recal_cv.R) ----------
eval_cal <- function(y_te, p_te) {
  lp2 <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  c(Brier = mean((p_te - y_te)^2),
    cal_intercept = unname(coef(glm(y_te ~ offset(lp2), family = binomial))[1]),
    cal_slope = unname(coef(glm(y_te ~ lp2, family = binomial))[2]))
}
set.seed(123)
B <- 500; n <- length(y)
ss <- list()
for (m in c("LR_clinical", "LR_full", "XGB_full")) {
  pr <- pd[[m]]
  mat <- t(replicate(B, {
    tr <- sample(n, floor(n / 2)); te <- setdiff(seq_len(n), tr)
    eval_cal(y[te], recal_fun(pr[tr], y[tr], pr[te], "platt"))
  }))
  ss[[m]] <- data.table(model = m, recalibration = "platt",
                        Brier_mean = mean(mat[, "Brier"]),
                        Brier_lo = quantile(mat[, "Brier"], 0.025),
                        Brier_hi = quantile(mat[, "Brier"], 0.975),
                        cal_slope_mean = mean(mat[, "cal_slope"]),
                        cal_slope_lo = quantile(mat[, "cal_slope"], 0.025),
                        cal_slope_hi = quantile(mat[, "cal_slope"], 0.975))
  cat("split-sample:", m, "done\n")
}
ss <- rbindlist(ss)
fwrite(ss, file.path(EXT, "mover_recal_splitsample_dedup.csv"))
print(ss)
cat("== 42 done ==\n")
