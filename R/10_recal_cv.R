#!/usr/bin/env Rscript
# Honest assessment of post-recalibration performance on MOVER:
# (A) 10-fold cross-validated Platt/intercept recalibrated probabilities
#     (recalibration fit on 9 folds, applied to held-out fold)
# (B) repeated 50/50 split-sample validation of the full recalibration
#     procedure -> distribution of Brier / calibration intercept / slope
suppressPackageStartupMessages({
  library(data.table)
})

preds <- readRDS("/workspace/ext_out/preds_mover.rds")
y <- preds$outcome
model_cols <- c(LR_clinical = "LR_clinical", LR_full = "LR_full", XGB_full = "XGB_full")

recal_fun <- function(p_tr, y_tr, p_te, mode) {
  lp_tr <- qlogis(pmin(pmax(p_tr, 1e-6), 1 - 1e-6))
  lp_te <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  if (mode == "intercept") {
    a <- unname(coef(glm(y_tr ~ offset(lp_tr), family = binomial))[1])
    return(plogis(lp_te + a))
  } else {
    cf <- unname(coef(glm(y_tr ~ lp_tr, family = binomial)))
    return(plogis(cf[1] + cf[2] * lp_te))
  }
}

# ---------- (A) 10-fold CV out-of-sample recalibrated probabilities ----------
set.seed(42)
K <- 10
fold_id <- sample(rep(1:K, length.out = length(y)))
cv_preds <- copy(preds)
for (m in names(model_cols)) {
  p <- preds[[model_cols[m]]]
  for (mode in c("intercept", "platt")) {
    p_cv <- numeric(length(y))
    for (k in 1:K) {
      te <- which(fold_id == k); tr <- which(fold_id != k)
      p_cv[te] <- recal_fun(p[tr], y[tr], p[te], mode)
    }
    cv_preds[, paste0(m, "_", mode, "_cv") := p_cv]
  }
}
saveRDS(cv_preds, "/workspace/ext_out/preds_mover_cvrecal.rds")

# ---------- (B) repeated split-sample validation of recalibration ----------
eval_cal <- function(y_te, p_te) {
  lp2 <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  c(Brier = mean((p_te - y_te)^2),
    cal_intercept = unname(coef(glm(y_te ~ offset(lp2), family = binomial))[1]),
    cal_slope = unname(coef(glm(y_te ~ lp2, family = binomial))[2]))
}

set.seed(123)
B <- 500
n <- length(y)
res_list <- list()
for (m in names(model_cols)) {
  p <- preds[[model_cols[m]]]
  for (mode in c("intercept", "platt")) {
    mat <- t(replicate(B, {
      tr <- sample(n, floor(n / 2))
      te <- setdiff(seq_len(n), tr)
      p_te <- recal_fun(p[tr], y[tr], p[te], mode)
      eval_cal(y[te], p_te)
    }))
    res_list[[paste(m, mode)]] <- data.table(
      model = m, recalibration = mode,
      Brier_mean = mean(mat[, "Brier"]),
      Brier_lo = quantile(mat[, "Brier"], 0.025),
      Brier_hi = quantile(mat[, "Brier"], 0.975),
      cal_intercept_mean = mean(mat[, "cal_intercept"]),
      cal_intercept_lo = quantile(mat[, "cal_intercept"], 0.025),
      cal_intercept_hi = quantile(mat[, "cal_intercept"], 0.975),
      cal_slope_mean = mean(mat[, "cal_slope"]),
      cal_slope_lo = quantile(mat[, "cal_slope"], 0.025),
      cal_slope_hi = quantile(mat[, "cal_slope"], 0.975)
    )
    cat(m, mode, "done\n")
  }
}
res <- rbindlist(res_list)
fwrite(res, "/workspace/ext_out/metrics_mover_recal_cv.csv")
print(res)
