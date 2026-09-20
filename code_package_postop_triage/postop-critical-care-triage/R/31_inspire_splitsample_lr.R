#!/usr/bin/env Rscript
# 31_inspire_splitsample_lr.R — extend 500x split-sample Platt validation to LR models
# (28_inspire_eval.R covered XGB_full/XGB_noArt; Table 2 needs LR_clinical/LR_full too)
suppressPackageStartupMessages(library(data.table))
DIR <- "/workspace/inspire"

pri <- readRDS(file.path(DIR, "preds_inspire.rds"))[excl_primary == FALSE]
y <- pri$outcome

recal_fun <- function(p_tr, y_tr, p_te, mode = "platt") {
  lp_tr <- qlogis(pmin(pmax(p_tr, 1e-6), 1 - 1e-6))
  lp_te <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  cf <- unname(coef(glm(y_tr ~ lp_tr, family = binomial)))
  plogis(cf[1] + cf[2] * lp_te)
}
eval_cal <- function(y_te, p_te) {
  lp2 <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  c(Brier = mean((p_te - y_te)^2),
    cal_intercept = unname(coef(glm(y_te ~ offset(lp2), family = binomial))[1]),
    cal_slope = unname(coef(glm(y_te ~ lp2, family = binomial))[2]))
}

set.seed(123)
B <- 500
ss_new <- rbindlist(lapply(c("LR_clinical", "LR_full"), function(m) {
  p <- pri[[m]]
  mat <- t(replicate(B, {
    te <- sample(seq_along(y), floor(length(y) / 2))
    tr <- setdiff(seq_along(y), te)
    p_te <- recal_fun(p[tr], y[tr], p[te])
    eval_cal(y[te], p_te)
  }))
  data.table(model = m, metric = colnames(mat),
             mean = colMeans(mat),
             lo = apply(mat, 2, quantile, 0.025), hi = apply(mat, 2, quantile, 0.975))
}))

ss_old <- fread(file.path(DIR, "inspire_recal_splitsample.csv"))
ss <- rbind(ss_old, ss_new)
fwrite(ss, file.path(DIR, "inspire_recal_splitsample.csv"))
print(ss)
cat("== 31 done ==\n")
