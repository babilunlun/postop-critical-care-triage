#!/usr/bin/env Rscript
# 28_inspire_eval.R — apply frozen VitalDB models to INSPIRE (full MOVER-aligned pipeline)
# Cohorts: primary (VitalDB-overlap excluded via case_id), with-overlap, conservative.
# Models: SASA, LR_clinical, LR_full, XGB_full, XGB_noArt.
# Metrics: AUROC (DeLong CI), AUPRC, Brier, calibration intercept/slope (raw);
#          10-fold CV Platt/intercept recalibration + 500x split-sample validation (primary);
#          DCA raw + CV-Platt for XGB_full/XGB_noArt (primary);
#          worst-case residual-overlap AUROC bound.
# Output: preds_inspire.rds, metrics_inspire.csv, inspire_cv_preds.rds,
#         dca_inspire_curves.csv, overlap_bound.csv

suppressPackageStartupMessages({
  library(data.table)
  library(tidymodels)
  library(pROC)
  library(PRROC)
  library(dcurves)
})
set.seed(42)
DIR <- "/workspace/inspire"

ai <- readRDS(file.path(DIR, "analysis_inspire.rds"))
ov <- readRDS(file.path(DIR, "inspire_overlap.rds"))$flags
ai <- merge(ai, ov, by = "op_id", all.x = TRUE)
ai[, ebl_missing := as.integer(is.na(intraop_ebl))]
# training types are integer for transfusion counts; 5-min medians give fractions
ai[, intraop_rbc := as.integer(round(intraop_rbc))]
ai[, intraop_ffp := as.integer(round(intraop_ffp))]
ai[, LOG_ID := op_id]   # external_eval compatibility

source("/mnt/shared-workspace/shared/05_external_eval.R")   # MODELS + external_eval + eval_one
noart <- readRDS("/workspace/model_noart.rds")

# ---------- predict all models on the FULL cohort (once) ----------
full_eval <- external_eval(ai, "inspire_all")
preds <- attr(full_eval, "predictions")
# XGB_noArt: same type conversions as external_eval, then workflow predict
ext <- as.data.table(ai)
ext[, `:=`(sex = as.character(sex), department = as.character(department),
           has_art = as.numeric(has_art))]
int_cols <- c("asa", "preop_plt", "preop_na", "preop_gluc", "preop_ast",
              "preop_alt", "preop_bun", "intraop_ebl", "intraop_uo",
              "intraop_crystalloid", "intraop_colloid", "intraop_ppf",
              "intraop_mdz", "intraop_ftn", "intraop_rocu", "intraop_eph",
              "intraop_phe", "intraop_epi")
for (cc in intersect(int_cols, names(ext))) ext[, (cc) := as.integer(round(get(cc)))]
ext[, outcome_f := factor(outcome, levels = c(0, 1), labels = c("no", "yes"))]
preds[, XGB_noArt := predict(noart$model, ext, type = "prob")$.pred_yes]
preds <- merge(preds, ai[, .(op_id, excl_primary, excl_conservative)],
               by.x = "LOG_ID", by.y = "op_id")
saveRDS(preds, file.path(DIR, "preds_inspire.rds"))
cat("predictions saved:", nrow(preds), "\n")

# ---------- metrics per cohort subset ----------
auc_ci <- function(y, p) {
  r <- roc(y, p, quiet = TRUE)
  ci <- ci.auc(r, method = "delong")
  c(AUROC = as.numeric(ci[2]), lo = as.numeric(ci[1]), hi = as.numeric(ci[3]))
}
eval_set <- function(d, label) {
  y <- d$outcome
  rbindlist(list(
    cbind(eval_one(y, d$SASA, label, "SASA", "none"),
          as.list(auc_ci(y, d$SASA)[c("lo", "hi")])),
    cbind(eval_one(y, d$LR_clinical, label, "LR_clinical", "none"),
          as.list(auc_ci(y, d$LR_clinical)[c("lo", "hi")])),
    cbind(eval_one(y, d$LR_full, label, "LR_full", "none"),
          as.list(auc_ci(y, d$LR_full)[c("lo", "hi")])),
    cbind(eval_one(y, d$XGB_full, label, "XGB_full", "none"),
          as.list(auc_ci(y, d$XGB_full)[c("lo", "hi")])),
    cbind(eval_one(y, d$XGB_noArt, label, "XGB_noArt", "none"),
          as.list(auc_ci(y, d$XGB_noArt)[c("lo", "hi")]))
  ))
}
pri <- preds[excl_primary == FALSE]
wov <- preds
con <- preds[excl_conservative == FALSE]
metrics <- rbindlist(list(
  eval_set(pri, "INSPIRE_primary"),
  eval_set(wov, "INSPIRE_with_overlap"),
  eval_set(con, "INSPIRE_conservative")
))
fwrite(metrics, file.path(DIR, "metrics_inspire.csv"))
print(metrics[model %in% c("XGB_full", "XGB_noArt"),
              .(cohort, model, n, events, AUROC, lo, hi, AUPRC, Brier,
                cal_intercept, cal_slope)])

# ---------- 10-fold CV recalibration on primary (mirror 10_recal_cv.R) ----------
recal_fun <- function(p_tr, y_tr, p_te, mode) {
  lp_tr <- qlogis(pmin(pmax(p_tr, 1e-6), 1 - 1e-6))
  lp_te <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  if (mode == "intercept") {
    a <- unname(coef(glm(y_tr ~ offset(lp_tr), family = binomial))[1])
    return(plogis(lp_te + a))
  }
  cf <- unname(coef(glm(y_tr ~ lp_tr, family = binomial)))
  plogis(cf[1] + cf[2] * lp_te)
}
y <- pri$outcome
K <- 10
set.seed(42)
fold_id <- sample(rep(1:K, length.out = length(y)))
cv_preds <- copy(pri)
for (m in c("LR_clinical", "LR_full", "XGB_full", "XGB_noArt")) {
  p <- pri[[m]]
  for (mode in c("intercept", "platt")) {
    p_cv <- numeric(length(y))
    for (k in 1:K) {
      te <- which(fold_id == k); tr <- which(fold_id != k)
      p_cv[te] <- recal_fun(p[tr], y[tr], p[te], mode)
    }
    cv_preds[, paste0(m, "_", mode, "_cv") := p_cv]
  }
}
saveRDS(cv_preds, file.path(DIR, "inspire_cv_preds.rds"))

# CV-recalibrated metrics (primary)
eval_cal <- function(y_te, p_te) {
  lp2 <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
  c(Brier = mean((p_te - y_te)^2),
    cal_intercept = unname(coef(glm(y_te ~ offset(lp2), family = binomial))[1]),
    cal_slope = unname(coef(glm(y_te ~ lp2, family = binomial))[2]))
}
cv_metrics <- rbindlist(lapply(c("LR_clinical", "LR_full", "XGB_full", "XGB_noArt"), function(m) {
  rbindlist(lapply(c("intercept", "platt"), function(mode) {
    p <- cv_preds[[paste0(m, "_", mode, "_cv")]]
    as.data.table(c(cohort = "INSPIRE_primary", model = m, recalibration = paste0(mode, "_cv"),
                    as.list(eval_cal(y, p))))
  }))
}))
print(cv_metrics)

# ---------- repeated split-sample validation of Platt (primary; XGB_full, XGB_noArt) ----------
set.seed(123)
B <- 500
ss_list <- list()
for (m in c("XGB_full", "XGB_noArt")) {
  p <- pri[[m]]
  mat <- t(replicate(B, {
    te <- sample(seq_along(y), floor(length(y) / 2))
    tr <- setdiff(seq_along(y), te)
    p_te <- recal_fun(p[tr], y[tr], p[te], "platt")
    eval_cal(y[te], p_te)
  }))
  ss_list[[m]] <- data.table(
    model = m, metric = colnames(mat),
    mean = colMeans(mat),
    lo = apply(mat, 2, quantile, 0.025), hi = apply(mat, 2, quantile, 0.975))
}
ss <- rbindlist(ss_list)
print(ss)
fwrite(ss, file.path(DIR, "inspire_recal_splitsample.csv"))

# ---------- DCA (primary): raw + CV-Platt, XGB_full vs XGB_noArt ----------
ths <- seq(0.05, 0.50, by = 0.01)
run_dca <- function(df, cohort_label) {
  d <- dca(outcome ~ XGB_full + XGB_noArt, data = as.data.frame(df), thresholds = ths)
  out <- as.data.table(d$dca)
  out[, cohort := cohort_label]
  out
}
dca_raw <- run_dca(pri[, .(outcome, XGB_full, XGB_noArt)], "INSPIRE_raw")
dca_cal <- run_dca(cv_preds[, .(outcome, XGB_full = XGB_full_platt_cv,
                                XGB_noArt = XGB_noArt_platt_cv)],
                   "INSPIRE_recalibrated")
dca_all <- rbindlist(list(dca_raw, dca_cal))
fwrite(dca_all, file.path(DIR, "dca_inspire_curves.csv"))
for (cc in c("INSPIRE_raw", "INSPIRE_recalibrated")) {
  dd <- dca_all[cohort == cc & label %in% c("XGB_full", "XGB_noArt")]
  dd <- dcast(dd, threshold ~ label, value.var = "net_benefit")
  dd[, diff := XGB_full - XGB_noArt]
  cat(cc, "NB diff (full-noArt) at 0.10/0.20/0.30:",
      round(dd[threshold %in% c(0.10, 0.20, 0.30), diff], 4), "\n")
}

# ---------- worst-case residual-overlap bound on AUROC ----------
# add 599 synthetic perfectly-predicted ops (upper estimate of missed VitalDB overlap)
roc0 <- auc(pri$outcome, pri$XGB_full, quiet = TRUE)
n_missed <- 599
syn <- data.table(outcome = c(rep(1, round(n_missed * mean(pri$outcome == 1))),
                              n_missed - round(n_missed * mean(pri$outcome == 1))))
syn[, p := fifelse(outcome == 1, 0.999, 0.001)]
roc1 <- auc(c(pri$outcome, syn$outcome), c(pri$XGB_full, syn$p), quiet = TRUE)
bound <- data.table(n_synthetic = n_missed, AUROC_observed = as.numeric(roc0),
                    AUROC_worstcase_inflated = as.numeric(roc1),
                    delta = as.numeric(roc1 - roc0))
fwrite(bound, file.path(DIR, "overlap_bound.csv"))
print(bound)
cat("== 28 done ==\n")
