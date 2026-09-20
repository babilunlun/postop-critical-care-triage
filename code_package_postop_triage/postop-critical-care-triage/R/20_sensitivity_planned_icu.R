#!/usr/bin/env Rscript
# Sensitivity analysis: quantify impact of outcome-definition mismatch (planned ICU
# admissions) on external validation in MOVER.
#   S1 (primary):   exclude "routine-ICU" procedures (procedure-level ICU flag rate
#                   >= threshold among procedures with >= 20 cases) -> likely planned.
#   S2 (secondary): ambulatory-only cohort (Hospital Outpatient Surgery) where any
#                   ICU admission is unplanned by definition.
# Metrics per subset x model (XGB_full primary; LR_full, SASA references):
#   AUROC (DeLong 95% CI), AUPRC, Brier, calibration intercept/slope (raw),
#   then 500x split-sample Platt recalibration refit WITHIN each subset.
# Also produces 10-fold CV Platt probabilities per subset for calibration plots.

suppressPackageStartupMessages({
  library(data.table)
  library(pROC)
  library(PRROC)
})

set.seed(42)
OUT <- "/workspace/sens_out"
dir.create(OUT, showWarnings = FALSE)

# ---------- load ----------
preds  <- readRDS("/mnt/shared-workspace/shared/preds_mover.rds")          # 49,394 x 6
noart  <- readRDS("/mnt/shared-workspace/shared/model_noart.rds")$pred_mover # numeric 49,394
info   <- fread("/workspace/mover_check/EPIC_EMR/EMR/patient_information.csv",
                select = c("LOG_ID", "PATIENT_CLASS_NM", "PRIMARY_PROCEDURE_NM",
                           "ICU_ADMIN_FLAG", "DISCH_DISP"))

# deduplicate patient_information by LOG_ID (take first row)
n_dup <- sum(duplicated(info$LOG_ID))
cat("duplicate LOG_ID rows in patient_information:", n_dup, "\n")
info <- info[!duplicated(LOG_ID)]

dt <- merge(preds, info, by = "LOG_ID", all.x = TRUE)
stopifnot(nrow(dt) == 49394)
dt[, pred_noart := noart]  # row order of analysis_mover == preds_mover (verified below)
cat("merged:", nrow(dt), "rows; events:", sum(dt$outcome),
    sprintf("(%.1f%%)\n", 100 * mean(dt$outcome)))
cat("missing PATIENT_CLASS_NM:", sum(is.na(dt$PATIENT_CLASS_NM)), "\n")

# ---------- alignment gate: reproduce known full-cohort AUROCs ----------
gate <- function(y, p, expect, tag) {
  a <- as.numeric(auc(roc(y, p, quiet = TRUE)))
  cat(sprintf("alignment %s: AUROC=%.3f (expect %.3f)\n", tag, a, expect))
  stopifnot(abs(a - expect) < 0.002)
}
gate(dt$outcome, dt$XGB_full, 0.794, "XGB_full full-cohort")
gate(dt$outcome, dt$pred_noart, 0.754, "XGB_noArt full-cohort")

# ---------- metrics helpers ----------
eval_raw <- function(y, p) {
  ok <- !is.na(p) & !is.na(y)
  y <- y[ok]; p <- p[ok]
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  roc_obj <- roc(y, p, quiet = TRUE)
  ci <- as.numeric(ci.auc(roc_obj, method = "delong"))
  pr <- pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0], curve = FALSE)
  c(n = length(y), events = sum(y),
    AUROC = ci[2], AUROC_lo = ci[1], AUROC_hi = ci[3],
    AUPRC = pr$auc.integral,
    Brier = mean((p - y)^2),
    cal_intercept = unname(coef(glm(y ~ offset(lp), family = binomial))[1]),
    cal_slope = unname(coef(glm(y ~ lp, family = binomial))[2]))
}

platt_split <- function(y, p, B = 500, seed = 123) {
  ok <- !is.na(p) & !is.na(y)
  y <- y[ok]; p <- p[ok]
  set.seed(seed)
  n <- length(y)
  mat <- t(replicate(B, {
    tr <- sample(n, floor(n / 2))
    te <- setdiff(seq_len(n), tr)
    lp_tr <- qlogis(pmin(pmax(p[tr], 1e-6), 1 - 1e-6))
    cf <- unname(coef(glm(y[tr] ~ lp_tr, family = binomial)))
    lp_te <- qlogis(pmin(pmax(p[te], 1e-6), 1 - 1e-6))
    p_te <- plogis(cf[1] + cf[2] * lp_te)
    lp2 <- qlogis(pmin(pmax(p_te, 1e-6), 1 - 1e-6))
    c(Brier = mean((p_te - y[te])^2),
      slope = unname(coef(glm(y[te] ~ lp2, family = binomial))[2]))
  }))
  c(platt_Brier = mean(mat[, "Brier"]),
    platt_Brier_lo = unname(quantile(mat[, "Brier"], 0.025)),
    platt_Brier_hi = unname(quantile(mat[, "Brier"], 0.975)),
    platt_slope = mean(mat[, "slope"]),
    platt_slope_lo = unname(quantile(mat[, "slope"], 0.025)),
    platt_slope_hi = unname(quantile(mat[, "slope"], 0.975)))
}

# 10-fold CV Platt probabilities (out-of-sample recalibrated, for calibration plots)
cv_platt <- function(y, p, K = 10, seed = 42) {
  ok <- !is.na(p) & !is.na(y)
  out <- rep(NA_real_, length(y))
  idx <- which(ok)
  set.seed(seed)
  fold <- sample(rep(1:K, length.out = length(idx)))
  for (k in 1:K) {
    te <- idx[fold == k]; tr <- idx[fold != k]
    lp_tr <- qlogis(pmin(pmax(p[tr], 1e-6), 1 - 1e-6))
    cf <- unname(coef(glm(y[tr] ~ lp_tr, family = binomial)))
    lp_te <- qlogis(pmin(pmax(p[te], 1e-6), 1 - 1e-6))
    out[te] <- plogis(cf[1] + cf[2] * lp_te)
  }
  out
}

# ---------- subset definitions ----------
# S1: routine-ICU procedures by ICU_ADMIN_FLAG rate (>=20 cases)
proc_rate <- dt[, .(n = .N, icu_rate = mean(ICU_ADMIN_FLAG == "Yes")),
                by = PRIMARY_PROCEDURE_NM]
make_s1 <- function(th) {
  hi <- proc_rate[n >= 20 & icu_rate >= th, PRIMARY_PROCEDURE_NM]
  !dt$PRIMARY_PROCEDURE_NM %in% hi
}
s1_90 <- make_s1(0.90)
s1_95 <- make_s1(0.95)
s2_amb <- dt$PATIENT_CLASS_NM == "Hospital Outpatient Surgery"
s2_amb[is.na(s2_amb)] <- FALSE

for (s in c("s1_90", "s1_95", "s2_amb")) {
  v <- get(s)
  cat(sprintf("%s: n=%d (%.1f%% of cohort), events=%d (%.1f%%)\n", s,
              sum(v), 100 * mean(v), sum(dt$outcome[v]), 100 * mean(dt$outcome[v])))
}
cat("S1(0.90) excludes", sum(!s1_90), "cases,",
    sum(dt$outcome[!s1_90]), "events, of which deaths:",
    sum(dt$DISCH_DISP[!s1_90] == "Expired"), "\n")
cat("excluded procedures (0.90):",
    nrow(proc_rate[n >= 20 & icu_rate >= 0.90]), "\n")
fwrite(proc_rate, file.path(OUT, "procedure_icu_rates.csv"))

# ---------- evaluate ----------
models <- c("XGB_full", "LR_full", "SASA", "XGB_noArt")
subsets <- list(full = rep(TRUE, nrow(dt)),
                S1_exclude_routineICU_90 = s1_90,
                S1_exclude_routineICU_95 = s1_95,
                S2_ambulatory_only = s2_amb)

res <- list()
cv_store <- list()
for (sn in names(subsets)) {
  v <- subsets[[sn]]
  d <- dt[v]
  for (m in models) {
    p <- if (m == "XGB_noArt") d$pred_noart else d[[m]]
    raw <- eval_raw(d$outcome, p)
    pl  <- platt_split(d$outcome, p)
    res[[paste(sn, m)]] <- as.data.table(c(list(subset = sn, model = m),
                                           as.list(raw), as.list(pl)))
    cat(sn, m, "AUROC:", round(raw["AUROC"], 3), "\n")
  }
  # CV Platt probabilities for calibration plots (XGB_full + noArt)
  cv_store[[sn]] <- data.table(LOG_ID = d$LOG_ID, outcome = d$outcome,
                               XGB_full = d$XGB_full,
                               XGB_full_cvplatt = cv_platt(d$outcome, d$XGB_full),
                               XGB_noArt = d$pred_noart,
                               XGB_noArt_cvplatt = cv_platt(d$outcome, d$pred_noart))
}
res <- rbindlist(res)
fwrite(res, file.path(OUT, "table_s1_sensitivity.csv"))
saveRDS(cv_store, file.path(OUT, "sens_cv_preds.rds"))
saveRDS(dt[, .(LOG_ID, outcome, PATIENT_CLASS_NM, PRIMARY_PROCEDURE_NM,
               ICU_ADMIN_FLAG, DISCH_DISP, XGB_full, pred_noart)],
        file.path(OUT, "sens_cohort.rds"))
print(res[, .(subset, model, n, events, AUROC, AUROC_lo, AUROC_hi, Brier,
              cal_intercept, cal_slope, platt_Brier, platt_slope)])
cat("DONE\n")
