#!/usr/bin/env Rscript
# 39_triage_bands.R (v2) — B3 uncertain-middle + B4 three-tier triage pathway
#
# KEY DESIGN: triage tiers are built on predictions recalibrated to the
# ICU-course–validated "any true critical care need" outcome (icu_dependent |
# missed_escalation), NOT the admission-based composite. Rationale: MOVER's
# composite is 72.8% observational admissions (practice culture), so
# composite-calibrated absolute risks are not clinically comparable across
# cohorts; need-calibrated risks are. Frozen model ranking is untouched —
# only the calibration target changes (10-fold CV-Platt, same as v3 pipeline).
# SASA is recalibrated to the same target for a same-scale tier comparison.
#
# Tiers: low <5%, middle 5-30%, high >30% (pre-specified, absolute).
# Outputs: table_b3_middle_band.csv, table4_triage_pathway.csv,
#          preds_need_recal_{inspire,mover}.csv

suppressPackageStartupMessages({library(data.table); library(pROC)})
OUT <- "/mnt/results/06_icu_course"
set.seed(42)

ins <- fread(file.path(OUT, "refined_categories_inspire.csv"))
mov <- fread(file.path(OUT, "refined_categories_mover.csv"))
ins[, died := as.integer(death_idx == 1)]
mov[, died := as.integer(expired == 1)]
for (d in list(ins, mov)) {
  d[, `:=`(admitted = category %in% c("icu_dependent", "observational_icu"),
           any_need = category %in% c("icu_dependent", "missed_escalation"))]
}

# ---- 10-fold CV Platt recalibration to any_need ----
cv_platt <- function(p, y, K = 10) {
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  fold <- sample(rep(1:K, length.out = length(y)))
  out <- numeric(length(y))
  for (k in 1:K) {
    te <- which(fold == k); tr <- which(fold != k)
    cf <- unname(coef(glm(y[tr] ~ lp[tr], family = binomial)))
    out[te] <- plogis(cf[1] + cf[2] * lp[te])
  }
  out
}
for (nm in c("ins", "mov")) {
  d <- get(nm)
  d[, xgb_need := cv_platt(XGB_full, as.integer(any_need))]
  d[, sasa_need := cv_platt(SASA, as.integer(any_need))]
  d[, tier := cut(xgb_need, c(-Inf, .05, .30, Inf), labels = c("low_<5%", "middle_5-30%", "high_>30%"))]
  d[, sasa_tier := cut(sasa_need, c(-Inf, .05, .30, Inf), labels = c("low_<5%", "middle_5-30%", "high_>30%"))]
  assign(nm, d)
  fwrite(d, file.path(OUT, sprintf("preds_need_recal_%s.csv", ifelse(nm == "ins", "inspire", "mover"))))
}

# ================= B3: uncertain middle =================
middle_stats <- function(d, cohort) {
  mid <- d[tier == "middle_5-30%"]
  adm <- mid[admitted == TRUE]; notadm <- mid[admitted == FALSE]
  auc_in <- function(y, p) { ci <- ci.auc(roc(y, p, quiet = TRUE), method = "delong")
                             sprintf("%.3f (%.3f-%.3f)", ci[2], ci[1], ci[3]) }
  data.table(
    cohort = cohort, band = "middle_5-30% (need-recalibrated)",
    n = nrow(mid), pct_of_cohort = round(100 * nrow(mid) / nrow(d), 1),
    admitted_n = nrow(adm), admitted_pct = round(100 * nrow(adm) / nrow(mid), 1),
    dep_among_admitted_pct = round(100 * mean(adm$category == "icu_dependent"), 1),
    notadm_n = nrow(notadm),
    missed_among_notadm_n = sum(notadm$category == "missed_escalation"),
    missed_among_notadm_pct = round(100 * mean(notadm$category == "missed_escalation"), 2),
    died_among_notadm_pct = round(100 * mean(notadm$died == 1), 2),
    auroc_any_need_inband_xgb = auc_in(mid$any_need, mid$xgb_need),
    auroc_any_need_inband_sasa = auc_in(mid$any_need, mid$sasa_need))
}
b3 <- rbind(middle_stats(ins, "INSPIRE"), middle_stats(mov, "MOVER"))
fwrite(b3, file.path(OUT, "table_b3_middle_band.csv"))
cat("=== B3: uncertain middle ===\n"); print(b3)

# ================= B4: three-tier triage pathway =================
tier_stats <- function(d, cohort, tier_col, label) {
  rbindlist(lapply(c("low_<5%", "middle_5-30%", "high_>30%"), function(t) {
    sub <- d[get(tier_col) == t]
    data.table(
      cohort = cohort, stratifier = label, tier = t,
      n = nrow(sub), pct = round(100 * nrow(sub) / nrow(d), 1),
      per_1000_ops = round(1000 * nrow(sub) / nrow(d), 0),
      composite_pct = round(100 * mean(sub$outcome == 1), 2),
      icu_dependent_pct = round(100 * mean(sub$category == "icu_dependent"), 2),
      missed_escalation_pct = round(100 * mean(sub$category == "missed_escalation"), 2),
      any_true_need_pct = round(100 * mean(sub$any_need), 2),
      died_pct = round(100 * mean(sub$died == 1), 2),
      npv_no_true_need = round(100 * mean(!sub$any_need), 2))
  }))
}
b4 <- rbind(
  tier_stats(ins, "INSPIRE", "tier", "XGB_full (need-recal)"),
  tier_stats(mov, "MOVER", "tier", "XGB_full (need-recal)"),
  tier_stats(ins, "INSPIRE", "sasa_tier", "SASA (need-recal)"),
  tier_stats(mov, "MOVER", "sasa_tier", "SASA (need-recal)"))
fwrite(b4, file.path(OUT, "table4_triage_pathway.csv"))
cat("\n=== B4: triage pathway (need-recalibrated scale) ===\n"); print(b4)

# safety boundary: stricter low thresholds on the need-recalibrated scale
cat("\n=== safety check: low-tier missed-escalation/death at stricter thresholds ===\n")
for (th in c(0.02, 0.03, 0.05)) {
  for (nm in c("ins", "mov")) {
    d <- get(nm); sub <- d[xgb_need < th]
    cat(sprintf("%s xgb_need<%d%%: n=%d (%.1f%%) | missed=%.3f%% | died=%.3f%% | any_need=%.3f%%\n",
                nm, th * 100, nrow(sub), 100 * nrow(sub) / nrow(d),
                100 * mean(sub$category == "missed_escalation"),
                100 * mean(sub$died == 1), 100 * mean(sub$any_need)))
  }
}
cat("== 39 done ==\n")
