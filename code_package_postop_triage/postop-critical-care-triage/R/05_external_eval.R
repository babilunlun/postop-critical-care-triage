#!/usr/bin/env Rscript
# External validation: apply frozen VitalDB models to a harmonized external cohort.
# external_eval(ext_df, label) -> AUROC/AUPRC/Brier/calibration before and after
# recalibration (intercept-only and intercept+slope / Platt).
# ext_df must contain: outcome (0/1) + all clin_feats + ts_feats columns
# (missing columns are created as NA so recipes imputation can handle them).

suppressPackageStartupMessages({
  library(data.table)
  library(tidymodels)
  library(pROC)
  library(PRROC)
})

MODELS <- readRDS("/workspace/model_out/models.rds")

eval_one <- function(y01, p, label, model_name, recal = c("none", "intercept", "platt")) {
  recal <- match.arg(recal)
  ok <- !is.na(p) & !is.na(y01)
  y <- y01[ok]; p <- p[ok]
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  if (recal == "intercept") {
    a <- coef(glm(y ~ offset(lp), family = binomial))[1]
    p <- plogis(lp + a)
  } else if (recal == "platt") {
    cf <- coef(glm(y ~ lp, family = binomial))
    p <- plogis(cf[1] + cf[2] * lp)
  }
  lp2 <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  roc_obj <- roc(y, p, quiet = TRUE)
  pr <- pr.curve(scores.class0 = p[y == 1], scores.class1 = p[y == 0], curve = FALSE)
  data.frame(
    cohort = label, model = model_name, recalibration = recal,
    n = length(y), events = sum(y),
    AUROC = as.numeric(auc(roc_obj)),
    AUPRC = pr$auc.integral,
    Brier = mean((p - y)^2),
    cal_intercept = coef(glm(y ~ offset(lp2), family = binomial))[1],
    cal_slope = coef(glm(y ~ lp2, family = binomial))[2]
  )
}

external_eval <- function(ext_df, label) {
  ext <- as.data.table(ext_df)
  # ensure all model features exist (missing -> NA for imputation)
  all_feats <- c(MODELS$clin_feats, MODELS$ts_feats)
  for (f in setdiff(all_feats, names(ext))) ext[, (f) := NA_real_]
  # align types expected by recipes
  ext[, `:=`(
    sex = as.character(sex),
    department = as.character(department),
    has_art = as.numeric(has_art)
  )]
  # match training column types (VitalDB ints)
  int_cols <- c("asa", "preop_plt", "preop_na", "preop_gluc", "preop_ast",
                "preop_alt", "preop_bun", "intraop_ebl", "intraop_uo",
                "intraop_crystalloid", "intraop_colloid", "intraop_ppf",
                "intraop_mdz", "intraop_ftn", "intraop_rocu", "intraop_eph",
                "intraop_phe", "intraop_epi")
  for (cc in intersect(int_cols, names(ext))) ext[, (cc) := as.integer(round(get(cc)))]
  ext[, outcome_f := factor(outcome, levels = c(0, 1), labels = c("no", "yes"))]

  # SASA (needs hr_min, map_min, intraop_ebl)
  sasa <- function(hr_min, map_min, ebl) {
    hr_s <- cut(hr_min, c(-Inf, 55, 65, 75, 85, Inf), labels = 4:0, right = TRUE)
    map_s <- cut(map_min, c(-Inf, 40, 54, 69, Inf), labels = 0:3, right = TRUE)
    ebl_s <- cut(ebl, c(-Inf, 100, 600, 1000, Inf), labels = 3:0, right = TRUE)
    as.numeric(as.character(hr_s)) + as.numeric(as.character(map_s)) +
      as.numeric(as.character(ebl_s))
  }
  ext[, sasa_score := sasa(hr_min, map_min, intraop_ebl)]
  ext[, sasa_prob := predict(MODELS$sasa_cal, newdata = data.frame(sasa = sasa_score),
                             type = "response")]

  p_lr_clin <- predict(MODELS$lr_clin, ext, type = "prob")$.pred_yes
  p_lr_full <- predict(MODELS$lr_full, ext, type = "prob")$.pred_yes
  p_xgb <- predict(MODELS$xgb, ext, type = "prob")$.pred_yes

  y <- ext$outcome
  out <- rbindlist(list(
    eval_one(y, ext$sasa_prob, label, "SASA", "none"),
    eval_one(y, p_lr_clin, label, "LR_clinical", "none"),
    eval_one(y, p_lr_full, label, "LR_full", "none"),
    eval_one(y, p_xgb, label, "XGB_full", "none"),
    eval_one(y, p_lr_clin, label, "LR_clinical", "intercept"),
    eval_one(y, p_lr_full, label, "LR_full", "intercept"),
    eval_one(y, p_xgb, label, "XGB_full", "intercept"),
    eval_one(y, p_lr_clin, label, "LR_clinical", "platt"),
    eval_one(y, p_lr_full, label, "LR_full", "platt"),
    eval_one(y, p_xgb, label, "XGB_full", "platt")
  ))
  attr(out, "predictions") <- data.table(
    LOG_ID = ext$LOG_ID, outcome = y, SASA = ext$sasa_prob,
    LR_clinical = p_lr_clin, LR_full = p_lr_full, XGB_full = p_xgb
  )
  out
}
cat("external_eval() ready\n")
