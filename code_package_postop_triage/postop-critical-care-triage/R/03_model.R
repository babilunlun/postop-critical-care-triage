#!/usr/bin/env Rscript
# Internal modeling: SASA comparator + LR (clinical / full) + XGBoost (full)
# Temporal split 70/30 by caseid (chronological proxy; VitalDB is deidentified).
# 5-fold stratified CV on train for XGBoost tuning (space-filling grid, 25).
# Outputs: model_out/models.rds, metrics_test.csv, test_predictions.csv, shapviz_xgb.rds

suppressPackageStartupMessages({
  library(data.table)
  library(tidymodels)
  library(xgboost)
  library(pROC)
})

OUT_DIR <- "/workspace/model_out"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
set.seed(20250817)

d <- readRDS("/workspace/analysis_vitaldb.rds")
d <- as.data.table(d)

# quality filter: monitoring coverage >= 0.5 and duration >= 30 min
d <- d[!is.na(coverage) & coverage >= 0.5 & !is.na(dur_min) & dur_min >= 30]
cat("quality filter: kept", nrow(d), "of 5985 (coverage>=0.5, duration>=30min)\n")
cat("modeling N:", nrow(d), " events:", sum(d$outcome),
    sprintf("(%.1f%%)", 100 * mean(d$outcome)), "\n")

# EBL missingness is informative (event rate 11.7% missing vs 24.2% recorded):
# add explicit indicator alongside median imputation
d$ebl_missing <- as.integer(is.na(d$intraop_ebl))
clin_feats <- c("age", "sex", "bmi", "asa", "emop", "department",
                "preop_htn", "preop_dm",
                "preop_hb", "preop_plt", "preop_na", "preop_k", "preop_gluc",
                "preop_alb", "preop_ast", "preop_alt", "preop_bun", "preop_cr",
                "intraop_ebl", "ebl_missing", "intraop_uo", "intraop_rbc", "intraop_ffp",
                "intraop_crystalloid", "intraop_colloid",
                "intraop_ppf", "intraop_mdz", "intraop_ftn", "intraop_rocu",
                "intraop_eph", "intraop_phe", "intraop_epi", "ane_dur_min")
ts_feats <- c("coverage", "n_min_valid",
              "map_mean", "map_sd", "map_cv", "map_min", "map_max", "map_slope",
              "map_early", "map_late",
              "min_map65", "min_map60", "area_map65", "area_map60",
              "twa_map65", "longest_map65", "pct_map65",
              "hr_mean", "hr_sd", "hr_cv", "hr_min", "hr_max",
              "min_hr100", "min_hr50", "hr_slope",
              "spo2_mean", "spo2_min", "min_spo2_92", "min_spo2_90",
              "etco2_mean", "etco2_sd", "min_etco2_low", "min_etco2_high",
              "bis_mean", "bis_sd", "min_bis40", "min_bis60",
              "has_art")

d[, has_art := as.numeric(has_art)]
d[, outcome := factor(outcome, levels = c(0, 1), labels = c("no", "yes"))]

# temporal split: first 70% of caseids = train, last 30% = test
setorder(d, caseid)
n_train <- floor(0.7 * nrow(d))
train <- d[1:n_train]
test <- d[(n_train + 1):.N]
cat("train:", nrow(train), " events:", sum(train$outcome == "yes"),
    " | test:", nrow(test), " events:", sum(test$outcome == "yes"), "\n")

# ---------- SASA comparator (Gawande 2007) ----------
# lowest HR: >85->0, 76-85->1, 66-75->2, 56-65->3, <=55->4
# lowest MAP: <=40->0, 41-54->1, 55-69->2, >=70->3
# EBL (mL): >1000->0, 601-1000->1, 101-600->2, <=100->3
sasa <- function(hr_min, map_min, ebl) {
  hr_s <- cut(hr_min, c(-Inf, 55, 65, 75, 85, Inf), labels = 4:0, right = TRUE)
  map_s <- cut(map_min, c(-Inf, 40, 54, 69, Inf), labels = 0:3, right = TRUE)
  ebl_s <- cut(ebl, c(-Inf, 100, 600, 1000, Inf), labels = 3:0, right = TRUE)
  as.numeric(as.character(hr_s)) + as.numeric(as.character(map_s)) +
    as.numeric(as.character(ebl_s))
}
test$sasa <- sasa(test$hr_min, test$map_min, test$intraop_ebl)
# SASA is a risk score 0-10 (lower = worse); calibrate via logistic on train
train$sasa <- sasa(train$hr_min, train$map_min, train$intraop_ebl)
sasa_cal <- glm(as.integer(outcome == "yes") ~ sasa, data = train, family = binomial)
test$sasa_prob <- predict(sasa_cal, newdata = test, type = "response")

# ---------- metrics ----------
eval_binary <- function(y, p, label) {
  ok <- !is.na(p)
  y2 <- y[ok]; p2 <- p[ok]
  roc_obj <- roc(y2, p2, quiet = TRUE)
  pr <- pr.curve(scores.class0 = p2[y2 == "yes"], scores.class1 = p2[y2 == "no"],
                 curve = FALSE)
  lp <- qlogis(pmin(pmax(p2, 1e-6), 1 - 1e-6))
  yi <- as.integer(y2 == "yes")
  cal_int <- coef(glm(yi ~ offset(lp), family = binomial))[1]
  cal_slope <- coef(glm(yi ~ lp, family = binomial))[2]
  brier <- mean((p2 - yi)^2)
  data.frame(model = label, AUROC = as.numeric(auc(roc_obj)),
             AUPRC = pr$auc.integral, Brier = brier,
             cal_intercept = cal_int, cal_slope = cal_slope)
}
suppressMessages(library(PRROC))

# ---------- recipes ----------
mk_rec <- function(feats) {
  recipe(as.formula(paste("outcome ~", paste(feats, collapse = "+"))),
         data = train) |>
    step_impute_median(all_numeric_predictors()) |>
    step_novel(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
}

# ---------- LR (glmnet ridge, lambda = 1e-4 ~ unpenalized MLE) ----------
lr_spec <- logistic_reg(penalty = 1e-4, mixture = 0) |>
  set_engine("glmnet")

cat("fitting LR (clinical)...\n")
fit_lr_clin <- workflow() |>
  add_recipe(mk_rec(clin_feats)) |>
  add_model(lr_spec) |>
  fit(train)

cat("fitting LR (full)...\n")
fit_lr_full <- workflow() |>
  add_recipe(mk_rec(c(clin_feats, ts_feats))) |>
  add_model(lr_spec) |>
  fit(train)

# ---------- XGBoost (tuned) ----------
# no scale_pos_weight: 19.8% event rate is mild imbalance; reweighting distorts
# probability calibration (observed cal intercept -1.05). Stratified CV suffices.
cat("tuning XGBoost...\n")
xgb_spec <- boost_tree(
  mode = "classification",
  trees = tune(), tree_depth = tune(), min_n = tune(),
  learn_rate = tune(), loss_reduction = tune(), sample_size = tune(),
  mtry = tune()
) |> set_engine("xgboost")

rec_full <- mk_rec(c(clin_feats, ts_feats))
n_feats_full <- rec_full |> prep() |> bake(new_data = NULL) |> ncol()
xgb_wf <- workflow() |> add_recipe(rec_full) |> add_model(xgb_spec)

folds <- vfold_cv(train, v = 5, strata = outcome)
grid <- grid_space_filling(
  trees(range = c(300L, 1500L)),
  tree_depth(range = c(2L, 8L)),
  min_n(range = c(2L, 40L)),
  learn_rate(range = c(-3, -1), trans = log10_trans()),
  loss_reduction(range = c(-2, 1), trans = log10_trans()),
  sample_prop(range = c(0.6, 1.0)),
  mtry(range = c(10L, min(60L, n_feats_full - 1L))),
  size = 25
)
tuned <- tune_grid(xgb_wf, resamples = folds, grid = grid,
                   metrics = metric_set(roc_auc),
                   control = control_grid(save_pred = FALSE, verbose = FALSE))
best <- select_best(tuned, metric = "roc_auc")
print(best)
fit_xgb <- finalize_workflow(xgb_wf, best) |> fit(train)

# ---------- test-set predictions ----------
pred_test <- function(fit, dat, label) {
  p <- predict(fit, dat, type = "prob")$.pred_yes
  eval_binary(dat$outcome, p, label) |> mutate(prob = list(p))
}

res <- bind_rows(
  eval_binary(test$outcome, test$sasa_prob, "SASA") |> mutate(prob = list(test$sasa_prob)),
  pred_test(fit_lr_clin, test, "LR_clinical"),
  pred_test(fit_lr_full, test, "LR_full"),
  pred_test(fit_xgb, test, "XGB_full")
)
print(res |> select(-prob))

fwrite(res |> select(-prob), file.path(OUT_DIR, "metrics_test.csv"))

preds <- data.table(
  caseid = test$caseid,
  outcome = as.integer(test$outcome == "yes"),
  SASA = res$prob[[1]],
  LR_clinical = res$prob[[2]],
  LR_full = res$prob[[3]],
  XGB_full = res$prob[[4]]
)
fwrite(preds, file.path(OUT_DIR, "test_predictions.csv"))

saveRDS(list(lr_clin = fit_lr_clin, lr_full = fit_lr_full, xgb = fit_xgb,
             sasa_cal = sasa_cal, clin_feats = clin_feats, ts_feats = ts_feats,
             best_xgb = best),
        file.path(OUT_DIR, "models.rds"))

# ---------- SHAP for XGBoost ----------
cat("computing SHAP...\n")
library(shapviz)
baked_test <- bake(prep(rec_full), new_data = test)
xgb_booster <- extract_fit_engine(fit_xgb)
if (!file.exists(file.path(OUT_DIR, "shapviz_xgb.rds"))) {
  feat_names <- setdiff(colnames(baked_test), "outcome")
  X <- as.matrix(as.data.frame(baked_test)[, feat_names])
  sv <- shapviz(xgb_booster, X_pred = X, which_class = 2)
  saveRDS(sv, file.path(OUT_DIR, "shapviz_xgb.rds"))
}

cat("DONE\n")
