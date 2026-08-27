#!/usr/bin/env Rscript
# Sensitivity: XGBoost WITHOUT has_art (arterial-line proxy of clinician judgment).
# Same cohort, split, recipe, and tuning pipeline as 03_model.R; evaluated on the
# VitalDB temporal test set AND externally on MOVER (none/intercept/platt).
# Outputs: /workspace/gru/out/metrics_noart.csv, preds_noart_mover.rds, model_noart.rds

suppressPackageStartupMessages({
  library(data.table)
  library(tidymodels)
  library(xgboost)
  library(pROC)
  library(PRROC)
})

OUT <- "/workspace/gru/out"; dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
set.seed(20250817)

# ---------- VitalDB: same filter + split as 03_model.R ----------
d <- as.data.table(readRDS("/workspace/gru/analysis_vitaldb.rds"))
d <- d[!is.na(coverage) & coverage >= 0.5 & !is.na(dur_min) & dur_min >= 30]
d$ebl_missing <- as.integer(is.na(d$intraop_ebl))
d[, has_art := as.numeric(has_art)]
d[, outcome := factor(outcome, levels = c(0, 1), labels = c("no", "yes"))]
setorder(d, caseid)
n_train <- floor(0.7 * nrow(d))
train <- d[1:n_train]
test <- d[(n_train + 1):.N]

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
              "bis_mean", "bis_sd", "min_bis40", "min_bis60")  # has_art removed

eval_binary <- function(y, p, label) {
  ok <- !is.na(p)
  y2 <- y[ok]; p2 <- p[ok]
  roc_obj <- roc(y2, p2, quiet = TRUE)
  pr <- pr.curve(scores.class0 = p2[y2 == "yes"], scores.class1 = p2[y2 == "no"],
                 curve = FALSE)
  lp <- qlogis(pmin(pmax(p2, 1e-6), 1 - 1e-6))
  yi <- as.integer(y2 == "yes")
  data.frame(model = label, AUROC = as.numeric(auc(roc_obj)),
             AUPRC = pr$auc.integral, Brier = mean((p2 - yi)^2),
             cal_intercept = unname(coef(glm(yi ~ offset(lp), family = binomial))[1]),
             cal_slope = unname(coef(glm(yi ~ lp, family = binomial))[2]))
}

mk_rec <- function(feats) {
  recipe(as.formula(paste("outcome ~", paste(feats, collapse = "+"))),
         data = train) |>
    step_impute_median(all_numeric_predictors()) |>
    step_novel(all_nominal_predictors()) |>
    step_dummy(all_nominal_predictors()) |>
    step_zv(all_predictors()) |>
    step_normalize(all_numeric_predictors())
}

# ---------- tune XGBoost without has_art (same pipeline as 03_model.R) ----------
xgb_spec <- boost_tree(
  mode = "classification",
  trees = tune(), tree_depth = tune(), min_n = tune(),
  learn_rate = tune(), loss_reduction = tune(), sample_size = tune(),
  mtry = tune()
) |> set_engine("xgboost")

rec_noart <- mk_rec(c(clin_feats, ts_feats))
n_feats <- rec_noart |> prep() |> bake(new_data = NULL) |> ncol()
xgb_wf <- workflow() |> add_recipe(rec_noart) |> add_model(xgb_spec)

folds <- vfold_cv(train, v = 5, strata = outcome)
grid <- grid_space_filling(
  trees(range = c(300L, 1500L)),
  tree_depth(range = c(2L, 8L)),
  min_n(range = c(2L, 40L)),
  learn_rate(range = c(-3, -1), trans = log10_trans()),
  loss_reduction(range = c(-2, 1), trans = log10_trans()),
  sample_prop(range = c(0.6, 1.0)),
  mtry(range = c(10L, min(60L, n_feats - 1L))),
  size = 25
)
cat("tuning XGB_noArt ...\n")
tuned <- tune_grid(xgb_wf, resamples = folds, grid = grid,
                   metrics = metric_set(roc_auc),
                   control = control_grid(save_pred = FALSE, verbose = FALSE))
best <- select_best(tuned, metric = "roc_auc")
print(best)
fit_xgb_noart <- finalize_workflow(xgb_wf, best) |> fit(train)

# ---------- internal test ----------
p_test <- predict(fit_xgb_noart, test, type = "prob")$.pred_yes
m_int <- eval_binary(test$outcome, p_test, "XGB_noArt_internal")
print(m_int)

# ---------- external: MOVER ----------
ext <- as.data.table(readRDS("/mnt/shared-workspace/shared/analysis_mover.rds"))
all_feats <- c(clin_feats, ts_feats)
for (f in setdiff(all_feats, names(ext))) ext[, (f) := NA_real_]
ext[, `:=`(sex = as.character(sex), department = as.character(department))]
int_cols <- c("asa", "preop_plt", "preop_na", "preop_gluc", "preop_ast",
              "preop_alt", "preop_bun", "intraop_ebl", "intraop_uo",
              "intraop_crystalloid", "intraop_colloid", "intraop_ppf",
              "intraop_mdz", "intraop_ftn", "intraop_rocu", "intraop_eph",
              "intraop_phe", "intraop_epi")
for (cc in intersect(int_cols, names(ext))) ext[, (cc) := as.integer(round(get(cc)))]
ext[, outcome_f := factor(outcome, levels = c(0, 1), labels = c("no", "yes"))]

p_ext <- predict(fit_xgb_noart, ext, type = "prob")$.pred_yes
y_ext <- ext$outcome

eval_recal <- function(y01, p, recal) {
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  if (recal == "intercept") {
    a <- unname(coef(glm(y01 ~ offset(lp), family = binomial))[1])
    p <- plogis(lp + a)
  } else if (recal == "platt") {
    cf <- coef(glm(y01 ~ lp, family = binomial))
    p <- plogis(cf[1] + cf[2] * lp)
  }
  m <- eval_binary(factor(y01, levels = c(0, 1), labels = c("no", "yes")), p,
                   paste0("XGB_noArt_mover_", recal))
  m$n <- length(y01); m$events <- sum(y01)
  m
}

m_ext <- rbindlist(list(eval_recal(y_ext, p_ext, "none"),
                        eval_recal(y_ext, p_ext, "intercept"),
                        eval_recal(y_ext, p_ext, "platt")))
print(m_ext)

fwrite(rbindlist(list(m_int, m_ext), fill = TRUE), file.path(OUT, "metrics_noart.csv"))
saveRDS(list(model = fit_xgb_noart, best = best,
             pred_test = p_test, pred_mover = p_ext),
        file.path(OUT, "model_noart.rds"))
cat("DONE\n")
