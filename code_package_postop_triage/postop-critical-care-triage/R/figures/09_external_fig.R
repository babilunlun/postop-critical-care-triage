#!/usr/bin/env Rscript
# External validation figure: ROC + calibration (pre/post recalibration) for MOVER
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggprism); library(patchwork)
  library(pROC); library(svglite)
})

preds <- readRDS("/workspace/ext_out/preds_mover.rds")
cv_recal <- readRDS("/workspace/ext_out/preds_mover_cvrecal.rds")  # 10-fold CV out-of-sample recalibrated probs
metrics <- fread("/workspace/ext_out/metrics_mover.csv")
FIG <- "/workspace/figures"
dir.create(FIG, showWarnings = FALSE)

theme_set(theme_prism(base_family = "Liberation Sans", base_size = 11))
cols <- c("SASA" = "#CC79A7", "LR (clinical)" = "#0072B2",
          "LR (full)" = "#E69F00", "XGBoost (full)" = "#009E73")

# recalibrate predictions
recal <- function(p, y, mode = c("none", "intercept", "platt")) {
  mode <- match.arg(mode)
  lp <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
  if (mode == "intercept") {
    a <- coef(glm(y ~ offset(lp), family = binomial))[1]
    plogis(lp + a)
  } else if (mode == "platt") {
    cf <- coef(glm(y ~ lp, family = binomial))
    plogis(cf[1] + cf[2] * lp)
  } else p
}

y <- preds$outcome
model_cols <- c(SASA = "SASA", `LR (clinical)` = "LR_clinical",
                `LR (full)` = "LR_full", `XGBoost (full)` = "XGB_full")

# ---------- ROC ----------
roc_dt <- rbindlist(lapply(names(model_cols), function(nm) {
  p <- preds[[model_cols[nm]]]
  ok <- !is.na(p)
  r <- roc(y[ok], p[ok], quiet = TRUE)
  data.frame(model = nm, fpr = 1 - r$specificities, tpr = r$sensitivities)
}))
roc_dt$model <- factor(roc_dt$model, levels = names(model_cols))
aucs <- metrics[recalibration == "none", .(model, AUROC)]
aucs[, lab := sprintf("%s (%.3f)", c("SASA","LR (clinical)","LR (full)","XGBoost (full)"), AUROC)]
lab_map <- setNames(aucs$lab, c("SASA","LR (clinical)","LR (full)","XGBoost (full)"))
roc_dt[, model_lab := lab_map[as.character(model)]]

p_roc <- ggplot(roc_dt, aes(fpr, tpr, color = model_lab)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = setNames(cols, aucs$lab)) +
  labs(x = "1 - Specificity", y = "Sensitivity", color = NULL) +
  theme(legend.position = "inside", legend.position.inside = c(0.62, 0.28),
        legend.text = element_text(size = 8))

# ---------- calibration (XGB pre/post Platt) ----------
cal_curve <- function(p, y, g = 15) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  br <- unique(quantile(p, probs = seq(0, 1, length.out = g + 1)))
  grp <- cut(p, br, include.lowest = TRUE)
  data.frame(pred = tapply(p, grp, mean), obs = tapply(y, grp, mean))
}
cal_xgb_raw <- cal_curve(preds$XGB_full, y)
cal_xgb_platt <- cal_curve(cv_recal$XGB_full_platt_cv, y)  # out-of-sample Platt
cal_lr_raw <- cal_curve(preds$LR_full, y)
cal_lr_platt <- cal_curve(cv_recal$LR_full_platt_cv, y)    # out-of-sample Platt

cal_dt <- rbind(
  data.frame(cal_xgb_raw, model = "XGBoost (full)", recal = "Before"),
  data.frame(cal_xgb_platt, model = "XGBoost (full)", recal = "After"),
  data.frame(cal_lr_raw, model = "LR (full)", recal = "Before"),
  data.frame(cal_lr_platt, model = "LR (full)", recal = "After")
)
cal_dt$recal <- factor(cal_dt$recal, levels = c("Before", "After"))

p_cal <- ggplot(cal_dt, aes(pred, obs, color = model, shape = recal)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(aes(linetype = recal), linewidth = 0.7) +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  scale_shape_manual(values = c(16, 17)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(x = "Predicted probability", y = "Observed frequency",
       color = NULL, shape = "Recalibration", linetype = "Recalibration") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(legend.position = "inside", legend.position.inside = c(0.03, 0.72),
        legend.text = element_text(size = 8), legend.key.width = unit(0.9, "lines"))

p <- p_roc + p_cal + plot_annotation(tag_levels = "a")
ggsave(file.path(FIG, "fig_external_validation.svg"), p, width = 9, height = 4.2)
ggsave(file.path(FIG, "fig_external_validation.png"), p, width = 9, height = 4.2, dpi = 300)
cat("external figure saved\n")
