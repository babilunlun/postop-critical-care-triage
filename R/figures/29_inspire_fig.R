#!/usr/bin/env Rscript
# Combined validation figure (main text): 2 cohorts x (ROC | calibration)
# Row 1: INSPIRE (same-institution temporal validation, primary cohort)
# Row 2: MOVER (independent external validation)
# Calibration: XGBoost (full) and LR (full) before/after 10-fold CV Platt recalibration.
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggprism); library(patchwork)
  library(pROC); library(svglite)
})

FIG <- "/workspace/figures"
dir.create(FIG, showWarnings = FALSE)
theme_set(theme_prism(base_family = "Liberation Sans", base_size = 11))
cols <- c("SASA" = "#CC79A7", "LR (clinical)" = "#0072B2",
          "LR (full)" = "#E69F00", "XGBoost (full)" = "#009E73")
model_cols <- c(SASA = "SASA", `LR (clinical)` = "LR_clinical",
                `LR (full)` = "LR_full", `XGBoost (full)` = "XGB_full")

cal_curve <- function(p, y, g = 15) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  br <- unique(quantile(p, probs = seq(0, 1, length.out = g + 1)))
  grp <- cut(p, br, include.lowest = TRUE)
  data.frame(pred = tapply(p, grp, mean), obs = tapply(y, grp, mean))
}

make_roc <- function(preds, metrics) {
  y <- preds$outcome
  roc_dt <- rbindlist(lapply(names(model_cols), function(nm) {
    p <- preds[[model_cols[nm]]]
    ok <- !is.na(p)
    r <- roc(y[ok], p[ok], quiet = TRUE)
    data.frame(model = nm, fpr = 1 - r$specificities, tpr = r$sensitivities)
  }))
  roc_dt$model <- factor(roc_dt$model, levels = names(model_cols))
  aucs <- metrics[recalibration == "none", .(model, AUROC)]
  aucs <- aucs[match(unname(model_cols), aucs$model)]
  aucs[, lab := sprintf("%s (%.3f)", names(model_cols), AUROC)]
  lab_map <- setNames(aucs$lab, names(model_cols))
  roc_dt[, model_lab := lab_map[as.character(model)]]
  ggplot(roc_dt, aes(fpr, tpr, color = model_lab)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = setNames(cols, aucs$lab)) +
    labs(x = "1 - Specificity", y = "Sensitivity", color = NULL) +
    theme(legend.position = "inside", legend.position.inside = c(0.62, 0.28),
          legend.text = element_text(size = 8),
          legend.background = element_rect(fill = "white", color = NA))
}

make_cal <- function(p_raw_xgb, p_platt_xgb, p_raw_lr, p_platt_lr, y) {
  cal_dt <- rbind(
    data.frame(cal_curve(p_raw_xgb, y),  model = "XGBoost (full)", recal = "Before"),
    data.frame(cal_curve(p_platt_xgb, y), model = "XGBoost (full)", recal = "After"),
    data.frame(cal_curve(p_raw_lr, y),   model = "LR (full)", recal = "Before"),
    data.frame(cal_curve(p_platt_lr, y), model = "LR (full)", recal = "After")
  )
  cal_dt$recal <- factor(cal_dt$recal, levels = c("Before", "After"))
  ggplot(cal_dt, aes(pred, obs, color = model, shape = recal)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
    geom_line(aes(linetype = recal), linewidth = 0.7) +
    geom_point(size = 2) +
    scale_color_manual(values = cols) +
    scale_shape_manual(values = c(16, 17)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(x = "Predicted probability", y = "Observed frequency",
         color = NULL, shape = "Recalibration", linetype = "Recalibration") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme(legend.position = "inside", legend.position.inside = c(0.98, 0.03),
          legend.justification.inside = c(1, 0),
          legend.text = element_text(size = 8), legend.key.width = unit(0.9, "lines"),
          legend.background = element_rect(fill = "white", color = NA))
}

row_lab <- function(txt) {
  wrap_elements(grid::textGrob(txt, rot = 90,
                gp = grid::gpar(fontfamily = "Liberation Sans",
                                fontsize = 11, fontface = "bold")))
}

# ---------------- INSPIRE ----------------
pi  <- readRDS("/workspace/inspire/preds_inspire.rds")
pic <- readRDS("/workspace/inspire/inspire_cv_preds.rds")   # primary cohort + CV-Platt cols
mi  <- fread("/workspace/inspire/metrics_inspire.csv")
pi  <- pi[excl_primary == FALSE]
stopifnot(nrow(pi) == nrow(pic))
mi <- mi[cohort == "INSPIRE_primary"]

p_roc_i <- make_roc(pi, mi) + ggtitle("Receiver operating characteristic") +
  labs(tag = "a")
p_cal_i <- make_cal(pi$XGB_full, pic$XGB_full_platt_cv,
                    pi$LR_full,  pic$LR_full_platt_cv, pi$outcome) +
  ggtitle("Calibration") + labs(tag = "b")

# ---------------- MOVER ----------------
pm  <- readRDS("/workspace/ext_out/preds_mover.rds")
pmc <- readRDS("/workspace/ext_out/preds_mover_cvrecal.rds")
mm  <- fread("/workspace/ext_out/metrics_mover.csv")

p_roc_m <- make_roc(pm, mm) + labs(tag = "c")
p_cal_m <- make_cal(pm$XGB_full, pmc$XGB_full_platt_cv,
                    pm$LR_full,  pmc$LR_full_platt_cv, pm$outcome) +
  labs(tag = "d")

# ---------------- assemble ----------------
p <- (row_lab("INSPIRE — temporal validation") + p_roc_i + p_cal_i +
      row_lab("MOVER — external validation")  + p_roc_m + p_cal_m) +
     plot_layout(widths = c(0.04, 1, 1)) &
     theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(FIG, "fig_validation_combined.svg"), p, width = 9, height = 8.2)
ggsave(file.path(FIG, "fig_validation_combined.png"), p, width = 9, height = 8.2, dpi = 300)
cat("combined validation figure saved\n")
