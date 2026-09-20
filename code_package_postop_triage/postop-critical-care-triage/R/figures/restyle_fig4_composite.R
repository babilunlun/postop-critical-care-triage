#!/usr/bin/env Rscript
# restyle_fig4_composite.R — journal-style composed Fig 4 (2 cohorts x ROC|cal)
# Same data as 44_fig4_v4.R; restyled: 7 pt base, thin axes, no in-panel titles,
# hero linewidth for XGBoost, shared color encoding (no repeated color legend in
# calibration panels), row titles above rows, panel letters a-d.
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(pROC); library(patchwork); library(svglite)
})
OUT <- "/workspace/figures_restyle"
dir.create(OUT, showWarnings = FALSE)

cols <- c("SASA" = "#CC79A7", "LR (clinical)" = "#0072B2",
          "LR (full)" = "#E69F00", "XGBoost (full)" = "#009E73")
model_cols <- c(SASA = "SASA", `LR (clinical)` = "LR_clinical",
                `LR (full)` = "LR_full", `XGBoost (full)` = "XGB_full")
lws <- c("SASA" = 0.45, "LR (clinical)" = 0.45, "LR (full)" = 0.45,
         "XGBoost (full)" = 0.95)

base_theme <- theme_classic(base_family = "Liberation Sans", base_size = 7) +
  theme(
    axis.line = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks = element_line(linewidth = 0.4, colour = "black"),
    axis.ticks.length = unit(0.6, "mm"),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6.5, colour = "black"),
    legend.text = element_text(size = 6),
    legend.key.width = unit(2.0, "lines"),
    legend.key.height = unit(0.7, "lines"),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.margin = margin(1, 2, 1, 2),
    plot.tag = element_text(face = "bold", size = 8),
    plot.tag.position = c(0.02, 0.98),
    plot.margin = margin(2, 4, 2, 2)
  )

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
  roc_dt[, model_lab := setNames(aucs$lab, names(model_cols))[as.character(model)]]
  ggplot(roc_dt, aes(fpr, tpr, color = model_lab, linewidth = model_lab)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted",
                color = "grey70", linewidth = 0.4) +
    geom_line() +
    scale_color_manual(values = setNames(cols, aucs$lab)) +
    scale_linewidth_manual(values = setNames(lws, aucs$lab)) +
    labs(x = "1 - Specificity", y = "Sensitivity", color = NULL) +
    guides(linewidth = "none") +
    base_theme +
    theme(legend.position = "inside", legend.position.inside = c(0.98, 0.02),
          legend.justification.inside = c(1, 0))
}

make_cal <- function(p_raw_xgb, p_platt_xgb, p_raw_lr, p_platt_lr, y) {
  cal_dt <- rbind(
    data.frame(cal_curve(p_raw_xgb, y),   model = "XGBoost (full)", recal = "Before"),
    data.frame(cal_curve(p_platt_xgb, y), model = "XGBoost (full)", recal = "After"),
    data.frame(cal_curve(p_raw_lr, y),    model = "LR (full)",      recal = "Before"),
    data.frame(cal_curve(p_platt_lr, y),  model = "LR (full)",      recal = "After")
  )
  cal_dt$recal <- factor(cal_dt$recal, levels = c("Before", "After"))
  ggplot(cal_dt, aes(pred, obs, color = model, linetype = recal, shape = recal)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted",
                color = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 0.45) +
    geom_point(size = 0.9, stroke = 0.2) +
    scale_color_manual(values = cols) +
    scale_shape_manual(values = c(16, 17)) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    labs(x = "Predicted probability", y = "Observed frequency",
         linetype = "Recalibration", shape = "Recalibration") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    guides(color = "none") +
    base_theme +
    theme(legend.position = "inside", legend.position.inside = c(0.98, 0.02),
          legend.justification.inside = c(1, 0))
}

row_title <- function(txt) {
  wrap_elements(grid::textGrob(txt, x = 0.03, just = "left",
                gp = grid::gpar(fontfamily = "Liberation Sans",
                                fontsize = 7.5, fontface = "bold")))
}

# ---------------- data (identical to 44_fig4_v4.R) ----------------
pi  <- readRDS("/workspace/inspire/preds_inspire.rds")
pic <- readRDS("/workspace/inspire/inspire_cv_preds.rds")
mi  <- fread("/workspace/inspire/metrics_inspire.csv")
pi  <- pi[excl_primary == FALSE]
stopifnot(nrow(pi) == nrow(pic), nrow(pi) == 96196)
mi  <- mi[cohort == "INSPIRE_primary"]

pm  <- readRDS("/workspace/ext_out/preds_mover_dedup.rds")
pmc <- readRDS("/workspace/ext_out/preds_mover_cvrecal_dedup.rds")
mm  <- fread("/workspace/ext_out/metrics_mover_dedup.csv")
stopifnot(nrow(pm) == 48370, nrow(pm) == nrow(pmc))

p_roc_i <- make_roc(pi, mi) + labs(tag = "a")
p_cal_i <- make_cal(pi$XGB_full, pic$XGB_full_platt_cv,
                    pi$LR_full,  pic$LR_full_platt_cv, pi$outcome) + labs(tag = "b")
p_roc_m <- make_roc(pm, mm) + labs(tag = "c")
p_cal_m <- make_cal(pm$XGB_full, pmc$XGB_full_platt_cv,
                    pm$LR_full,  pmc$LR_full_platt_cv, pm$outcome) + labs(tag = "d")

p <- (row_title("INSPIRE — temporal validation") / (p_roc_i | p_cal_i) /
      row_title("MOVER — external validation") / (p_roc_m | p_cal_m)) +
     plot_layout(heights = c(0.07, 1, 0.07, 1))

W <- 183 / 25.4; H <- 168 / 25.4
ggsave(file.path(OUT, "fig4_restyle_composite.png"), p, width = W, height = H, dpi = 600)
ggsave(file.path(OUT, "fig4_restyle_composite.svg"), p, width = W, height = H)
cat("== composite done ==\n")
