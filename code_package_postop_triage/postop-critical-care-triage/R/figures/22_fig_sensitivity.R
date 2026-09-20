#!/usr/bin/env Rscript
# Supplementary figures for sensitivity analyses:
#   fig_s2: planned-ICU sensitivity (ROC + calibration pre/post recalibration + AUROC CIs)
#   fig_s3: noArt ablation DCA (internal, external raw, external recalibrated)
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggprism); library(patchwork)
  library(pROC); library(svglite)
})

OUT <- "/workspace/sens_out"
theme_set(theme_prism(base_family = "Liberation Sans", base_size = 11))

cv   <- readRDS(file.path(OUT, "sens_cv_preds.rds"))
res  <- fread(file.path(OUT, "table_s1_sensitivity.csv"))
dca  <- fread(file.path(OUT, "dca_noart_curves.csv"))

cohort_cols <- c("Full cohort" = "#0072B2",
                 "Excluding routine-ICU procedures" = "#E69F00",
                 "Ambulatory surgery only" = "#009E73")
subset_map <- c(full = "Full cohort",
                S1_exclude_routineICU_90 = "Excluding routine-ICU procedures",
                S2_ambulatory_only = "Ambulatory surgery only")

# ================= fig_s2 =================
# ---- panel a: ROC of XGB_full across cohorts ----
roc_dt <- rbindlist(lapply(names(subset_map), function(sn) {
  d <- cv[[sn]]
  r <- roc(d$outcome, d$XGB_full, quiet = TRUE)
  a <- as.numeric(auc(r))
  data.frame(cohort = sprintf("%s (%.3f)", subset_map[sn], a),
             fpr = 1 - r$specificities, tpr = r$sensitivities)
}))
levs <- sprintf("%s (%.3f)", subset_map,
                sapply(names(subset_map), function(sn)
                  as.numeric(auc(roc(cv[[sn]]$outcome, cv[[sn]]$XGB_full, quiet = TRUE)))))
roc_dt[, cohort := factor(cohort, levels = levs)]
cols_lab <- setNames(unname(cohort_cols), levs)

p_a <- ggplot(roc_dt, aes(fpr, tpr, color = cohort)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols_lab) +
  labs(x = "1 - Specificity", y = "Sensitivity", color = NULL) +
  theme(legend.position = "inside", legend.position.inside = c(0.55, 0.22),
        legend.text = element_text(size = 7.5))

# ---- panels b/c: calibration of XGB_full, raw vs CV-Platt ----
cal_curve <- function(p, y, g = 15) {
  ok <- !is.na(p) & !is.na(y)
  p <- p[ok]; y <- y[ok]
  br <- unique(quantile(p, probs = seq(0, 1, length.out = g + 1)))
  grp <- cut(p, br, include.lowest = TRUE)
  data.frame(pred = tapply(p, grp, mean), obs = tapply(y, grp, mean))
}
cal_raw <- rbindlist(lapply(names(subset_map), function(sn) {
  d <- cv[[sn]]
  data.frame(cal_curve(d$XGB_full, d$outcome), cohort = subset_map[sn])
}))
cal_pl <- rbindlist(lapply(names(subset_map), function(sn) {
  d <- cv[[sn]]
  data.frame(cal_curve(d$XGB_full_cvplatt, d$outcome), cohort = subset_map[sn])
}))
mk_cal <- function(dd, xmax) {
  dd[, cohort := factor(cohort, levels = unname(subset_map))]
  ggplot(dd, aes(pred, obs, color = cohort)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey60") +
    geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
    scale_color_manual(values = cohort_cols) +
    labs(x = "Predicted probability", y = "Observed frequency", color = NULL) +
    coord_cartesian(xlim = c(0, xmax), ylim = c(0, xmax)) +
    theme(legend.position = "none")   # shared cohort legend lives in panel a
}
p_b <- mk_cal(cal_raw, 1.0)
p_c <- mk_cal(cal_pl, 1.0)

# ---- panel d: AUROC + DeLong CI by subset and model ----
pd <- res[subset %in% names(subset_map)]
pd[, cohort := factor(subset_map[subset], levels = unname(subset_map))]
pd[, model := factor(model, levels = c("SASA", "LR_full", "XGB_noArt", "XGB_full"))]
model_cols <- c(SASA = "#CC79A7", LR_full = "#E69F00",
                XGB_noArt = "#56B4E9", XGB_full = "#009E73")
p_d <- ggplot(pd, aes(AUROC, cohort, color = model)) +
  geom_errorbarh(aes(xmin = AUROC_lo, xmax = AUROC_hi),
                 height = 0.25, position = position_dodge(width = 0.6),
                 show.legend = FALSE) +
  geom_point(size = 2.2, position = position_dodge(width = 0.6)) +
  scale_color_manual(values = model_cols,
                     labels = c("SASA", "LR (full)", "XGBoost (no Art)", "XGBoost (full)")) +
  scale_x_continuous(limits = c(0.5, 0.85)) +
  labs(x = "AUROC (95% CI)", y = NULL, color = NULL) +
  theme(legend.position = "inside", legend.position.inside = c(0.03, 0.03),
        legend.justification = c(0, 0),
        legend.text = element_text(size = 7.5),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA))

p_s2 <- (p_a | p_b) / (p_c | p_d) + plot_annotation(tag_levels = "a")
ggsave(file.path(OUT, "fig_s2_sensitivity_planned_icu.svg"), p_s2, width = 9, height = 8)
ggsave(file.path(OUT, "fig_s2_sensitivity_planned_icu.png"), p_s2, width = 9, height = 8, dpi = 300)

# ================= fig_s3: DCA =================
dca_mod <- dca[variable %in% c("XGB_full", "XGB_noArt", "all", "none")]
dca_mod[, series := fcase(variable == "XGB_full", "XGBoost (full)",
                          variable == "XGB_noArt", "XGBoost (no Art)",
                          variable == "all", "Treat all",
                          variable == "none", "Treat none")]
series_cols <- c("XGBoost (full)" = "#009E73", "XGBoost (no Art)" = "#56B4E9",
                 "Treat all" = "grey60", "Treat none" = "black")
series_lt <- c("XGBoost (full)" = "solid", "XGBoost (no Art)" = "solid",
               "Treat all" = "dashed", "Treat none" = "dotted")
panel_titles <- c(internal = "Internal test (VitalDB)",
                  external_raw = "External (MOVER), uncalibrated",
                  external_recalibrated = "External (MOVER), recalibrated")

mk_dca <- function(cc, ymin, ymax) {
  dd <- dca_mod[cohort == cc]
  dd[, series := factor(series, levels = names(series_cols))]
  ggplot(dd, aes(threshold, net_benefit, color = series, linetype = series)) +
    geom_hline(yintercept = 0, color = "grey85", linewidth = 0.4) +
    geom_line(linewidth = 0.7) +
    scale_color_manual(values = series_cols) +
    scale_linetype_manual(values = series_lt) +
    labs(x = "Threshold probability", y = "Net benefit",
         title = panel_titles[cc], color = NULL, linetype = NULL) +
    coord_cartesian(xlim = c(0.05, 0.50), ylim = c(ymin, ymax)) +
    theme(plot.title = element_text(size = 9.5),
          legend.position = "inside", legend.position.inside = c(0.58, 0.72),
          legend.text = element_text(size = 7.5),
          legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA))
}
p1 <- mk_dca("internal", -0.05, 0.22)
p2 <- mk_dca("external_raw", -0.10, 0.45)
p3 <- mk_dca("external_recalibrated", -0.10, 0.45)

p_s3 <- p1 | p2 | p3
p_s3 <- p_s3 + plot_annotation(tag_levels = "a")
ggsave(file.path(OUT, "fig_s3_dca_noart.svg"), p_s3, width = 11, height = 3.8)
ggsave(file.path(OUT, "fig_s3_dca_noart.png"), p_s3, width = 11, height = 3.8, dpi = 300)
cat("figures saved\n")
