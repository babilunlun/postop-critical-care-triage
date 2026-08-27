#!/usr/bin/env Rscript
# Regenerate single-panel figures that cannot be produced by SVG cropping:
#   figS5 (old S2, sensitivity planned ICU)  -> 4 native panels a-d
#   figS6 (old S3, DCA no-art ablation)      -> 3 native panels a-c
#   fig6  (old Fig 7, safety-efficiency frontier) -> 2 panels a/b (INSPIRE/MOVER)
#   fig7  (old Fig 6, DCA need-recalibrated)      -> 2 panels a/b
# No panel letters are drawn; legends live inside each panel.
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggprism); library(pROC)
  library(svglite); library(scales)
})

OUT <- "/workspace/regen_out"
dir.create(OUT, showWarnings = FALSE)

save_both <- function(g, name, w, h) {
  svglite(file.path(OUT, paste0(name, ".svg")), width = w, height = h)
  print(g); dev.off()
  ggsave(file.path(OUT, paste0(name, ".png")), g, width = w, height = h, dpi = 300)
  cat("saved", name, "\n")
}

# ============ figS5: sensitivity to outcome definition (MOVER subsets) ============
theme_set(theme_prism(base_family = "Liberation Sans", base_size = 11))
cv   <- readRDS("/workspace/sens_out/sens_cv_preds.rds")
res  <- fread("/workspace/sens_out/table_s1_sensitivity.csv")

cohort_cols <- c("Full cohort" = "#0072B2",
                 "Excluding routine-ICU procedures" = "#E69F00",
                 "Ambulatory surgery only" = "#009E73")
subset_map <- c(full = "Full cohort",
                S1_exclude_routineICU_90 = "Excluding routine-ICU procedures",
                S2_ambulatory_only = "Ambulatory surgery only")

# panel a: ROC of XGB_full across subsets
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

# panels b/c: calibration raw vs CV-Platt
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
    theme(legend.position = "none")
}
p_b <- mk_cal(cal_raw, 1.0)
p_c <- mk_cal(cal_pl, 1.0)

# panel d: AUROC + DeLong CI by subset and model
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

save_both(p_a, "figS5a", 4.7, 4.0)
save_both(p_b, "figS5b", 4.7, 4.0)
save_both(p_c, "figS5c", 4.7, 4.0)
save_both(p_d, "figS5d", 4.7, 4.0)

# ============ figS6: DCA of arterial-line ablation ============
dca  <- fread("/workspace/sens_out/dca_noart_curves.csv")
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
save_both(mk_dca("internal", -0.05, 0.22), "figS6a", 3.9, 3.8)
save_both(mk_dca("external_raw", -0.10, 0.45), "figS6b", 3.9, 3.8)
save_both(mk_dca("external_recalibrated", -0.10, 0.45), "figS6c", 3.9, 3.8)

# ============ fig6 / fig7: need-recalibrated triage analyses ============
theme_set(theme_bw(base_size = 11))
ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
ins[, y := as.integer(any_need)]
mov[, y := as.integer(any_need)]

# ---- fig7: DCA of need-recalibrated model (old Fig 6) ----
thr_grid <- seq(0, 0.50, by = 0.0025)
nb_curve <- function(pred, y, thr) {
  n <- length(y)
  rbindlist(lapply(thr, function(pt) {
    tr <- pred >= pt
    tp <- sum(tr & y == 1); fp <- sum(tr & y == 0)
    data.table(threshold = pt, nb = tp/n - fp/n * pt/(1-pt))
  }))
}
mk_dca_need <- function(dt, cohort_lab) {
  p <- mean(dt$y)
  pd <- rbind(nb_curve(dt$xgb_need, dt$y, thr_grid)[, series := "Model (need-recalibrated XGB)"],
              data.table(threshold = thr_grid, nb = p - (1-p)*thr_grid/(1-thr_grid),
                         series = "Treat all"),
              data.table(threshold = c(0, 0.5), nb = 0, series = "Treat none"))
  pd[, series := factor(series, levels = c("Model (need-recalibrated XGB)", "Treat all", "Treat none"))]
  ggplot(pd, aes(threshold, nb, colour = series, linetype = series)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = c(0.05, 0.30), linetype = 3, colour = "grey45", linewidth = 0.5) +
    annotate("text", x = 0.05, y = Inf, label = "5%", vjust = 1.6, hjust = -0.15, size = 3, colour = "grey30") +
    annotate("text", x = 0.30, y = Inf, label = "30%", vjust = 1.6, hjust = -0.15, size = 3, colour = "grey30") +
    scale_colour_manual(values = c("#0072B2", "#D55E00", "grey60")) +
    scale_linetype_manual(values = c(1, 2, 3)) +
    scale_y_continuous(expand = expansion(mult = c(0.06, 0.20))) +
    coord_cartesian(xlim = c(0, 0.5)) +
    labs(x = "Threshold probability for true postoperative critical care need",
         y = "Net benefit", title = cohort_lab, colour = NULL, linetype = NULL) +
    theme(text = element_text(family = "Liberation Sans"),
          plot.title = element_text(size = 11, hjust = 0.5),
          legend.position = "inside", legend.position.inside = c(0.97, 0.97),
          legend.justification = c(1, 1), legend.text = element_text(size = 8),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = element_blank())
}
save_both(mk_dca_need(ins, "INSPIRE (temporal)"), "fig7a", 4.6, 4.2)
save_both(mk_dca_need(mov, "MOVER (geographic)"), "fig7b", 4.6, 4.2)

# ---- fig6: safety-efficiency frontier (old Fig 7) ----
frontier <- function(pred, y, grid = seq(0.001, 0.10, by = 0.0005)) {
  rbindlist(lapply(grid, function(t) {
    low <- pred < t
    data.table(thr = t, sens = sum(!low & y==1)/sum(y==1),
               low_frac = mean(low), npv_low = 1 - mean(y[low]==1))
  }))
}
mk_frontier <- function(dt, cohort_lab) {
  f_m <- frontier(dt$xgb_need, dt$y)[, series := "Model (all ops)"]
  d <- dt[!is.na(sasa_need)]
  f_s <- frontier(d$sasa_need, d$y)[, series := "SASA (evaluable subset)"]
  fr <- rbind(f_m[, .(sens, low_frac, series)], f_s[, .(sens, low_frac, series)])
  # operating points at >=96% sensitivity (recomputed from data)
  t_m <- f_m[sens >= 0.96, max(thr)]
  t_s <- f_s[sens >= 0.96, max(thr)]
  ops <- rbind(f_m[thr == t_m, .(sens, low_frac, series = "Model (all ops)")],
               f_s[thr == t_s, .(sens, low_frac, series = "SASA (evaluable subset)")])
  cat(cohort_lab, "operating points:\n"); print(ops)
  ggplot(fr, aes(sens, 100*low_frac, colour = series)) +
    geom_line(linewidth = 0.8) +
    geom_vline(xintercept = 0.96, linetype = 3, colour = "grey45", linewidth = 0.5) +
    geom_point(data = ops, aes(sens, 100*low_frac, colour = series), size = 2.5) +
    geom_text(data = ops, aes(sens, 100*low_frac, label = paste0(round(100*low_frac), "%")),
              vjust = -0.9, size = 3, show.legend = FALSE) +
    annotate("text", x = 0.96, y = Inf, label = "96% sensitivity", vjust = 1.6, hjust = 1.05,
             size = 3, colour = "grey30") +
    scale_colour_manual(values = c("#0072B2", "#D55E00")) +
    scale_x_continuous(limits = c(0.80, 1.0), breaks = seq(0.8, 1, 0.05)) +
    labs(x = "Sensitivity for true critical care need (middle + high tiers)",
         y = "Operations safely triaged to low tier (%)", title = cohort_lab, colour = NULL) +
    theme(text = element_text(family = "Liberation Sans"),
          plot.title = element_text(size = 11, hjust = 0.5),
          legend.position = "inside", legend.position.inside = c(0.03, 0.03),
          legend.justification = c(0, 0), legend.text = element_text(size = 8),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = element_blank())
}
save_both(mk_frontier(ins, "INSPIRE"), "fig6a", 4.6, 4.2)
save_both(mk_frontier(mov, "MOVER"), "fig6b", 4.6, 4.2)

cat("all regenerated panels in", OUT, "\n")
