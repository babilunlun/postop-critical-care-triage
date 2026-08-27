#!/usr/bin/env Rscript
# Regenerate ONLY fig7a/b (DCA need-recalibrated) with extra top headroom so the
# 5% / 30% threshold labels are not clipped. Matches original regen_single_panels.R
# exactly (theme_bw, y := as.integer(any_need)) plus the scale_y expansion fix.
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(svglite); library(scales)
})
OUT <- "/workspace/regen_out"; dir.create(OUT, showWarnings = FALSE)
save_both <- function(g, name, w, h) {
  svglite(file.path(OUT, paste0(name, ".svg")), width = w, height = h); print(g); dev.off()
  ggsave(file.path(OUT, paste0(name, ".png")), g, width = w, height = h, dpi = 300)
  cat("saved", name, "\n")
}
theme_set(theme_bw(base_size = 11))
ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
ins[, y := as.integer(any_need)]
mov[, y := as.integer(any_need)]
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
              data.table(threshold = thr_grid, nb = p - (1-p)*thr_grid/(1-thr_grid), series = "Treat all"),
              data.table(threshold = c(0, 0.5), nb = 0, series = "Treat none"))
  pd[, series := factor(series, levels = c("Model (need-recalibrated XGB)", "Treat all", "Treat none"))]
  # explicit label height: 10% of the y-range above the data max, i.e. halfway into
  # the 20% top expansion -> both threshold labels sit at the same, fully visible height
  lab_y <- max(pd$nb, na.rm = TRUE) + 0.10 * diff(range(pd$nb, na.rm = TRUE))
  ggplot(pd, aes(threshold, nb, colour = series, linetype = series)) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = c(0.05, 0.30), linetype = 3, colour = "grey45", linewidth = 0.5) +
    annotate("text", x = 0.05, y = lab_y, label = "5%",  vjust = 0.5, hjust = -0.15, size = 3, colour = "grey30") +
    annotate("text", x = 0.30, y = lab_y, label = "30%", vjust = 0.5, hjust = -0.15, size = 3, colour = "grey30") +
    scale_colour_manual(values = c("#0072B2", "#D55E00", "grey60")) +
    scale_linetype_manual(values = c(1, 2, 3)) +
    scale_y_continuous(expand = expansion(mult = c(0.06, 0.20))) +
    coord_cartesian(xlim = c(0, 0.5)) +
    labs(x = "Threshold probability for true postoperative critical care need",
         y = "Net benefit", title = cohort_lab, colour = NULL, linetype = NULL) +
    theme(text = element_text(family = "Liberation Sans"),
          plot.title = element_text(size = 11, hjust = 0.5),
          legend.position = "inside", legend.position.inside = c(0.03, 0.03),
          legend.justification = c(0, 0), legend.text = element_text(size = 8),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.key = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = element_blank())
}
save_both(mk_dca_need(ins, "INSPIRE (temporal)"), "fig7a", 4.6, 4.2)
save_both(mk_dca_need(mov, "MOVER (geographic)"), "fig7b", 4.6, 4.2)
