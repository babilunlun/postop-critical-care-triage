#!/usr/bin/env Rscript
# ============================================================================
# B3 — Decision curve analysis on validation cohorts, tied to triage thresholds
# ============================================================================
# Revision analysis B3 (manuscript Fig. 7, Supplementary Table S10).
# Outcome: any_need (ICU-dependent course | missed escalation | death).
# Prediction: xgb_need (need-recalibrated XGB) — the score behind the triage
# pathway. Net benefit across thresholds 0-50%, with the 5%/30% triage band
# boundaries marked.
#
# Inputs:
#   - preds_need_recal_inspire.csv / preds_need_recal_mover.csv (step 36-39)
# Outputs:
#   - table_b3_dca_net_benefit.csv
#   - fig_b3_dca_validation.svg/.png (basis of Fig. 7)
#
# Paths refer to the original analysis workspace; adapt to your local layout.
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(svglite); library(scales)})

OUT <- "/mnt/results/07_revision_analyses"
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
ins[, y := as.integer(any_need)]
mov[, y := as.integer(any_need)]
cat("INSPIRE need prevalence:", mean(ins$y), "| MOVER:", mean(mov$y), "\n")

thr_grid <- seq(0, 0.50, by = 0.0025)
nb_curve <- function(pred, y, thr) {
  n <- length(y)
  rbindlist(lapply(thr, function(pt) {
    tr <- pred >= pt
    tp <- sum(tr & y == 1); fp <- sum(tr & y == 0)
    data.table(threshold = pt, nb = tp/n - fp/n * pt/(1-pt),
               treat_per1000 = 1000*mean(tr), tp_per1000 = 1000*tp/n, fp_per1000 = 1000*fp/n)
  }))
}
cur <- rbindlist(list(
  nb_curve(ins$xgb_need, ins$y, thr_grid)[, cohort := "INSPIRE (temporal)"],
  nb_curve(mov$xgb_need, mov$y, thr_grid)[, cohort := "MOVER (geographic)"]
))
# treat-all NB = prevalence - (1-prevalence)*pt/(1-pt)
ta <- rbindlist(lapply(c("INSPIRE (temporal)", "MOVER (geographic)"), function(cc) {
  p <- if (grepl("INSPIRE", cc)) mean(ins$y) else mean(mov$y)
  data.table(cohort = cc, threshold = thr_grid, nb = p - (1-p)*thr_grid/(1-thr_grid))
}))

# Net benefit at the triage thresholds (and 2% stricter boundary)
key_thr <- c(0.02, 0.05, 0.10, 0.30)
b3_tab <- rbindlist(lapply(key_thr, function(pt) {
  rbindlist(list(
    nb_curve(ins$xgb_need, ins$y, pt)[, cohort := "INSPIRE"],
    nb_curve(mov$xgb_need, mov$y, pt)[, cohort := "MOVER"]
  ))
}))
b3_tab[, `:=`(nb_per1000 = round(1000*nb, 2), treat_per1000 = round(treat_per1000, 1),
              tp_per1000 = round(tp_per1000, 2), fp_per1000 = round(fp_per1000, 2))]
b3_tab <- b3_tab[, .(cohort, threshold, nb_per1000, treat_per1000, tp_per1000, fp_per1000)]
print(b3_tab)
fwrite(b3_tab, file.path(OUT, "tables", "table_b3_dca_net_benefit.csv"))

# ---- DCA figure (two panels, treat-all / treat-none references) -------------
pd <- cur[, .(cohort, threshold, nb, series = "Model (need-recalibrated XGB)")]
pd <- rbind(pd, ta[, .(cohort, threshold, nb, series = "Treat all")])
pd <- rbind(pd, data.table(cohort = rep(unique(pd$cohort), each=2),
                           threshold = rep(c(0, 0.5), 2), nb = 0, series = "Treat none"))
pd[, series := factor(series, levels = c("Model (need-recalibrated XGB)", "Treat all", "Treat none"))]

g <- ggplot(pd, aes(threshold, nb, colour = series, linetype = series)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = c(0.05, 0.30), linetype = 3, colour = "grey45", linewidth = 0.5) +
  annotate("text", x = 0.05, y = Inf, label = "5%", vjust = 1.5, hjust = -0.15, size = 3, colour = "grey30") +
  annotate("text", x = 0.30, y = Inf, label = "30%", vjust = 1.5, hjust = -0.15, size = 3, colour = "grey30") +
  facet_wrap(~cohort, scales = "free_y") +
  scale_colour_manual(values = c("#0072B2", "#D55E00", "grey60")) +
  scale_linetype_manual(values = c(1, 2, 3)) +
  coord_cartesian(xlim = c(0, 0.5)) +
  labs(x = "Threshold probability for true postoperative critical care need",
       y = "Net benefit", colour = NULL, linetype = NULL) +
  theme_bw(base_size = 11) +
  theme(text = element_text(family = "Liberation Sans"),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA))

svglite(file.path(OUT, "figures", "fig_b3_dca_validation.svg"), width = 8, height = 4.2)
print(g); dev.off()
ggsave(file.path(OUT, "figures", "fig_b3_dca_validation.png"), g, width = 8, height = 4.2, dpi = 300)
cat("DONE b3\n")
