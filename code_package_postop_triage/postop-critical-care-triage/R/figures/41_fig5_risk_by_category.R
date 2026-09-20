#!/usr/bin/env Rscript
# 41_fig5_risk_by_category.R — Fig 5: distribution of the model's predicted risk
# (CV-Platt recalibrated vs composite outcome) across the four ICU-course-validated
# outcome categories, in INSPIRE and MOVER.
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(ggprism); library(svglite)
})

out_dir <- "/mnt/results/06_icu_course"
ins <- fread(file.path(out_dir, "preds_need_recal_inspire.csv"))
mov <- fread(file.path(out_dir, "preds_need_recal_mover.csv"))
ins[, cohort := "INSPIRE (Korea)"]
mov[, cohort := "MOVER (USA)"]

cat_levels <- c("icu_dependent", "observational_icu", "missed_escalation", "uncomplicated_ward")
cat_labels <- c("ICU-\ndependent", "Observational\nICU", "Missed\nescalation", "Uncomplicated\nward")

dt <- rbind(ins[, data.table(cohort, category, risk = XGB_full_platt_cv)],
            mov[, data.table(cohort, category, risk = XGB_full_platt_cv)])
dt[, category := factor(category, levels = cat_levels, labels = cat_labels)]
dt[, cohort := factor(cohort, levels = c("INSPIRE (Korea)", "MOVER (USA)"))]
dt[, lrisk := log10(risk)]

meds <- dt[, {
  qs <- quantile(lrisk, c(.25, .75))
  .(med = median(risk), lmed = median(lrisk),
    uw = min(max(lrisk), qs[2] + 1.5 * (qs[2] - qs[1])), n = .N)
}, by = .(cohort, category)]
cat("== medians (consistency check vs table3) ==\n"); print(meds)
cat("== risk quantiles by cohort ==\n")
print(dt[, as.list(quantile(risk, c(0, .001, .01, .5, .99, 1))), by = cohort])

# Okabe-Ito colorblind-friendly palette
pal <- setNames(c("#D55E00", "#0072B2", "#CC79A7", "#009E73"), cat_labels)

ymin <- floor(min(dt$lrisk))
brk  <- seq(ymin, 0)
pct  <- sapply(brk, function(b) {
  v <- 100 * 10^b
  if (v >= 1) sprintf("%g%%", v) else sprintf("%g%%", signif(v, 2))
})

p <- ggplot(dt, aes(x = category, y = lrisk, fill = category)) +
  geom_violin(scale = "width", trim = TRUE, alpha = 0.75, color = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, alpha = 0.95, color = "black", fill = "white") +
  geom_text(data = meds, aes(y = uw, label = sprintf("%.1f%%", 100 * med)),
            vjust = -0.5, size = 2.7, family = "Liberation Sans") +
  scale_y_continuous(breaks = brk, labels = pct,
                     expand = expansion(mult = c(0.03, 0.14))) +
  scale_fill_manual(values = pal, guide = "none") +
  facet_wrap(~cohort) +
  labs(x = NULL, y = "Predicted risk of composite outcome") +
  theme_prism(base_family = "Liberation Sans", base_size = 11) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.9),
        strip.text = element_text(face = "bold"),
        plot.margin = margin(8, 10, 8, 8))

ggsave(file.path(out_dir, "fig5_risk_by_category.svg"), p, width = 8.3, height = 4.6, device = svglite)
ggsave(file.path(out_dir, "fig5_risk_by_category.png"), p, width = 8.3, height = 4.6, dpi = 300)
cat("== 41 done ==\n")
