#!/usr/bin/env Rscript
# ============================================================================
# B2 — Calibration-drift decomposition of the external calibration intercept
# ============================================================================
# Revision analysis B2 (manuscript Results; Supplementary Table S2, Fig. S1).
# Decomposes the observed external calibration intercept a_obs into:
#   a_obs = [logit(prev_ext) - logit(prev_dev)]   prevalence / outcome-definition
#           - [meanLP_ext - meanLP_dev]           case-mix as scored by the model
#           + residual                            distribution shape / measurement
# Development reference: VitalDB internal temporal test set.
#
# Inputs:
#   - preds_need_recal_inspire.csv / preds_need_recal_mover.csv (step 36-39)
#   - model_out/test_predictions.csv (VitalDB internal test predictions; step 03)
# Outputs:
#   - table_b2_calibration_decomposition.csv
#   - fig_b2_calibration_decomposition.svg/.png (basis of Suppl. Fig. S1)
#
# Paths refer to the original analysis workspace; adapt to your local layout.
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(svglite)})

OUT <- "/mnt/results/07_revision_analyses"
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
te  <- fread("/mnt/shared-workspace/shared/model_out/test_predictions.csv")

logit <- function(p) log(p/(1-p))
clip  <- function(p) pmin(pmax(p, 1e-6), 1-1e-6)
calfit <- function(y, p) {
  lp <- logit(clip(p))
  unname(coef(glm(y ~ lp, family = binomial)))
}

# 0) anchor: reproduce the manuscript calibration intercept/slope on the composite
cat("INSPIRE composite prevalence:", mean(ins$outcome), "\n")
cat("MOVER   composite prevalence:", mean(mov$outcome), "\n")
cat("INSPIRE cal (raw XGB):     ", round(calfit(ins$outcome, ins$XGB_full), 3), "\n")
cat("INSPIRE cal (platt_cv):    ", round(calfit(ins$outcome, ins$XGB_full_platt_cv), 3), "\n")
cat("MOVER   cal (raw XGB):     ", round(calfit(mov$outcome, mov$XGB_full), 3), "\n")
cat("MOVER   cal (platt_cv):    ", round(calfit(mov$outcome, mov$XGB_full_platt_cv), 3), "\n")

# 1) development reference (VitalDB internal test)
prev_dev  <- mean(te$outcome)
mlp_dev   <- mean(logit(clip(te$XGB_full)))
cal_dev   <- unname(coef(glm(outcome ~ logit(clip(XGB_full)), data = te, family = binomial)))
cat(sprintf("VitalDB internal test: n=%d, prevalence=%.3f, meanLP=%.3f, cal intercept/slope=%.3f/%.3f\n",
            nrow(te), prev_dev, mlp_dev, cal_dev[1], cal_dev[2]))

# 2) decomposition per external cohort
decomp <- function(y, p, nm) {
  lp <- logit(clip(p))
  a_mle <- unname(coef(glm(y ~ 1, offset = lp, family = binomial)))   # intercept-only recalibration MLE
  two   <- unname(coef(glm(y ~ lp, family = binomial)))               # intercept+slope
  prev  <- mean(y); mlp <- mean(lp)
  comp_prev    <- logit(prev) - logit(prev_dev)                       # prevalence / case-definition
  comp_casemix <- -(mlp - mlp_dev)                                    # case-mix as scored by model
  resid        <- a_mle - comp_prev - comp_casemix                    # distribution/measurement residual
  data.table(cohort = nm, n = length(y), prevalence = round(prev, 4),
             intercept_mle = round(a_mle, 3), slope = round(two[2], 3),
             prevalence_component = round(comp_prev, 3),
             casemix_component   = round(comp_casemix, 3),
             residual_component  = round(resid, 3),
             pct_prevalence = round(100*comp_prev/a_mle, 1),
             pct_casemix    = round(100*comp_casemix/a_mle, 1),
             pct_residual   = round(100*resid/a_mle, 1))
}
b2_tab <- rbind(decomp(ins$outcome, ins$XGB_full, "INSPIRE"),
                decomp(mov$outcome, mov$XGB_full, "MOVER"))
print(b2_tab)
fwrite(b2_tab, file.path(OUT, "tables", "table_b2_calibration_decomposition.csv"))

# 3) waterfall figure
wf <- rbindlist(lapply(1:nrow(b2_tab), function(i) {
  r <- b2_tab[i]
  comps <- data.table(
    cohort = r$cohort,
    comp = factor(c("Prevalence", "Case-mix", "Residual", "Observed"),
                  levels = c("Prevalence", "Case-mix", "Residual", "Observed")),
    val  = c(r$prevalence_component, r$casemix_component, r$residual_component, r$intercept_mle))
  comps[, `:=`(start = c(0, cumsum(val[1:2]), 0, 0)[1:4], end = cumsum(val))]
  comps[comp == "Observed", `:=`(start = 0, end = val)]
  comps
}))

g2 <- ggplot(wf) +
  geom_rect(aes(xmin = as.integer(comp) - 0.38, xmax = as.integer(comp) + 0.38,
                ymin = pmin(start, end), ymax = pmax(start, end), fill = comp), alpha = 0.85) +
  geom_segment(data = wf[comp != "Observed"],
               aes(x = as.integer(comp) + 0.38, xend = as.integer(comp) + 0.62,
                   y = end, yend = end), colour = "grey50", linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.5) +
  facet_wrap(~cohort, scales = "free_y") +
  scale_fill_manual(values = c("#0072B2", "#56B4E9", "#E69F00", "grey35"), guide = "none") +
  scale_x_continuous(breaks = 1:4, labels = levels(wf$comp)) +
  labs(x = NULL, y = "Calibration intercept contribution (log-odds)") +
  theme_bw(base_size = 11) +
  theme(text = element_text(family = "Liberation Sans"),
        axis.text.x = element_text(size = 9.5),
        plot.margin = margin(6, 12, 6, 12),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA))

svglite(file.path(OUT, "figures", "fig_b2_calibration_decomposition.svg"), width = 8, height = 4)
print(g2); dev.off()
ggsave(file.path(OUT, "figures", "fig_b2_calibration_decomposition.png"), g2, width = 8, height = 4, dpi = 300)
cat("DONE b2\n")
