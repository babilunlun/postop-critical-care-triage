#!/usr/bin/env Rscript
# ============================================================================
# B1 — Pathway-constrained impact simulation at fixed safety
# ============================================================================
# Revision analysis B1 (manuscript Table 5, Fig. 6, Supplementary Table S9).
# Safety rule: >=96% of true-need cases must fall ABOVE the low-tier threshold
# (i.e., into the middle + high tiers). Impact metrics: ICU bed-days freed,
# senior-review workload, SASA comparator at equal safety.
#
# Inputs  (produced by the numbered pipeline; see README):
#   - preds_need_recal_inspire.csv / preds_need_recal_mover.csv
#     (need-recalibrated predictions + ICU-course categories; step 36-39)
# Outputs:
#   - table_b1_impact_simulation.csv, table_b1_sasa_comparator.csv
#   - fig_b1_safety_efficiency_frontier.svg/.png (basis of Fig. 6)
#   - table5_impact_simulation_v5.csv (manuscript-formatted Table 5)
#
# Paths refer to the original analysis workspace; adapt to your local layout.
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(svglite)})

OUT <- "/mnt/results/07_revision_analyses"
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
ins[, y := as.integer(any_need)]
mov[, y := as.integer(any_need)]
ins[, los_d := as.numeric(icu_los_h)/24]

cat("INSPIRE ICU LOS (days) by category:\n")
print(ins[admitted == TRUE, .(n=.N, median_los=round(median(los_d, na.rm=TRUE),2),
                              mean_los=round(mean(los_d, na.rm=TRUE),2),
                              total_bd=round(sum(los_d, na.rm=TRUE),0)), by=category][order(-n)])

# ---- safety-efficiency frontier: low-tier coverage vs sensitivity ----------
frontier <- function(pred, y, grid = seq(0.001, 0.10, by = 0.0005)) {
  rbindlist(lapply(grid, function(t) {
    low <- pred < t
    data.table(thr = t, sens = sum(!low & y==1)/sum(y==1),
               low_frac = mean(low), npv_low = 1 - mean(y[low]==1))
  }))
}
f_ins <- frontier(ins$xgb_need, ins$y)
f_mov <- frontier(mov$xgb_need, mov$y)
t_ins <- f_ins[sens >= 0.96, max(thr)]
t_mov <- f_mov[sens >= 0.96, max(thr)]
cat("\n96%-sensitivity thresholds: INSPIRE", t_ins, " MOVER", t_mov, "\n")

# ---- impact simulation at the safety-first operating point -----------------
impact <- function(dt, t, nm, los_available = TRUE) {
  low  <- dt$xgb_need < t
  high <- dt$xgb_need >= 0.30
  mid  <- !low & !high
  n <- nrow(dt); need_tot <- sum(dt$y==1)
  res <- data.table(
    cohort = nm, threshold = t,
    sens_need = round(sum(!low & dt$y==1)/need_tot, 4),
    low_n = sum(low), low_pct = round(100*mean(low),1),
    low_need_missed_n = sum(low & dt$y==1),
    low_need_missed_pct_of_need = round(100*sum(low & dt$y==1)/need_tot, 2),
    low_deaths = sum(low & dt$died==1),
    npv_low = round(100*(1 - mean(dt$y[low]==1)), 2),
    mid_n = sum(mid), mid_per1000 = round(1000*mean(mid),0),
    high_n = sum(high), high_per1000 = round(1000*mean(high),0))
  res[, `:=`(
    escap_captured_pct = round(100*sum(!low & dt$category=="missed_escalation")/max(1,sum(dt$category=="missed_escalation")),1),
    deaths_captured_pct = round(100*sum(!low & dt$died==1)/max(1,sum(dt$died==1)),1))]
  if (los_available) {
    bd_total <- sum(dt$los_d[dt$admitted==TRUE], na.rm=TRUE)
    bd_low_obs <- sum(dt$los_d[low & dt$category=="observational_icu"], na.rm=TRUE)
    bd_low_dep <- sum(dt$los_d[low & dt$category=="icu_dependent"], na.rm=TRUE)
    res[, `:=`(bd_total_per1000 = round(1000*bd_total/n,1),
               bd_freed_per1000 = round(1000*bd_low_obs/n,1),
               bd_freed_pct = round(100*bd_low_obs/bd_total,1),
               bd_low_dependent_per1000 = round(1000*bd_low_dep/n,1),
               low_obs_admissions_per1000 = round(1000*sum(low & dt$category=="observational_icu")/n,1))]
  }
  res
}
b1_ins <- impact(ins, t_ins, "INSPIRE", TRUE)
b1_mov <- impact(mov, t_mov, "MOVER", FALSE)
print(t(b1_ins)); print(t(b1_mov))

# ---- review pool (middle band) and MOVER admission counts ------------------
review_pool <- function(dt, t, nm) {
  low <- dt$xgb_need < t; high <- dt$xgb_need >= 0.30; mid <- !low & !high
  data.table(cohort = nm,
    mid_obs_adm_per1000 = round(1000*sum(mid & dt$category=="observational_icu")/nrow(dt),1),
    mid_obs_beddays_per1000 = if ("los_d" %in% names(dt)) round(1000*sum(dt$los_d[mid & dt$category=="observational_icu"], na.rm=TRUE)/nrow(dt),1) else NA_real_)
}
rp <- rbind(review_pool(ins, t_ins, "INSPIRE"), review_pool(mov, t_mov, "MOVER"))
print(rp)

low_mov <- mov$xgb_need < t_mov
cat("MOVER low-tier observational admissions per 1000 ops:",
    round(1000*sum(low_mov & mov$category=="observational_icu")/nrow(mov),1), "\n")
cat("MOVER total observational admissions per 1000 ops:",
    round(1000*sum(mov$category=="observational_icu")/nrow(mov),1), "\n")

# ---- SASA comparator at the SAME 96% sensitivity (evaluable subsets) -------
sasa_cmp <- function(dt, nm) {
  d <- dt[!is.na(sasa_need)]
  g <- frontier(d$sasa_need, d$y)
  t_s <- g[sens >= 0.96, max(thr)]
  low_s <- d$sasa_need < t_s
  low_m <- d$xgb_need < (if (nm=="INSPIRE") t_ins else t_mov)
  data.table(cohort = nm,
    sasa_thr = t_s, sasa_sens = round(g[thr==t_s, sens],4),
    sasa_low_pct = round(100*mean(low_s),1),
    sasa_npv_low = round(100*(1-mean(d$y[low_s]==1)),2),
    sasa_low_deaths = sum(low_s & d$died==1),
    model_low_pct_same_subset = round(100*mean(low_m),1),
    model_npv_low_same_subset = round(100*(1-mean(d$y[low_m]==1)),2))
}
b1_sasa <- rbind(sasa_cmp(ins, "INSPIRE"), sasa_cmp(mov, "MOVER"))
print(b1_sasa)

# ---- consolidated B1 tables -------------------------------------------------
b1_tab <- rbind(b1_ins, b1_mov, fill = TRUE)
b1_tab <- merge(b1_tab, rp, by = "cohort")
fwrite(b1_tab, file.path(OUT, "tables", "table_b1_impact_simulation.csv"))
fwrite(b1_sasa, file.path(OUT, "tables", "table_b1_sasa_comparator.csv"))
cat("\nsaved B1 tables\n")

# ---- safety-efficiency frontier figure --------------------------------------
fr_all <- rbindlist(list(
  f_ins[, .(sens, low_frac, series = "Model (all ops)", cohort = "INSPIRE")],
  f_mov[, .(sens, low_frac, series = "Model (all ops)", cohort = "MOVER")],
  {d <- ins[!is.na(sasa_need)]; frontier(d$sasa_need, d$y)[, .(sens, low_frac, series = "SASA (evaluable subset)", cohort = "INSPIRE")]},
  {d <- mov[!is.na(sasa_need)]; frontier(d$sasa_need, d$y)[, .(sens, low_frac, series = "SASA (evaluable subset)", cohort = "MOVER")]}
))
ops_pt <- data.table(
  cohort = c("INSPIRE","INSPIRE","MOVER","MOVER"),
  series = rep(c("Model (all ops)","SASA (evaluable subset)"), 2),
  sens = c(0.9626, 0.9604, 0.9605, 0.9641),
  low_frac = c(0.578, 0.202, 0.280, 0.109))

g1 <- ggplot(fr_all, aes(sens, 100*low_frac, colour = series)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 0.96, linetype = 3, colour = "grey45", linewidth = 0.5) +
  geom_point(data = ops_pt, aes(sens, 100*low_frac, colour = series), size = 2.5) +
  geom_text(data = ops_pt, aes(sens, 100*low_frac, label = paste0(round(100*low_frac), "%")),
            vjust = -0.9, size = 3, show.legend = FALSE) +
  annotate("text", x = 0.96, y = Inf, label = "96% sensitivity", vjust = 1.6, hjust = 1.05,
           size = 3, colour = "grey30") +
  facet_wrap(~cohort) +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  scale_x_continuous(limits = c(0.80, 1.0), breaks = seq(0.8, 1, 0.05)) +
  labs(x = "Sensitivity for true critical care need (middle + high tiers)",
       y = "Operations safely triaged to low tier (%)", colour = NULL) +
  theme_bw(base_size = 11) +
  theme(text = element_text(family = "Liberation Sans"),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA))

svglite(file.path(OUT, "figures", "fig_b1_safety_efficiency_frontier.svg"), width = 8, height = 4.2)
print(g1); dev.off()
ggsave(file.path(OUT, "figures", "fig_b1_safety_efficiency_frontier.png"), g1, width = 8, height = 4.2, dpi = 300)

# ---- manuscript-formatted Table 5 -------------------------------------------
imp  <- fread(file.path(OUT, "tables", "table_b1_impact_simulation.csv"))
sasa <- fread(file.path(OUT, "tables", "table_b1_sasa_comparator.csv"))
fmt_row <- function(label, vi, vm) data.table(Metric = label, INSPIRE = vi, MOVER = vm)
i <- imp[cohort == "INSPIRE"]; m <- imp[cohort == "MOVER"]
si <- sasa[cohort == "INSPIRE"]; sm <- sasa[cohort == "MOVER"]
t5 <- rbind(
  fmt_row("Low-tier risk threshold, %", sprintf("%.1f", i$threshold*100), sprintf("%.1f", m$threshold*100)),
  fmt_row("Sensitivity for true need, %", sprintf("%.1f", i$sens_need*100), sprintf("%.1f", m$sens_need*100)),
  fmt_row("Low-tier coverage, % (n)", sprintf("%.1f (%s)", i$low_pct, format(i$low_n, big.mark = ",")),
                                     sprintf("%.1f (%s)", m$low_pct, format(m$low_n, big.mark = ","))),
  fmt_row("Low-tier NPV for true need, %", sprintf("%.2f", i$npv_low), sprintf("%.2f", m$npv_low)),
  fmt_row("Low-tier mortality, per 1,000 operations", sprintf("%.2f", i$low_deaths/i$low_n*1000),
                                                      sprintf("%.2f", m$low_deaths/m$low_n*1000)),
  fmt_row("Missed escalations above low-tier threshold, %", sprintf("%.1f", i$escap_captured_pct), sprintf("%.1f", m$escap_captured_pct)),
  fmt_row("Deaths above low-tier threshold, %", sprintf("%.1f", i$deaths_captured_pct), sprintf("%.1f", m$deaths_captured_pct)),
  fmt_row("Intermediate band, per 1,000 operations", sprintf("%.0f", i$mid_per1000), sprintf("%.0f", m$mid_per1000)),
  fmt_row("High tier, per 1,000 operations", sprintf("%.0f", i$high_per1000), sprintf("%.0f", m$high_per1000)),
  fmt_row("Total ICU bed-days, per 1,000 operations", sprintf("%.1f", i$bd_total_per1000), "NA"),
  fmt_row("Directly avoidable bed-days, per 1,000 (% of total)", sprintf("%.1f (%.1f)", i$bd_freed_per1000, i$bd_freed_pct), "NA"),
  fmt_row("Review pool: intermediate-band observational admissions, per 1,000", sprintf("%.1f", i$mid_obs_adm_per1000), sprintf("%.1f", m$mid_obs_adm_per1000)),
  fmt_row("Review pool: intermediate-band observational bed-days, per 1,000", sprintf("%.0f", i$mid_obs_beddays_per1000), "NA"),
  fmt_row("SASA at equal 96% safety: low-tier coverage, %", sprintf("%.1f", si$sasa_low_pct), sprintf("%.1f", sm$sasa_low_pct)),
  fmt_row("Model on same SASA-evaluable subset: low-tier coverage, %", sprintf("%.1f", si$model_low_pct_same_subset), sprintf("%.1f", sm$model_low_pct_same_subset)),
  fmt_row("SASA vs model NPV on evaluable subset, %", sprintf("%.2f vs %.2f", si$sasa_npv_low, si$model_npv_low_same_subset),
                                                     sprintf("%.2f vs %.2f", sm$sasa_npv_low, sm$model_npv_low_same_subset))
)
fwrite(t5, "/mnt/results/04_manuscript/table5_impact_simulation_v5.csv")
print(t5)
cat("DONE b1\n")
