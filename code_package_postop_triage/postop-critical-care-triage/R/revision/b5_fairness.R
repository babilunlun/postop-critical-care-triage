#!/usr/bin/env Rscript
# ============================================================================
# B5 — Fairness of the triage operating point across demographic strata
# ============================================================================
# Revision analysis B5 (manuscript Supplementary Fig. S4, Table S14).
# Strata: sex (Female/Male) and age (<65, 65-79, >=80). Per cohort x stratum at
# the cohort-specific >=96%-sensitivity operating point (INSPIRE 0.02, MOVER
# 0.0455): AUROC (DeLong), sensitivity (Clopper-Pearson), low-tier NPV,
# low-tier coverage, calibration intercept; plus sensitivity-homogeneity
# chi-square tests.
#
# Inputs:
#   - preds_need_recal_inspire.csv / preds_need_recal_mover.csv (step 36-39)
#   - inspire_cohort.rds (step 25); analysis_mover_dedup.rds (step 42)
# Outputs:
#   - table_b5_fairness.csv
#   - fig_b5_fairness.svg/.png (basis of Suppl. Fig. S4)
#
# Paths refer to the original analysis workspace; adapt to your local layout.
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(svglite); library(pROC)})

OUT <- "/mnt/results/07_revision_analyses"
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
ins[, y := as.integer(any_need)]; mov[, y := as.integer(any_need)]
co    <- as.data.table(readRDS("/workspace/inspire/inspire_cohort.rds"))
mov_a <- as.data.table(readRDS("/workspace/ext_out/analysis_mover_dedup.rds"))
ins_c <- merge(ins, co[, .(op_id, age, sex)], by.x = "LOG_ID", by.y = "op_id", all.x = TRUE)
mov_c <- merge(mov, mov_a[, .(LOG_ID, age, sex)], by = "LOG_ID", all.x = TRUE)
mk <- function(dt) dt[, `:=`(
  age_grp = factor(fcase(age < 65, "<65", age < 80, "65-79", age >= 80, ">=80"), levels = c("<65","65-79",">=80")),
  sex_grp = fifelse(sex %in% c("F","Female"), "Female", "Male"))]
ins_c <- mk(ins_c); mov_c <- mk(mov_c)
cat("covariate match — INSPIRE:", mean(!is.na(ins_c$age)), " MOVER:", mean(!is.na(mov_c$age)), "\n")

thr <- c(INSPIRE = 0.02, MOVER = 0.0455)
cp <- function(x, n) as.numeric(binom.test(x, n)$conf.int)  # Clopper-Pearson

fair_one <- function(dt, cohort_nm) {
  t <- thr[[cohort_nm]]
  dims <- list(Sex = "sex_grp", Age = "age_grp")
  rbindlist(lapply(names(dims), function(dnm) {
    v <- dims[[dnm]]
    rbindlist(lapply(levels(factor(dt[[v]])), function(lv) {
      d <- dt[get(v) == lv & !is.na(xgb_need)]
      n <- nrow(d); ev <- sum(d$y); if (ev < 20 || (n - ev) < 20) return(NULL)
      roc_o <- pROC::roc(d$y, d$xgb_need, quiet = TRUE, direction = "<")
      ci_a <- as.numeric(pROC::ci.auc(roc_o, method = "delong"))
      low <- d$xgb_need < t
      tp <- sum(d$y == 1 & !low); fn <- sum(d$y == 1 & low)
      tn <- sum(d$y == 0 & low)
      s_ci <- cp(tp, tp + fn); n_ci <- cp(tn, tn + fn)
      cal <- glm(y ~ 1, offset = qlogis(xgb_need), family = binomial, data = d)
      b <- coef(cal); se <- sqrt(vcov(cal)[1, 1])
      data.table(cohort = cohort_nm, dimension = dnm, level = lv, n = n, events = ev,
                 need_pct = round(100 * ev / n, 2),
                 auroc = round(ci_a[2], 3), auroc_lo = round(ci_a[1], 3), auroc_hi = round(ci_a[3], 3),
                 sens = round(100 * tp / (tp + fn), 2), sens_lo = round(100 * s_ci[1], 2), sens_hi = round(100 * s_ci[2], 2),
                 npv_low = round(100 * tn / (tn + fn), 2), npv_lo = round(100 * n_ci[1], 2), npv_hi = round(100 * n_ci[2], 2),
                 low_cov_pct = round(100 * mean(low), 2),
                 cal_intercept = round(b, 3), cal_lo = round(b - 1.96 * se, 3), cal_hi = round(b + 1.96 * se, 3))
    }))
  }))
}
b5 <- rbind(fair_one(ins_c, "INSPIRE"), fair_one(mov_c, "MOVER"))
print(b5)
fwrite(b5, file.path(OUT, "tables", "table_b5_fairness.csv"))

# fairness summary: sensitivity parity vs the 96% design target, coverage ratio, tests
for (cc in c("INSPIRE", "MOVER")) {
  s <- b5[cohort == cc]
  cat(sprintf("\n%s: sensitivity range %.2f-%.2f%% (target >=96); coverage range %.1f-%.1f%% (ratio %.2f); cal-intercept range %.2f to %.2f\n",
      cc, min(s$sens), max(s$sens), min(s$low_cov_pct), max(s$low_cov_pct),
      max(s$low_cov_pct)/min(s$low_cov_pct), min(s$cal_intercept), max(s$cal_intercept)))
  for (dnm in c("Sex", "Age")) {
    dt <- get(if (cc == "INSPIRE") "ins_c" else "mov_c")[!is.na(xgb_need)]
    v <- if (dnm == "Sex") "sex_grp" else "age_grp"
    t <- thr[[cc]]
    tab <- dt[, .(tp = sum(y == 1 & xgb_need >= t), fn = sum(y == 1 & xgb_need < t)), by = get(v)]
    m <- as.matrix(tab[, .(tp, fn)]); p <- suppressWarnings(chisq.test(m)$p.value)
    cat(sprintf("  %s sensitivity homogeneity chi-square p = %.3g\n", dnm, p))
  }
}

# ---- figure: sensitivity / NPV / coverage by stratum ------------------------
long <- rbind(
  b5[, .(cohort, dimension, level, metric = "Sensitivity at operating point, %", est = sens, lo = sens_lo, hi = sens_hi)],
  b5[, .(cohort, dimension, level, metric = "Low-tier NPV, %",                  est = npv_low, lo = npv_lo, hi = npv_hi)],
  b5[, .(cohort, dimension, level, metric = "Low-tier coverage, %",             est = low_cov_pct, lo = NA_real_, hi = NA_real_)]
)
long[, level := factor(level, levels = c("<65", "65-79", ">=80", "Female", "Male"))]
long[, metric := factor(metric, levels = c("Sensitivity at operating point, %", "Low-tier NPV, %", "Low-tier coverage, %"))]
ref <- data.frame(cohort = c("INSPIRE", "MOVER"),
                  metric = factor("Sensitivity at operating point, %", levels = levels(long$metric)), y = 96)

p <- ggplot(long, aes(level, est, color = dimension)) +
  geom_hline(data = ref, aes(yintercept = y), linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.45, na.rm = TRUE) +
  geom_point(size = 1.8) +
  facet_grid(metric ~ cohort, scales = "free_y") +
  scale_color_manual(values = c(Age = "#0072B2", Sex = "#D55E00"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 8, base_family = "Liberation Sans") +
  theme(strip.background = element_rect(fill = "grey92", color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(4, 6, 2, 4))

svglite(file.path(OUT, "figures", "fig_b5_fairness.svg"), width = 6.7, height = 5.6)
print(p); dev.off()
png(file.path(OUT, "figures", "fig_b5_fairness.png"), width = 6.7, height = 5.6, units = "in", res = 300)
print(p); dev.off()
cat("DONE b5\n")
