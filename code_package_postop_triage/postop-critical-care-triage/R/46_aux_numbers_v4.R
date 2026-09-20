#!/usr/bin/env Rscript
# 46_aux_numbers_v4.R — auxiliary numbers for the v4 manuscript text.
suppressPackageStartupMessages(library(data.table))

# (1) INSPIRE xgb_need<2% safety check
pi <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
lo <- pi[xgb_need < 0.02]
cat("INSPIRE xgb_need<2%:", nrow(lo), sprintf("(%.1f%%)", 100 * nrow(lo) / nrow(pi)),
    "| missed", sprintf("%.3f%%", 100 * mean(lo$category == "missed_escalation")),
    "| died", sprintf("%.3f%%", 100 * mean(lo$died)), "\n")

# (2) INSPIRE missed_escalation breakdown + composite partition
rc <- fread("/mnt/results/06_icu_course/refined_categories_inspire.csv")
ms <- rc[category == "missed_escalation"]
cat("missed total", nrow(ms), "| delayed-only", sum(ms$delayed_icu & !ms$death_idx),
    "| died", sum(ms$death_idx == 1), "\n")
cat("composite events:", sum(rc$outcome), "= early_icu", sum(rc$early_icu),
    "+ death w/o early_icu", sum(!rc$early_icu & rc$death_idx == 1), "\n")
cat("early_icu total:", sum(rc$early_icu), "= dep", sum(rc$category == "icu_dependent"),
    "+ obs", sum(rc$category == "observational_icu"), "\n")

# (3) MOVER middle-band: share of all missed escalations falling in middle band (context)
pm <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
cat("MOVER low<5% tier: n", sum(pm$tier == "low_<5%"),
    sprintf("(%.1f%%)", 100 * mean(pm$tier == "low_<5%")),
    "| missed", sprintf("%.3f%%", 100 * mean(pm$tier == "low_<5%" & pm$category == "missed_escalation") / mean(pm$tier == "low_<5%")),
    "\n")

# (4) INSPIRE middle band share of missed escalations and deaths (for narrative)
mb <- pi[tier == "middle_5-30%"]
cat("INSPIRE middle band: missed n", sum(mb$category == "missed_escalation"),
    "of total missed", sum(pi$category == "missed_escalation"),
    sprintf("(%.1f%%)", 100 * sum(mb$category == "missed_escalation") / sum(pi$category == "missed_escalation")),
    "| middle-band deaths", sum(mb$died), "of", sum(pi$died),
    sprintf("(%.1f%%)", 100 * sum(mb$died) / sum(pi$died)), "\n")
cat("== 46 done ==\n")
