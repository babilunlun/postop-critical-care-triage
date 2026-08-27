#!/usr/bin/env Rscript
# 25b_inspire_outcome_fix.R — CORRECT the INSPIRE composite outcome
#
# Bug (found 2026-08-25 during ICU-course analysis): 25_inspire_cohort.R defined
#   outcome := as.integer(!is.na(icuin_time) | !is.na(inhosp_death_time))
# i.e. ANY recorded postop ICU admission / in-hospital death on the PATIENT-level
# relative clock, which spans admissions. The manuscript methods claim a 24h window.
# Consequences in the 98,633-op cohort:
#   - 3,552 ops had icuin_time AFTER discharge_time (later admissions, up to 11.6 y)
#   - 543 of 1,274 death records occurred after the index discharge
#   - 790 ops had same-admission delayed ICU (>24h postop) — not a disposition event
#
# Corrected definition (aligns code with manuscript; matches VitalDB/MOVER encounter scope):
#   early_icu   = icuin_time <= discharge_time (same admission)
#                 AND icuin_time <= anend_time + 24*60 (within 24h of surgery end;
#                 negative lag = patient already in ICU preop, counts as ICU-bound)
#   death_idx   = inhosp_death_time <= discharge_time + 1440 (index-admission death;
#                 +1d buffer for midnight/discharge-time noise)
#   outcome     = early_icu | death_idx
# Also derives v4 four-category flags:
#   delayed_icu = same-admission ICU with lag > 24h (missed-escalation signal)
#   outcome_anyicu = ORIGINAL buggy definition (audit trail only)
#
# Patches inspire_cohort.rds (old saved as inspire_cohort_v3bug.rds) and
# analysis_inspire.rds outcome column in place (TS features are outcome-independent).

suppressPackageStartupMessages(library(data.table))
DIR <- "/workspace/inspire"

ops <- fread(file.path(DIR, "operations.csv.gz"),
             select = c("op_id", "discharge_time"))
co <- readRDS(file.path(DIR, "inspire_cohort.rds"))
stopifnot(!"discharge_time" %in% names(co))
co <- merge(co, ops, by = "op_id", all.x = TRUE)
stopifnot(!any(is.na(co$discharge_time)))

# --- corrected flags ---
co[, lag_icu_h := as.numeric(icuin_time - anend_time) / 60]
co[, early_icu := !is.na(icuin_time) & icuin_time <= discharge_time &
     (icuin_time - anend_time) <= 24 * 60]
co[, delayed_icu := !is.na(icuin_time) & icuin_time <= discharge_time &
     (icuin_time - anend_time) > 24 * 60]
co[, death_idx := !is.na(inhosp_death_time) & inhosp_death_time <= discharge_time + 1440]
co[, outcome_anyicu := as.integer(!is.na(icuin_time) | !is.na(inhosp_death_time))]
co[, outcome_new := as.integer(early_icu | death_idx)]

cat("=== cohort", nrow(co), "ops ===\n")
cat("old outcome events:", sum(co$outcome_anyicu), sprintf("(%.2f%%)\n", 100 * mean(co$outcome_anyicu)))
cat("new outcome events:", sum(co$outcome_new), sprintf("(%.2f%%)\n", 100 * mean(co$outcome_new)))
cat("  early_icu:", sum(co$early_icu), "| delayed_icu:", sum(co$delayed_icu),
    "| death_idx:", sum(co$death_idx), "\n")
cat("  cross-admission ICU artifacts removed:",
    sum(!is.na(co$icuin_time) & co$icuin_time > co$discharge_time), "\n")
cat("  post-discharge death records removed:",
    sum(!is.na(co$inhosp_death_time) & co$inhosp_death_time > co$discharge_time + 1440), "\n")

# --- backup + overwrite cohort ---
file.rename(file.path(DIR, "inspire_cohort.rds"), file.path(DIR, "inspire_cohort_v3bug.rds"))
co[, outcome := NULL]
setnames(co, "outcome_new", "outcome")
saveRDS(co, file.path(DIR, "inspire_cohort.rds"))
cat("inspire_cohort.rds rewritten (old -> inspire_cohort_v3bug.rds)\n")

# --- patch analysis_inspire.rds outcome column (TS features unaffected) ---
ai <- readRDS(file.path(DIR, "analysis_inspire.rds"))
cat("\nanalysis_inspire.rds:", nrow(ai), "ops; old events:", sum(ai$outcome), "\n")
ai[, outcome := NULL]
ai <- merge(ai, co[, .(op_id, outcome, early_icu, delayed_icu, death_idx,
                       outcome_anyicu, lag_icu_h)],
            by = "op_id", all.x = TRUE, sort = FALSE)
stopifnot(!any(is.na(ai$outcome)))
saveRDS(ai, file.path(DIR, "analysis_inspire.rds"))
cat("analysis_inspire.rds patched; new events:", sum(ai$outcome), "\n")
cat("== 25b done ==\n")
