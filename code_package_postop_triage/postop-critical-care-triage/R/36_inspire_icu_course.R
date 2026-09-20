#!/usr/bin/env Rscript
# 36_inspire_icu_course.R — extract ICU-course interventions from ward_vitals + medications
# for every operation in analysis_inspire.rds (98,633 ops).
#
# ICU-level interventions (within the ICU window [icuin_time, icuout_time]):
#   ward_vitals flags (value==1): vent (mechanical ventilation), crrt, ecmo, iabp
#   medications (route=="iv"):    norepinephrine, epinephrine, dopamine, dobutamine,
#                                 phenylephrine, vasopressin, terlipressin, milrinone
# Intraoperative administrations (chart_time within [anstart_time, anend_time]) are
# EXCLUDED — they are anesthesia decisions, not ICU therapy (matters when the ICU
# window overlaps the intraop window, i.e. patient already in ICU preop).
# Output: inspire_icu_course.rds (per-op flags) + sanity checks.

suppressPackageStartupMessages(library(data.table))
setDTthreads(8)
DIR <- "/workspace/inspire"

ai <- readRDS(file.path(DIR, "analysis_inspire.rds"))
co <- readRDS(file.path(DIR, "inspire_cohort.rds"))
ops <- merge(ai[, .(op_id, subject_id, outcome, early_icu, delayed_icu, death_idx)],
             co[, .(op_id, anstart_time, anend_time, icuin_time, icuout_time,
                    discharge_time)],
             by = "op_id", all.x = TRUE, sort = FALSE)
stopifnot(!any(is.na(ops$subject_id)))
cat("ops:", nrow(ops), "| with ICU window:", sum(!is.na(ops$icuin_time) & !is.na(ops$icuout_time)), "\n")

# ---------- 1. ward_vitals intervention flags ----------
wv <- fread(file.path(DIR, "ward_vitals.csv.gz"))
cat("ward_vitals rows:", nrow(wv), "\n")
flag_items <- c("vent", "crrt", "ecmo", "iabp")
wv <- wv[item_name %in% flag_items & value == 1]
cat("intervention-flag rows (value==1):", nrow(wv), "\n")
print(wv[, .N, by = item_name])

# keep only subjects in cohort, then non-equi join on the ICU window
wv <- wv[subject_id %in% ops$subject_id]
ops_icu <- ops[!is.na(icuin_time) & !is.na(icuout_time)]
setnames(wv, "chart_time", "t")
wv_join <- wv[ops_icu, on = .(subject_id, t >= icuin_time, t <= icuout_time),
              .(op_id, item_name), nomatch = 0]
cat("ward_vitals rows inside ICU windows:", nrow(wv_join), "\n")
wv_flags <- dcast(wv_join[, .N, by = .(op_id, item_name)], op_id ~ item_name,
                  value.var = "N", fill = 0)
for (cc in flag_items) if (!cc %in% names(wv_flags)) wv_flags[, (cc) := 0L]

# ---------- 2. vasopressor / inotrope infusions from medications ----------
vaso_drugs <- c("norepinephrine", "epinephrine", "dopamine", "dobutamine",
                "phenylephrine", "vasopressin", "terlipressin", "milrinone")
meds <- fread(file.path(DIR, "medications.csv.gz"))
meds[, drug_name := tolower(drug_name)]
meds <- meds[drug_name %in% vaso_drugs & route == "iv"]
cat("\niv vasopressor/inotrope rows:", nrow(meds), "\n")
print(meds[, .N, by = drug_name])
meds <- meds[subject_id %in% ops$subject_id]
setnames(meds, "chart_time", "t")
# ICU window membership
m_join <- meds[ops_icu, on = .(subject_id, t >= icuin_time, t <= icuout_time),
               .(op_id, drug_name, t), nomatch = 0]
# exclude intraop administrations (ICU window overlapping surgery)
m_join <- merge(m_join, ops_icu[, .(op_id, anstart_time, anend_time)], by = "op_id")
m_join <- m_join[!(t >= anstart_time & t <= anend_time)]
cat("iv vaso rows inside ICU windows (excl. intraop):", nrow(m_join), "\n")
m_flags <- dcast(m_join[, .N, by = .(op_id, drug_name)], op_id ~ drug_name,
                 value.var = "N", fill = 0)

# ---------- 3. assemble per-op flags ----------
out <- ops[, .(op_id, subject_id, outcome, early_icu, delayed_icu, death_idx,
               icuin_time, icuout_time, discharge_time)]
out <- merge(out, wv_flags, by = "op_id", all.x = TRUE)
for (cc in flag_items) set(out, which(is.na(out[[cc]])), cc, 0L)
out <- merge(out, m_flags, by = "op_id", all.x = TRUE)
for (cc in vaso_drugs) if (!cc %in% names(out)) out[, (cc) := 0L]
for (cc in intersect(vaso_drugs, names(out))) set(out, which(is.na(out[[cc]])), cc, 0L)

out[, `:=`(
  icu_vent = as.integer(vent > 0),
  icu_crrt = as.integer(crrt > 0),
  icu_ecmo = as.integer(ecmo > 0),
  icu_iabp = as.integer(iabp > 0),
  icu_vaso = as.integer(norepinephrine > 0 | epinephrine > 0 | dopamine > 0 |
                        dobutamine > 0 | phenylephrine > 0 | vasopressin > 0 |
                        terlipressin > 0 | milrinone > 0),
  icu_vaso_ne = as.integer(norepinephrine > 0)
)]
out[, icu_intervention := as.integer(icu_vent | icu_crrt | icu_ecmo | icu_iabp | icu_vaso)]
out[, icu_los_h := as.numeric(icuout_time - icuin_time) / 60]

saveRDS(out, file.path(DIR, "inspire_icu_course.rds"))

# ---------- 4. sanity checks ----------
cat("\n===== SANITY CHECKS =====\n")
e <- out[early_icu == TRUE]
cat("early-ICU ops:", nrow(e), "\n")
cat("  with >=1 ICU intervention:", sum(e$icu_intervention),
    sprintf("(%.1f%%)\n", 100 * mean(e$icu_intervention)))
cat("  vent:", sum(e$icu_vent), sprintf("(%.1f%%)", 100 * mean(e$icu_vent)),
    "| vaso:", sum(e$icu_vaso), sprintf("(%.1f%%)", 100 * mean(e$icu_vaso)),
    "| crrt:", sum(e$icu_crrt), "| ecmo:", sum(e$icu_ecmo), "| iabp:", sum(e$icu_iabp), "\n")
d <- out[delayed_icu == TRUE]
cat("delayed-ICU ops:", nrow(d), "| with intervention:", sum(d$icu_intervention),
    sprintf("(%.1f%%)\n", 100 * mean(d$icu_intervention)))
cat("ICU LOS (h) quantiles among early-ICU:\n")
print(quantile(e$icu_los_h, c(.05, .25, .5, .75, .95), na.rm = TRUE))
# coverage: fraction of early-ICU ops with ANY ward_vitals record inside window
# (sample-based; full table re-read, any item)
set.seed(1)
samp <- e[sample(.N, min(1000, .N))]
wv_any <- fread(file.path(DIR, "ward_vitals.csv.gz"))
wv_any <- wv_any[subject_id %in% samp$subject_id]
setnames(wv_any, "chart_time", "t")
m_cov <- merge(wv_any, samp[, .(op_id, subject_id, icuin_time, icuout_time)],
               by = "subject_id", allow.cartesian = TRUE)
inwin <- m_cov[t >= icuin_time & t <= icuout_time]
cat("early-ICU ops with ANY ward_vitals row in window (n=1000 sample):",
    uniqueN(inwin$op_id), sprintf("(%.1f%%)\n", 100 * uniqueN(inwin$op_id) / nrow(samp)))
cat("== 36 done ==\n")
