#!/usr/bin/env Rscript
# Feature engineering: per-case intraoperative time-series features + clinical features
# Input: /workspace/vitaldb_ts/batch_*.rds, vitaldb_cases.csv
# Output: /workspace/analysis_vitaldb.rds (one row per case)

suppressPackageStartupMessages({
  library(data.table)
})

TS_DIR <- "/workspace/vitaldb_ts"
CASES_FILE <- "/workspace/vitaldb_cases.csv"
OUT_FILE <- "/workspace/analysis_vitaldb.rds"

# ---------- load time series ----------
ts_files <- list.files(TS_DIR, pattern = "^batch_.*\\.rds$", full.names = TRUE)
stopifnot(length(ts_files) > 0)
ts <- rbindlist(lapply(ts_files, readRDS))
cat("loaded", length(ts_files), "batches,", uniqueN(ts$caseid), "cases,",
    nrow(ts), "rows\n")

# unify channels: prefer arterial MAP over NIBP; merge two EtCO2 sources
ts[channel == "etco2_alt", channel := "etco2"]
ts[, channel := fifelse(channel == "map_art", "map_art",
                 fifelse(channel == "map_nibp", "map_nibp", channel))]

# wide per case-minute
w <- dcast(ts, caseid + tmin ~ channel, value.var = "v",
           fun.aggregate = mean, fill = NA_real_)
# combined MAP: arterial preferred
w[, map := fifelse(!is.na(map_art), map_art, map_nibp)]

# ---------- per-case time-series features ----------
safe_sd <- function(x) if (sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else NA_real_
slope <- function(t, y) {
  ok <- !is.na(y) & !is.na(t)
  if (sum(ok) < 5) return(NA_real_)
  coef(lm(y ~ t, subset = ok))[2]
}
# longest consecutive run below threshold (in minutes)
longest_run <- function(flag) {
  flag <- ifelse(is.na(flag), FALSE, flag)
  r <- rle(flag)
  if (!any(r$values)) return(0)
  as.numeric(max(r$lengths[r$values]))
}

feat <- w[, {
  n_min <- .N
  dur <- max(tmin) + 1
  list(
    n_min_valid = n_min,
    dur_min = dur,
    coverage = n_min / dur,
    # MAP
    map_mean = mean(map, na.rm = TRUE),
    map_sd = safe_sd(map),
    map_cv = safe_sd(map) / mean(map, na.rm = TRUE),
    map_min = suppressWarnings(min(map, na.rm = TRUE)),
    map_max = suppressWarnings(max(map, na.rm = TRUE)),
    map_slope = slope(tmin, map),
    map_early = mean(map[tmin <= 30], na.rm = TRUE),
    map_late = mean(map[tmin > max(tmin) - 30], na.rm = TRUE),
    # hypotension burden
    min_map65 = sum(map < 65, na.rm = TRUE),
    min_map60 = sum(map < 60, na.rm = TRUE),
    area_map65 = sum(pmax(65 - map, 0), na.rm = TRUE),          # mmHg*min
    area_map60 = sum(pmax(60 - map, 0), na.rm = TRUE),
    twa_map65 = sum(pmax(65 - map, 0), na.rm = TRUE) / dur,     # time-weighted
    longest_map65 = longest_run(map < 65),
    pct_map65 = sum(map < 65, na.rm = TRUE) / sum(!is.na(map)),
    # HR
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = safe_sd(hr),
    hr_cv = safe_sd(hr) / mean(hr, na.rm = TRUE),
    hr_min = suppressWarnings(min(hr, na.rm = TRUE)),
    hr_max = suppressWarnings(max(hr, na.rm = TRUE)),
    min_hr100 = sum(hr > 100, na.rm = TRUE),
    min_hr50 = sum(hr < 50, na.rm = TRUE),
    hr_slope = slope(tmin, hr),
    # SpO2
    spo2_mean = mean(spo2, na.rm = TRUE),
    spo2_min = suppressWarnings(min(spo2, na.rm = TRUE)),
    min_spo2_92 = sum(spo2 < 92, na.rm = TRUE),
    min_spo2_90 = sum(spo2 < 90, na.rm = TRUE),
    # EtCO2
    etco2_mean = mean(etco2, na.rm = TRUE),
    etco2_sd = safe_sd(etco2),
    min_etco2_low = sum(etco2 < 30, na.rm = TRUE),
    min_etco2_high = sum(etco2 > 45, na.rm = TRUE),
    # BIS (depth of anesthesia)
    bis_mean = mean(bis, na.rm = TRUE),
    bis_sd = safe_sd(bis),
    min_bis40 = sum(bis < 40, na.rm = TRUE),
    min_bis60 = sum(bis > 60, na.rm = TRUE),
    # monitoring source
    has_art = any(!is.na(map_art))
  )
}, by = caseid]

# clean Inf/-Inf from empty windows
for (j in names(feat)) {
  if (is.numeric(feat[[j]])) {
    set(feat, which(is.infinite(feat[[j]])), j, NA_real_)
  }
}

# ---------- clinical features + outcome ----------
cases <- fread(CASES_FILE)
cohort <- cases[age >= 18 & ane_type == "General"]
cohort[, outcome := as.integer(icu_days > 0 | death_inhosp == 1)]
cohort[, ane_dur_min := fifelse(!is.na(opend) & opend > 0 & !is.na(anestart),
                                (opend - pmax(anestart, 0)) / 60,
                                (caseend - casestart) / 60)]

clin_cols <- c("caseid", "outcome", "age", "sex", "bmi", "asa", "emop",
               "department", "preop_htn", "preop_dm",
               "preop_hb", "preop_plt", "preop_na", "preop_k", "preop_gluc",
               "preop_alb", "preop_ast", "preop_alt", "preop_bun", "preop_cr",
               "intraop_ebl", "intraop_uo", "intraop_rbc", "intraop_ffp",
               "intraop_crystalloid", "intraop_colloid",
               "intraop_ppf", "intraop_mdz", "intraop_ftn", "intraop_rocu",
               "intraop_eph", "intraop_phe", "intraop_epi",
               "ane_dur_min")
clin <- cohort[, ..clin_cols]

analysis <- merge(clin, feat, by = "caseid", all.x = TRUE)
cat("analysis dataset:", nrow(analysis), "cases; events:",
    sum(analysis$outcome), sprintf("(%.1f%%)", 100 * mean(analysis$outcome)), "\n")
cat("with time-series features:", sum(!is.na(analysis$map_mean)), "\n")

saveRDS(analysis, OUT_FILE)
fwrite(analysis, sub("\\.rds$", ".csv", OUT_FILE))
cat("saved:", OUT_FILE, "\n")
