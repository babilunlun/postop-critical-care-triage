#!/usr/bin/env Rscript
# MOVER flowsheets -> 1-min intraop vitals + I/O -> VitalDB-identical features
# Input: /workspace/mover/slim/slim_*.csv (grep-extracted), mover_emr.rds
# Output: /workspace/mover/analysis_mover.rds

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

SLIM <- "/workspace/mover/slim"
OUT <- "/workspace/mover"
setDTthreads(4)

emr <- readRDS(file.path(OUT, "mover_emr.rds"))
keep_ids <- unique(emr$LOG_ID)

# ---------- load slim flowsheets ----------
files <- sort(list.files(SLIM, pattern = "\\.csv\\.gz$", full.names = TRUE))
stopifnot(length(files) > 0)
cols <- c("LOG_ID", "FLO_MEAS_NAME", "RECORDED_TIME", "MEAS_VALUE",
          "AN_START_DATETIME", "AN_STOP_DATETIME")
hdr <- c("OR_CASE_ID","LOG_ID","PAT_ID","MRN","HSP_ACCOUNT_ID","OR_LINK_CSN",
         "PAT_ENC_CSN_ID","ENC_TYPE_C","ENC_TYPE_NM","SURGERY_DATE","IN_OR_DTTM",
         "OUT_OR_DTTM","AN_START_DATETIME","AN_STOP_DATETIME","INPATIENT_DATA_ID",
         "FSD_ID","FLO_MEAS_ID","FLO_TEMPLATE_NAME","FLO_NAME","FLO_MEAS_NAME",
         "FLO_DISPLAY_NAME","RECORD_TYPE","RECORDED_TIME","MEAS_VALUE","UNITS",
         "MEAS_COMMENT","LINE")
fs <- rbindlist(lapply(files, function(f) {
  dt <- fread(f, select = cols, showProgress = FALSE)
  if (!"LOG_ID" %in% names(dt)) {  # headerless part file
    dt <- fread(f, col.names = hdr, showProgress = FALSE)
    dt <- dt[, ..cols]
  }
  dt[LOG_ID %in% keep_ids]
}), fill = TRUE)
cat("slim rows in cohort:", nrow(fs), "\n")

parse_dt <- function(x) {
  x <- trimws(x)
  as.POSIXct(suppressWarnings(parse_date_time(x, orders = c("ymd HMS", "mdy HM", "mdy HMS"))))
}
fs[, `:=`(rec_t = parse_dt(RECORDED_TIME),
          an_start = parse_dt(AN_START_DATETIME),
          an_stop = parse_dt(AN_STOP_DATETIME))]
fs <- fs[!is.na(rec_t) & !is.na(an_start) & !is.na(an_stop)]
fs[, tmin := floor(as.numeric(difftime(rec_t, an_start, units = "mins")))]
fs[, dur := as.numeric(difftime(an_stop, an_start, units = "mins"))]
fs <- fs[tmin >= 0 & tmin <= dur]   # within anesthesia window

# ---------- channel mapping ----------
ch_map <- c(
  "UC ANE R ARTERIAL LINE MAP - ART" = "map_art",
  "UC ANE R ARTERIAL LINE - ART" = "bp_art",
  "UC ANE R BLOOD PRESSURE - MAP" = "map_nibp",
  "ANESTHESIA BLOOD PRESSURE" = "bp_nibp",
  "UC ANE HEART RATE" = "hr",
  "UC ANE R PULSE OXIMETRY" = "spo2",
  "UC ANE R VENT ETCO2" = "etco2",
  "UC ANE R BIS" = "bis",
  "IP R ESTIMATED BLOOD LOSS" = "ebl",
  "UC ANE R URINE OUTPUT" = "uo",
  "IP R MAINTENANCE FLUID VOLUME" = "cryst"
)
fs[, ch := ch_map[FLO_MEAS_NAME]]
fs <- fs[!is.na(ch)]

# parse values: "120/80" for BP rows, numeric otherwise
parse_bp_map <- function(v) {
  m <- regmatches(v, regexec("^\\s*(\\d+)\\s*/\\s*(\\d+)", v))
  sapply(m, function(x) {
    if (length(x) < 3) return(NA_real_)
    s <- as.numeric(x[2]); d <- as.numeric(x[3])
    (s + 2 * d) / 3
  })
}
fs[ch %in% c("bp_art", "bp_nibp"), map_bp := parse_bp_map(MEAS_VALUE)]
fs[, v := suppressWarnings(as.numeric(MEAS_VALUE))]
fs[ch %in% c("bp_art", "bp_nibp"), v := map_bp]
fs[ch == "bp_art", ch := "map_art2"]   # fallback arterial MAP
fs[ch == "bp_nibp", ch := "map_nibp2"] # fallback NIBP MAP
fs <- fs[!is.na(v)]

# plausibility ranges (identical to VitalDB pipeline)
fs[ch %in% c("map_art", "map_art2", "map_nibp", "map_nibp2") & (v < 20 | v > 250), v := NA_real_]
fs[ch == "hr" & (v < 20 | v > 250), v := NA_real_]
fs[ch == "spo2" & (v < 50 | v > 100), v := NA_real_]
fs[ch == "etco2" & (v < 0 | v > 150), v := NA_real_]
fs[ch == "bis" & (v < 0 | v > 100), v := NA_real_]
fs <- fs[!is.na(v)]
cat("after cleaning:", nrow(fs), "rows,", uniqueN(fs$LOG_ID), "cases\n")

# ---------- per case-minute aggregation ----------
minutely <- fs[ch %in% c("map_art", "map_art2", "map_nibp", "map_nibp2",
                         "hr", "spo2", "etco2", "bis"),
               .(v = mean(v)), by = .(LOG_ID, tmin, ch)]
w <- dcast(minutely, LOG_ID + tmin ~ ch, value.var = "v")
# combined MAP: direct art MAP > art BP-derived > NIBP MAP > NIBP-derived
w[, map := fifelse(!is.na(map_art), map_art,
            fifelse(!is.na(map_art2), map_art2,
             fifelse(!is.na(map_nibp), map_nibp, map_nibp2)))]
w[, map_art_any := fifelse(!is.na(map_art) | !is.na(map_art2), 1, NA_real_)]

# ---------- per-case features (identical formulas to 02_features.R) ----------
safe_sd <- function(x) if (sum(!is.na(x)) > 1) sd(x, na.rm = TRUE) else NA_real_
slope <- function(t, y) {
  ok <- !is.na(y) & !is.na(t)
  if (sum(ok) < 5) return(NA_real_)
  coef(lm(y ~ t, subset = ok))[2]
}
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
    map_mean = mean(map, na.rm = TRUE),
    map_sd = safe_sd(map),
    map_cv = safe_sd(map) / mean(map, na.rm = TRUE),
    map_min = suppressWarnings(min(map, na.rm = TRUE)),
    map_max = suppressWarnings(max(map, na.rm = TRUE)),
    map_slope = slope(tmin, map),
    map_early = mean(map[tmin <= 30], na.rm = TRUE),
    map_late = mean(map[tmin > max(tmin) - 30], na.rm = TRUE),
    min_map65 = sum(map < 65, na.rm = TRUE),
    min_map60 = sum(map < 60, na.rm = TRUE),
    area_map65 = sum(pmax(65 - map, 0), na.rm = TRUE),
    area_map60 = sum(pmax(60 - map, 0), na.rm = TRUE),
    twa_map65 = sum(pmax(65 - map, 0), na.rm = TRUE) / dur,
    longest_map65 = longest_run(map < 65),
    pct_map65 = sum(map < 65, na.rm = TRUE) / sum(!is.na(map)),
    hr_mean = mean(hr, na.rm = TRUE),
    hr_sd = safe_sd(hr),
    hr_cv = safe_sd(hr) / mean(hr, na.rm = TRUE),
    hr_min = suppressWarnings(min(hr, na.rm = TRUE)),
    hr_max = suppressWarnings(max(hr, na.rm = TRUE)),
    min_hr100 = sum(hr > 100, na.rm = TRUE),
    min_hr50 = sum(hr < 50, na.rm = TRUE),
    hr_slope = slope(tmin, hr),
    spo2_mean = mean(spo2, na.rm = TRUE),
    spo2_min = suppressWarnings(min(spo2, na.rm = TRUE)),
    min_spo2_92 = sum(spo2 < 92, na.rm = TRUE),
    min_spo2_90 = sum(spo2 < 90, na.rm = TRUE),
    etco2_mean = mean(etco2, na.rm = TRUE),
    etco2_sd = safe_sd(etco2),
    min_etco2_low = sum(etco2 < 30, na.rm = TRUE),
    min_etco2_high = sum(etco2 > 45, na.rm = TRUE),
    bis_mean = mean(bis, na.rm = TRUE),
    bis_sd = safe_sd(bis),
    min_bis40 = sum(bis < 40, na.rm = TRUE),
    min_bis60 = sum(bis > 60, na.rm = TRUE),
    has_art = any(!is.na(map_art_any))
  )
}, by = LOG_ID]
for (j in names(feat)) {
  if (is.numeric(feat[[j]])) set(feat, which(is.infinite(feat[[j]])), j, NA_real_)
}

# ---------- I/O: EBL (max = running total), UO (sum), crystalloid (sum) ----------
io <- fs[ch %in% c("ebl", "uo", "cryst"),
         .(intraop_ebl = suppressWarnings(max(v[ch == "ebl"], na.rm = TRUE)),
           intraop_uo = sum(v[ch == "uo"], na.rm = TRUE),
           intraop_crystalloid = sum(v[ch == "cryst"], na.rm = TRUE)),
         by = LOG_ID]
io[is.infinite(intraop_ebl), intraop_ebl := NA_real_]
io[intraop_ebl < 0 | intraop_ebl > 20000, intraop_ebl := NA_real_]
io[intraop_uo < 0 | intraop_uo > 20000, intraop_uo := NA_real_]
io[intraop_crystalloid < 0 | intraop_crystalloid > 30000, intraop_crystalloid := NA_real_]

# ---------- merge with EMR clinical features ----------
analysis <- merge(emr, feat, by = "LOG_ID", all.x = TRUE)
analysis <- merge(analysis, io, by = "LOG_ID", all.x = TRUE)
analysis[, ebl_missing := as.integer(is.na(intraop_ebl))]

cat("analysis_mover:", nrow(analysis), "cases; events:", sum(analysis$outcome),
    sprintf("(%.1f%%)", 100 * mean(analysis$outcome)), "\n")
cat("with ts features:", sum(!is.na(analysis$map_mean)), "\n")
cat("EBL coverage:", mean(!is.na(analysis$intraop_ebl)), "\n")

saveRDS(analysis, file.path(OUT, "analysis_mover.rds"))
cat("saved analysis_mover.rds\n")
