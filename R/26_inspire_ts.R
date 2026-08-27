#!/usr/bin/env Rscript
# 26_inspire_ts.R — INSPIRE 5-min vitals -> VitalDB-identical TS features + I/O + drug totals
# Strategy: aggregate to 5-min epochs, expand each epoch to 5 one-minute rows
# (constant-within-epoch assumption), then apply the EXACT feature formulas of
# 02_features.R / 08_mover_vitals.R.
# I/O semantics fixed by distribution matching vs VitalDB: ebl=sum, uo=max,
# crystalloid/colloid=sum of components, rbc/ffp=sum, drugs=sum bolus + infusion integral.
# Input: /workspace/inspire/{vitals.csv.gz, operations.csv.gz, inspire_cohort.rds}
# Output: /workspace/inspire/analysis_inspire.rds

suppressPackageStartupMessages(library(data.table))
setDTthreads(8)
DIR <- "/workspace/inspire"

cohort <- readRDS(file.path(DIR, "inspire_cohort.rds"))
op_meta <- fread(file.path(DIR, "operations.csv.gz"),
                 select = c("op_id", "anstart_time", "anend_time", "weight"))
cohort <- merge(cohort, op_meta[, .(op_id, weight)], by = "op_id", all.x = TRUE)
keep_ops <- cohort$op_id

# ---------- load vitals, cohort ops only, needed items ----------
sig_items <- c("art_mbp", "art_sbp", "art_dbp", "nibp_mbp", "nibp_sbp", "nibp_dbp",
               "hr", "spo2", "etco2", "bis")
io_items <- c("ebl", "uo", "ns", "hs", "hns", "d5w", "d10w", "d50w", "ds", "psa",
              "alb5", "alb20", "hes", "rbc", "ffp")
drug_items <- c("ppf", "mdz", "ftn", "eph", "phe", "epi", "pepi", "epii")
v <- fread(file.path(DIR, "vitals.csv.gz"),
           select = c("op_id", "chart_time", "item_name", "value"))
cat("vitals raw rows:", nrow(v), "\n")
v <- v[op_id %in% keep_ops & item_name %in% c(sig_items, io_items, drug_items)]
cat("rows in cohort for needed items:", nrow(v), "\n")

v <- merge(v, op_meta, by = "op_id")
v[, `:=`(tmin = chart_time - anstart_time,
         dur = anend_time - anstart_time)]
v <- v[tmin >= 0 & tmin <= dur]

# ---------- signals: plausibility (identical to MOVER pipeline) ----------
sig <- v[item_name %in% sig_items]
sig[, ch := fcase(
  item_name == "art_mbp", "map_art",
  item_name == "nibp_mbp", "map_nibp",
  item_name == "hr", "hr",
  item_name == "spo2", "spo2",
  item_name == "etco2", "etco2",
  item_name == "bis", "bis",
  default = "bp")]
# derived MAP from SBP/DBP pairs (fallback channels, as in MOVER)
bp <- v[item_name %in% c("art_sbp", "art_dbp", "nibp_sbp", "nibp_dbp")]
bp[, ch := fifelse(grepl("^art", item_name), "map_art2", "map_nibp2")]
bp[, part := fifelse(grepl("sbp$", item_name), "sbp", "dbp")]
bp_w <- dcast(bp, op_id + tmin ~ ch + part, value.var = "value",
              fun.aggregate = mean)
bp_w[, `:=`(map_art2 = (map_art2_sbp + 2 * map_art2_dbp) / 3,
            map_nibp2 = (map_nibp2_sbp + 2 * map_nibp2_dbp) / 3)]
bp_long <- melt(bp_w, id.vars = c("op_id", "tmin"),
                measure.vars = c("map_art2", "map_nibp2"),
                variable.name = "ch", value.name = "value", na.rm = TRUE)

sig <- rbindlist(list(sig[, .(op_id, tmin, ch, value)],
                      bp_long[, .(op_id, tmin, ch, value = as.numeric(value))]))
sig[ch %in% c("map_art", "map_art2", "map_nibp", "map_nibp2") &
    (value < 20 | value > 250), value := NA_real_]
sig[ch == "hr" & (value < 20 | value > 250), value := NA_real_]
sig[ch == "spo2" & (value < 50 | value > 100), value := NA_real_]
sig[ch == "etco2" & (value < 0 | value > 150), value := NA_real_]
sig[ch == "bis" & (value < 0 | value > 100), value := NA_real_]
sig <- sig[!is.na(value)]
cat("signals after cleaning:", nrow(sig), "rows,", uniqueN(sig$op_id), "ops\n")

# ---------- 5-min epoch aggregation -> expand to 1-min grid ----------
sig[, epoch := floor(tmin / 5)]
ep <- sig[, .(value = mean(value)), by = .(op_id, epoch, ch)]
w5 <- dcast(ep, op_id + epoch ~ ch, value.var = "value")
w5[, map := fifelse(!is.na(map_art), map_art,
             fifelse(!is.na(map_art2), map_art2,
              fifelse(!is.na(map_nibp), map_nibp, map_nibp2)))]
w5[, map_art_any := fifelse(!is.na(map_art) | !is.na(map_art2), 1, NA_real_)]

# expand: each epoch -> minutes epoch*5 + 0:4
exp_idx <- rep(seq_len(nrow(w5)), each = 5)
w <- w5[exp_idx]
w[, tmin := epoch * 5 + rep(0:4, times = nrow(w5))]

# ---------- per-case features (identical formulas to 02/08) ----------
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
}, by = op_id]
for (j in names(feat)) {
  if (is.numeric(feat[[j]])) set(feat, which(is.infinite(feat[[j]])), j, NA_real_)
}
rm(w, w5, ep, sig, bp, bp_w, bp_long); gc()

# ---------- I/O totals ----------
io <- v[item_name %in% io_items]
io_w <- io[, .(
  intraop_ebl = sum(value[item_name == "ebl"]),
  intraop_uo = suppressWarnings(max(value[item_name == "uo"], na.rm = TRUE)),
  intraop_crystalloid = sum(value[item_name %in% c("ns", "hs", "hns", "d5w", "d10w",
                                                   "d50w", "ds", "psa")]),
  intraop_colloid = sum(value[item_name %in% c("alb5", "alb20", "hes")]),
  intraop_rbc = sum(value[item_name == "rbc"]),
  intraop_ffp = sum(value[item_name == "ffp"]),
  has_ebl = any(item_name == "ebl"), has_uo = any(item_name == "uo"),
  has_cryst = any(item_name %in% c("ns", "hs", "hns", "d5w", "d10w", "d50w", "ds", "psa")),
  has_coll = any(item_name %in% c("alb5", "alb20", "hes"))
), by = op_id]
# absent record: NA for ebl/uo/crystalloid/colloid (imputed), 0 for rbc/ffp (no transfusion)
io_w[has_ebl == FALSE, intraop_ebl := NA_real_]
io_w[has_uo == FALSE, intraop_uo := NA_real_]
io_w[has_cryst == FALSE, intraop_crystalloid := NA_real_]
io_w[has_coll == FALSE, intraop_colloid := NA_real_]
io_w[is.infinite(intraop_uo), intraop_uo := NA_real_]
# plausibility caps (identical to MOVER)
io_w[intraop_ebl < 0 | intraop_ebl > 20000, intraop_ebl := NA_real_]
io_w[intraop_uo < 0 | intraop_uo > 20000, intraop_uo := NA_real_]
io_w[intraop_crystalloid < 0 | intraop_crystalloid > 30000, intraop_crystalloid := NA_real_]
io_w[intraop_colloid < 0 | intraop_colloid > 10000, intraop_colloid := NA_real_]
io_w[, c("has_ebl", "has_uo", "has_cryst", "has_coll") := NULL]
cat("I/O coverage: ebl", round(mean(!is.na(io_w$intraop_ebl)), 3),
    " uo", round(mean(!is.na(io_w$intraop_uo)), 3),
    " cryst", round(mean(!is.na(io_w$intraop_crystalloid)), 3), "\n")

# ---------- drug totals: bolus sum + infusion integral ----------
dg <- v[item_name %in% drug_items]
dg <- merge(dg[, .(op_id, tmin, item_name, value)],
            cohort[, .(op_id, weight)], by = "op_id", all.x = TRUE)
dg[, dose := fcase(
  item_name == "pepi", value / 60 * 5,                 # ug/h -> ug per 5-min epoch
  item_name == "epii", value * weight * 5,             # ug/kg/min -> ug per epoch
  default = value)]
dg[, drug := fcase(
  item_name == "ppf", "intraop_ppf",
  item_name == "mdz", "intraop_mdz",
  item_name == "ftn", "intraop_ftn",
  item_name == "eph", "intraop_eph",
  item_name %in% c("phe", "pepi"), "intraop_phe",
  item_name %in% c("epi", "epii"), "intraop_epi",
  default = NA_character_)]
dg <- dg[!is.na(drug) & !is.na(dose) & dose >= 0]
med_tot <- dg[, .(tot = sum(dose)), by = .(op_id, drug)]
# physiological caps (identical to MOVER) + 99.9th pct cap
phys_cap <- c(intraop_ppf = 3000, intraop_mdz = 50, intraop_ftn = 3000,
              intraop_rocu = 300, intraop_eph = 500, intraop_phe = 20000,
              intraop_epi = 10000)
med_tot[, tot := pmin(tot, phys_cap[drug])]
caps <- med_tot[, .(cap = quantile(tot, 0.999, na.rm = TRUE)), by = drug]
print(caps)
med_tot <- merge(med_tot, caps, by = "drug")
med_tot[tot > cap, tot := cap]
med_w <- dcast(med_tot, op_id ~ drug, value.var = "tot", fill = 0)

# ---------- merge all ----------
analysis <- merge(cohort, feat, by = "op_id", all.x = TRUE)
analysis <- merge(analysis, io_w, by = "op_id", all.x = TRUE)
analysis <- merge(analysis, med_w, by = "op_id", all.x = TRUE)
for (dg2 in c("intraop_ppf", "intraop_mdz", "intraop_ftn",
              "intraop_eph", "intraop_phe", "intraop_epi")) {
  if (!dg2 %in% names(analysis)) analysis[, (dg2) := 0]
  analysis[is.na(get(dg2)), (dg2) := 0]
}
for (dg2 in c("intraop_rbc", "intraop_ffp")) {
  if (!dg2 %in% names(analysis)) analysis[, (dg2) := 0]
  analysis[is.na(get(dg2)), (dg2) := 0]
}
analysis[, intraop_rocu := NA_real_]   # rocuronium dose not recorded in INSPIRE

cat("analysis_inspire:", nrow(analysis), "ops; events:", sum(analysis$outcome),
    sprintf("(%.1f%%)", 100 * mean(analysis$outcome)), "\n")
cat("with ts features:", sum(!is.na(analysis$map_mean)),
    sprintf("(%.1f%%)", 100 * mean(!is.na(analysis$map_mean))), "\n")
cat("has_art rate:", round(mean(analysis$has_art, na.rm = TRUE), 3), "\n")
cat("coverage med:", round(median(analysis$coverage, na.rm = TRUE), 3), "\n")

saveRDS(analysis, file.path(DIR, "analysis_inspire.rds"))
cat("saved analysis_inspire.rds\n")
