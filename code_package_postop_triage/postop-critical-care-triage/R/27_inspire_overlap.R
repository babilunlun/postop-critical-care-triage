#!/usr/bin/env Rscript
# 27_inspire_overlap.R — VitalDB<->INSPIRE overlap: exclusion flags + trajectory-match validation
# Primary exclusion: case_id in 1..6388 (verified VitalDB linker: 100% sex concordance)
# Conservative sensitivity: exclude ANY non-NA case_id
# Trajectory matching with lag search (VitalDB tmin origin = casestart, INSPIRE = anstart):
#   positive controls = known-linked pairs; negative controls = best non-true matches.
# Output: /workspace/inspire/inspire_overlap.rds + console report

suppressPackageStartupMessages(library(data.table))
setDTthreads(8)
DIR <- "/workspace/inspire"

ai <- readRDS(file.path(DIR, "analysis_inspire.rds"))
vb <- readRDS("/mnt/shared-workspace/shared/analysis_vitaldb.rds")

# ---------- exclusion flags ----------
ai[, excl_primary := !is.na(case_id) & case_id >= 1 & case_id <= 6388]
ai[, excl_conservative := !is.na(case_id)]
cat("cohort ops:", nrow(ai), "\n")
cat("primary exclusion (case_id 1-6388):", sum(ai$excl_primary),
    sprintf("(%.2f%%)", 100 * mean(ai$excl_primary)), "\n")
cat("conservative exclusion (any case_id):", sum(ai$excl_conservative),
    sprintf("(%.2f%%)", 100 * mean(ai$excl_conservative)), "\n")
cat("linked to our VitalDB analysis set:",
    sum(ai$case_id[!is.na(ai$case_id)] %in% vb$caseid), "\n")

# ---------- epoch-level trajectories ----------
ts_files <- list.files("/mnt/shared-workspace/shared/vitaldb_ts",
                       pattern = "^batch_.*\\.rds$", full.names = TRUE)
ts <- rbindlist(lapply(ts_files, readRDS))
ts <- ts[channel %in% c("map_art", "map_nibp", "hr")]
w <- dcast(ts, caseid + tmin ~ channel, value.var = "v", fun.aggregate = mean)
w[, map := fifelse(!is.na(map_art), map_art, map_nibp)]
w[, epoch := floor(tmin / 5)]
vb_ep <- w[, .(map = mean(map, na.rm = TRUE), hr = mean(hr, na.rm = TRUE)),
           by = .(caseid, epoch)]
vb_ep <- vb_ep[is.finite(map) | is.finite(hr)]
vb_meta <- vb[, .(caseid, sex, age, department, dur = dur_min)]
rm(ts, w); gc()

pos_ops <- ai[excl_primary == TRUE & case_id %in% vb$caseid, .(op_id, case_id)]
set.seed(42)
test_ops <- ai[excl_conservative == FALSE,
               .(op_id, case_id = NA_integer_)][sample(.N, min(500, .N))]
need_ops <- rbind(pos_ops, test_ops)

v <- fread(file.path(DIR, "vitals.csv.gz"),
           select = c("op_id", "chart_time", "item_name", "value"))
v <- v[op_id %in% need_ops$op_id & item_name %in% c("art_mbp", "nibp_mbp", "hr")]
v <- merge(v, ai[, .(op_id, anstart_time)], by = "op_id")
v[, tmin := chart_time - anstart_time]
v <- v[tmin >= 0]
v[item_name %in% c("art_mbp", "nibp_mbp") & (value < 20 | value > 250), value := NA_real_]
v[item_name == "hr" & (value < 20 | value > 250), value := NA_real_]
v[, epoch := floor(tmin / 5)]
vw <- dcast(v, op_id + epoch ~ item_name, value.var = "value",
            fun.aggregate = mean)
vw[, map := fifelse(!is.na(art_mbp), art_mbp, nibp_mbp)]
in_ep <- vw[is.finite(map) | is.finite(hr), .(op_id, epoch, map, hr)]
in_meta <- ai[, .(op_id, sex, age, department, dur = dur_min)]

# ---------- lag-aware pair scoring ----------
# score_pair: best over lag in -3..+3 epochs of (cor_map, mad_map, cor_hr, mad_hr)
score_pair <- function(ep_in, ep_vb) {
  best <- NULL
  for (lag in -3:3) {
    ev <- copy(ep_vb)[, epoch := epoch + lag]
    m <- merge(ep_in, ev, by = "epoch", suffixes = c("_in", "_vb"))
    m <- m[is.finite(map_in) & is.finite(map_vb)]
    if (nrow(m) < 12) next
    sd_in <- sd(m$map_in); sd_vb <- sd(m$map_vb)
    if (sd_in == 0 || sd_vb == 0) next
    cc <- cor(m$map_in, m$map_vb)
    md <- mean(abs(m$map_in - m$map_vb))
    hr_ok <- is.finite(m$hr_in) & is.finite(m$hr_vb)
    ch <- if (sum(hr_ok) >= 12 && sd(m$hr_in[hr_ok]) > 0 && sd(m$hr_vb[hr_ok]) > 0)
            cor(m$hr_in[hr_ok], m$hr_vb[hr_ok]) else NA_real_
    mh <- if (sum(hr_ok) >= 12) mean(abs(m$hr_in[hr_ok] - m$hr_vb[hr_ok])) else NA_real_
    if (is.null(best) || cc > best$cor_map)
      best <- list(cor_map = cc, mad_map = md, cor_hr = ch, mad_hr = mh,
                   n_common = nrow(m), lag = lag)
  }
  best
}

match_one <- function(op_row, ep_op, exclude_caseid = NA) {
  cand <- vb_meta[sex == op_row$sex & abs(age - op_row$age) <= 2.6 &
                  department == op_row$department & abs(dur - op_row$dur) <= 20 &
                  caseid != exclude_caseid]
  best <- NULL
  for (cid in cand$caseid) {
    s <- score_pair(ep_op, vb_ep[caseid == cid])
    if (is.null(s)) next
    if (is.null(best) || s$cor_map > best$cor_map) {
      s$caseid <- cid; best <- s
    }
  }
  best
}

run_match <- function(ops_dt, exclude_true) {
  res <- list()
  for (i in seq_len(nrow(ops_dt))) {
    op_row <- in_meta[op_id == ops_dt$op_id[i]]
    ep_op <- in_ep[op_id == ops_dt$op_id[i]]
    if (nrow(ep_op) < 12) next
    excl <- if (exclude_true) ops_dt$case_id[i] else NA_integer_
    best <- match_one(op_row, ep_op, exclude_caseid = excl)
    if (!is.null(best))
      res[[length(res) + 1]] <- cbind(ops_dt[i, .(op_id, case_id)], as.data.table(best))
    if (i %% 200 == 0) cat("  matched", i, "of", nrow(ops_dt), "\n")
  }
  if (length(res) == 0) return(data.table())
  rbindlist(res)
}

cat("== positive controls: true-pair stats (lag-aligned) ==\n")
true_stats <- list()
for (i in seq_len(nrow(pos_ops))) {
  ep_op <- in_ep[op_id == pos_ops$op_id[i]]
  if (nrow(ep_op) < 12) next
  s <- score_pair(ep_op, vb_ep[caseid == pos_ops$case_id[i]])
  if (is.null(s)) next
  true_stats[[length(true_stats) + 1]] <- cbind(pos_ops[i, .(op_id, case_id)],
                                                as.data.table(s))
}
true_stats <- rbindlist(true_stats)
cat("true pairs evaluated:", nrow(true_stats), "of", nrow(pos_ops), "\n")
cat("cor_map quantiles:\n"); print(round(quantile(true_stats$cor_map, c(.01, .05, .25, .5, .75), na.rm = TRUE), 3))
cat("mad_map quantiles:\n"); print(round(quantile(true_stats$mad_map, c(.5, .75, .95, .99), na.rm = TRUE), 2))
cat("lag distribution:\n"); print(table(true_stats$lag))

cat("== negative controls: best non-true match (lag-aligned) ==\n")
neg_stats <- run_match(pos_ops, exclude_true = TRUE)
cat("negative best-match cor_map quantiles:\n")
print(round(quantile(neg_stats$cor_map, c(.5, .9, .95, .99, 1), na.rm = TRUE), 3))
cat("negative best-match mad_map quantiles:\n")
print(round(quantile(neg_stats$mad_map, c(0, .01, .05, .1, .5), na.rm = TRUE), 2))

# threshold scan: sensitivity vs FPR trade-off
scan <- rbindlist(lapply(c(0.90, 0.95, 0.98), function(tc) {
  rbindlist(lapply(c(3, 4, 5, 6), function(tm) {
    data.table(thr_cor = tc, thr_mad = tm,
               sens = true_stats[, mean(cor_map >= tc & mad_map <= tm, na.rm = TRUE)],
               fpr = neg_stats[, mean(cor_map >= tc & mad_map <= tm, na.rm = TRUE)])
  }))
}))
print(scan)

# choose threshold: max sensitivity with FPR <= 0.01
ok <- scan[fpr <= 0.01][order(-sens)]
THR_COR <- ok$thr_cor[1]; THR_MAD <- ok$thr_mad[1]
cat(sprintf("chosen threshold: cor>=%.2f & MAD<=%.1f (sens=%.3f, FPR=%.4f)\n",
            THR_COR, THR_MAD, ok$sens[1], ok$fpr[1]))

cat("== test sample: case_id-NA ops ==\n")
test_stats <- run_match(test_ops, exclude_true = FALSE)
if (nrow(test_stats) > 0) {
  n_suspect <- test_stats[, sum(cor_map >= THR_COR & mad_map <= THR_MAD, na.rm = TRUE)]
} else n_suspect <- 0
n_test <- nrow(test_ops)
cat("suspected missed overlaps:", n_suspect, "of", n_test,
    sprintf("(%.2f%%)", 100 * n_suspect / n_test), "\n")
# FPR-corrected estimate of missed-overlap rate
raw_rate <- n_suspect / n_test
fpr <- ok$fpr[1]; sens <- ok$sens[1]
corr_rate <- max(0, (raw_rate - fpr) / max(sens, 0.01))
est_missed <- corr_rate * ai[, sum(!excl_conservative)]
cat(sprintf("FPR/sensitivity-corrected missed-overlap rate: %.3f%% -> ~%.0f ops (%.3f%% of cohort)\n",
            100 * corr_rate, est_missed, 100 * est_missed / nrow(ai)))

saveRDS(list(flags = ai[, .(op_id, case_id, excl_primary, excl_conservative)],
             true_stats = true_stats, neg_stats = neg_stats,
             test_stats = test_stats, scan = scan,
             thr = c(cor = THR_COR, mad = THR_MAD),
             n_suspect = n_suspect, n_test = n_test,
             corr_rate = corr_rate, est_missed = est_missed),
        file.path(DIR, "inspire_overlap.rds"))
cat("saved inspire_overlap.rds\n")
