#!/usr/bin/env Rscript
# GRU data prep: build [N, T<=480, 10] sequence tensor (5 value channels ffill'd
# + 5 observation masks) from VitalDB 1-min batches, same cohort/split as 03_model.R.
# Also bakes static clinical features (same recipe as 03_model.R) for GRU-full.
# Output: /workspace/gru/gru_data.rds

suppressPackageStartupMessages({
  library(data.table)
  library(recipes)
})

set.seed(20250817)
T_MAX <- 480L
CHS <- c("map", "hr", "spo2", "etco2", "bis")

# ---------- cohort + split (identical to 03_model.R) ----------
d <- as.data.table(readRDS("/workspace/gru/analysis_vitaldb.rds"))
d <- d[!is.na(coverage) & coverage >= 0.5 & !is.na(dur_min) & dur_min >= 30]
d$ebl_missing <- as.integer(is.na(d$intraop_ebl))
d[, has_art := as.numeric(has_art)]
d[, outcome := factor(outcome, levels = c(0, 1), labels = c("no", "yes"))]
setorder(d, caseid)
n_train <- floor(0.7 * nrow(d))
d[, split := c(rep("train", n_train), rep("test", .N - n_train))]
cat("cohort:", nrow(d), " train:", n_train, " test:", nrow(d) - n_train, "\n")

# verify alignment with saved test predictions
tp <- fread("/workspace/gru/test_predictions.csv")
stopifnot(identical(sort(tp$caseid), sort(d[split == "test"]$caseid)))
cat("test caseid alignment with 03_model.R: OK\n")

# ---------- load sequences ----------
ts <- rbindlist(lapply(sprintf("/workspace/gru/vitaldb_ts/batch_%04d.rds", 1:60),
                       readRDS))
ts <- ts[caseid %in% d$caseid]
# combine map: arterial preferred, else nibp
map <- rbind(ts[channel == "map_art", .(caseid, tmin, v, src = 1L)],
             ts[channel == "map_nibp", .(caseid, tmin, v, src = 0L)])
setorder(map, caseid, tmin, -src)
map <- unique(map, by = c("caseid", "tmin"))[, .(caseid, tmin, channel = "map", v)]
ts2 <- rbind(ts[channel %in% c("hr", "spo2", "etco2", "bis"),
                .(caseid, tmin, channel, v)], map)
rm(ts, map); gc()

# relative minute from each case's first observation, truncate at T_MAX
ts2[, t0 := min(tmin), by = caseid]
ts2[, t := as.integer(tmin - t0) + 1L]
ts2 <- ts2[t <= T_MAX]
ts2[, channel := factor(channel, levels = CHS)]

# ---------- build arrays ----------
cases <- d$caseid
N <- length(cases)
Xv <- array(NA_real_, dim = c(N, T_MAX, 5))   # raw values
Mm <- array(0, dim = c(N, T_MAX, 5))          # observation mask
lens <- integer(N)
idx <- match(ts2$caseid, cases)
cc <- as.integer(ts2$channel)
Xv[cbind(idx, ts2$t, cc)] <- ts2$v
Mm[cbind(idx, ts2$t, cc)] <- 1
lens <- as.integer(d$dur_min)  # placeholder, recompute below
lens <- vapply(seq_len(N), function(i) {
  m <- Mm[i, , ]; max(which(rowSums(m) > 0), 0L)
}, integer(1))
lens <- pmax(lens, 1L)
cat("length: median", median(lens), " p90", quantile(lens, 0.9),
    " max", max(lens), "\n")

# ---------- standardize with train statistics (observed values only) ----------
tr <- which(d$split == "train")
mu <- sds <- numeric(5)
for (k in 1:5) {
  vals <- Xv[tr, , k][Mm[tr, , k] == 1]
  mu[k] <- mean(vals, na.rm = TRUE)
  sds[k] <- sd(vals, na.rm = TRUE)
}
names(mu) <- names(sds) <- CHS
print(round(mu, 3)); print(round(sds, 3))

# forward-fill within case, then standardize; unobserved-after-ffill -> 0 (=train mean)
Xf <- array(0, dim = c(N, T_MAX, 5))
for (k in 1:5) {
  Vk <- Xv[, , k]
  Vk <- (Vk - mu[k]) / sds[k]
  # ffill per case over observed entries
  for (i in seq_len(N)) {
    v <- Vk[i, ]
    obs <- which(!is.na(v))
    if (length(obs) == 0) next
    last <- v[obs[1]]
    for (t in seq_along(v)) {
      if (!is.na(v[t])) last <- v[t] else v[t] <- last
    }
    Vk[i, ] <- v
  }
  Vk[is.na(Vk)] <- 0  # channels never observed in a case -> train mean (0)
  Xf[, , k] <- Vk
  cat("channel", CHS[k], "done\n")
}
X <- abind::abind(Xf, Mm, along = 3)  # [N, 480, 10]

# zero out positions beyond observed length (padding)
for (i in seq_len(N)) if (lens[i] < T_MAX) X[i, (lens[i] + 1):T_MAX, ] <- 0

# ---------- static clinical features (same recipe as 03_model.R) ----------
clin_feats <- c("age", "sex", "bmi", "asa", "emop", "department",
                "preop_htn", "preop_dm",
                "preop_hb", "preop_plt", "preop_na", "preop_k", "preop_gluc",
                "preop_alb", "preop_ast", "preop_alt", "preop_bun", "preop_cr",
                "intraop_ebl", "ebl_missing", "intraop_uo", "intraop_rbc", "intraop_ffp",
                "intraop_crystalloid", "intraop_colloid",
                "intraop_ppf", "intraop_mdz", "intraop_ftn", "intraop_rocu",
                "intraop_eph", "intraop_phe", "intraop_epi", "ane_dur_min")
train_df <- d[split == "train"]
rec <- recipe(as.formula(paste("outcome ~", paste(clin_feats, collapse = "+"))),
              data = train_df) |>
  step_impute_median(all_numeric_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())
rec <- prep(rec)
S_train <- as.matrix(bake(rec, new_data = NULL) |> select(-outcome))
S_test <- as.matrix(bake(rec, new_data = d[split == "test"]) |> select(-outcome))
S <- rbind(S_train, S_test)  # rows align with d order (train first, then test)
stopifnot(nrow(S) == N)
cat("static dim:", ncol(S), "\n")

# ---------- train/val split within train (80/20 stratified, fixed) ----------
y <- as.integer(d$outcome == "yes")
set.seed(20250817)
tr_idx <- which(d$split == "train")
va_idx <- c()
for (cls in 0:1) {
  pool <- tr_idx[y[tr_idx] == cls]
  va_idx <- c(va_idx, sample(pool, ceiling(0.2 * length(pool))))
}
va_idx <- sort(va_idx)
sub_idx <- setdiff(tr_idx, va_idx)
cat("subtrain:", length(sub_idx), " val:", length(va_idx),
    " test:", sum(d$split == "test"), "\n")

saveRDS(list(X = X, lens = lens, S = S, y = y, caseid = d$caseid,
             split = d$split, sub_idx = sub_idx, va_idx = va_idx,
             mu = mu, sds = sds, chs = CHS, t_max = T_MAX),
        "/workspace/gru/gru_data.rds")
cat("DONE\n")
