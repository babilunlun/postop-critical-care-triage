#!/usr/bin/env Rscript
# VitalDB cohort + intraop time-series downloader (1-min aggregation)
# Usage: Rscript 01_download_vitaldb.R [batch_size] [start_batch]
# NOTE: never source() this file — it runs main() when no trailing args beyond script.

suppressPackageStartupMessages({
  library(data.table)
  library(future)
  library(future.apply)
})

BASE <- "https://api.vitaldb.net"
WS <- "/workspace"
TS_DIR <- file.path(WS, "vitaldb_ts")
dir.create(TS_DIR, showWarnings = FALSE, recursive = TRUE)

# ---------- cohort ----------
if (!file.exists(file.path(WS, "vitaldb_cases.csv"))) {
  download.file(file.path(BASE, "cases"), file.path(WS, "vitaldb_cases.csv"), mode = "wb")
}
if (!file.exists(file.path(WS, "vitaldb_trks.csv"))) {
  download.file(file.path(BASE, "trks"), file.path(WS, "vitaldb_trks.csv"), mode = "wb")
}
cases <- fread(file.path(WS, "vitaldb_cases.csv"))
trks <- fread(file.path(WS, "vitaldb_trks.csv"))

cohort <- cases[age >= 18 & ane_type == "General"]
cat("cohort:", nrow(cohort), "cases\n")

# track selection per case: prefer arterial MAP, else NIBP; HR; SpO2; EtCO2; BIS
pick <- function(dt, patterns) {
  for (p in patterns) {
    hit <- dt[grepl(p, tname, fixed = TRUE)]
    if (nrow(hit) > 0) return(hit[1])
  }
  NULL
}
sel <- trks[caseid %in% cohort$caseid, {
  list(tracks = list(list(
    map_art  = pick(.SD, c("Solar8000/ART_MBP")),
    map_nibp = pick(.SD, c("Solar8000/NIBP_MBP")),
    hr       = pick(.SD, c("Solar8000/HR")),
    spo2     = pick(.SD, c("Solar8000/PLETH_SPO2")),
    etco2    = pick(.SD, c("Solar8000/ETCO2", "Primus/ETCO2")),
    bis      = pick(.SD, c("BIS/BIS"))
  )))
}, by = caseid]

# flatten to long task list
tasks <- rbindlist(lapply(seq_len(nrow(sel)), function(i) {
  tr <- sel$tracks[[1]][[1]]  # placeholder, rebuilt below
}))
# simpler: build task table explicitly
task_list <- list()
for (i in seq_len(nrow(sel))) {
  cid <- sel$caseid[i]
  tl <- sel$tracks[[i]]
  for (ch in names(tl)) {
    row <- tl[[ch]]
    if (!is.null(row)) task_list[[length(task_list) + 1]] <- list(caseid = cid, channel = ch, tid = row$tid)
  }
}
tasks <- rbindlist(task_list)
cat("track tasks:", nrow(tasks), "\n")

# plausibility ranges
rng <- list(map_art = c(20, 250), map_nibp = c(20, 250), hr = c(20, 250),
            spo2 = c(50, 100), etco2 = c(0, 150), bis = c(0, 100))

fetch_track <- function(tid, channel) {
  url <- file.path(BASE, as.character(tid))
  dt <- tryCatch(fread(url, showProgress = FALSE), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) < 2) return(NULL)
  names(dt) <- c("time", "v")
  dt <- dt[is.finite(v)]
  r <- rng[[channel]]
  dt <- dt[v >= r[1] & v <= r[2]]
  if (nrow(dt) == 0) return(NULL)
  dt[, tmin := floor(time / 60)]
  dt[, .(v = mean(v)), by = tmin][, channel := channel]
}

process_case <- function(cid) {
  library(data.table)
  tk <- tasks[caseid == cid]
  if (nrow(tk) == 0) return(NULL)
  out <- rbindlist(lapply(seq_len(nrow(tk)), function(j) {
    r <- fetch_track(tk$tid[j], tk$channel[j])
    if (is.null(r)) return(NULL)
    r[, caseid := cid]
    r
  }), fill = TRUE)
  if (nrow(out) == 0) return(NULL)
  out[, .(caseid, tmin, channel, v)]
}

main <- function(batch_size = 100, start_batch = 1) {
  ids <- sort(unique(cohort$caseid))
  done <- gsub("batch_|\\.rds", "", list.files(TS_DIR, pattern = "^batch_.*\\.rds$"))
  done_ids <- if (length(done) > 0) {
    rbindlist(lapply(list.files(TS_DIR, pattern = "^batch_.*\\.rds$", full.names = TRUE),
                     readRDS))$caseid |> unique()
  } else integer(0)
  ids <- setdiff(ids, done_ids)
  cat("cases to process:", length(ids), "(already done:", length(done_ids), ")\n")
  batches <- split(ids, ceiling(seq_along(ids) / batch_size))
  plan(multisession, workers = 4)
  for (i in seq_along(batches)) {
    bnum <- i + start_batch - 1 + length(done)
    t0 <- Sys.time()
    res <- future_lapply(batches[[i]], process_case, future.seed = TRUE)
    batch_dt <- rbindlist(res, fill = TRUE)
    saveRDS(batch_dt, file.path(TS_DIR, sprintf("batch_%04d.rds", bnum)))
    # checkpoint to shared workspace (S3) after each batch
    system(sprintf("cp %s /mnt/shared-workspace/shared/vitaldb_ts/ 2>/dev/null",
                   file.path(TS_DIR, sprintf("batch_%04d.rds", bnum))))
    cat(sprintf("batch %d: %d cases, %.1f min elapsed\n", bnum, length(batches[[i]]),
                as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  cat("ALL DONE\n")
}

dir.create("/mnt/shared-workspace/shared/vitaldb_ts", showWarnings = FALSE, recursive = TRUE)
args <- as.numeric(commandArgs(trailingOnly = TRUE))
if (length(args) == 0) main() else main(args[1], args[2])
