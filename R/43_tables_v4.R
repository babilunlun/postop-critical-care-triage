#!/usr/bin/env Rscript
# 43_tables_v4.R — v4 tables with corrected INSPIRE outcome and deduped MOVER:
#   Table 1 v4: VitalDB (dev) vs INSPIRE (temporal) vs MOVER (external, deduped)
#   Table 2 v4: performance rows for all three cohorts
#   Supplementary: INSPIRE sensitivity/overlap table
suppressPackageStartupMessages(library(data.table))

OUT <- "/workspace/tables"; dir.create(OUT, showWarnings = FALSE)

# ================= inputs =================
v <- readRDS("/mnt/shared-workspace/shared/analysis_vitaldb.rds")
g <- readRDS("/workspace/gru/gru_data.rds")
v <- as.data.table(v)[caseid %in% g$caseid]

ai <- readRDS("/workspace/inspire/analysis_inspire.rds")
ov <- readRDS("/workspace/inspire/inspire_overlap.rds")$flags
ai <- merge(ai, ov, by = "op_id", all.x = TRUE)
i <- as.data.table(ai[excl_primary == FALSE])

m <- as.data.table(readRDS("/workspace/ext_out/analysis_mover_dedup.rds"))
stopifnot(nrow(v) == 5987, nrow(i) == 96196, nrow(m) == 48370)

# ================= Table 1 machinery (same spec as 17/32) =================
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
miss <- function(x) sprintf("%.1f", 100 * mean(is.na(x)))
s_norm <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
s_nonn <- function(x) sprintf("%.2f [%.2f, %.2f]", median(x, na.rm = TRUE),
                              quantile(x, 0.25, na.rm = TRUE), quantile(x, 0.75, na.rm = TRUE))
s_cat  <- function(x, lev) sprintf("%d (%.1f)", sum(x == lev, na.rm = TRUE),
                                   100 * mean(x == lev, na.rm = TRUE))
p_cont <- function(d, var, nonnorm) {
  x0 <- d[[var]][d$outcome == 0]; x1 <- d[[var]][d$outcome == 1]
  x0 <- x0[!is.na(x0)]; x1 <- x1[!is.na(x1)]
  if (length(x0) < 2 || length(x1) < 2) return(NA_real_)
  if (nonnorm) suppressWarnings(wilcox.test(x0, x1))$p.value else t.test(x0, x1)$p.value
}
p_cat <- function(d, var) {
  tb <- table(d[[var]], d$outcome)
  if (nrow(tb) < 2) return(NA_real_)
  suppressWarnings(if (any(chisq.test(tb)$expected < 5)) fisher.test(tb, simulate.p.value = TRUE)$p.value
                   else chisq.test(tb)$p.value)
}
spec <- list(
  list("age", "Age, yr", "norm"),
  list("sex", "Male sex", "cat", c("M")),
  list("bmi", "BMI, kg/m²", "norm"),
  list("asa", "ASA physical status", "cat", c("1","2","3","4","5","6")),
  list("emop", "Emergency surgery", "cat", c("1")),
  list("department", "Surgical department", "cat",
       c("General surgery", "Thoracic surgery", "Urology", "Gynecology")),
  list("preop_htn", "Hypertension", "cat", c("1")),
  list("preop_dm", "Diabetes mellitus", "cat", c("1")),
  list("preop_hb", "Preop hemoglobin, g/dL", "norm"),
  list("preop_plt", "Preop platelets, 10³/µL", "norm"),
  list("preop_na", "Preop sodium, mmol/L", "norm"),
  list("preop_k", "Preop potassium, mmol/L", "norm"),
  list("preop_gluc", "Preop glucose, mg/dL", "norm"),
  list("preop_alb", "Preop albumin, g/dL", "norm"),
  list("preop_ast", "Preop AST, IU/L", "norm"),
  list("preop_alt", "Preop ALT, IU/L", "norm"),
  list("preop_bun", "Preop BUN, mg/dL", "norm"),
  list("preop_cr", "Preop creatinine, mg/dL", "norm"),
  list("ane_dur_min", "Anesthesia duration, min", "nonnorm"),
  list("intraop_ebl", "Estimated blood loss, mL", "nonnorm"),
  list("intraop_crystalloid", "Crystalloids, mL", "nonnorm"),
  list("intraop_uo", "Urine output, mL", "nonnorm"),
  list("intraop_rbc", "Red cell transfusion, units", "norm"),
  list("has_art", "Invasive arterial monitoring", "cat", c("1")),
  list("map_mean", "MAP mean, mmHg", "norm"),
  list("map_min", "MAP minimum, mmHg", "norm"),
  list("min_map65", "Minutes with MAP <65 mmHg", "nonnorm"),
  list("area_map65", "Area under MAP 65 mmHg, mmHg·min", "nonnorm"),
  list("hr_mean", "Heart rate mean, /min", "norm"),
  list("spo2_min", "SpO2 minimum, %", "norm"),
  list("bis_mean", "BIS mean", "norm"),
  list("coverage", "Monitoring coverage, fraction", "norm")
)
build_side <- function(d, sp, emop_na = FALSE) {
  var <- sp[[1]]; lab <- sp[[2]]; typ <- sp[[3]]; levs <- if (length(sp) >= 4) sp[[4]] else NULL
  x <- d[[var]]
  if (typ == "cat") { if (is.logical(x)) x <- as.integer(x); x <- as.character(x) }
  rows <- list()
  if (typ == "norm") {
    rows[[1]] <- c(lab, s_norm(x), s_norm(x[d$outcome == 0]), s_norm(x[d$outcome == 1]),
                   fmt_p(p_cont(d, var, FALSE)), miss(x))
  } else if (typ == "nonnorm") {
    rows[[1]] <- c(lab, s_nonn(x), s_nonn(x[d$outcome == 0]), s_nonn(x[d$outcome == 1]),
                   fmt_p(p_cont(d, var, TRUE)), miss(x))
  } else {
    p <- fmt_p(p_cat(d, var))
    rows[[1]] <- c(lab, "", "", "", p, miss(x))
    for (lv in levs) {
      if (emop_na && var == "emop") {
        rows[[length(rows) + 1]] <- c(paste0("  ", lv), "—", "—", "—", "", "")
      } else {
        rows[[length(rows) + 1]] <- c(paste0("  ", lv), s_cat(x, lv),
                                      s_cat(x[d$outcome == 0], lv), s_cat(x[d$outcome == 1], lv), "", "")
      }
    }
    if (length(levs) == 1) {
      if (emop_na && var == "emop") {
        rows <- list(c(lab, "—", "—", "—", "", miss(x)))
      } else {
        rows <- list(c(lab, s_cat(x, levs), s_cat(x[d$outcome == 0], levs),
                       s_cat(x[d$outcome == 1], levs), p, miss(x)))
      }
    }
  }
  rows
}
mk <- function(d, emop_na = FALSE)
  as.data.table(do.call(rbind, lapply(lapply(spec, build_side, d = d, emop_na = emop_na),
                                      function(x) do.call(rbind, x))))
tv <- mk(v); ti <- mk(i); tm <- mk(m, emop_na = TRUE)
coln <- c("Variable", "Overall", "NoEvent", "Event", "p", "Miss")
setnames(tv, coln); setnames(ti, coln); setnames(tm, coln)
stopifnot(identical(tv$Variable, ti$Variable), identical(tv$Variable, tm$Variable))

comb <- cbind(data.table(Variable = c("n", tv$Variable)),
              rbind(data.table(V_Overall = "5987", V_NoEvent = "4803", V_Event = "1184",
                               V_p = "", V_Miss = ""),
                    tv[, .(V_Overall = Overall, V_NoEvent = NoEvent, V_Event = Event,
                           V_p = p, V_Miss = Miss)]),
              rbind(data.table(I_Overall = "96196", I_NoEvent = "85826", I_Event = "10370",
                               I_p = "", I_Miss = ""),
                    ti[, .(I_Overall = Overall, I_NoEvent = NoEvent, I_Event = Event,
                           I_p = p, I_Miss = Miss)]),
              rbind(data.table(M_Overall = "48370", M_NoEvent = "26630", M_Event = "21740",
                               M_p = "", M_Miss = ""),
                    tm[, .(M_Overall = Overall, M_NoEvent = NoEvent, M_Event = Event,
                           M_p = p, M_Miss = Miss)]))
fwrite(comb, file.path(OUT, "table1_combined_v4.csv"))
cat("table1 v4 written:", nrow(comb), "rows\n")

# ================= Table 2 v4 =================
f3 <- function(x) sprintf("%.3f", x)
disp <- c(SASA = "SASA (reference)", LR_clinical = "LR (clinical)",
          LR_full = "LR (full)", XGB_full = "XGBoost (full, primary)",
          XGB_noArt = "XGBoost (no arterial-line flag)")

raw_block <- function(mi, cohort_lab, models = names(disp)) {
  rbindlist(lapply(models, function(md) {
    r <- mi[model == md & recalibration %in% c("none", NA)]
    if (nrow(r) != 1) stop(paste("bad row for", md))
    data.table(Cohort = cohort_lab, Model = disp[[md]],
               `AUROC (95% CI)` = sprintf("%.3f (%.3f–%.3f)", r$AUROC, r$lo, r$hi),
               AUPRC = f3(r$AUPRC), Brier = f3(r$Brier),
               `Cal intercept` = f3(r$cal_intercept), `Cal slope` = f3(r$cal_slope), Note = "")
  }))
}
recal_block <- function(mi, ss, cohort_lab, auroc_from) {
  rbindlist(lapply(c("LR_clinical", "LR_full", "XGB_full"), function(md) {
    r <- auroc_from[model == md & recalibration %in% c("none", NA)]
    b <- ss[model == md]; 
    data.table(Cohort = cohort_lab, Model = disp[[md]],
               `AUROC (95% CI)` = sprintf("%.3f (%.3f–%.3f)", r$AUROC, r$lo, r$hi),
               AUPRC = "—",
               Brier = sprintf("%.3f (%.3f–%.3f)", b$Brier_mean, b$Brier_lo, b$Brier_hi),
               `Cal intercept` = "0.00 (by design)",
               `Cal slope` = sprintf("%.3f (%.3f–%.3f)", b$cal_slope_mean, b$cal_slope_lo, b$cal_slope_hi),
               Note = "split-sample validated (500×)")
  }))
}

# INSPIRE (corrected outcome)
mi <- fread("/workspace/inspire/metrics_inspire.csv")[cohort == "INSPIRE_primary"]
mi[, recalibration := "none"]
ssi <- fread("/workspace/inspire/inspire_recal_splitsample.csv")
# harmonize split-sample format (long by metric -> wide)
if ("metric" %in% names(ssi)) {
  ssi <- dcast(ssi, model ~ metric, value.var = c("mean", "lo", "hi"))
  ssi <- ssi[, .(model, Brier_mean = mean_Brier, Brier_lo = lo_Brier, Brier_hi = hi_Brier,
                 cal_slope_mean = mean_cal_slope, cal_slope_lo = lo_cal_slope,
                 cal_slope_hi = hi_cal_slope)]
}
ins_raw <- raw_block(mi, "INSPIRE temporal (no recalibration)")
ins_rec <- recal_block(NULL, ssi, "INSPIRE temporal (Platt recalibrated)", mi)

# MOVER (deduped)
mm <- fread("/workspace/ext_out/metrics_mover_dedup.csv")
ssm <- fread("/workspace/ext_out/mover_recal_splitsample_dedup.csv")
mov_raw <- raw_block(mm, "MOVER external (no recalibration)")
mov_rec <- recal_block(NULL, ssm, "MOVER external (Platt recalibrated)",
                       mm[recalibration == "none"])

# VitalDB internal rows unchanged from v2-era table
t2old <- fread("/mnt/results/04_manuscript/table2_performance.csv")
vit_rows <- t2old[Cohort == "VitalDB internal test"]

t2v4 <- rbind(vit_rows, ins_raw, ins_rec, mov_raw, mov_rec)
fwrite(t2v4, file.path(OUT, "table2_performance_v4.csv"))
cat("table2 v4 written:", nrow(t2v4), "rows\n")
print(t2v4[, .(Cohort, Model, `AUROC (95% CI)`)])

# ================= Supplementary: INSPIRE sensitivity (corrected outcome) =================
mall <- fread("/workspace/inspire/metrics_inspire.csv")
bound <- fread("/workspace/inspire/overlap_bound.csv")
sens <- mall[cohort != "INSPIRE_primary" & model %in% c("XGB_full", "XGB_noArt"),
             .(cohort, model, n, events, AUROC, lo, hi, AUPRC, Brier,
               cal_intercept, cal_slope)]
sens[, `:=`(AUROC = sprintf("%.4f (%.4f–%.4f)", AUROC, lo, hi),
            AUPRC = sprintf("%.4f", AUPRC), Brier = sprintf("%.4f", Brier),
            cal_intercept = sprintf("%.3f", cal_intercept),
            cal_slope = sprintf("%.3f", cal_slope))]
sens[, `:=`(lo = NULL, hi = NULL)]
px <- mall[cohort == "INSPIRE_primary" & model == "XGB_full"]
hdr <- data.table(
  cohort = "INSPIRE_primary (reference)", model = "XGB_full", n = px$n, events = px$events,
  AUROC = sprintf("%.4f (%.4f–%.4f)", px$AUROC, px$lo, px$hi),
  AUPRC = sprintf("%.4f", px$AUPRC), Brier = sprintf("%.4f", px$Brier),
  cal_intercept = sprintf("%.3f", px$cal_intercept), cal_slope = sprintf("%.3f", px$cal_slope))
bnd <- data.table(
  cohort = "Worst-case residual-overlap bound", model = "XGB_full",
  n = px$n + bound$n_synthetic, events = NA_integer_,
  AUROC = sprintf("%.4f (+%.4f vs primary)", bound$AUROC_worstcase_inflated, bound$delta),
  AUPRC = "", Brier = "", cal_intercept = "", cal_slope = "")
sens_out <- rbind(hdr, sens, bnd, fill = TRUE)
fwrite(sens_out, file.path(OUT, "table_s_inspire_overlap_sensitivity_v4.csv"))
print(sens_out)
cat("== 43 done ==\n")
