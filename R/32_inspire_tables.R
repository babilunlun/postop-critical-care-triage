#!/usr/bin/env Rscript
# 32_inspire_tables.R — Table 1 v3 (add INSPIRE column), Table 2 v3 (add INSPIRE rows),
# supplementary INSPIRE sensitivity table.
suppressPackageStartupMessages(library(data.table))

DIR <- "/workspace/inspire"
OUT <- "/workspace/tables"
dir.create(OUT, showWarnings = FALSE)

# ================= Table 1: INSPIRE column =================
ai <- readRDS(file.path(DIR, "analysis_inspire.rds"))
ov <- readRDS(file.path(DIR, "inspire_overlap.rds"))$flags
ai <- merge(ai, ov, by = "op_id", all.x = TRUE)
d <- ai[excl_primary == FALSE]
stopifnot(nrow(d) == 96196)

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
  if (nonnorm) suppressWarnings(wilcox.test(x0, x1))$p.value
  else t.test(x0, x1)$p.value
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
  list("bmi", "BMI, kg/m\u00b2", "norm"),
  list("asa", "ASA physical status", "cat", c("1","2","3","4","5","6")),
  list("emop", "Emergency surgery", "cat", c("1")),
  list("department", "Surgical department", "cat",
       c("General surgery", "Thoracic surgery", "Urology", "Gynecology")),
  list("preop_htn", "Hypertension", "cat", c("1")),
  list("preop_dm", "Diabetes mellitus", "cat", c("1")),
  list("preop_hb", "Preop hemoglobin, g/dL", "norm"),
  list("preop_plt", "Preop platelets, 10\u00b3/\u00b5L", "norm"),
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
  list("area_map65", "Area under MAP 65 mmHg, mmHg\u00b7min", "nonnorm"),
  list("hr_mean", "Heart rate mean, /min", "norm"),
  list("spo2_min", "SpO2 minimum, %", "norm"),
  list("bis_mean", "BIS mean", "norm"),
  list("coverage", "Monitoring coverage, fraction", "norm")
)
build_side <- function(d, sp) {
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
      rows[[length(rows) + 1]] <- c(paste0("  ", lv), s_cat(x, lv),
                                    s_cat(x[d$outcome == 0], lv), s_cat(x[d$outcome == 1], lv), "", "")
    }
    if (length(levs) == 1) {
      rows <- list(c(lab, s_cat(x, levs), s_cat(x[d$outcome == 0], levs),
                     s_cat(x[d$outcome == 1], levs), p, miss(x)))
    }
  }
  rows
}
ti <- as.data.table(do.call(rbind, lapply(lapply(spec, build_side, d = d),
                                          function(x) do.call(rbind, x))))
setnames(ti, c("Variable", "Overall", "NoEvent", "Event", "p", "Miss"))

old <- fread("/mnt/results/04_manuscript/table1_combined.csv")
stopifnot(identical(trimws(old$Variable[-1]), trimws(ti$Variable)))
ti_hdr <- data.table(Overall = "", NoEvent = "", Event = "", p = "", Miss = "")
ti2 <- rbind(ti_hdr, ti[, .(Overall, NoEvent, Event, p, Miss)])
comb <- cbind(data.table(Variable = c("n", ti$Variable)),
              old[, .(V_Overall, V_NoEvent, V_Event, V_p, V_Miss)],
              ti2[, .(I_Overall = Overall, I_NoEvent = NoEvent, I_Event = Event,
                      I_p = p, I_Miss = Miss)],
              old[, .(M_Overall, M_NoEvent, M_Event, M_p, M_Miss)])
comb[Variable == "n", `:=`(I_Overall = "96196", I_NoEvent = "81569", I_Event = "14627",
                           I_p = "", I_Miss = "")]
fwrite(comb, file.path(OUT, "table1_combined_v3.csv"))
cat("table1 v3 written\n")

# ================= Table 2: INSPIRE rows =================
mi <- fread(file.path(DIR, "metrics_inspire.csv"))[cohort == "INSPIRE_primary"]
ss <- fread(file.path(DIR, "inspire_recal_splitsample.csv"))

f_auc <- function(m) {
  r <- mi[model == m]
  sprintf("%.3f (%.3f\u2013%.3f)", r$AUROC, r$lo, r$hi)
}
f3 <- function(x) sprintf("%.3f", x)
disp <- c(SASA = "SASA (reference)", LR_clinical = "LR (clinical)",
          LR_full = "LR (full)", XGB_full = "XGBoost (full, primary)",
          XGB_noArt = "XGBoost (no arterial-line flag)")

raw_rows <- rbindlist(lapply(names(disp), function(m) {
  r <- mi[model == m]
  data.table(Cohort = "INSPIRE temporal (no recalibration)", Model = disp[[m]],
             `AUROC (95% CI)` = f_auc(m), AUPRC = f3(r$AUPRC), Brier = f3(r$Brier),
             `Cal intercept` = f3(r$cal_intercept), `Cal slope` = f3(r$cal_slope),
             Note = "")
}))

recal_rows <- rbindlist(lapply(c("LR_clinical", "LR_full", "XGB_full"), function(m) {
  b <- ss[model == m & metric == "Brier"]
  s <- ss[model == m & metric == "cal_slope"]
  data.table(Cohort = "INSPIRE temporal (Platt recalibrated)", Model = disp[[m]],
             `AUROC (95% CI)` = f_auc(m), AUPRC = "\u2014",
             Brier = sprintf("%.3f (%.3f\u2013%.3f)", b$mean, b$lo, b$hi),
             `Cal intercept` = "0.00 (by design)",
             `Cal slope` = sprintf("%.3f (%.3f\u2013%.3f)", s$mean, s$lo, s$hi),
             Note = "split-sample validated (500\u00d7)")
}))

t2 <- fread("/mnt/results/04_manuscript/table2_performance.csv")
idx <- which(t2$Cohort == "MOVER external (no recalibration)")[1]
t2v3 <- rbind(t2[1:(idx - 1)], raw_rows, recal_rows, t2[idx:nrow(t2)])
fwrite(t2v3, file.path(OUT, "table2_performance_v3.csv"))
cat("table2 v3 written\n")

# ================= Supplementary: INSPIRE sensitivity =================
mall <- fread(file.path(DIR, "metrics_inspire.csv"))
bound <- fread(file.path(DIR, "overlap_bound.csv"))
sens <- mall[cohort != "INSPIRE_primary" & model %in% c("XGB_full", "XGB_noArt"),
             .(cohort, model, n, events, AUROC, lo, hi, AUPRC, Brier,
               cal_intercept, cal_slope)]
sens[, `:=`(AUROC = sprintf("%.4f (%.4f\u2013%.4f)", AUROC, lo, hi),
            AUPRC = sprintf("%.4f", AUPRC), Brier = sprintf("%.4f", Brier),
            cal_intercept = sprintf("%.3f", cal_intercept),
            cal_slope = sprintf("%.3f", cal_slope))]
sens[, `:=`(lo = NULL, hi = NULL)]
primary_xgb <- mall[cohort == "INSPIRE_primary" & model == "XGB_full"]
hdr <- data.table(
  cohort = "INSPIRE_primary (reference)", model = "XGB_full",
  n = primary_xgb$n, events = primary_xgb$events,
  AUROC = sprintf("%.4f (%.4f\u2013%.4f)", primary_xgb$AUROC, primary_xgb$lo, primary_xgb$hi),
  AUPRC = sprintf("%.4f", primary_xgb$AUPRC), Brier = sprintf("%.4f", primary_xgb$Brier),
  cal_intercept = sprintf("%.3f", primary_xgb$cal_intercept),
  cal_slope = sprintf("%.3f", primary_xgb$cal_slope))
bnd <- data.table(
  cohort = "Worst-case residual-overlap bound", model = "XGB_full",
  n = primary_xgb$n + bound$n_synthetic, events = NA_integer_,
  AUROC = sprintf("%.4f (+%.4f vs primary)", bound$AUROC_worstcase_inflated, bound$delta),
  AUPRC = "", Brier = "", cal_intercept = "", cal_slope = "")
sens_out <- rbind(hdr, sens, bnd, fill = TRUE)
fwrite(sens_out, file.path(OUT, "table_s_inspire_overlap_sensitivity.csv"))
print(sens_out)
cat("== 32 done ==\n")
