#!/usr/bin/env Rscript
# Unified manuscript Table 1: VitalDB (n=5,987) vs MOVER (n=49,394),
# overall and stratified by event, with within-cohort p and missingness.
suppressPackageStartupMessages(library(data.table))

v <- readRDS("/mnt/shared-workspace/shared/analysis_vitaldb.rds")
g <- readRDS("/workspace/gru/gru_data.rds")
v <- v[caseid %in% g$caseid]
m <- as.data.table(readRDS("/mnt/shared-workspace/shared/analysis_mover.rds"))
stopifnot(nrow(v) == 5987, nrow(m) == 49394)

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

# spec: name, label, type ("norm"/"nonnorm"/"cat"), levels (for cat)
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
        rows[[length(rows) + 1]] <- c(paste0("  ", lv), "\u2014", "\u2014", "\u2014", "", "")
      } else {
        rows[[length(rows) + 1]] <- c(paste0("  ", lv), s_cat(x, lv),
                                      s_cat(x[d$outcome == 0], lv), s_cat(x[d$outcome == 1], lv), "", "")
      }
    }
    # single-level display vars: collapse to one row "var = level"
    if (length(levs) == 1) {
      if (emop_na && var == "emop") {
        rows <- list(c(lab, "\u2014", "\u2014", "\u2014", "", miss(x)))
      } else {
        rows <- list(c(lab, s_cat(x, levs), s_cat(x[d$outcome == 0], levs),
                       s_cat(x[d$outcome == 1], levs), p, miss(x)))
      }
    }
  }
  rows
}

tv <- do.call(rbind, lapply(lapply(spec, build_side, d = v),
                            function(x) do.call(rbind, x)))
tm <- do.call(rbind, lapply(lapply(spec, build_side, d = m, emop_na = TRUE),
                            function(x) do.call(rbind, x)))
coln <- c("Variable", "Overall", "No event", "Event", "p", "Missing, %")
tv <- as.data.table(tv); tm <- as.data.table(tm)
setnames(tv, coln); setnames(tm, coln)

comb <- cbind(tv[, .(Variable)],
              tv[, .(V_Overall = Overall, V_NoEvent = `No event`, V_Event = Event,
                     V_p = p, V_Miss = `Missing, %`)],
              tm[, .(M_Overall = Overall, M_NoEvent = `No event`, M_Event = Event,
                     M_p = p, M_Miss = `Missing, %`)])
hdr <- data.table(Variable = "n",
                  V_Overall = "5987", V_NoEvent = "4803", V_Event = "1184",
                  V_p = "", V_Miss = "",
                  M_Overall = "49394", M_NoEvent = "27235", M_Event = "22159",
                  M_p = "", M_Miss = "")
comb <- rbind(hdr, comb)
fwrite(comb, "/workspace/tables/table1_combined.csv")
print(comb, nrows = 60)
cat("DONE\n")
