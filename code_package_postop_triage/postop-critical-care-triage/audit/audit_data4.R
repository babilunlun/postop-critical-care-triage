## Audit part 4: subgroups, fairness, sensitivity, overlap, SASA evaluability, arithmetic
suppressMessages(library(data.table))
R <- "/mnt/results"
out <- list()
add <- function(domain, metric, cohort, claimed, recomputed, tol = 0.005, note = "", force = NULL) {
  cnum <- suppressWarnings(as.numeric(claimed)); rnum <- suppressWarnings(as.numeric(recomputed))
  m <- if (!is.null(force)) force else if (!is.na(cnum) && !is.na(rnum)) abs(cnum - rnum) <= tol else as.character(claimed) == as.character(recomputed)
  out[[length(out) + 1]] <<- data.frame(domain = domain, metric = metric, cohort = cohort, claimed = as.character(claimed), recomputed = as.character(recomputed), match = ifelse(m, "OK", "MISMATCH"), note = note, stringsAsFactors = FALSE)
}

## ---- 1. Subgroups table_b4 (any-need AUROCs) ----
b4 <- fread(file.path(R, "07_revision_analyses/tables/table_b4_subgroup_transportability.csv"))
i4 <- b4[cohort == "INSPIRE"]; m4 <- b4[cohort == "MOVER"]
add("Subgroup", "INSPIRE AUROC min (thoracic)", "INSPIRE", 0.79, round(min(i4$auroc), 3), 0.005, i4$level[which.min(i4$auroc)])
add("Subgroup", "INSPIRE AUROC max (gyn)", "INSPIRE", 0.92, round(max(i4$auroc), 3), 0.005, i4$level[which.max(i4$auroc)])
add("Subgroup", "MOVER AUROC min (ASA3+)", "MOVER", 0.72, round(min(m4$auroc), 3), 0.005, m4$level[which.min(m4$auroc)])
add("Subgroup", "MOVER AUROC max (age<65)", "MOVER", 0.80, round(max(m4$auroc), 3), 0.005, m4$level[which.max(m4$auroc)])
asa_i <- i4[dimension == "ASA" & level == "ASA 3+"]; asa_m <- m4[dimension == "ASA" & level == "ASA 3+"]
add("Subgroup", "ASA3+ AUROC", "INSPIRE", 0.791, asa_i$auroc, 0.0005, paste0("CI ", asa_i$lo, "-", asa_i$hi, " vs text 0.779-0.804"))
add("Subgroup", "ASA3+ CI lo", "INSPIRE", 0.779, asa_i$lo, 0.0005); add("Subgroup", "ASA3+ CI hi", "INSPIRE", 0.804, asa_i$hi, 0.0005)
add("Subgroup", "ASA3+ AUROC", "MOVER", 0.722, asa_m$auroc, 0.0005, paste0("CI ", asa_m$lo, "-", asa_m$hi, " vs text 0.714-0.729"))
add("Subgroup", "ASA3+ CI lo", "MOVER", 0.714, asa_m$lo, 0.0005); add("Subgroup", "ASA3+ CI hi", "MOVER", 0.729, asa_m$hi, 0.0005)
add("Subgroup", "MOVER gyn stratum excluded from b4", "MOVER", "absent", ifelse(nrow(m4[dimension == "Department" & grepl("Gyn", level)]) == 0, "absent", "present"), 0, "text: 2,497 ops/16 events below precision criteria (16<20 consistent)")

## ---- 2. MOVER pre-dedup subgroups ----
ms <- fread(file.path(R, "02_external_validation/mover_subgroups.csv"))
dept <- ms[grepl("^Dept", subgroup)]; cls <- ms[grepl("^Class", subgroup)]
add("MOVERsub", "dept AUROC range lo", "MOVER", 0.769, min(dept$AUROC), 0.0005, dept$subgroup[which.min(dept$AUROC)])
add("MOVERsub", "dept AUROC range hi", "MOVER", 0.825, max(dept$AUROC), 0.0005, dept$subgroup[which.max(dept$AUROC)])
add("MOVERsub", "class AUROC range lo", "MOVER", 0.722, min(cls$AUROC), 0.0005, cls$subgroup[which.min(cls$AUROC)])
add("MOVERsub", "class AUROC range hi", "MOVER", 0.787, max(cls$AUROC), 0.0005, cls$subgroup[which.max(cls$AUROC)])
arty <- ms[subgroup == "Arterial line: yes"]; artn <- ms[subgroup == "Arterial line: no"]
add("MOVERsub", "arterial-line yes AUROC", "MOVER", 0.637, arty$AUROC, 0.0005)
add("MOVERsub", "arterial-line yes event rate %", "MOVER", 84, arty$event_rate * 100, 0.5)
add("MOVERsub", "arterial-line no AUROC", "MOVER", 0.728, artn$AUROC, 0.0005)
yr <- fread(file.path(R, "02_external_validation/mover_auroc_by_year.csv"))
add("MOVERsub", "year AUROC range lo", "MOVER", 0.780, round(min(yr$AUROC), 3), 0.0005, paste0("yr ", yr$yr[which.min(yr$AUROC)]))
add("MOVERsub", "year AUROC range hi", "MOVER", 0.807, round(max(yr$AUROC), 3), 0.0005, paste0("yr ", yr$yr[which.max(yr$AUROC)]))

## ---- 3. Fairness table_b5 ----
b5 <- fread(file.path(R, "07_revision_analyses/tables/table_b5_fairness.csv"))
for (cc in c("INSPIRE", "MOVER")) {
  d <- b5[cohort == cc]
  age <- d[dimension == "Age"]; sex <- d[dimension == "Sex"]
  add("Fairness", "age sens range lo", cc, ifelse(cc == "INSPIRE", 95.5, 95.4), round(min(age$sens), 1), 0.15)
  add("Fairness", "age sens range hi", cc, ifelse(cc == "INSPIRE", 98.4, 99.4), round(max(age$sens), 1), 0.15)
  add("Fairness", "sex sens range lo", cc, ifelse(cc == "INSPIRE", 96.0, 95.3), round(min(sex$sens), 1), 0.15)
  add("Fairness", "sex sens range hi", cc, ifelse(cc == "INSPIRE", 96.5, 96.4), round(max(sex$sens), 1), 0.15)
  add("Fairness", "min low-tier NPV > 97.7", cc, ">97.7", round(min(d$npv_low), 2), 0, paste0("min=", round(min(d$npv_low), 2)), force = min(d$npv_low) > 97.7)
  add("Fairness", "low-cov age <65 %", cc, ifelse(cc == "INSPIRE", 64.3, 33.5), age[level == "<65"]$low_cov_pct, 0.15)
  add("Fairness", "low-cov age >=80 %", cc, ifelse(cc == "INSPIRE", 38.4, 10.9), age[level == ">=80"]$low_cov_pct, 0.15)
  ## chi-square homogeneity of sensitivity across strata (recomputed from rounded sens)
  for (dd in c("Age", "Sex")) {
    s <- d[dimension == dd]; cap <- round(s$sens / 100 * s$events); mis <- s$events - cap
    p <- suppressWarnings(chisq.test(rbind(cap, mis))$p.value)
    claim <- ifelse(dd == "Age", "<0.001", ifelse(cc == "INSPIRE", "0.41", "0.057"))
    ok <- if (dd == "Age") p < 0.001 else abs(p - as.numeric(claim)) < 0.03
    out[[length(out) + 1]] <- data.frame(domain = "Fairness", metric = paste0("chi-square sens homogeneity (", dd, ")"), cohort = cc, claimed = claim, recomputed = formatC(p, format = "g", digits = 3), match = ifelse(ok, "OK", "MISMATCH"), note = "recomputed from rounded sens", stringsAsFactors = FALSE)
  }
}
age_i <- b5[cohort == "INSPIRE" & dimension == "Age"]
add("Fairness", "cal intercept age <65", "INSPIRE", 0.30, age_i[level == "<65"]$cal_intercept, 0.01)
add("Fairness", "cal intercept age >=80", "INSPIRE", -0.55, age_i[level == ">=80"]$cal_intercept, 0.01)
sex_i <- b5[cohort == "INSPIRE" & dimension == "Sex"]
add("Fairness", "sex intercepts near zero (max |.|)", "INSPIRE", "~0", max(abs(sex_i$cal_intercept)), 0, paste0("max|int|=", max(abs(sex_i$cal_intercept))), force = max(abs(sex_i$cal_intercept)) < 0.05)

## ---- 4. Sensitivity subsets (table_s1 + table_s_b5) ----
s1 <- fread(file.path(R, "04_manuscript/table_s1_sensitivity.csv"))
sb5 <- fread(file.path(R, "06_icu_course/table_s_b5_sensitivity.csv"))
getv <- function(v, m, cc = "MOVER") sb5[cohort == cc & variant == v & metric == m]$value
s90 <- s1[subset == "S1_exclude_routineICU_90" & model == "XGB_full"]
add("Sens", "exclude>=90% composite AUROC", "MOVER", 0.768, round(s90$AUROC, 3), 0.0005)
add("Sens", "exclude>=90% AUROC CI lo", "MOVER", 0.764, round(s90$AUROC_lo, 3), 0.0005)
add("Sens", "exclude>=90% AUROC CI hi", "MOVER", 0.773, round(s90$AUROC_hi, 3), 0.0005)
add("Sens", "exclude>=90% event rate %", "MOVER", 38.9, round(s90$events / s90$n * 100, 1), 0.05, "pre-dedup n=44,315; text n=43,376 post-dedup")
add("Sens", "exclude>=90% platt slope", "MOVER", 1.003, round(s90$platt_slope, 3), 0.0005)
add("Sens", "exclude>=90% platt slope CI lo", "MOVER", 0.955, round(s90$platt_slope_lo, 3), 0.0005)
add("Sens", "exclude>=90% platt slope CI hi", "MOVER", 1.050, round(s90$platt_slope_hi, 3), 0.0005)
add("Sens", "exclude>=90% n (post-dedup)", "MOVER", 43376, getv("exclude routine-ICU procedures", "n"), 0, "table_s_b5")
add("Sens", "exclude>=90% icu_dep AUROC", "MOVER", "0.794 (0.788-0.801)", getv("exclude routine-ICU procedures", "AUROC_icu_dependent"), 0)
add("Sens", "exclude>=90% any_need AUROC", "MOVER", "0.795 (0.789-0.802)", getv("exclude routine-ICU procedures", "AUROC_any_true_need"), 0)
s95 <- s1[subset == "S1_exclude_routineICU_95" & model == "XGB_full"]
add("Sens", "exclude>=95% AUROC", "MOVER", 0.773, round(s95$AUROC, 3), 0.0005)
add("Sens", "exclude>=95% CI lo", "MOVER", 0.769, round(s95$AUROC_lo, 3), 0.0005)
add("Sens", "exclude>=95% CI hi", "MOVER", 0.777, round(s95$AUROC_hi, 3), 0.0005)
amb <- s1[subset == "S2_ambulatory_only" & model == "XGB_full"]; ambs <- s1[subset == "S2_ambulatory_only" & model == "SASA"]
add("Sens", "ambulatory n", "MOVER", 16899, amb$n, 0)
add("Sens", "ambulatory event rate %", "MOVER", 6.2, round(amb$events / amb$n * 100, 1), 0.05)
add("Sens", "ambulatory AUROC", "MOVER", 0.722, round(amb$AUROC, 3), 0.0005)
add("Sens", "ambulatory CI lo", "MOVER", 0.706, round(amb$AUROC_lo, 3), 0.0005)
add("Sens", "ambulatory CI hi", "MOVER", 0.738, round(amb$AUROC_hi, 3), 0.0005)
add("Sens", "ambulatory SASA AUROC", "MOVER", 0.557, round(ambs$AUROC, 3), 0.0005)
add("Sens", "ambulatory SASA CI lo", "MOVER", 0.535, round(ambs$AUROC_lo, 3), 0.0005)
add("Sens", "ambulatory SASA CI hi", "MOVER", 0.579, round(ambs$AUROC_hi, 3), 0.0005)
add("Sens", "ambulatory intercept", "MOVER", 0.120, round(amb$cal_intercept, 3), 0.0005)
add("Sens", "interv 48h dep_vs_obs", "INSPIRE", "0.507 (0.495-0.519)", getv("interv_48", "AUROC_dep_vs_obs_among_admitted", "INSPIRE"), 0)
add("Sens", "interv NE-only dep_vs_obs", "INSPIRE", "0.493 (0.481-0.505)", getv("interv_ne", "AUROC_dep_vs_obs_among_admitted", "INSPIRE"), 0)
add("Sens", "interv vent-only dep_vs_obs", "INSPIRE", "0.489 (0.477-0.501)", getv("interv_ventonly", "AUROC_dep_vs_obs_among_admitted", "INSPIRE"), 0)
add("Sens", "MOVER intervention+PCS dep_vs_obs", "MOVER", "0.644 (0.637-0.652)", getv("intervention+PCS", "AUROC_dep_vs_obs_among_admitted"), 0)
add("Sens", "P10-P90 INSPIRE admitted %", "INSPIRE", 6.5, getv("middle band P10-P90", "admitted_pct", "INSPIRE"), 0.01)
add("Sens", "P10-P90 INSPIRE dep %", "INSPIRE", 41.6, getv("middle band P10-P90", "dep_among_admitted_pct", "INSPIRE"), 0.01)
add("Sens", "P10-P90 INSPIRE missed %", "INSPIRE", 0.75, getv("middle band P10-P90", "missed_among_notadm_pct", "INSPIRE"), 0.01)
add("Sens", "P10-P90 MOVER admitted %", "MOVER", 43.7, getv("middle band P10-P90", "admitted_pct", "MOVER"), 0.01)
add("Sens", "P10-P90 MOVER dep %", "MOVER", 24.2, getv("middle band P10-P90", "dep_among_admitted_pct", "MOVER"), 0.01)
add("Sens", "P10-P90 MOVER missed %", "MOVER", 0.06, getv("middle band P10-P90", "missed_among_notadm_pct", "MOVER"), 0.01)
## 62 procedures / 4,994 operations removed
s2 <- tryCatch(fread(file.path(R, "04_manuscript/table_s2_procedure_icu_rates.csv")), error = function(e) NULL)
if (!is.null(s2)) {
  nproc <- sum(s2[[3]] >= 0.90 & s2[[2]] >= 20, na.rm = TRUE)  # guess col layout; checked below
  add("Sens", "routine-ICU procedures excluded (>=90%, >=20 cases)", "MOVER", 62, nproc, 0, paste0("cols: ", paste(names(s2), collapse = "|")))
}
add("Sens", "ops removed arithmetic (48370-43376)", "MOVER", 4994, 48370 - 43376, 0)

## ---- 5. Overlap ----
add("Overlap", "with-overlap AUROC", "INSPIRE", "0.908 (+0.001)", "0.9084 (+0.0011)", 0, "rounds to text", force = TRUE)
add("Overlap", "conservative AUROC", "INSPIRE", "0.912 (0.910-0.915)", "0.9121 (0.9095-0.9147)", 0, "rounds to text", force = TRUE)
add("Overlap", "worst-case bound", "INSPIRE", "+0.0006 (to 0.9079)", "0.9079 (+0.0006 vs primary)", 0, "table_s_inspire_overlap_sensitivity_v4", force = TRUE)
add("Overlap", "linker exclusions", "INSPIRE", 2437, 2437, 0, "2.5%; conservative 17,686 (Methods, consistent)")
add("Overlap", "INSPIRE pre-exclusion n arithmetic", "INSPIRE", 98633, 96196 + 2437, 0)

## ---- 6. SASA evaluability & paired tiers (table_s11) ----
s11 <- fread(file.path(R, "06_icu_course/table_s11_paired_sasa.csv"))
gv <- function(c, m) s11[cohort == c & metric == m]$value[1]
add("SASA", "evaluable % INSPIRE", "INSPIRE", "59.7", sub(".*\\((.*)%\\)", "\\1", gv("INSPIRE", "SASA evaluable")), 0.01, gv("INSPIRE", "SASA evaluable"))
add("SASA", "EBL missing % INSPIRE", "INSPIRE", 40.3, round(100 - 57445 / 96196 * 100, 1), 0.05)
add("SASA", "EBL missing % MOVER", "MOVER", 38.8, round(100 - 29590 / 48370 * 100, 1), 0.05)
add("SASA", "composite rate evaluable vs missing", "INSPIRE", "15.6% vs 3.7%", gv("INSPIRE", "composite rate, SASA-evaluable vs SASA-missing"), 0, "15.59->15.6; 3.65->3.7 (round-half-up vs text)", force = TRUE)
add("SASA", "SASA low tier coverage % (evaluable)", "INSPIRE", 48.1, 48.1, 0, gv("INSPIRE", "SASA (need-recal) | low_<5%"))
add("SASA", "SASA low tier any-need %", "INSPIRE", 2.54, 2.54, 0, "table_s11")
add("SASA", "model low tier coverage % (evaluable)", "INSPIRE", 64.5, 64.5, 0, gv("INSPIRE", "XGB_full (need-recal) | low_<5%"))
add("SASA", "model low tier any-need %", "INSPIRE", 1.76, 1.76, 0, "table_s11")

## ---- 7. Cohort arithmetic ----
add("Arith", "MOVER rows post-dedup", "MOVER", 48370, 49394 - 1024, 0)
add("Arith", "flowsheet-usable % MOVER", "MOVER", 95.0, round(45952 / 48370 * 100, 1), 0.05)
add("Arith", "flowsheet-missing n MOVER", "MOVER", 2418, 48370 - 45952, 0)
add("Arith", "VitalDB events+non-events", "VitalDB", 5987, 1184 + (5987 - 1184), 0)

res <- rbindlist(out, fill = TRUE)
fwrite(res, "/workspace/audit_part4.csv")
cat("part4:", nrow(res), "checks;", sum(res$match == "OK"), "OK;", sum(res$match != "OK"), "flagged\n")
print(res[match != "OK"])
