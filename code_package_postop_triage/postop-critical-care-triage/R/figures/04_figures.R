#!/usr/bin/env Rscript
# Figures + Table 1 for internal validation
# Style: Liberation Sans, colorblind-friendly, SVG via svglite

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggprism)
  library(patchwork)
  library(svglite)
  library(pROC)
  library(tidymodels)
  library(shapviz)
})

FONT <- "Liberation Sans"
theme_set(theme_prism(base_family = FONT, base_size = 11))
MODELS <- c("SASA", "LR_clinical", "LR_full", "XGB_full")
PAL <- c("SASA" = "#CC79A7", "LR_clinical" = "#0072B2", "LR_full" = "#E69F00",
         "XGB_full" = "#009E73")
PAL_PRETTY <- c("SASA" = "#CC79A7", "LR (clinical)" = "#0072B2",
                "LR (full)" = "#E69F00", "XGBoost (full)" = "#009E73",
                "Treat All" = "grey50")

OUT_DIR <- "/workspace/model_out"
FIG_DIR <- "/workspace/figures"
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

preds <- fread(file.path(OUT_DIR, "test_predictions.csv"))
analysis <- readRDS("/workspace/analysis_vitaldb.rds")

# ---------- Fig: ROC + calibration + DCA panel ----------
roc_df <- rbindlist(lapply(MODELS, function(m) {
  r <- roc(preds$outcome, preds[[m]], quiet = TRUE)
  data.frame(model = m, sens = r$sensitivities, spec = r$specificities,
             auc = as.numeric(auc(r)))
}))

roc_df$model <- factor(roc_df$model,
                       levels = MODELS,
                       labels = c("SASA", "LR (clinical)", "LR (full)", "XGBoost (full)"))
p_roc <- ggplot(roc_df, aes(1 - spec, sens, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3, color = "grey60") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = PAL_PRETTY) +
  labs(x = "1 - Specificity", y = "Sensitivity", color = NULL) +
  coord_equal() +
  theme(legend.position = "inside", legend.position.inside = c(0.62, 0.28))

# calibration: decile bins + loess
cal_df <- rbindlist(lapply(MODELS, function(m) {
  data.frame(model = m, p = preds[[m]], y = preds$outcome)
}))
cal_df <- cal_df[!is.na(p)]  # SASA inapplicable when EBL not charted
cal_bin <- cal_df[, {
  qs <- unique(quantile(p, probs = seq(0, 1, 0.1)))
  bin <- cut(p, breaks = qs, include.lowest = TRUE)
  .(p_mean = tapply(p, bin, mean), y_obs = tapply(y, bin, mean))
}, by = model]
cal_bin$model <- factor(cal_bin$model,
                        levels = MODELS,
                        labels = c("SASA", "LR (clinical)", "LR (full)", "XGBoost (full)"))

p_cal <- ggplot(cal_bin, aes(p_mean, y_obs, color = model)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3, color = "grey60") +
  geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
  scale_color_manual(values = PAL_PRETTY) +
  labs(x = "Predicted probability", y = "Observed frequency", color = NULL) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme(legend.position = "none")

# DCA: manual net benefit, complete-case cohort (consistent denominator;
# SASA requires charted EBL). NB = TP/n - FP/n * pt/(1-pt)
cc <- complete.cases(preds[, ..MODELS])
dca_src <- preds[cc]
ths <- seq(0.01, 0.6, by = 0.01)
n_cc <- nrow(dca_src); y_cc <- dca_src$outcome; prev_cc <- mean(y_cc)
pretty <- c(SASA = "SASA", LR_clinical = "LR (clinical)",
            LR_full = "LR (full)", XGB_full = "XGBoost (full)", all = "Treat All")
dca_plot_df <- rbindlist(lapply(c(MODELS, "all"), function(m) {
  nb <- sapply(ths, function(t) {
    if (m == "all") return(prev_cc - (1 - prev_cc) * t / (1 - t))
    p <- dca_src[[m]]
    sum(p >= t & y_cc == 1) / n_cc - sum(p >= t & y_cc == 0) / n_cc * t / (1 - t)
  })
  data.frame(model = pretty[m], threshold = ths, net_benefit = unname(nb))
}))
dca_plot_df$model <- factor(dca_plot_df$model,
                            levels = c("Treat All", "SASA", "LR (clinical)",
                                       "LR (full)", "XGBoost (full)"))

p_dca <- ggplot(dca_plot_df,
                aes(threshold, net_benefit, color = model)) +
  geom_hline(yintercept = 0, linetype = 3, color = "grey60") +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = PAL_PRETTY) +
  labs(x = "Threshold probability", y = "Net benefit", color = NULL) +
  coord_cartesian(xlim = c(0, 0.6), ylim = c(-0.05, 0.25)) +
  guides(color = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = NA))

fig_eval <- (p_roc | p_cal | p_dca) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", family = FONT))
ggsave(file.path(FIG_DIR, "fig_internal_validation.svg"), fig_eval,
       width = 11, height = 3.8, device = svglite)
ggsave(file.path(FIG_DIR, "fig_internal_validation.png"), fig_eval,
       width = 11, height = 3.8, dpi = 300)

# ---------- Fig: SHAP summary ----------
sv <- readRDS(file.path(OUT_DIR, "shapviz_xgb.rds"))

# publication-readable feature labels
label_map <- c(
  has_art = "Arterial line in place",
  department_Thoracic.surgery = "Thoracic surgery",
  department_General.surgery = "General surgery",
  department_Gynecology = "Gynecology", department_Urology = "Urology",
  ane_dur_min = "Anesthesia duration (min)",
  n_min_valid = "Monitored minutes",
  age = "Age (yr)", bmi = "BMI", sex_M = "Male sex",
  asa = "ASA class",
  emop = "Emergency surgery",
  preop_htn = "Preop hypertension", preop_dm = "Preop diabetes",
  preop_hb = "Preop hemoglobin", preop_plt = "Preop platelets",
  preop_na = "Preop sodium", preop_k = "Preop potassium",
  preop_gluc = "Preop glucose", preop_alb = "Preop albumin",
  preop_ast = "Preop AST", preop_alt = "Preop ALT",
  preop_bun = "Preop BUN", preop_cr = "Preop creatinine",
  intraop_ebl = "Estimated blood loss (mL)", ebl_missing = "EBL not charted",
  intraop_uo = "Urine output (mL)", intraop_rbc = "RBC transfusion (units)",
  intraop_ffp = "FFP transfusion (units)",
  intraop_crystalloid = "Crystalloid (mL)", intraop_colloid = "Colloid (mL)",
  intraop_ppf = "Propofol dose (mg)", intraop_mdz = "Midazolam dose (mg)",
  intraop_ftn = "Fentanyl dose (mcg)", intraop_rocu = "Rocuronium dose (mg)",
  intraop_eph = "Ephedrine dose (mg)", intraop_phe = "Phenylephrine dose (mcg)",
  intraop_epi = "Epinephrine dose (mcg)",
  map_mean = "Mean MAP (mmHg)", map_sd = "MAP variability (SD)",
  map_cv = "MAP variability (CV)", map_min = "Minimum MAP (mmHg)",
  map_max = "Maximum MAP (mmHg)", map_slope = "MAP slope",
  min_map65 = "Time MAP < 65 mmHg (min)", min_map60 = "Time MAP < 60 mmHg (min)",
  area_map65 = "Area below MAP 65", area_map60 = "Area below MAP 60",
  twa_map65 = "TWA MAP < 65", longest_map65 = "Longest MAP < 65 episode (min)",
  pct_map65 = "% time MAP < 65",
  hr_mean = "Mean heart rate (bpm)", hr_sd = "HR variability (SD)",
  hr_cv = "HR variability (CV)", hr_min = "Minimum HR (bpm)",
  hr_max = "Maximum HR (bpm)", hr_slope = "HR slope",
  min_hr100 = "Time HR > 100 (min)", min_hr50 = "Time HR < 50 (min)",
  spo2_mean = "Mean SpO2 (%)", spo2_min = "Minimum SpO2 (%)",
  min_spo2_92 = "Time SpO2 < 92% (min)", min_spo2_90 = "Time SpO2 < 90% (min)",
  etco2_mean = "Mean EtCO2 (mmHg)", etco2_sd = "EtCO2 variability (SD)",
  min_etco2_low = "Time EtCO2 < 30 (min)", min_etco2_high = "Time EtCO2 > 45 (min)",
  bis_mean = "Mean BIS", bis_sd = "BIS variability (SD)",
  min_bis40 = "Time BIS < 40 (min)", min_bis60 = "Time BIS < 60 (min)",
  coverage = "Monitoring coverage"
)
cn <- colnames(sv$S)
new_cn <- ifelse(cn %in% names(label_map), label_map[cn], cn)
colnames(sv$S) <- new_cn
colnames(sv$X) <- new_cn

p_shap <- sv_importance(sv, kind = "beeswarm", max_display = 15) +
  theme_prism(base_family = FONT, base_size = 11)
ggsave(file.path(FIG_DIR, "fig_shap.svg"), p_shap, width = 8, height = 5.5,
       device = svglite)
ggsave(file.path(FIG_DIR, "fig_shap.png"), p_shap, width = 8, height = 5.5, dpi = 300)

# ---------- Table 1 (VitalDB, by outcome) ----------
library(tableone)
tab1_vars <- c("age", "sex", "bmi", "asa", "emop", "department", "preop_htn",
               "preop_dm", "preop_hb", "preop_plt", "preop_na", "preop_k",
               "preop_gluc", "preop_alb", "preop_ast", "preop_alt",
               "preop_bun", "preop_cr", "ane_dur_min",
               "intraop_ebl", "intraop_crystalloid", "intraop_rbc",
               "map_mean", "min_map65", "area_map65", "hr_mean", "spo2_min",
               "bis_mean")
tab1 <- CreateTableOne(vars = tab1_vars, strata = "outcome",
                       data = as.data.frame(analysis),
                       factorVars = c("sex", "asa", "emop", "department",
                                      "preop_htn", "preop_dm"),
                       addOverall = TRUE)
tab1_mat <- print(tab1, printToggle = FALSE, nonnormal = c("intraop_ebl",
                  "intraop_crystalloid", "min_map65", "area_map65", "ane_dur_min"))
write.csv(tab1_mat, file.path(FIG_DIR, "table1_vitaldb.csv"))

cat("figures + table1 done\n")
