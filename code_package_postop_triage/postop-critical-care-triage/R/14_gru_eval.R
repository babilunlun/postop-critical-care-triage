#!/usr/bin/env Rscript
# GRU arm evaluation: combined metrics table + ROC overlay figure + DeLong tests.
# Outputs: /mnt/results/03_gru_comparator/{metrics_gru.csv, fig_gru_comparison.svg/png}

suppressPackageStartupMessages({
  library(data.table)
  library(pROC)
  library(ggplot2)
  library(svglite)
})

OUT <- "/mnt/results/03_gru_comparator"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---------- load ----------
m_gru <- fread("/workspace/gru/out/metrics_gru.csv")          # GRU test metrics
m_noart <- fread("/workspace/gru/out/metrics_noart.csv")      # has_art sensitivity
m_int <- fread("/mnt/results/01_internal_validation/metrics_test.csv")  # SASA/LR/XGB
preds_gru <- fread("/workspace/gru/out/preds_gru.csv")
tp <- fread("/workspace/gru/test_predictions.csv")
noart <- readRDS("/workspace/gru/out/model_noart.rds")
stopifnot(identical(preds_gru$caseid, tp$caseid))

# ---------- combined internal test metrics ----------
ens <- m_gru[model %in% c("gru_seq_ens", "gru_full_ens")]
ens[, `:=`(val_AUROC = NULL, best_epoch = NULL)]
internal <- rbindlist(list(
  m_int[, model := paste0(model)],                       # SASA, LR_clinical, LR_full, XGB_full
  data.table(model = "XGB_noArt", m_noart[model == "XGB_noArt_internal", .(AUROC, AUPRC, Brier, cal_intercept, cal_slope)]),
  ens[, .(model, AUROC, AUPRC, Brier, cal_intercept, cal_slope)]
), fill = TRUE)
internal[, cohort := "VitalDB_test"]

external <- rbindlist(list(
  data.table(model = "XGB_noArt_mover", m_noart[model == "XGB_noArt_mover_none",
             .(AUROC, AUPRC, Brier, cal_intercept, cal_slope)]),
  data.table(model = "XGB_noArt_mover_platt", m_noart[model == "XGB_noArt_mover_platt",
             .(AUROC, AUPRC, Brier, cal_intercept, cal_slope)])
), fill = TRUE)
external[, cohort := "MOVER"]

combined <- rbindlist(list(internal, external), fill = TRUE)
fwrite(combined, file.path(OUT, "metrics_gru.csv"))
print(combined)

# ---------- DeLong tests (paired, same test set) ----------
y <- tp$outcome
dl <- list()
roc_xgb <- roc(y, tp$XGB_full, quiet = TRUE)
for (nm in c("LR_full", "LR_clinical", "SASA")) {
  r <- roc(y, tp[[nm]], quiet = TRUE)
  dl[[nm]] <- roc.test(roc_xgb, r, method = "delong", paired = TRUE)$p.value
}
dl[["GRU_seq_ens"]] <- roc.test(roc_xgb, roc(y, preds_gru$gru_seq_ens, quiet = TRUE),
                                method = "delong", paired = TRUE)$p.value
dl[["GRU_full_ens"]] <- roc.test(roc_xgb, roc(y, preds_gru$gru_full_ens, quiet = TRUE),
                                 method = "delong", paired = TRUE)$p.value
dl[["XGB_noArt"]] <- roc.test(roc_xgb, roc(y, noart$pred_test, quiet = TRUE),
                              method = "delong", paired = TRUE)$p.value
dl_dt <- data.table(comparator = names(dl), delong_p_vs_XGB_full = unlist(dl))
fwrite(dl_dt, file.path(OUT, "delong_vs_xgb.csv"))
print(dl_dt)

# ---------- ROC overlay figure ----------
roc_list <- list(
  "XGBoost full (0.932)" = roc(y, tp$XGB_full, quiet = TRUE),
  "GRU full ensemble (0.938)" = roc(y, preds_gru$gru_full_ens, quiet = TRUE),
  "LR full (0.915)" = roc(y, tp$LR_full, quiet = TRUE),
  "GRU sequences only (0.875)" = roc(y, preds_gru$gru_seq_ens, quiet = TRUE),
  "SASA (0.716)" = roc(y, tp$SASA, quiet = TRUE)
)
roc_df <- rbindlist(lapply(names(roc_list), function(nm) {
  r <- roc_list[[nm]]
  data.frame(model = nm, sens = r$sensitivities, spec = r$specificities)
}))
roc_df[, fpr := 1 - spec]
roc_df[, model := factor(model, levels = names(roc_list))]

cols <- c("XGBoost full (0.932)" = "#000000",
          "GRU full ensemble (0.938)" = "#D55E00",
          "LR full (0.915)" = "#0072B2",
          "GRU sequences only (0.875)" = "#009E73",
          "SASA (0.716)" = "#999999")

lts <- c("XGBoost full (0.932)" = "solid",
         "GRU full ensemble (0.938)" = "longdash",
         "LR full (0.915)" = "dotdash",
         "GRU sequences only (0.875)" = "dotted",
         "SASA (0.716)" = "twodash")

p <- ggplot(roc_df, aes(x = fpr, y = sens, color = model, linetype = model)) +
  geom_segment(x = 0, y = 0, xend = 1, yend = 1, linetype = "dashed",
               color = "grey80", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols, name = NULL) +
  scale_linetype_manual(values = lts, name = NULL) +
  coord_equal() +
  labs(x = "1 - Specificity", y = "Sensitivity") +
  theme_classic(base_family = "Liberation Sans") +
  theme(legend.position = c(0.62, 0.22),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.5, "cm"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10))

ggsave(file.path(OUT, "fig_gru_comparison.svg"), p, width = 5.5, height = 5.5,
       device = svglite)
ggsave(file.path(OUT, "fig_gru_comparison.png"), p, width = 5.5, height = 5.5,
       dpi = 300)
cat("DONE\n")
