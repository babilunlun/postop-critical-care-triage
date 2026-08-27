#!/usr/bin/env Rscript
# ============================================================================
# B4 — Subgroup transportability of need discrimination
# ============================================================================
# Revision analysis B4 (manuscript Supplementary Fig. S3, Table S13).
# AUROC (DeLong 95% CI) of the need-recalibrated XGB model for any true
# critical care need across pre-specified subgroups in INSPIRE and MOVER.
#
# Correction history: the Urgency dimension is dropped for MOVER because
# emergency status is not recorded there (emop == 0 for all rows = "absent";
# the initial "Elective" stratum was a missing-data artifact). Strata failing
# the pre-specified precision gate (n >= 500, events >= 20, non-events >= 20)
# are recorded as excluded rather than plotted.
#
# Inputs:
#   - preds_need_recal_inspire.csv / preds_need_recal_mover.csv (step 36-39)
#   - inspire_cohort.rds (INSPIRE covariates; step 25)
#   - analysis_mover_dedup.rds (de-duplicated MOVER analysis set; step 42)
# Outputs:
#   - table_b4_subgroup_transportability.csv
#   - fig_b4_subgroup_transportability.svg/.png (basis of Suppl. Fig. S3)
#
# Paths refer to the original analysis workspace; adapt to your local layout.
# ============================================================================
suppressPackageStartupMessages({library(data.table); library(ggplot2); library(svglite); library(pROC)})

OUT <- "/mnt/results/07_revision_analyses"
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "figures"), showWarnings = FALSE, recursive = TRUE)

ins <- fread("/mnt/results/06_icu_course/preds_need_recal_inspire.csv")
mov <- fread("/mnt/results/06_icu_course/preds_need_recal_mover.csv")
ins[, y := as.integer(any_need)]; mov[, y := as.integer(any_need)]
co    <- as.data.table(readRDS("/workspace/inspire/inspire_cohort.rds"))
mov_a <- as.data.table(readRDS("/workspace/ext_out/analysis_mover_dedup.rds"))

ins_c <- merge(ins, co[, .(op_id, age, sex, asa, emop, department)], by.x = "LOG_ID", by.y = "op_id", all.x = TRUE)
mov_c <- merge(mov, mov_a[, .(LOG_ID, age, sex, asa, emop, department)], by = "LOG_ID", all.x = TRUE)
cat("INSPIRE covariate match:", mean(!is.na(ins_c$age)), "| MOVER:", mean(!is.na(mov_c$age)), "\n")

mk_groups <- function(dt) {
  dt[, `:=`(
    age_grp = fcase(age < 65, "<65", age < 80, "65-79", age >= 80, ">=80"),
    sex_grp = fifelse(sex %in% c("F", "Female"), "Female", "Male"),
    asa_grp = fifelse(asa >= 3, "ASA 3+", "ASA 1-2"),
    emop_grp = fifelse(emop == 1, "Emergency", "Elective"))]
  top_dep <- names(sort(table(dt$department), decreasing = TRUE))[1:6]
  dt[, dep_grp := fifelse(department %in% top_dep, as.character(department), "Other")]
  dt
}
ins_c <- mk_groups(ins_c); mov_c <- mk_groups(mov_c)

# sanity: MOVER urgency is unrecorded (all emop==0); MOVER gynecology is small-event
mov_c[, .(n = .N, events = sum(y)), by = emop_grp]
mov_c[dep_grp == "Gynecology", .(n = .N, events = sum(y))]

sub_auc <- function(dt, cohort_nm, drop_dims = character()) {
  dims <- list(Age = "age_grp", Sex = "sex_grp", ASA = "asa_grp",
               Urgency = "emop_grp", Department = "dep_grp")
  dims <- dims[!names(dims) %in% drop_dims]
  rbindlist(lapply(names(dims), function(dnm) {
    v <- dims[[dnm]]
    rbindlist(lapply(unique(dt[[v]]), function(lv) {
      d <- dt[get(v) == lv & !is.na(xgb_need)]
      n <- nrow(d); ev <- sum(d$y)
      if (n < 500 || ev < 20 || (n - ev) < 20)
        return(data.table(cohort = cohort_nm, dimension = dnm, level = lv, n = n, events = ev,
                          need_pct = NA_real_, auroc = NA_real_, lo = NA_real_, hi = NA_real_,
                          excluded = TRUE))
      roc_o <- pROC::roc(d$y, d$xgb_need, quiet = TRUE, direction = "<")
      ci <- as.numeric(pROC::ci.auc(roc_o, method = "delong"))
      data.table(cohort = cohort_nm, dimension = dnm, level = lv, n = n, events = ev,
                 need_pct = round(100*ev/n, 2), auroc = round(ci[2], 3),
                 lo = round(ci[1], 3), hi = round(ci[3], 3), excluded = FALSE)
    }))
  }))
}
b4_all <- rbind(sub_auc(ins_c, "INSPIRE"), sub_auc(mov_c, "MOVER", drop_dims = "Urgency"))
cat("\nStrata excluded by the precision gate (n>=500, events>=20, non-events>=20):\n")
print(b4_all[excluded == TRUE, .(cohort, dimension, level, n, events)])
b4_tab <- b4_all[excluded == FALSE]
setorder(b4_tab, cohort, dimension, -n)
fwrite(b4_tab[, .(cohort, dimension, level, n, need_pct, auroc, lo, hi)],
       file.path(OUT, "tables", "table_b4_subgroup_transportability.csv"))
cat("\nkept rows:", nrow(b4_tab), "\n")

# ---- overall need-AUROC reference + forest-style subgroup figure ------------
overall <- rbindlist(list(
  {r <- pROC::roc(ins_c$y, ins_c$xgb_need, quiet=TRUE); ci <- as.numeric(pROC::ci.auc(r))
   data.table(cohort="INSPIRE", auroc=ci[2], lo=ci[1], hi=ci[3])},
  {r <- pROC::roc(mov_c$y, mov_c$xgb_need, quiet=TRUE); ci <- as.numeric(pROC::ci.auc(r))
   data.table(cohort="MOVER", auroc=ci[2], lo=ci[1], hi=ci[3])}
))
print(overall)

b4_tab[, level := factor(level, levels = unique(level[order(dimension, n)]))]
g4 <- ggplot(b4_tab, aes(auroc, level, colour = cohort)) +
  geom_vline(data = overall, aes(xintercept = auroc, colour = cohort),
             linetype = 2, linewidth = 0.5, alpha = 0.7) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.25,
                 position = position_dodge(width = 0.6), linewidth = 0.6) +
  geom_point(aes(size = n), position = position_dodge(width = 0.6)) +
  facet_grid(dimension ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c("#0072B2", "#D55E00")) +
  scale_size_continuous(range = c(1.5, 3.5), guide = "none") +
  scale_x_continuous(limits = c(0.68, 0.96), breaks = seq(0.7, 0.95, 0.05)) +
  labs(x = "AUROC for true critical care need (need-recalibrated XGB)", y = NULL, colour = NULL) +
  theme_bw(base_size = 11) +
  theme(text = element_text(family = "Liberation Sans"),
        legend.position = "top", panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", colour = NA),
        strip.text.y = element_text(angle = 0, hjust = 0))

svglite(file.path(OUT, "figures", "fig_b4_subgroup_transportability.svg"), width = 7.5, height = 7)
print(g4); dev.off()
ggsave(file.path(OUT, "figures", "fig_b4_subgroup_transportability.png"), g4, width = 7.5, height = 7, dpi = 300)
cat("DONE b4\n")
