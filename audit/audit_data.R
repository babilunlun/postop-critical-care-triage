#!/usr/bin/env Rscript
# Comprehensive audit: recompute manuscript data-derived numbers from per-operation source data.
# Authoritative refined outcome = refined_categories_* (composite) and preds_need_recal_* (any_need).
suppressMessages({library(data.table); library(pROC); library(PRROC)})
options(scipen = 999)
RD <- "/mnt/results"

res <- data.frame(domain=character(), metric=character(), cohort=character(),
                  claimed=character(), recomputed=character(), match=character(), note=character(),
                  stringsAsFactors=FALSE)
add <- function(domain, metric, cohort, claimed, recomputed, tol=NA, note="") {
  # match by numeric tolerance if tol given, else string compare
  m <- "CHECK"
  if (!is.na(tol)) {
    cnum <- suppressWarnings(as.numeric(claimed)); rnum <- suppressWarnings(as.numeric(recomputed))
    m <- ifelse(!is.na(cnum) && !is.na(rnum) && abs(cnum-rnum) <= tol, "OK", "MISMATCH")
  } else {
    m <- ifelse(as.character(claimed)==as.character(recomputed), "OK", "MISMATCH")
  }
  res <<- rbind(res, data.frame(domain, metric, cohort,
                                as.character(claimed), as.character(recomputed), m, note,
                                stringsAsFactors=FALSE))
}
slogit <- function(p){p<-pmin(pmax(p,1e-9),1-1e-9); log(p/(1-p))}
cal_int <- function(y,p){ unname(coef(glm(y~offset(slogit(p)), family=binomial))[1]) }
cal_slp <- function(y,p){ unname(coef(glm(y~slogit(p), family=binomial))[2]) }
brier   <- function(y,p){ mean((p-y)^2) }
auprc   <- function(y,p){ PRROC::pr.curve(scores.class0=p[y==1], scores.class1=p[y==0], curve=FALSE)$auc.integral }
auroc   <- function(y,p){ as.numeric(pROC::auc(pROC::roc(y, p, quiet=TRUE))) }
auc_ci  <- function(y,p){ as.numeric(pROC::ci.auc(pROC::roc(y, p, quiet=TRUE))) }  # DeLong

fmt <- function(x, d=3) formatC(x, format="f", digits=d)

## ============================================================
## DOMAIN C/D: INSPIRE & MOVER headline composite metrics (refined outcome)
## ============================================================
for (cf in c("inspire","mover")) {
  dt <- fread(file.path(RD, "06_icu_course", paste0("refined_categories_", cf, ".csv")))
  y <- dt$outcome
  CO <- toupper(cf)
  cat(sprintf("\n===== %s: n=%d events=%d rate=%.4f =====\n", CO, nrow(dt), sum(y), mean(y)))
  # cohort size & event rate
  if (cf=="inspire"){ add("Cohort","operations",CO,"96196",nrow(dt),0); add("Cohort","events",CO,"10370",sum(y),0)
                      add("Cohort","event rate %",CO,"10.8",fmt(100*mean(y),1),0.05) }
  if (cf=="mover"){ add("Cohort","operations",CO,"48370",nrow(dt),0); add("Cohort","events",CO,"21740",sum(y),0)
                    add("Cohort","event rate %",CO,"44.9",fmt(100*mean(y),1),0.05) }
  # XGB_full headline
  ci <- auc_ci(y, dt$XGB_full)
  add("Headline","XGB AUROC",CO, ifelse(cf=="inspire","0.907","0.794"), fmt(ci[2],3), 0.0005)
  add("Headline","XGB AUROC CI lo",CO, ifelse(cf=="inspire","0.905","0.790"), fmt(ci[1],3), 0.0005)
  add("Headline","XGB AUROC CI hi",CO, ifelse(cf=="inspire","0.910","0.798"), fmt(ci[3],3), 0.0005)
  add("Headline","XGB AUPRC",CO, ifelse(cf=="inspire","0.552","0.761"), fmt(auprc(y,dt$XGB_full),3), 0.0005)
  add("Headline","XGB Brier (no recal)",CO, ifelse(cf=="inspire","0.075","0.282"), fmt(brier(y,dt$XGB_full),3), 0.0005)
  add("Headline","XGB cal intercept",CO, ifelse(cf=="inspire","-0.809","2.352"), fmt(cal_int(y,dt$XGB_full),3), 0.0005)
  add("Headline","XGB cal slope",CO, ifelse(cf=="inspire","0.875","0.717"), fmt(cal_slp(y,dt$XGB_full),3), 0.0005)
  # comparator AUROCs
  add("Headline","LR_full AUROC",CO, ifelse(cf=="inspire","0.915","0.759"), fmt(auroc(y,dt$LR_full),3), 0.0005)
  if ("XGB_noArt" %in% names(dt)) {
    add("Headline","XGB_noArt AUROC",CO, "0.898", fmt(auroc(y,dt$XGB_noArt),3), 0.0005)
  } else {
    mg <- fread(file.path(RD,"03_gru_comparator","metrics_gru.csv"))
    add("Headline","XGB_noArt AUROC",CO, "0.753", fmt(mg[model=="XGB_noArt_mover"]$AUROC,3), 0.0005,
        note="from metrics_gru.csv (no XGB_noArt col in refined_categories_mover)")
  }
  sasa_ok <- !is.na(dt$SASA)
  add("Headline","SASA AUROC",CO, ifelse(cf=="inspire","0.719","0.686"), fmt(auroc(y[sasa_ok],dt$SASA[sasa_ok]),3), 0.0005,
      note="SASA complete-case")
  # DeLong XGB vs LR_full
  rt <- pROC::roc.test(pROC::roc(y,dt$XGB_full,quiet=TRUE), pROC::roc(y,dt$LR_full,quiet=TRUE), method="delong", paired=TRUE)
  add("Headline","DeLong p XGB vs LR_full",CO, ifelse(cf=="inspire","3.3e-22","1e-148"), formatC(rt$p.value,format="e",digits=1), NA,
      note=ifelse(cf=="inspire","LR>XGB in INSPIRE","XGB>LR in MOVER"))
}

## ============================================================
## DOMAIN F: ICU-course decomposition (Table 3)
## ============================================================
for (cf in c("inspire","mover")) {
  dt <- fread(file.path(RD, "06_icu_course", paste0("refined_categories_", cf, ".csv")))
  CO <- toupper(cf); N <- nrow(dt); comp <- sum(dt$outcome)
  tab <- table(dt$category)
  getn <- function(x) ifelse(x %in% names(tab), as.numeric(tab[x]), 0)
  icu_dep <- getn("icu_dependent"); obs <- getn("observational_icu"); miss <- getn("missed_escalation"); ward <- getn("uncomplicated_ward")
  if (cf=="inspire"){
    add("Table3","icu_dependent n",CO,"3969",icu_dep,0); add("Table3","observational n",CO,"5958",obs,0)
    add("Table3","missed_escalation n",CO,"1080",miss,0); add("Table3","ward n",CO,"85189",ward,0)
    add("Table3","obs % of composite",CO,"57.5",fmt(100*obs/comp,1),0.05)
    add("Table3","icu_dep % of composite",CO,"38.3",fmt(100*icu_dep/comp,1),0.05)
    # median recal risk by category
    add("Table3","median recal risk obs",CO,"0.375",fmt(median(dt$XGB_full_platt_cv[dt$category=="observational_icu"]),3),0.0005)
    add("Table3","median recal risk icu_dep",CO,"0.340",fmt(median(dt$XGB_full_platt_cv[dt$category=="icu_dependent"]),3),0.0005)
    add("Table3","median recal risk missed",CO,"0.363",fmt(median(dt$XGB_full_platt_cv[dt$category=="missed_escalation"]),3),0.0005)
    add("Table3","median recal risk ward",CO,"0.015",fmt(median(dt$XGB_full_platt_cv[dt$category=="uncomplicated_ward"]),3),0.0005)
    # intervention rates among early ICU
    early <- dt[dt$early_icu==1,]
    add("Table3","vent among early ICU %",CO,"35",fmt(100*mean(early$icu_vent),0),1,note="~35%")
    add("Table3","vaso among early ICU %",CO,"5.0",fmt(100*mean(early$icu_vaso),1),0.1)
  }
  if (cf=="mover"){
    add("Table3","icu_dependent n",CO,"5883",icu_dep,0); add("Table3","observational n",CO,"15821",obs,0)
    add("Table3","missed_escalation n",CO,"36",miss,0); add("Table3","ward n",CO,"26630",ward,0)
    add("Table3","obs % of composite",CO,"72.8",fmt(100*obs/comp,1),0.05)
    add("Table3","icu_dep % of composite",CO,"27.1",fmt(100*icu_dep/comp,1),0.05)
    add("Table3","median recal risk icu_dep",CO,"0.718",fmt(median(dt$XGB_full_platt_cv[dt$category=="icu_dependent"]),3),0.0005)
    add("Table3","median recal risk obs",CO,"0.561",fmt(median(dt$XGB_full_platt_cv[dt$category=="observational_icu"]),3),0.0005)
    add("Table3","median recal risk ward",CO,"0.277",fmt(median(dt$XGB_full_platt_cv[dt$category=="uncomplicated_ward"]),3),0.0005)
    add("Table3","obs admissions % of ops",CO,"32.7",fmt(100*obs/N,1),0.05,note="text: 32.7% MOVER obs")
    add("Table3","vaso among ICU admissions %",CO,"23",fmt(100*mean(dt$postop_vaso[dt$icu_flag==1], na.rm=TRUE),0),1,note="~23% of ICU admissions")
  }
}

## ============================================================
## DOMAIN F2: Refined-outcome AUROCs (Table S7 / table_b2)
## ============================================================
for (cf in c("inspire","mover")) {
  dt <- fread(file.path(RD, "06_icu_course", paste0("refined_categories_", cf, ".csv")))
  CO <- toupper(cf)
  y_comp <- dt$outcome
  y_icudep <- as.integer(dt$category=="icu_dependent")
  y_anyneed <- as.integer(dt$category %in% c("icu_dependent","missed_escalation"))
  # obsICU vs ward
  idx_ow <- dt$category %in% c("observational_icu","uncomplicated_ward")
  y_ow <- as.integer(dt$category[idx_ow]=="observational_icu")
  # dep vs obs among admitted
  idx_adm <- dt$category %in% c("icu_dependent","observational_icu")
  y_adm <- as.integer(dt$category[idx_adm]=="icu_dependent")
  if (cf=="inspire"){
    add("TableS7","icu_dependent AUROC",CO,"0.877",fmt(auroc(y_icudep,dt$XGB_full),3),0.0005)
    add("TableS7","any_true_need AUROC",CO,"0.879",fmt(auroc(y_anyneed,dt$XGB_full),3),0.0005)
    add("TableS7","obsICU_vs_ward AUROC",CO,"0.909",fmt(auroc(y_ow,dt$XGB_full[idx_ow]),3),0.0005)
    add("TableS7","dep_vs_obs_admitted AUROC",CO,"0.502",fmt(auroc(y_adm,dt$XGB_full[idx_adm]),3),0.0005)
  } else {
    add("TableS7","icu_dependent AUROC",CO,"0.783",fmt(auroc(y_icudep,dt$XGB_full),3),0.0005)
    add("TableS7","any_true_need AUROC",CO,"0.783",fmt(auroc(y_anyneed,dt$XGB_full),3),0.0005)
    add("TableS7","obsICU_vs_ward AUROC",CO,"0.767",fmt(auroc(y_ow,dt$XGB_full[idx_ow]),3),0.0005)
    add("TableS7","dep_vs_obs_admitted AUROC",CO,"0.640",fmt(auroc(y_adm,dt$XGB_full[idx_adm]),3),0.0005)
  }
}

cat("\n===== INTERIM AUDIT (headline + decomposition + refined AUROC) =====\n")
print(res, row.names=FALSE)
write.csv(res, "/workspace/audit_part1.csv", row.names=FALSE)
cat("\nSaved /workspace/audit_part1.csv ; rows:", nrow(res), "\n")
