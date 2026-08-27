#!/usr/bin/env Rscript
# Audit part 3: split-sample recalibration (refined outcome), DCA net benefit, internal/GRU cross-check, Table 1.
suppressMessages({library(data.table); library(pROC)})
options(scipen=999)
RD <- "/mnt/results"
res <- data.frame(domain=character(), metric=character(), cohort=character(),
                  claimed=character(), recomputed=character(), match=character(), note=character(), stringsAsFactors=FALSE)
add <- function(domain, metric, cohort, claimed, recomputed, tol=NA, note="") {
  m <- "CHECK"
  if (!is.na(tol)) { cn<-suppressWarnings(as.numeric(claimed)); rn<-suppressWarnings(as.numeric(recomputed))
    m <- ifelse(!is.na(cn)&&!is.na(rn)&&abs(cn-rn)<=tol,"OK","MISMATCH")
  } else m <- ifelse(as.character(claimed)==as.character(recomputed),"OK","MISMATCH")
  res <<- rbind(res, setNames(data.frame(domain,metric,cohort,as.character(claimed),as.character(recomputed),m,note,stringsAsFactors=FALSE),names(res)))
}
fmt <- function(x,d=3) formatC(x,format="f",digits=d)
slogit <- function(p){p<-pmin(pmax(p,1e-9),1-1e-9); log(p/(1-p))}
brier <- function(y,p) mean((p-y)^2)

## ---- Split-sample recalibration (500x 50/50 Platt), refined outcome ----
set.seed(1)
for (cf in c("inspire","mover")) {
  dt <- fread(file.path(RD,"06_icu_course",paste0("refined_categories_",cf,".csv")))
  CO <- toupper(cf); y <- dt$outcome; lp <- slogit(dt$XGB_full); n <- nrow(dt)
  br <- numeric(500); sl <- numeric(500)
  for (i in 1:500) {
    idx <- sample.int(n, floor(n/2))
    fit <- glm(y ~ lp, data=data.frame(y=y[idx], lp=lp[idx]), family=binomial)
    pv <- as.numeric(predict(fit, newdata=data.frame(lp=lp[-idx]), type="response"))
    br[i] <- brier(y[-idx], pv)
    sl[i] <- coef(glm(y ~ sp, data=data.frame(y=y[-idx], sp=slogit(pv)), family=binomial))[2]
  }
  if (CO=="INSPIRE"){ cl<-list(b=0.066,blo=0.065,bhi=0.067,s=0.999,slo=0.968,shi=1.030)
  } else { cl<-list(b=0.183,blo=0.181,bhi=0.185,s=1.001,slo=0.962,shi=1.041) }
  add("SplitSample","Brier mean",CO,cl$b,fmt(mean(br),3),0.002)
  add("SplitSample","Brier lo",CO,cl$blo,fmt(quantile(br,0.025),3),0.002)
  add("SplitSample","Brier hi",CO,cl$bhi,fmt(quantile(br,0.975),3),0.002)
  add("SplitSample","slope mean",CO,cl$s,fmt(mean(sl),3),0.002)
  add("SplitSample","slope lo",CO,cl$slo,fmt(quantile(sl,0.025),3),0.01)
  add("SplitSample","slope hi",CO,cl$shi,fmt(quantile(sl,0.975),3),0.01)
}

## ---- DCA net benefit of need-recalibrated risk (per 1000) ----
for (cf in c("inspire","mover")) {
  dt <- fread(file.path(RD,"06_icu_course",paste0("preds_need_recal_",cf,".csv")))
  CO <- toupper(cf); N <- nrow(dt); y <- dt$any_need; p <- dt$xgb_need
  nb_at <- function(pt){ pos <- p>=pt; TP<-sum(pos&y==1); FP<-sum(pos&y==0); 1000*(TP/N - FP/N*pt/(1-pt)) }
  if (CO=="INSPIRE") cl<-32.1 else cl<-86.5
  add("DCA","net benefit @5% per1000",CO,cl,fmt(nb_at(0.05),1),0.2)
  # positive across 5-30%
  nbs <- sapply(seq(0.05,0.30,by=0.01), nb_at)
  add("DCA","min net benefit 5-30% (positive?) ",CO,">0",fmt(min(nbs),2),NA,note=sprintf("range %.1f..%.1f",min(nbs),max(nbs)))
}

## ---- Internal validation & GRU cross-check (authoritative metrics CSVs) ----
mt <- fread(file.path(RD,"01_internal_validation","metrics_test.csv"))
mg <- fread(file.path(RD,"03_gru_comparator","metrics_gru.csv"))
dl <- fread(file.path(RD,"03_gru_comparator","delong_vs_xgb.csv"))
g  <- function(df,mod,col) df[model==mod][[col]][1]
add("Internal","XGB AUROC","VitalDB","0.932",fmt(g(mt,"XGB_full","AUROC"),3),0.0005)
add("Internal","XGB AUPRC","VitalDB","0.799",fmt(g(mt,"XGB_full","AUPRC"),3),0.0005)
add("Internal","XGB Brier","VitalDB","0.078",fmt(g(mt,"XGB_full","Brier"),3),0.0005)
add("Internal","XGB intercept","VitalDB","0.119",fmt(g(mt,"XGB_full","cal_intercept"),3),0.0005)
add("Internal","XGB slope","VitalDB","0.917",fmt(g(mt,"XGB_full","cal_slope"),3),0.0005)
add("Internal","LR_full AUROC","VitalDB","0.915",fmt(g(mt,"LR_full","AUROC"),3),0.0005)
add("Internal","LR_clinical AUROC","VitalDB","0.898",fmt(g(mt,"LR_clinical","AUROC"),3),0.0005)
add("Internal","SASA AUROC","VitalDB","0.717",fmt(g(mt,"SASA","AUROC"),3),0.0005)
add("Internal","XGB_noArt AUROC","VitalDB","0.928",fmt(g(mg,"XGB_noArt","AUROC"),3),0.0005)
add("Internal","DeLong p XGB vs LR_full","VitalDB","1.0e-5",formatC(dl[comparator=="LR_full"]$delong_p_vs_XGB_full,format="e",digits=1),NA)
add("Internal","DeLong p XGB vs LR_clinical","VitalDB","3.3e-9",formatC(dl[comparator=="LR_clinical"]$delong_p_vs_XGB_full,format="e",digits=1),NA)
add("Internal","DeLong p XGB vs SASA","VitalDB","3.0e-35",formatC(dl[comparator=="SASA"]$delong_p_vs_XGB_full,format="e",digits=1),NA)
add("Internal","DeLong p XGB vs noArt","VitalDB","0.055",fmt(dl[comparator=="XGB_noArt"]$delong_p_vs_XGB_full,3),0.001)
# GRU
add("GRU","seq-only AUROC","VitalDB","0.875",fmt(g(mg,"gru_seq_ens","AUROC"),3),0.0005)
add("GRU","seq+static AUROC","VitalDB","0.938",fmt(g(mg,"gru_full_ens","AUROC"),3),0.0005)
add("GRU","AUROC diff (GRU_full - XGB)","VitalDB","0.006",fmt(g(mg,"gru_full_ens","AUROC")-g(mt,"XGB_full","AUROC"),3),0.0005)
add("GRU","DeLong p GRU_full vs XGB","VitalDB","0.267",fmt(dl[comparator=="GRU_full_ens"]$delong_p_vs_XGB_full,3),0.001)
add("GRU","DeLong p GRU_seq vs XGB","VitalDB","1.5e-8",formatC(dl[comparator=="GRU_seq_ens"]$delong_p_vs_XGB_full,format="e",digits=1),NA)
add("GRU","GRU_full Brier","VitalDB","0.072",fmt(g(mg,"gru_full_ens","Brier"),3),0.0005)
add("GRU","GRU_full slope","VitalDB","0.997",fmt(g(mg,"gru_full_ens","cal_slope"),3),0.0005)
add("GRU","GRU_full intercept","VitalDB","-0.240",fmt(g(mg,"gru_full_ens","cal_intercept"),3),0.0005)
# GRU DCA max net-benefit diff in 5-30%
b6 <- fread(file.path(RD,"07_revision_analyses","tables","table_b6_gru_dca_internal.csv"))
sub <- b6[pt>=0.05 & pt<=0.30]
add("GRU","max |GRU-XGB| NB per1000 (5-30%)","VitalDB","6.5",fmt(max(abs(sub$gru_minus_xgb_per1000)),1),0.1)

## ---- Table 1 internal consistency + key cells ----
t1 <- fread(file.path(RD,"04_manuscript","table1_combined_v4.csv"))
# cohort n row
nrow1 <- t1[Variable=="n"]
add("Table1","VitalDB n","VitalDB","5987",nrow1$V_Overall,0)
add("Table1","VitalDB events","VitalDB","1184",nrow1$V_Event,0)
add("Table1","INSPIRE n","INSPIRE","96196",nrow1$I_Overall,0)
add("Table1","INSPIRE events","INSPIRE","10370",nrow1$I_Event,0)
add("Table1","MOVER n","MOVER","48370",nrow1$M_Overall,0)
add("Table1","MOVER events","MOVER","21740",nrow1$M_Event,0)
# event rates
add("Table1","VitalDB event rate %","VitalDB","19.8",fmt(100*1184/5987,1),0.05)
add("Table1","INSPIRE event rate %","INSPIRE","10.8",fmt(100*10370/96196,1),0.05)
add("Table1","MOVER event rate %","MOVER","44.9",fmt(100*21740/48370,1),0.05)
# arterial line: text says MOVER 21.4%, Table1 says 21.5
art <- t1[Variable=="Invasive arterial monitoring"]
add("Table1","MOVER arterial-line % (Table1 cell)","MOVER","21.5", gsub(".*\\(|\\)","",art$M_Overall), NA, note="TEXT says 21.4% - check consistency")
add("Table1","VitalDB arterial-line % (Table1 cell)","VitalDB","58.2", gsub(".*\\(|\\)","",art$V_Overall), NA)
# ASA row sums (VitalDB): 1693+3508+621+37+0+12 = 5871 vs 5987 (ASA missing 1.9%)
asa_v <- c(1693,3508,621,37,0,12)
add("Table1","VitalDB ASA rows sum (miss 1.9%)","VitalDB", 5987, sum(asa_v), NA, note=sprintf("sum=%d (ASA missing ~1.9%%)",sum(asa_v)))

cat("\n===== AUDIT PART 3 =====\n")
cat("TOTAL:",nrow(res)," OK:",sum(res$match=="OK")," MISMATCH:",sum(res$match=="MISMATCH")," CHECK:",sum(res$match=="CHECK"),"\n\n")
print(res[,c("domain","metric","cohort","claimed","recomputed","match","note")], row.names=FALSE)
write.csv(res,"/workspace/audit_part3.csv",row.names=FALSE)
cat("\nSaved /workspace/audit_part3.csv\n")
