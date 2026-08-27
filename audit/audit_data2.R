#!/usr/bin/env Rscript
# Audit part 2: triage pathway (Table 4), impact simulation (Table 5), middle band (S11/b3), SASA comparator.
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
auroc <- function(y,p) as.numeric(pROC::auc(pROC::roc(y,p,quiet=TRUE)))

THR <- c(INSPIRE=0.02, MOVER=0.0455)
for (cf in c("inspire","mover")) {
  dt <- fread(file.path(RD,"06_icu_course",paste0("preds_need_recal_",cf,".csv")))
  CO <- toupper(cf); N <- nrow(dt); thr <- THR[[CO]]
  dt[, icu_dep := as.integer(category=="icu_dependent")]
  dt[, obs     := as.integer(category=="observational_icu")]
  dt[, missed  := as.integer(category=="missed_escalation")]
  ## ---- Table 4: standard bands on xgb_need ----
  band <- cut(dt$xgb_need, breaks=c(-Inf,0.05,0.30,Inf), labels=c("low","middle","high"), right=FALSE)
  # right=FALSE => [0,0.05),[0.05,0.30),[0.30,Inf). Check against tier column counts.
  for (bk in c("low","middle","high")) {
    idx <- band==bk; n_b <- sum(idx)
    # claimed values
    if (CO=="INSPIRE" && bk=="low")    cl<-list(n=71711,pct=74.5,need=1.24,miss=0.30,died=0.08,npv=98.76)
    if (CO=="INSPIRE" && bk=="middle") cl<-list(n=21237,pct=22.1,need=13.76,miss=2.84,died=1.73,npv=86.24)
    if (CO=="INSPIRE" && bk=="high")   cl<-list(n=3248, pct=3.4, need=38.12,miss=7.97,died=8.96,npv=61.88)
    if (CO=="MOVER" && bk=="low")      cl<-list(n=15463,pct=32.0,need=1.93,miss=0.01,died=0.04,npv=98.07)
    if (CO=="MOVER" && bk=="middle")   cl<-list(n=28251,pct=58.4,need=13.75,miss=0.05,died=1.15,npv=86.25)
    if (CO=="MOVER" && bk=="high")     cl<-list(n=4656, pct=9.6, need=37.26,miss=0.47,died=7.47,npv=62.74)
    add("Table4",paste0(bk," n"),CO,cl$n,n_b,0)
    add("Table4",paste0(bk," pct"),CO,cl$pct,fmt(100*n_b/N,1),0.05)
    add("Table4",paste0(bk," any_need%"),CO,cl$need,fmt(100*mean(dt$any_need[idx]),2),0.01)
    add("Table4",paste0(bk," missed%"),CO,cl$miss,fmt(100*mean(dt$missed[idx]),2),0.01)
    add("Table4",paste0(bk," died%"),CO,cl$died,fmt(100*mean(dt$died[idx]),2),0.01)
    add("Table4",paste0(bk," NPV"),CO,cl$npv,fmt(100*(1-mean(dt$any_need[idx])),2),0.01)
  }
  ## ---- Table 5: safety-first operating point ----
  low <- dt$xgb_need <= thr
  mid <- dt$xgb_need > thr & dt$xgb_need <= 0.30
  hig <- dt$xgb_need > 0.30
  sens <- mean(dt$xgb_need[dt$any_need==1] > thr)
  if (CO=="INSPIRE"){ cl<-list(thr=2.0,sens=96.3,lowpct=57.8,lown=55595,npv=99.66,lowdeaths=12,esc=94.2,dth=98.3,review=388,high=34)
  } else { cl<-list(thr=4.6,sens=96.0,lowpct=28.0,lown=13534,npv=98.27,lowdeaths=3,esc=97.2,dth=99.6,review=624,high=96) }
  add("Table5","threshold %",CO,cl$thr,fmt(round(100*thr,2),2),0.06)
  add("Table5","sensitivity %",CO,cl$sens,fmt(100*sens,1),0.05)
  add("Table5","low coverage %",CO,cl$lowpct,fmt(100*mean(low),1),0.05)
  add("Table5","low n",CO,cl$lown,sum(low),0)
  add("Table5","low NPV %",CO,cl$npv,fmt(100*(1-mean(dt$any_need[low])),2),0.01)
  add("Table5","low deaths",CO,cl$lowdeaths,sum(dt$died[low]),0)
  add("Table5","low mortality per1000",CO,0.22,fmt(1000*sum(dt$died[low])/sum(low),2),0.01)
  add("Table5","missed esc captured %",CO,cl$esc,fmt(100*mean(dt$xgb_need[dt$missed==1]>thr),1),0.05)
  add("Table5","deaths captured %",CO,cl$dth,fmt(100*mean(dt$xgb_need[dt$died==1]>thr),1),0.05)
  add("Table5","review band per1000",CO,cl$review,round(1000*sum(mid)/N),1)
  add("Table5","high tier per1000",CO,cl$high,round(1000*sum(hig)/N),1)
  ## bed-days (INSPIRE only)
  if (CO=="INSPIRE"){
    bd <- dt$icu_los_h/24
    bd_total <- sum(bd[dt$admitted==1],na.rm=TRUE)   # total ICU bed-days = early-ICU admissions only
    add("Table5","total bed-days per1000",CO,220.7,fmt(1000*bd_total/N,1),0.1)
    freed <- sum(bd[low & dt$obs==1],na.rm=TRUE)
    add("Table5","reallocatable bed-days per1000",CO,3.1,fmt(1000*freed/N,1),0.1)
    add("Table5","reallocatable % of total",CO,1.4,fmt(100*freed/bd_total,1),0.1)
    add("Table5","review-band obs bed-days per1000",CO,57,fmt(1000*sum(bd[mid & dt$obs==1],na.rm=TRUE)/N,0),1)
    add("Table5","review-band obs admissions per1000",CO,46.9,fmt(1000*sum(mid & dt$obs==1)/N,1),0.1)
  } else {
    add("Table5","review-band obs admissions per1000",CO,238.1,fmt(1000*sum(mid & dt$obs==1)/N,1),0.1)
  }
  ## ---- Middle band (Table S11 / b3) ----
  mb <- band=="middle"
  adm <- dt$admitted==1
  if (CO=="INSPIRE"){ cl<-list(pct=22.1,adm=28.9,dep=37.8,miss=4.0,died=1.63,auc=0.599)
  } else { cl<-list(pct=58.4,adm=53.0,dep=25.9,miss=0.1,died=0.1,auc=0.671) }
  add("MidBand","% of cohort",CO,cl$pct,fmt(100*mean(mb),1),0.05)
  add("MidBand","admitted %",CO,cl$adm,fmt(100*mean(adm[mb]),1),0.05)
  add("MidBand","ICU-dep among admitted %",CO,cl$dep,fmt(100*mean(dt$icu_dep[mb & adm]),1),0.05)
  add("MidBand","missed among not-admitted %",CO,cl$miss,fmt(100*mean(dt$missed[mb & !adm]),1),0.05)
  add("MidBand","died among not-admitted %",CO,cl$died,fmt(100*mean(dt$died[mb & !adm]),2),0.01)
  add("MidBand","in-band any-need AUROC",CO,cl$auc,fmt(auroc(dt$any_need[mb],dt$xgb_need[mb]),3),0.0005)
  # capture of cohort missed/deaths by middle band
  if (CO=="INSPIRE"){
    add("MidBand","% cohort missed in band",CO,55.9,fmt(100*sum(dt$missed[mb])/sum(dt$missed),1),0.05)
    add("MidBand","% cohort deaths in band",CO,51.3,fmt(100*sum(dt$died[mb])/sum(dt$died),1),0.05)
    add("MidBand","missed in band n",CO,604,sum(dt$missed[mb]),0)
    add("MidBand","deaths in band n",CO,367,sum(dt$died[mb]),0)
    add("MidBand","cohort deaths total",CO,715,sum(dt$died),0)
  }
  ## ---- SASA comparator (table_b1_sasa_comparator) ----
  ev <- !is.na(dt$sasa_need)
  # SASA's own 96% threshold on evaluable subset
  sasa_needs <- sort(dt$sasa_need[ev & dt$any_need==1])
  sthr <- quantile(dt$sasa_need[ev & dt$any_need==1], 0.04, na.rm=TRUE, type=1)  # highest thr w/ >=96% sens
  # search grid for highest threshold achieving >=96% sensitivity
  grid <- sort(unique(dt$sasa_need[ev]))
  sens_at <- sapply(grid, function(t) mean(dt$sasa_need[ev & dt$any_need==1] > t))
  sthr <- max(grid[sens_at >= 0.96])
  slow <- ev & dt$sasa_need <= sthr
  if (CO=="INSPIRE"){ cl<-list(sasa_low=20.2,sasa_npv=98.54,model_low=44.0,model_npv=99.51)
  } else { cl<-list(sasa_low=10.9,sasa_npv=96.26,model_low=27.7,model_npv=98.42) }
  add("SASAcomp","SASA low-tier coverage %",CO,cl$sasa_low,fmt(100*mean(slow[ev]),1),0.05)
  add("SASAcomp","SASA low NPV %",CO,cl$sasa_npv,fmt(100*(1-mean(dt$any_need[slow])),2),0.01)
  # model on same evaluable subset at the COHORT operating threshold (not re-derived)
  mlow <- ev & dt$xgb_need <= thr
  add("SASAcomp","model low-tier coverage % (subset)",CO,cl$model_low,fmt(100*mean(mlow[ev]),1),0.05)
  add("SASAcomp","model low NPV % (subset)",CO,cl$model_npv,fmt(100*(1-mean(dt$any_need[mlow])),2),0.01)
}

cat("\n===== AUDIT PART 2 =====\n")
names(res)[4:6] <- c("claimed","recomputed","match")
cat("TOTAL:",nrow(res)," OK:",sum(res$match=="OK")," MISMATCH:",sum(res$match=="MISMATCH"),"\n\n")
print(res[,c("domain","metric","cohort","claimed","recomputed","match")], row.names=FALSE)
write.csv(res,"/workspace/audit_part2.csv",row.names=FALSE)
cat("\nSaved /workspace/audit_part2.csv\n")
