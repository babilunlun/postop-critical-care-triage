#!/usr/bin/env python3
"""Transform manuscript_v2.md -> manuscript_v3.md: add INSPIRE temporal validation.
Targeted string replacements with assertions (unchanged text preserved verbatim)."""

SRC = "/mnt/results/04_manuscript/manuscript_v2.md"
OUT = "/mnt/results/04_manuscript/manuscript_v3.md"

t = open(SRC, encoding="utf-8").read()

def sub(old, new, t=None):
    # returns replaced text; asserts exactly one occurrence
    global _t
    txt = _t if t is None else t
    assert txt.count(old) == 1, f"expected 1 occurrence, found {txt.count(old)}:\n{old[:120]}"
    return txt.replace(old, new)

_t = t

# ---------------- 1. Abstract ----------------
_t = sub(
"On temporal hold-out testing, discrimination was excellent (AUROC 0.932, 95% CI 0.918\u2013"
"0.945). In external validation on 49,394 operations from an American academic center (MOVER), "
"discrimination remained good (0.794, 0.791\u20130.798) but absolute risks were miscalibrated; "
"Platt recalibration restored calibration (Brier 0.281 to 0.183; calibration slope 1.00). "
"A gated recurrent unit network trained on raw minute-resolution sequences did not significantly "
"outperform the feature-based model (0.938; p = 0.27). Discrimination was stable when likely planned "
"intensive care admissions were excluded from the external cohort. Intraoperative machine-learning "
"risk rankings transfer internationally; absolute risk estimates require local calibration.",
"On temporal hold-out testing, discrimination was excellent (AUROC 0.932, 95% CI 0.918\u2013"
"0.945). We applied the frozen model to two independent cohorts: a same-institution earlier-decade "
"cohort (INSPIRE, 2011\u20132020; 96,196 operations; AUROC 0.852, 0.848\u2013"
"0.855) and a US academic center (MOVER, 49,394 operations; 0.794, 0.791\u20130.798). Calibration "
"drift was mild temporally but large internationally; Platt recalibration restored "
"calibration in both (slope 1.00). A gated recurrent unit network trained on raw sequences did not "
"significantly outperform the feature-based model (0.938; p = 0.27). Discrimination "
"degraded gracefully from internal to same-institution temporal to international validation; "
"absolute risk estimates require local calibration.")

# ---------------- 2. Introduction aims ----------------
_t = sub(
"(ii) externally validate the frozen model in an independent cohort from a different country, "
"continent, and health record system, quantifying both discrimination transport and calibration drift, and",
"(ii) validate the frozen model in two independent cohorts \u2014 a same-institution cohort spanning "
"a different, earlier decade (temporal validation) and a cohort from a different country, continent, "
"and health record system (international external validation) \u2014 quantifying both discrimination "
"transport and calibration drift, and")

# ---------------- 3. Cohort characteristics ----------------
_t = sub(
"The external validation cohort comprised 49,394 operations in 34,144 unique patients from MOVER "
"(University of California Irvine Medical Center, USA; November 2017\u2013August 2023), with 22,159 "
"events (44.9%). The two cohorts differed markedly (Table 1):",
"The temporal validation cohort comprised 96,196 operations in 78,305 patients from INSPIRE "
"(Seoul National University Hospital; January 2011\u2013December 2020, after excluding operations "
"linked to the development cohort), with 14,627 events (15.2%). The international external validation "
"cohort comprised 49,394 operations in 34,144 unique patients from MOVER (University of California "
"Irvine Medical Center, USA; November 2017\u2013August 2023), with 22,159 events (44.9%). VitalDB "
"and MOVER differed markedly (Table 1):")

# ---------------- 4. Results: new Temporal validation subsection ----------------
temporal_section = """### Temporal validation (INSPIRE)

We first applied the frozen models to INSPIRE, an independent cohort from the same institution as the development data (Seoul National University Hospital) but spanning an earlier and much longer period (2011\u20132020) with no calendar overlap with the development window. After excluding 2,437 operations (2.5%) whose VitalDB case linker identified them as belonging to the development cohort, the temporal validation cohort comprised 96,196 operations in 78,305 patients, with 14,627 events (15.2%). Applied without refitting, the XGBoost model achieved an AUROC of 0.852 (95% CI 0.848\u20130.855), an AUPRC of 0.565, and a Brier score of 0.096 (Table 2, Fig. 4a,b), with only mild calibration drift (intercept \u22120.138, slope 0.706) \u2014 far smaller than in the international cohort. Logistic regression on the full feature set performed comparably (0.862, 0.859\u20130.865), the model without the arterial-line flag retained an AUROC of 0.847 (0.844\u20130.851), and the SASA reference scored 0.704. Ten-fold cross-validated Platt recalibration restored calibration almost perfectly (slope 1.000, Brier 0.092), confirmed by 500-repetition split-sample validation (XGBoost Brier 0.092, 95% CI 0.091\u20130.094; slope 1.000, 0.969\u20131.029; Table 2). Discrimination was therefore intermediate between the internal test set (0.932) and the international external cohort (0.794), consistent with graceful degradation as the validation setting becomes progressively more distant from development.

Because INSPIRE contains no absolute dates, residual overlap with the development cohort could not be excluded by date; we therefore combined the VitalDB case linker with a physiological-trajectory screen (lag-aligned correlation of minute-level mean arterial pressure and heart-rate epochs against candidate VitalDB cases). None of 500 randomly sampled non-linked operations matched a VitalDB trajectory at the pre-calibrated threshold, and retaining all case-linked operations changed the XGBoost AUROC by only +0.001 (to 0.853). Under a conservative rule-of-three upper bound on the undetected-overlap rate (\u2248 0.6%, \u2248 599 operations), assigning perfect predictions to all such operations inflated the AUROC by only +0.0009, so residual leakage cannot account for the observed performance (Supplementary Table S7).

"""
_t = sub("### External validation\n", temporal_section + "### External validation\n")

# ---------------- 5. External validation opening (bridge) ----------------
_t = sub(
"Applied without refitting to MOVER, the XGBoost model retained good discrimination (AUROC 0.794, "
"95% CI 0.791\u20130.798; AUPRC 0.761)",
"Applied without refitting to the more distant international MOVER cohort, the XGBoost model retained "
"good discrimination (AUROC 0.794, 95% CI 0.791\u20130.798; AUPRC 0.761)")

# ---------------- 6. Discussion opening ----------------
_t = sub(
"and showed that its discrimination transports to an independent American cohort of nearly 50,000 "
"operations while its absolute risk estimates require local recalibration. A GRU network",
"and showed that its discrimination transports both to a large same-institution cohort spanning an "
"earlier decade (temporal validation) and to an independent American cohort of nearly 50,000 "
"operations (international validation), while its absolute risk estimates require local recalibration. "
"A GRU network")

# ---------------- 7. Discussion: new validation-gradient paragraph ----------------
grad_para = """

A distinctive feature of this study is the nested validation gradient. Discrimination declined smoothly from internal testing (AUROC 0.932) through same-institution temporal validation on INSPIRE (0.852) to cross-continent external validation on MOVER (0.794). The temporal step isolates the effect of time and case mix from that of institution, country, and health-record system: within the same hospital but a different, earlier decade, the model lost roughly 0.08 of AUROC and showed only mild calibration drift (intercept \u22120.14), whereas the international step lost a further 0.06 of AUROC and produced large calibration drift (intercept 2.35). This decomposition suggests that most of the transportability penalty for absolute risk arises from crossing institutions and outcome definitions rather than from the passage of time, and that a model can remain well calibrated across a decade of practice change within one institution while still requiring recalibration when moved abroad."""
_t = sub(
"discrimination, the harder property to fix, is the one that transfers.",
"discrimination, the harder property to fix, is the one that transfers." + grad_para)

# ---------------- 8. Limitations: INSPIRE-specific ----------------
_t = sub(
"Fifth, the temporal split within VitalDB is a chronological proxy because case dates are deidentified. "
"Finally, we report no prospective validation;",
"Fifth, the temporal split within VitalDB is a chronological proxy because case dates are deidentified. "
"Sixth, the temporal validation carries constraints specific to INSPIRE: it shares its institution with "
"the development cohort, so it probes transport across time and case mix but not across sites; the "
"VitalDB linker and trajectory screen bound but cannot entirely exclude residual overlap, although the "
"worst-case inflation was negligible (+0.0009 AUROC); ages are given in five-year bins and times as "
"minutes relative to admission, so age and temporal precision are coarser than in development; "
"intraoperative signals are released at five-minute resolution, which we expanded to a one-minute grid "
"before applying identical feature definitions, so fine-grained burden and variability features are "
"approximations; rocuronium dosing is not recorded and was imputed; preoperative comorbidities were "
"ascertained differently across cohorts (VitalDB: preanesthesia assessment; INSPIRE: diagnosis codes "
"plus preoperative antihypertensive/antidiabetic prescriptions; MOVER: ICD history), so comorbidity "
"prevalence is not strictly comparable; and the INSPIRE ICU flag captures admission within 24 hours of "
"surgery, a slightly narrower window than the development and MOVER definitions. Finally, we report no "
"prospective validation;")

# ---------------- 9. Methods: Data sources ----------------
_t = sub(
"We used two publicly available, deidentified perioperative datasets.",
"We used three publicly available, deidentified perioperative datasets.")
_t = sub(
"between August 2016 and June 2017 [9].",
"between August 2016 and June 2017 [9]. INSPIRE (a publicly available research dataset for "
"perioperative medicine) contains deidentified perioperative records for approximately 130,000 "
"operations at the same institution, Seoul National University Hospital, over 2011\u20132020, released "
"via PhysioNet [22]; we used version 1.4.2. INSPIRE records times as minutes relative to each patient's "
"first hospital admission and ages in five-year bins, and provides a case linker to the VitalDB Open Dataset.")
_t = sub(
"VitalDB was accessed via its public API; MOVER was downloaded under a signed data use agreement.",
"VitalDB was accessed via its public API; INSPIRE was downloaded from PhysioNet under the Korea "
"Credentialed Health Data License; MOVER was downloaded under a signed data use agreement.")

# ---------------- 10. Methods: Participants ----------------
_t = sub(
"Usable intraoperative flowsheet time series were available for 46,971 MOVER operations (95.1%); "
"for the remaining 2,423, time-series features were handled by the model's imputation pipeline.",
"From INSPIRE we applied the same clinical gates \u2014 age \u226518 years, general anesthesia, "
"non-cardiac surgery (excluding operations with recorded cardiopulmonary bypass or a primary "
"cardiac-surgery procedure code) and anesthesia duration \u226530 minutes \u2014 yielding 98,633 "
"operations; after removing 2,437 operations linked to the VitalDB development cohort (see Cohort "
"overlap), the temporal validation cohort comprised 96,196 operations in 78,305 patients. Usable "
"intraoperative flowsheet time series were available for 46,971 MOVER operations (95.1%); for the "
"remaining 2,423, time-series features were handled by the model's imputation pipeline.")

# ---------------- 11. Methods: Outcome ----------------
_t = sub(
"ICU admission after surgery (VitalDB: ICU stay \u22651 day; MOVER: ICU administration flag) or "
"in-hospital death (VitalDB: in-hospital death flag; MOVER: discharge disposition \"Expired\"). "
"The MOVER flag does not distinguish planned from unplanned ICU admission.",
"ICU admission after surgery (VitalDB: ICU stay \u22651 day; INSPIRE: ICU admission within 24 hours of "
"surgery end; MOVER: ICU administration flag) or in-hospital death (VitalDB and INSPIRE: in-hospital "
"death flag; MOVER: discharge disposition \"Expired\"). Neither the MOVER nor the INSPIRE flag "
"distinguishes planned from unplanned ICU admission.")

# ---------------- 12. Methods: Predictors (INSPIRE harmonization) ----------------
_t = sub(
"Units were harmonized across datasets (mg/\u00b5g/mL; height/weight converted from imperial units in "
"MOVER), and physiologically implausible values were set to missing.",
"Units were harmonized across datasets (mg/\u00b5g/mL; height/weight converted from imperial units in "
"MOVER), and physiologically implausible values were set to missing. For INSPIRE, the same 33 clinical "
"and 38 time-series predictors were reconstructed from its schema: preoperative hypertension and "
"diabetes were defined by a preoperative diagnosis code (ICD-10 I10\u2013I16 or E10\u2013E14) or a "
"preoperative antihypertensive or antidiabetic prescription, because diagnosis codes alone "
"substantially under-ascertained these conditions; intraoperative drug and fluid exposures were "
"computed from the medication table (bolus sums plus infusion integrals), with estimated blood loss "
"and crystalloid/colloid volumes summed and urine output taken as its maximum, and rocuronium \u2014 "
"not recorded in INSPIRE \u2014 imputed by the pipeline; and minute-resolution features were derived by "
"expanding the five-minute intraoperative series to a one-minute grid (values held constant within each "
"five-minute epoch) before applying the identical feature definitions.")

# ---------------- 13. Methods: new Cohort overlap subsection ----------------
overlap_section = """### Cohort overlap between VitalDB and INSPIRE

Because INSPIRE and VitalDB originate from the same institution and overlapping years, we removed operations that could correspond to development cases. INSPIRE provides a case linker to the VitalDB Open Dataset; operations whose linker fell within the development identifier range (1\u20136,388) were excluded (primary analysis: 2,437 operations). As a robustness check, a conservative cohort additionally excluded every operation with any non-missing linker value (17,686 operations). Because INSPIRE lacks absolute dates, we further screened for undetected overlap by matching each operation's minute-level mean arterial pressure and heart-rate trajectories \u2014 allowing a \u00b13-epoch lag \u2014 against candidate VitalDB cases of the same sex and department and similar age and duration; a decision threshold (correlation \u2265 0.90 and mean absolute difference \u2264 6 mmHg) was calibrated on true linker pairs (positive controls) versus best non-matching pairs (negative controls). None of 500 randomly sampled non-linked operations were flagged, bounding residual overlap below approximately 0.6% (rule of three). The worst-case impact of residual overlap on discrimination was quantified by adding synthetic perfectly-predicted operations at this upper bound and recomputing AUROC.

"""
_t = sub("### External validation and recalibration\n",
         overlap_section + "### External validation and recalibration\n")

# ---------------- 14. Methods: External validation applies to both ----------------
_t = sub(
"All models were frozen after development and applied to MOVER without refitting.",
"All models were frozen after development and applied to INSPIRE and MOVER without refitting.")

# ---------------- 15. Methods: Ethics ----------------
_t = sub(
"MOVER is released under a data use agreement with approval from the University of California Irvine "
"institutional review board.",
"INSPIRE was released under Seoul National University Hospital institutional review board approval "
"(IRB No. H-2210-078-1368) with a waiver of informed consent, and is distributed under the Korea "
"Credentialed Health Data License via PhysioNet. MOVER is released under a data use agreement with "
"approval from the University of California Irvine institutional review board.")

# ---------------- 16. Data availability ----------------
_t = sub(
"VitalDB is publicly available at https://vitaldb.net and MOVER at https://mover.ics.uci.edu under a "
"data use agreement.",
"VitalDB is publicly available at https://vitaldb.net, INSPIRE on PhysioNet at "
"https://physionet.org/content/inspire/ under the Korea Credentialed Health Data License, and MOVER at "
"https://mover.ics.uci.edu under a data use agreement.")

# ---------------- 17. References: add ref 22 ----------------
_t = sub(
"21. Sjoberg DD. dcurves: decision curve analysis for model evaluation. R package version 0.5.0. 2024. "
"https://cran.r-project.org/package=dcurves.",
"21. Sjoberg DD. dcurves: decision curve analysis for model evaluation. R package version 0.5.0. 2024. "
"https://cran.r-project.org/package=dcurves.\n"
"22. Lim L, Lee H, Jung C-W, Sim D, Borrat X, Pollard TJ, Celi LA, Mark RG, Vistisen ST, Lee H-C. "
"INSPIRE, a publicly available research dataset for perioperative medicine. *Sci Data* 2024; "
"**11**: 655.")

# ---------------- 18. Figure legends ----------------
_t = sub(
"**Figure 1. Cohort flow diagram.** Development cohort from VitalDB (Seoul National University "
"Hospital, Korea) with temporal split into training and internal test sets, and external validation "
"cohort from MOVER (UC Irvine Medical Center, USA). Event = composite of postoperative intensive care "
"unit admission or in-hospital death.",
"**Figure 1. Cohort flow diagram.** Development cohort from VitalDB (Seoul National University "
"Hospital, Korea; 2016\u20132017) with temporal split into training and internal test sets; "
"same-institution temporal validation cohort from INSPIRE (Seoul National University Hospital; "
"2011\u20132020), with operations linked to the development cohort excluded; and international external "
"validation cohort from MOVER (UC Irvine Medical Center, USA; 2017\u20132023). Event = composite of "
"postoperative intensive care unit admission or in-hospital death.")

_t = sub(
"**Figure 4. External validation on MOVER (n = 49,394).** a, Receiver operating characteristic curves "
"of the frozen models. b, Calibration of the XGBoost model before recalibration, after intercept-only "
"recalibration, and after Platt scaling.",
"**Figure 4. Temporal and external validation of the frozen models.** Top row, INSPIRE same-institution "
"temporal validation cohort (n = 96,196); bottom row, MOVER international external validation cohort "
"(n = 49,394). a,c, Receiver operating characteristic curves for SASA, logistic regression (clinical "
"and full feature sets), and XGBoost (full), with AUROC in parentheses. b,d, Calibration of the "
"XGBoost (full) and logistic regression (full) models before and after 10-fold cross-validated Platt "
"recalibration (points are 15 equal-count bins).")

# ---------------- 19. Table legends ----------------
_t = sub(
"V, VitalDB development cohort; M, MOVER external validation cohort. Emergency surgery was not recorded "
"in MOVER (\u2014). [Full table: table1_combined.csv]",
"V, VitalDB development cohort; I, INSPIRE temporal validation cohort; M, MOVER external validation "
"cohort. Emergency surgery was not recorded in MOVER (\u2014). [Full table: table1_combined_v3.csv]")

_t = sub(
"**Table 2. Model performance on internal and external validation.** AUROC with DeLong 95% CI; AUPRC, "
"area under the precision-recall curve; Cal, calibration. Recalibrated rows report split-sample-"
"validated estimates (500 repetitions of 50/50 splits); calibration intercept is 0 by design after "
"recalibration. [Full table: table2_performance.csv]",
"**Table 2. Model performance on internal, temporal (INSPIRE), and external (MOVER) validation.** "
"AUROC with DeLong 95% CI; AUPRC, area under the precision-recall curve; Cal, calibration. Recalibrated "
"rows report split-sample-validated estimates (500 repetitions of 50/50 splits); calibration intercept "
"is 0 by design after recalibration. [Full table: table2_performance_v3.csv]")

# ---------------- 20. Supplementary material list ----------------
_t = sub(
"- TRIPOD+AI checklist (tripod_ai_checklist_v2).",
"- Supplementary Table S7. INSPIRE overlap-sensitivity analysis and residual-overlap AUROC bound: "
"performance in the with-overlap and conservative cohorts and the worst-case inflation "
"(table_s_inspire_overlap_sensitivity.csv).\n"
"- TRIPOD+AI checklist (tripod_ai_checklist_v3).")

open(OUT, "w", encoding="utf-8").write(_t)
# word count of main body (rough: title..end of Discussion conclusion)
import re
body = _t.split("## Methods")[0]
words = len(re.findall(r"\S+", body))
print(f"wrote {OUT}")
print(f"main-text (title..Methods) word count: {words}")
abs_txt = _t.split("## Abstract")[1].split("---")[0]
abs_n = len(re.findall(r"\S+", abs_txt))
print("abstract words: " + str(abs_n))
