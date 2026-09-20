#!/usr/bin/env python3
"""Build manuscript_v5.md from v4 by targeted insertion of A1 text + B1-B4 results.
Every edit is an exact-anchor string replacement; the script fails loudly if an
anchor is missing, so a silent no-op edit is impossible."""
import re, sys

SRC = "/mnt/results/04_manuscript/manuscript_v4.md"
DST = "/mnt/results/04_manuscript/manuscript_v5.md"

txt = open(SRC, encoding="utf-8").read()
edits = []

def rep(anchor, new, note):
    if anchor not in txt:
        sys.exit(f"ANCHOR NOT FOUND [{note}]: {anchor[:90]}...")
    edits.append((anchor, new, note))

# ---------------- 1. Title ----------------
rep("# Development, external validation, and ICU-course–verified triage evaluation of an intraoperative machine learning model for postoperative critical care\n",
    "# Development, dual external validation, and ICU-course–verified triage evaluation of an intraoperative machine learning model for postoperative critical care need\n",
    "title")

# ---------------- 2. Abstract sentence ----------------
rep("A need-recalibrated three-tier triage pathway showed consistent low-tier safety across cohorts; the low-risk tier covered 74.5% of INSPIRE operations with 0.08% mortality.",
    "At a fixed 96%-sensitivity operating point for true need, the pathway triaged 57.8% (INSPIRE) and 28.0% (MOVER) of operations to ward-level care (negative predictive value 99.7% and 98.3%), concentrated 94–97% of missed escalations in the review band, and yielded positive net benefit across the 5–30% decision-threshold range in both cohorts.",
    "abstract")

# ---------------- 3. Introduction aims paragraph (add item v) ----------------
old_intro = ("We therefore set out to (i) develop an ML model that predicts a composite of postoperative ICU admission or in-hospital death from routinely collected perioperative and intraoperative monitoring data in adults undergoing non-cardiac surgery under general anesthesia, (ii) validate the frozen model in two independent cohorts — a same-institution cohort spanning a different, earlier decade (temporal validation) and a cohort from a different country, continent, and health record system (international external validation) — quantifying both discrimination transport and calibration drift, (iii) test, head to head, whether a gated recurrent unit (GRU) network trained on raw minute-resolution sequences beats the same gradient-boosting model trained on engineered features, and (iv) using ICU-course data, separate true critical care need from admission behavior and derive a risk-stratified triage pathway aimed at the clinically uncertain middle. The study is reported according to the TRIPOD+AI 2024 statement [8].")
new_intro = ("We therefore set out to (i) develop an ML model that predicts a composite of postoperative ICU admission or in-hospital death from routinely collected perioperative and intraoperative monitoring data in adults undergoing non-cardiac surgery under general anesthesia; (ii) validate the frozen model in two independent cohorts — a same-institution cohort spanning a different, earlier decade (temporal validation) and a cohort from a different country, continent, and health record system (international external validation) — quantifying both discrimination transport and calibration drift; (iii) test, head to head, whether a gated recurrent unit (GRU) network trained on raw minute-resolution sequences beats the same gradient-boosting model trained on engineered features; (iv) using ICU-course data, separate true critical care need from admission behavior and derive a risk-stratified triage pathway aimed at the clinically uncertain middle; and (v) under a fixed safety constraint (≥96% sensitivity for true need, mirroring the operating point of validated perioperative decision-support systems [26]), quantify the pathway's decision impact — the proportion of operations safely triaged away from routine intensive care, the senior-review workload generated, and the pool of observational ICU bed-days subject to review. The study is reported according to the TRIPOD+AI 2024 statement [8].")
rep(old_intro, new_intro, "intro aims")

# ---------------- 4. Results: temporal validation decomposition sentence ----------------
rep("this drift was nonetheless far smaller than in the international cohort.",
    "this drift was nonetheless far smaller than in the international cohort. Decomposing the calibration intercept (Methods) attributed 90.5% of the shift (−0.733 of −0.809 log-odds) to the lower composite prevalence, with case mix as scored by the model nearly identical (+0.044) and only a small residual (−0.121; Supplementary Table S13, Supplementary Fig. S4).",
    "results temporal decomposition")

# ---------------- 5. Results: external validation decomposition sentence ----------------
rep("indicating that institutional differences in arterial-line practice are a measurable contributor to miscalibration in transport.",
    "indicating that institutional differences in arterial-line practice are a measurable contributor to miscalibration in transport. Intercept decomposition told the same story from the outcome side: only half of the +2.352 intercept shift reflected the higher composite prevalence (+1.178); the remainder was a large residual (+1.365) together with a shallow calibration slope (0.717), consistent with differences in admission practice — 32.7% of MOVER operations were observational admissions versus 6.2% in INSPIRE (Supplementary Table S13, Supplementary Fig. S4).",
    "results external decomposition")

# ---------------- 6. Results: triage pathway — B1 operating point + B3 DCA paragraph ----------------
b1_para = (
    " To quantify decision impact under a fixed safety constraint, we set the low-tier threshold at the highest need-recalibrated risk achieving at least 96% sensitivity for true need in each cohort (2.0% in INSPIRE — the stricter low-tier threshold examined above — and 4.6% in MOVER; Table 5, Fig. 7) [26]. "
    "At this operating point the low tier covered 57.8% of INSPIRE operations (55,595; negative predictive value 99.66%; 12 deaths, 0.22 per 1,000 operations) and 28.0% of MOVER operations (negative predictive value 98.27%; 3 deaths), while 94.2% (INSPIRE) and 97.2% (MOVER) of missed escalations and 98.3% and 99.6% of deaths occurred above the low-tier threshold. "
    "The intermediate band defined the senior-review workload: 388 operations per 1,000 in INSPIRE and 624 per 1,000 in MOVER. "
    "Directly avoidable ICU bed-days were modest in INSPIRE (3.1 bed-days per 1,000 operations, 1.4% of the cohort total), because current practice already admits selectively; the larger opportunity was the pool of observational stays flagged for review — 57 observational bed-days per 1,000 operations in the INSPIRE intermediate band, and 238 observational admissions per 1,000 operations in MOVER, where ICU length-of-stay data were unavailable. "
    "At an equal 96% safety level on the SASA-evaluable subset, the SASA triaged less than half as many operations to the low tier as the model (INSPIRE 20.2% vs 44.0%, negative predictive value 98.54% vs 99.51%; MOVER 10.9% vs 27.7%, 96.26% vs 98.42%; Supplementary Table S15): the model's advantage is efficiency at fixed safety, not safety itself. "
    "Decision-curve analysis of the need-recalibrated risk showed greater net benefit than treat-all or treat-none strategies across threshold probabilities of approximately 4–50% in both cohorts (Fig. 6); at a 5% threshold the model yielded a net benefit of 32.1 (INSPIRE) and 86.5 (MOVER) true-need equivalents per 1,000 operations (Supplementary Table S12).")
rep("so the full model dominates the simple score as a de-escalation aid on either denominator (Supplementary Table S11).",
    "so the full model dominates the simple score as a de-escalation aid on either denominator (Supplementary Table S11)." + b1_para,
    "results triage B1+B3")

# ---------------- 7. Results: subgroup transportability (B4) ----------------
b4_para = (
    "\n\nDiscrimination for true critical care need showed the same graded pattern across pre-specified subgroups of both validation cohorts (Supplementary Fig. S5, Supplementary Table S14): AUROCs ranged from 0.79 (thoracic surgery) to 0.92 (gynecology) in INSPIRE and from 0.72 (ASA 3–5) to 0.80 (age <65 years) in MOVER, with the weakest transport in higher-acuity strata (ASA 3–5: 0.791, 95% CI 0.779–0.804 in INSPIRE; 0.722, 0.714–0.729 in MOVER) — the subgroups in which triage errors carry the greatest consequence and where local monitoring should be concentrated after deployment. MOVER records only elective operations and contains no gynecological surgery, so the urgency and gynecology strata were evaluable in INSPIRE only.")
rep("than in cases without an arterial line (0.728).",
    "than in cases without an arterial line (0.728)." + b4_para,
    "results subgroup B4")

# ---------------- 8. Discussion: fourth finding in opening paragraph ----------------
rep("and the intermediate band captured over half of all missed escalations and deaths. A GRU network",
    "and the intermediate band captured over half of all missed escalations and deaths. At a safety-first operating point (≥96% sensitivity for true need), the pathway triaged 57.8% of INSPIRE and 28.0% of MOVER operations to ward-level care with negative predictive values of 99.7% and 98.3% — more than twice the low-tier coverage of the three-variable SASA at equal safety. A GRU network",
    "discussion opening")

# ---------------- 9. Discussion: calibration-drift decomposition paragraph (B2) ----------------
b2_disc = (
    "\n\nDecomposing the external calibration intercept clarified *what* drifts and *what to do about it*. In the temporal cohort, 90.5% of the intercept shift (−0.733 of −0.809 log-odds) was attributable to the lower composite prevalence, case mix as scored by the model was nearly identical (+0.044), and the residual was small (−0.121) — explaining why intercept-only recalibration restored calibration (slope 0.999 after recalibration). In the geographic cohort, only half of the +2.35 intercept reflected prevalence; the remainder was a large residual (+1.37) together with a calibration slope of 0.72, consistent with practice-pattern differences in observational admission (32.7% of MOVER operations versus 6.2% at INSPIRE). The practical guidance follows: temporal deployments need intercept-only updates, whereas geographic transport requires full logistic recalibration [27, 28] — or, preferably, redefinition of the target from admission to verified need, which is the framing used throughout our triage evaluation.")
rep("discrimination, the harder property to fix, is the one that transfers.",
    "discrimination, the harder property to fix, is the one that transfers." + b2_disc,
    "discussion calibration B2")

# ---------------- 10. Discussion: nested-gradient paragraph consistency tweak ----------------
rep("with moderate calibration drift (intercept −0.81, slope 0.88) attributable largely to the lower event prevalence (10.8% vs 19.8%)",
    "with moderate calibration drift (intercept −0.81, slope 0.88) attributable largely — 90.5% of the intercept shift, as decomposed above — to the lower event prevalence (10.8% vs 19.8%)",
    "discussion nested-gradient tweak")

# ---------------- 11. Discussion: Monday-morning paragraph after triage paragraph ----------------
monday = (
    "\n\nWhat would this pathway change in practice? At the safety-first operating point, 57.8% of INSPIRE operations and 28.0% of MOVER operations were triaged to the low tier, with negative predictive values of 99.7% and 98.3% and low-tier mortality of 0.2 per 1,000 operations in both cohorts. The intermediate band — 388 operations per 1,000 at INSPIRE and 624 per 1,000 at MOVER — defines the senior-review workload, and it is where the pathway concentrates risk: 94–97% of missed escalations and 98–100% of deaths occurred above the low-tier threshold. Decision-curve analysis showed greater net benefit than treat-all or treat-none strategies across the full 5–30% threshold range in both cohorts. Two honest caveats temper the resource claim. First, directly avoidable bed-days were modest at our institution (3.1 bed-days per 1,000 operations, 1.4% of ICU bed-days), because current practice already admits selectively; the larger opportunity is the pool of observational bed-days that the pathway flags for senior review — 57 bed-days per 1,000 operations at INSPIRE, and 238 observational admissions per 1,000 operations at MOVER, where observational admission is far more liberal. Second, at an equal 96% safety level, the three-variable Surgical Apgar Score would triage less than half as many operations to the low tier (20.2% versus 44.0% on the same evaluable subset), so the model's advantage is efficiency at fixed safety, not safety itself. The model is therefore best deployed as an attention-allocation device — concentrating senior review and monitored beds on the uncertain middle — rather than as an admission oracle.")
rep("Local recalibration is therefore not an optional refinement but an integral deployment step.",
    "Local recalibration is therefore not an optional refinement but an integral deployment step." + monday,
    "discussion monday-morning")

# ---------------- 12. Methods: triage pathway evaluation additions ----------------
methods_triage = (
    " To quantify decision impact under a fixed safety constraint, we additionally defined a safety-first operating point as the highest need-recalibrated risk threshold achieving at least 96% sensitivity for any true need within each cohort, mirroring the operating point of prospectively validated perioperative decision-support systems [26]. "
    "At this operating point we report the low-tier coverage and negative predictive value, the intermediate-band size (the senior-review workload), and the proportion of cohort missed escalations and deaths occurring above the low-tier threshold. "
    "Resource use was quantified per 1,000 operations as directly avoidable ICU bed-days (bed-days of low-tier observational admissions under current practice; INSPIRE only, because MOVER lacks ICU length-of-stay data) and the review pool (intermediate-band observational admissions and, where available, their bed-days). "
    "As a low-dimensional benchmark, the recalibrated SASA was evaluated at its own 96%-sensitivity threshold and compared with the model on the same SASA-evaluable subset. "
    "Decision-curve analysis of the need-recalibrated risk was performed in both validation cohorts across threshold probabilities of 0.01–0.50 [21].")
rep("Robustness analyses replaced the 5%/30% bands with cohort-specific 10th–90th percentile bands and repeated the evaluation after excluding routine-ICU procedures in MOVER.",
    "Robustness analyses replaced the 5%/30% bands with cohort-specific 10th–90th percentile bands and repeated the evaluation after excluding routine-ICU procedures in MOVER." + methods_triage,
    "methods triage")

# ---------------- 13. Methods: statistical analysis additions ----------------
methods_stats = (
    " To characterize calibration drift, the external calibration intercept was decomposed exactly into three additive components on the log-odds scale, taking the VitalDB internal test set as the development reference: a prevalence component (logit of cohort outcome prevalence minus logit of development prevalence), a case-mix component (development minus cohort mean predicted log-odds, i.e., case mix as scored by the frozen model), and a residual capturing distributional and measurement differences; the three components sum to the intercept-only recalibration maximum-likelihood estimate by construction. "
    "Subgroup transportability was assessed as the AUROC for any true need within pre-specified strata (ASA physical status, age, sex, surgical department, and urgency where recorded), with DeLong 95% confidence intervals.")
rep("Net benefit was assessed by decision-curve analysis.",
    "Net benefit was assessed by decision-curve analysis." + methods_stats,
    "methods stats")

# ---------------- 14. Figure legends: add Fig 6 + Fig 7 after Fig 5 legend ----------------
fig67 = (
    "\n\n**Figure 6. Decision-curve analysis of the need-recalibrated model in the validation cohorts.** Net benefit of the need-recalibrated XGBoost model (target: any true critical care need) versus treat-all and treat-none strategies across threshold probabilities in INSPIRE (temporal validation, left) and MOVER (geographic validation, right). Dotted vertical lines mark the 5% and 30% band boundaries of the three-tier pathway. The model dominates both default strategies across threshold probabilities of approximately 4–50% in both cohorts; y-axes are free-scaled because true-need prevalence differs (5.3% in INSPIRE, 12.2% in MOVER).\n"
    "\n**Figure 7. Safety–efficiency frontier of the triage pathway.** Proportion of operations triaged to the low tier (ward-level care) as a function of the sensitivity for true critical care need achieved by the middle plus high tiers, for the need-recalibrated model (all operations) and the recalibrated SASA (SASA-evaluable subset), in INSPIRE (left) and MOVER (right). Dotted lines mark the safety-first operating point (96% sensitivity): the model triages 58% (INSPIRE) and 28% (MOVER) of operations to the low tier, versus 20% and 11% for the SASA at equal safety.")
rep("Median predicted risk is similar across the three event categories within each cohort and far above that of ward courses.",
    "Median predicted risk is similar across the three event categories within each cohort and far above that of ward courses." + fig67,
    "figure legends 6-7")

# ---------------- 15. Supplementary figure legends S4 + S5 ----------------
s45 = (
    "\n\n**Supplementary Figure S4. Exact decomposition of the external calibration intercept.** Additive decomposition of the calibration intercept into prevalence, case-mix, and residual components (log-odds scale; Methods), with the VitalDB internal test set as the development reference. In INSPIRE the intercept shift (−0.809) is dominated by the prevalence component (−0.733); in MOVER the shift (+2.352) splits between prevalence (+1.178) and a large residual (+1.365). y-axes are free-scaled.\n"
    "\n**Supplementary Figure S5. Subgroup transportability of need discrimination.** AUROC with DeLong 95% confidence intervals of the XGBoost model for any true critical care need across pre-specified subgroups in INSPIRE and MOVER. MOVER records only elective operations and contains no gynecological surgery, so urgency and gynecology strata are shown for INSPIRE only.")
rep("MOVER panels computed before de-duplication.\n\n## Tables",
    "MOVER panels computed before de-duplication." + s45 + "\n\n## Tables",
    "supp figures S4-S5")

# ---------------- 16. Table 5 ----------------
table5 = (
    "\n\n**Table 5. Decision-impact simulation at the safety-first operating point (≥96% sensitivity for true need).** The low-tier threshold is the highest need-recalibrated risk achieving ≥96% sensitivity for any true critical care need within each cohort (INSPIRE 2.0%; MOVER 4.6%). NPV, negative predictive value for true need in the low tier. Capture = proportion of all cohort missed escalations or deaths occurring above the low-tier threshold. Directly avoidable bed-days = ICU bed-days of low-tier observational admissions under current practice (INSPIRE only; MOVER lacks ICU length-of-stay data). Review pool = intermediate-band observational admissions (and bed-days where available). The SASA comparator is evaluated at its own 96%-sensitivity threshold on the SASA-evaluable subset (59.7% of INSPIRE and 61.2% of MOVER operations), with model metrics on the same subset for paired comparison. [Full tables: table_b1_impact_simulation.csv, table_b1_sasa_comparator.csv]")
rep("[Full table: table4_triage_pathway.csv]",
    "[Full table: table4_triage_pathway.csv]" + table5,
    "table 5")

# ---------------- 17. Supplementary material list additions ----------------
rep("- Supplementary Fig. S3. Decision-curve analysis of the arterial-line ablation (fig_s3_dca_noart.svg; MOVER panels computed before de-duplication).",
    "- Supplementary Fig. S3. Decision-curve analysis of the arterial-line ablation (fig_s3_dca_noart.svg; MOVER panels computed before de-duplication).\n- Supplementary Fig. S4. Exact decomposition of the external calibration intercept into prevalence, case-mix, and residual components (fig_b2_calibration_decomposition.svg).\n- Supplementary Fig. S5. Subgroup transportability of need discrimination (fig_b4_subgroup_transportability.svg).",
    "supp list figures")
rep("- TRIPOD+AI checklist (tripod_ai_checklist_v4).",
    "- Supplementary Table S12. Decision-curve net benefit of the need-recalibrated model per 1,000 operations at threshold probabilities of 2–30% (table_b3_dca_net_benefit.csv).\n- Supplementary Table S13. Exact decomposition of the external calibration intercept (table_b2_calibration_decomposition.csv).\n- Supplementary Table S14. Subgroup transportability: AUROC (DeLong 95% CI) for any true need across pre-specified subgroups in both validation cohorts (table_b4_subgroup_transportability.csv).\n- Supplementary Table S15. SASA comparator at an equal 96% safety level: low-tier coverage and negative predictive value of the recalibrated SASA versus the model on the SASA-evaluable subset (table_b1_sasa_comparator.csv).\n- TRIPOD+AI checklist (tripod_ai_checklist_v5).",
    "supp list tables + tripod")

# ---------------- 18. New references 26-28 ----------------
rep("25. Gopalan PD, Pershad S. Decision-making in ICU — a systematic review of factors considered important by ICU clinician decision makers with regard to ICU triage decisions. *J Crit Care* 2019; **50**: 99–110.",
    "25. Gopalan PD, Pershad S. Decision-making in ICU — a systematic review of factors considered important by ICU clinician decision makers with regard to ICU triage decisions. *J Crit Care* 2019; **50**: 99–110.\n26. Lou SS, Kumar S, Goss CW, et al. Multicenter validation of a machine learning model for surgical transfusion risk at 45 US hospitals. *JAMA Netw Open* 2025; **8**: e2517760.\n27. Cox EGM, Wiersema R, Eck RJ, et al. External validation of mortality prediction models for critical illness reveals preserved discrimination but poor calibration. *Crit Care Med* 2023; **51**: 80–90.\n28. de Hond AAH, Kant IMJ, Fornasa M, et al. Predicting readmission or death after discharge from the ICU: external validation and retraining of a machine learning model. *Crit Care Med* 2023; **51**: 291–300.",
    "references 26-28")

# ---------------- apply ----------------
for anchor, new, note in edits:
    assert txt.count(anchor) == 1, f"anchor not unique [{note}]: {txt.count(anchor)} occurrences"
    txt = txt.replace(anchor, new, 1)

open(DST, "w", encoding="utf-8").write(txt)

# ---------------- verification ----------------
abs_m = re.search(r"## Abstract\n\n(.+?)\n\n---", txt, re.S)
wc = len(abs_m.group(1).split())
print(f"edits applied: {len(edits)}")
print(f"abstract words: {wc} (limit 150)")
print(f"total words: {len(txt.split())}")
refs = re.findall(r"\[(\d+(?:[–, \d]+)?)\]", txt)
print("v5 written:", DST)
