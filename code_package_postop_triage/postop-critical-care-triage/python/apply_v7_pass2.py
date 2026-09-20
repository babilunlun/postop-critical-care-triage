#!/usr/bin/env python3
"""Apply second-pass compression edits to manuscript_v7.md.
Each (old, new) pair must match exactly once. Aborts on any mismatch."""
import sys

PATH = '/workspace/manuscript_v7.md'
txt = open(PATH, encoding='utf-8').read()

EDITS = [
# ===== DISCUSSION =====
# ¶1 opening
("In this study we developed a gradient-boosting model on routinely collected perioperative and minute-resolution intraoperative monitoring data, validated it",
 "We developed a gradient-boosting model on routinely collected perioperative and minute-resolution intraoperative data, validated it"),
("Second, admission is not need: more than half of composite events in both validation cohorts were observational admissions",
 "Second, admission is not need: more than half of composite events were observational admissions"),
("Third, a need-recalibrated three-tier triage pathway showed consistent safety properties in both health systems, triaging the low-risk majority to ward-level care at a safety-first operating point while concentrating",
 "Third, a need-recalibrated three-tier triage pathway showed consistent safety in both health systems, triaging the low-risk majority to ward care while concentrating"),
# ¶5 nested gradient
("One model-comparison nuance should be noted: logistic regression",
 "One nuance: logistic regression"),
("The absolute differences are small (≤ 0.04 AUROC) and directionally inconsistent across settings, consistent with the broader tabular-clinical-data literature in which gradient boosting and linear models often trade places [8], so we retain",
 "The absolute differences are small (≤ 0.04 AUROC) and directionally inconsistent, consistent with the broader tabular-clinical-data literature in which gradient boosting and linear models often trade places [8]; we retain"),
# ¶6 second message
("ICU admission is a decision made by clinicians, and decades of health-services research show that it varies widely",
 "ICU admission is a clinician decision that varies widely"),
("What the model has learned therefore approximates what clinicians already use when deciding whom to admit; indeed, in INSPIRE the highest median predicted risk belonged to observational admissions, which suggests the model has internalized the same conservative logic clinicians use when they admit just in case.",
 "What the model has learned approximates what clinicians already use when deciding whom to admit: in INSPIRE the highest median predicted risk belonged to observational admissions, reflecting the same conservative logic clinicians use when they admit just in case."),
# ¶7 deployment merged
("We therefore propose and evaluated a different deployment mode:",
 "We therefore evaluated a different deployment mode:"),
("Between these poles lies the clinically uncertain middle, roughly a fifth of operations in INSPIRE, which captured",
 "Between these poles lies the clinically uncertain middle (roughly a fifth of INSPIRE operations), which captured"),
("The honest conclusion is that for these patients the end-of-surgery data do not resolve the decision;",
 "For these patients the end-of-surgery data do not resolve the decision;"),
("the transferable evidence is the ranking itself, and tier sizes will vary with case mix. Local recalibration is therefore not an optional refinement but an integral deployment step. What would this pathway change in practice? Two honest caveats temper the resource claim.",
 "the transferable evidence is the ranking itself, and tier sizes will vary with case mix, so local recalibration is an integral deployment step, not an optional refinement. Two caveats temper the resource claim."),
# ¶9 sensitivity interpretation
("and restricting to ambulatory surgery, where any ICU admission is unplanned by definition. Neither improved discrimination, because routine-ICU procedures are the easiest to rank. The external performance gap is therefore not an artifact of outcome definition; it reflects genuine case-mix and practice differences.",
 "and restricting to ambulatory surgery. Neither improved discrimination, because routine-ICU procedures are the easiest to rank: the external performance gap is not an artifact of outcome definition but reflects genuine case-mix and practice differences."),
("Nonetheless, the breadth of the MOVER outcome flag, an encounter-level indicator that folds planned postoperative admissions into the event definition,",
 "Nonetheless, the MOVER outcome flag, an encounter-level indicator that folds planned admissions into the event definition,"),
("The decision-curve ablation adds a practical finding for institutions with low arterial-line use: after recalibration, the model without the arterial-line indicator surrendered almost no net benefit at clinically relevant thresholds in both validation cohorts,",
 "The decision-curve ablation adds a practical finding for low-arterial-line settings: after recalibration, the model without the flag surrendered almost no net benefit at clinically relevant thresholds in both cohorts,"),
# ¶10 arterial-line
("The full model is the default: prediction occurs at the end of surgery, after the line decision has been made, so the flag is available and legitimate at prediction time and carries real signal. At institutions whose arterial-line practice differs markedly from the development cohort, particularly low-line-use settings, the model without the flag is the safer choice: it costs little discrimination, roughly halves the calibration drift that local recalibration must correct, and, after recalibration, surrenders almost no net benefit at clinically relevant thresholds. Either way, the deployment checklist is the same: audit local line practice against the development cohort, choose the version accordingly, recalibrate locally, and never present the line flag to clinicians as a modifiable risk factor. We nevertheless retain the full model as the primary model: it was pre-specified, it discriminates better where transport is hardest, and its strongest predictor is available and legitimate at the end-of-surgery prediction time. The no-flag version is a deployment option for mismatched practice settings, not a replacement.",
 "The full model is the default: prediction occurs at the end of surgery, after the line decision, so the flag is available and legitimate at prediction time. At institutions whose arterial-line practice differs markedly from the development cohort, the no-flag model is the safer choice: it costs little discrimination, roughly halves the calibration drift that local recalibration must correct, and surrenders almost no recalibrated net benefit. Either way, the checklist is the same: audit local line practice, choose the version accordingly, recalibrate locally, and never present the line flag as a modifiable risk factor. We nevertheless retain the full model as primary: it was pre-specified and discriminates better where transport is hardest; the no-flag version is a deployment option for mismatched practice settings, not a replacement."),
# ¶12 limitations part 1
("and an observational admission is not necessarily an unnecessary one, since",
 "and an observational admission is not necessarily unnecessary, since"),
("our proxy-based sensitivity analyses showed that this explains only part of the calibration drift",
 "our proxy sensitivity analyses showed this explains only part of the calibration drift"),
("any validation analysis and the triage thresholds were pre-specified, so the refinement",
 "any validation analysis and triage thresholds were pre-specified, so the refinement"),
# ¶13 limitations part 2
("it shares its institution with the development cohort, so it probes transport across time and case mix but not across sites; the VitalDB linker and trajectory screen bound but cannot entirely exclude residual overlap, although the worst-case inflation was negligible; ages are given in five-year bins and times as minutes relative to admission, so age and temporal precision are coarser than in development; intraoperative signals are released at five-minute resolution, which we expanded to a one-minute grid before applying identical feature definitions, so fine-grained burden and variability features are approximations; rocuronium dosing is not recorded and was imputed; preoperative comorbidities were ascertained differently across cohorts (preanesthesia assessment in VitalDB, diagnosis codes and preoperative prescriptions in INSPIRE, ICD history in MOVER), so comorbidity prevalence is not strictly comparable; and the INSPIRE outcome",
 "it shares its institution with the development cohort, probing transport across time and case mix but not across sites; the linker and trajectory screen bound but cannot entirely exclude residual overlap, although worst-case inflation was negligible; ages are five-year-binned and times are relative to admission; intraoperative signals are released at five-minute resolution and were expanded to a one-minute grid before applying identical feature definitions, so fine-grained burden and variability features are approximations; rocuronium dosing was imputed; comorbidities were ascertained differently across cohorts (preanesthesia assessment in VitalDB, diagnosis codes and prescriptions in INSPIRE, ICD history in MOVER); and the INSPIRE outcome"),
("Tenth, the SASA comparisons are complete-case analyses: the score requires an estimated-blood-loss entry, which was missing for 40.3% of INSPIRE and 38.8% of MOVER operations, and missingness was informative (in INSPIRE the composite event rate was 15.6% among SASA-evaluable versus 3.7% among SASA-missing operations); we therefore report paired analyses",
 "Tenth, the SASA comparisons are complete-case: the score requires an estimated-blood-loss entry, missing for 40.3% of INSPIRE and 38.8% of MOVER operations, and missingness was informative (composite event rate 15.6% among SASA-evaluable versus 3.7% among SASA-missing INSPIRE operations); we report paired analyses"),
("Twelfth, the decision-impact simulation is a retrospective what-if analysis under current practice: it assumes that triage follows the pathway and that low-tier observational admissions could be safely de-escalated, the operating-point thresholds are cohort-specific",
 "Twelfth, the decision-impact simulation is a retrospective what-if analysis: it assumes that triage follows the pathway and that low-tier observational admissions could be safely de-escalated, thresholds are cohort-specific"),
# ¶8 review-band cost
("for example by applying the pathway first to high-volume departments or to a fraction of the band, rather than re-tuning the threshold alone.",
 "for example by applying the pathway first to high-volume departments, rather than re-tuning the threshold alone."),
# ¶11 clinical positioning
("The model is not intended to replace clinical judgment, it should never be used to deny ICU admission to a patient a clinician believes needs one, and any bedside use",
 "The model is not intended to replace clinical judgment or to deny ICU admission, and any bedside use"),
# ¶2 prior work
("The practical implication: for minute-resolution intraoperative monitoring of this dimensionality,",
 "The practical implication: for minute-resolution monitoring of this dimensionality,"),
# ===== RESULTS =====
# line 30 cohort
("The higher event rate in MOVER reflects its broader ICU admission flag, which captures planned postoperative ICU admissions in addition to unplanned escalations.",
 "The higher MOVER event rate reflects its broader ICU admission flag, which captures planned postoperative admissions in addition to unplanned escalations."),
# line 38 feature importance
("The flag is legitimate as a predictor because the model is intended for use at the end of surgery,",
 "The flag is legitimate as a predictor because the model is used at the end of surgery,"),
# line 42 temporal
("the temporal validation cohort comprised 96,196 operations in 78,305 patients, with 10,370 events (10.8%). Applied without refitting,",
 "the temporal validation cohort comprised 96,196 operations with 10,370 events (10.8%). Applied without refitting,"),
("and the calibration slope of 0.875 indicates mildly over-dispersed predictions; this drift was nonetheless far smaller than in the international cohort.",
 "and the calibration slope of 0.875 indicates mildly over-dispersed predictions."),
("Logistic regression on the full feature set was nominally and statistically superior to XGBoost in this cohort (0.915, 0.913–0.917; DeLong p = 3.3 × 10⁻²²), a reversal of the internal-test ordering that we examine in the Discussion.",
 "Logistic regression on the full feature set was statistically superior to XGBoost in this cohort (0.915, 0.913–0.917; DeLong p = 3.3 × 10⁻²²), a reversal of the internal-test ordering (Discussion)."),
# line 44 overlap
("(lag-aligned correlation of minute-level mean arterial pressure and heart-rate epochs",
 "(lag-aligned correlation of minute-level MAP and heart-rate epochs"),
# line 48 external
("Applied without refitting to the more distant international MOVER cohort",
 "Applied without refitting to the international MOVER cohort"),
("contribute measurably to miscalibration in transport.",
 "contribute measurably to miscalibration."),
# line 52 GRU
("This study was not powered as an equivalence comparison, so we cannot exclude a small genuine advantage that the internal test set (1,797 operations, 361 events) was too small to confirm; we also evaluated the GRU only internally, so its transport is unknown.",
 "This study was not powered as an equivalence comparison, so a small genuine advantage cannot be excluded; we also evaluated the GRU only internally, so its transport is unknown."),
("XGBoost on engineered features remains the primary model: simpler, faster to train, easier to recalibrate and audit, and, with no material deficit in discrimination, calibration, or net benefit, the lower-risk choice for regulatory and clinical validation workflows.",
 "XGBoost on engineered features remains the primary model: simpler, easier to recalibrate and audit, and the lower-risk choice for clinical validation workflows."),
# line 56 ICU-course extraction
("(INSPIRE: bedside flowsheet and medication records, flowsheet coverage 99.1% of early-ICU operations; MOVER: medication administration records and airway-device documentation, data-quality gate passed by 93.6% of operations; Methods)",
 "(INSPIRE: flowsheet and medication records, 99.1% coverage; MOVER: medication and airway-device records, 93.6% data-quality gate; Methods)"),
# line 58 ICU categories
("not a direct measure of them, and an observational admission is not synonymous with an unnecessary one, since monitored observation after high-risk surgery can itself be the indication. These stays define a pool for review, not a count of waste.",
 "not a direct measure of them. These stays define a pool for review, not a count of waste."),
# line 60 outcome refinement
("On the SASA-evaluable subset (59.7% of INSPIRE and 61.2% of MOVER operations, because the SASA requires an estimated-blood-loss entry), paired DeLong tests",
 "On the SASA-evaluable subset (59.7% of INSPIRE and 61.2% of MOVER operations), paired DeLong tests"),
# line 64 triage
("Band event rates approximate the band definitions by construction (intermediate-tier true need 13.76% vs 13.75%; high-tier 38.1% vs 37.3%), so their cross-cohort agreement is not itself evidence of transport; what transports is the ranking (low-tier mortality below 0.1% and NPV near 98% in both systems), while tier sizes, which are not fixed by construction, differed substantially (low tier 74.5% vs 32.0%), reflecting case mix.",
 "Band event rates approximate the band definitions by construction, so their cross-cohort agreement is not itself evidence of transport; what transports is the ranking (low-tier mortality below 0.1% and NPV near 98% in both systems), while tier sizes, which are not fixed by construction, differed substantially, reflecting case mix."),
# line 66 intermediate band
("We therefore recommend that the intermediate band trigger heightened human assessment (senior review, extended post-anesthesia care, or a monitored ward bed) rather than any automated disposition.",
 "We therefore recommend that the intermediate band trigger heightened human assessment rather than any automated disposition."),
# line 72 subgroups
("did not meet the pre-specified precision criteria for subgroup evaluation, so the urgency and gynecology strata",
 "did not meet pre-specified precision criteria, so the urgency and gynecology strata"),
# line 74 fairness
("should be checked within strata at local deployment.",
 "should be checked within strata locally."),
# line 78 sensitivity
("for which postoperative ICU admission is routine and therefore likely planned: 62 procedures",
 "for which ICU admission is routine and likely planned: 62 procedures"),
# line 82 DCA ablation
("and in INSPIRE differences were likewise small at thresholds of 0.10–0.30 (raw: +0.0044 to −0.002; after recalibration: ≤0.0024, favoring the full model).",
 "and in INSPIRE were likewise small at 0.10–0.30 (raw +0.0044 to −0.002; ≤0.0024 after recalibration, favoring the full model)."),
("after recalibration the curves were nearly identical at clinically relevant thresholds (difference 0.0004 at 0.10",
 "after recalibration the curves were nearly identical (difference 0.0004 at 0.10"),
]

fails = []
for i, (old, new) in enumerate(EDITS):
    n = txt.count(old)
    if n != 1:
        fails.append((i, n, old[:80]))
if fails:
    for i, n, s in fails:
        print(f'FAIL edit {i}: {n} matches — {s}')
    sys.exit(1)

for old, new in EDITS:
    txt = txt.replace(old, new, 1)

open(PATH, 'w', encoding='utf-8').write(txt)
print(f'All {len(EDITS)} edits applied.')

# re-measure section word counts
lines = txt.split('\n')
secs = {}
cur = 'PREAMBLE'
for l in lines:
    if l.startswith('## '):
        cur = l.strip()
    secs.setdefault(cur, []).append(l)
def wc(ls):
    return sum(len(l.split()) for l in ls if not l.startswith('#'))
main = 0
for k in ['## Abstract', '## Introduction', '## Results', '## Discussion', '## Methods']:
    w = wc(secs[k])
    print(f'{w:6d}  {k}')
    if k != '## Abstract':
        main += w
print(f'MAIN TEXT (excl abstract): {main}')
