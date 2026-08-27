# Postoperative critical care triage — XGBoost model with multi-cohort validation

Analysis code for the manuscript:

> **Machine-learning triage of postoperative critical care need: development in VitalDB
> with temporal (INSPIRE) and geographic (MOVER) external validation**
> *(manuscript under review; TRIPOD+AI reporting)*

A gradient-boosting model (XGBoost, 71 predictors: 33 clinical + 38 intraoperative
time-series features) was trained on the VitalDB cohort (Seoul National University
Hospital, Korea) to predict postoperative critical care after non-cardiac surgery,
and validated without refitting on INSPIRE (temporal hold-out, same institution)
and MOVER (UC Irvine, USA; geographic/external). A GRU sequence model, logistic
regression, and the SASA score serve as comparators. The model is evaluated both
as a composite-outcome predictor and, after need-recalibration, as a three-tier
perioperative triage pathway with a pre-specified ≥96% sensitivity safety constraint.

## Headline results

| Cohort | Operations | Events | XGBoost AUROC (95% CI) |
|---|---|---|---|
| VitalDB internal test | 1,797 | 361 (20.1%) | 0.932 (0.918–0.945) |
| INSPIRE (temporal) | 96,196 | 10,370 (10.8%) | 0.907 (0.905–0.910) |
| MOVER (geographic) | 48,370 | 21,740 (44.9%) | 0.794 (0.790–0.798) |

## Data access

Raw patient-level data are **not** redistributed in this repository.

- **VitalDB** — publicly available at <https://vitaldb.net> (VitalDB Data Korea /
  Seoul National University Hospital).
- **INSPIRE** — available via PhysioNet (<https://physionet.org/>; credentialed
  access, data-use agreement required).
- **MOVER** — Medical Informatics Operating Room Vitals and Events Repository,
  UC Irvine; available from the MOVER project under its data-use agreement.

Model objects (`.rds`) and GRU weights (`.pt`) are likewise not included; the
pipeline below regenerates all derived datasets, models, tables, and figures
from the source databases.

## Repository layout

```
R/                      Numbered analysis pipeline (execution order below)
R/revision/             Revision analyses B1–B6 (impact simulation, calibration
                        decomposition, DCA, subgroups, fairness, GRU net benefit)
R/figures/              Figure-generating scripts (incl. single-panel regeneration
                        and SVG panel-splitting utilities)
python/                 Manuscript/table document builders, TRIPOD checklists,
                        verification scripts
audit/                  Numeric consistency audit of the manuscript vs source data
                        (290 checks; audit_report.csv)
environment/            R package versions (R 4.4.3) and Python requirements
```

## Pipeline overview (numeric order)

| Step | Script | Purpose |
|---|---|---|
| 01–03 | `R/01_download_vitaldb.R` → `03_model.R` | VitalDB download, feature engineering (71 predictors), XGBoost/LR training |
| 04, 09 | `R/figures/` | Internal/external validation figures |
| 05–08 | `R/05_external_eval.R` → `08_mover_vitals.R` | MOVER EMR + vitals processing, external evaluation |
| 10 | `R/10_recal_cv.R` | Cross-validated Platt recalibration |
| 11–14 | `R/11_sensitivity_noart.R` → `14_gru_eval.R` | Arterial-line ablation; GRU sequence model (data, training, evaluation) |
| 15–17 | `R/16_tables.R`, `R/17_table1_combined.R`; `R/figures/15_fig1_flow.R` | Tables 1–2, cohort flowchart |
| 18–23 | `python/` | Manuscript docx + TRIPOD checklist (v1–v2) |
| 20–22 | `R/20_sensitivity_planned_icu.R`, `21_dca_noart.R`, `R/figures/22_fig_sensitivity.R` | Outcome-definition sensitivity + DCA ablation |
| 24–28 | `R/24_inspire_download.sh` → `28_inspire_eval.R` | INSPIRE cohort build, time series, overlap screen, temporal evaluation |
| 29–35 | `R/figures/29_inspire_fig.R`, `30_fig1_flow_v3.R`; `R/31_inspire_splitsample_lr.R`, `32_inspire_tables.R`; `python/33–35` | INSPIRE figures/tables, split-sample validation, manuscript v3 |
| 36–41 | `R/36_inspire_icu_course.R` → `40_sensitivity_b5.R`; `R/figures/41_fig5_risk_by_category.R` | ICU-course–verified outcome refinement, triage bands, robustness |
| 42–51 | `R/42_mover_dedup_recompute.R` → `51_consolidate_paired.R`; `python/47–51` | MOVER de-duplication, v4/v5 tables & figures, paired SASA comparisons, TRIPOD v5 |
| B1–B6 | `R/revision/` | Revision analyses (Table 5, Suppl. Tables S2/S6/S9/S10/S13/S14, Figs. 6–7, Suppl. Figs. S1/S3/S4) |
| — | `R/figures/regen_single_panels.R`, `regen_fig7.R`, `split_panels.py` | Final single-panel figure files for submission |
| — | `audit/` | 290-check numeric audit of every quantitative claim in the manuscript |

Note: there is intentionally no script numbered 07 (numbering gap in the original
analysis log).

## Environment

- R 4.4.3 — see `environment/r_packages.txt` (key packages: xgboost, tidymodels,
  pROC, PRROC, dcurves, torch, ggplot2/ggprism/svglite)
- Python 3.11 — see `environment/requirements.txt` (pandas, python-docx)

## Citation

If you use this code, please cite the manuscript (citation to be added upon
publication).

## License

MIT (see `LICENSE`).
