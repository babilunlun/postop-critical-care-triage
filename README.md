# Input restriction and local updating in cross-cohort prediction model transport

Analysis code for the manuscript:

> **Input restriction and local updating in cross-cohort prediction model transport:
> an empirical evaluation of frozen masking versus source-internal refitting**
> *(manuscript under review; TRIPOD+AI reporting)*

A surgical prediction model developed in VitalDB (XGBoost and ridge logistic
regression, 71 inputs: 33 clinical + 38 intraoperative monitoring summaries) was
transported to two further surgical datasets — INSPIRE (same institution,
overlapping period) and MOVER (UC Irvine, USA; geographically external). The
repository contains the full analysis pipeline: frozen 16-input (R16) masking of
the full models, source-internal R16 refitting, local probability updating
(intercept-only or two-parameter Platt scaling fitted in patient-level
calibration partitions C), and evaluation in patient-separated partitions E with
patient-cluster bootstrap uncertainty, plus a numeric Surgical Apgar Score (SAS)
reference comparison on identical evaluable populations.

The recorded postoperative ICU admission-or-death endpoint is used purely as an
**evaluation vehicle**: the estimands are differences between model
configurations, and no claim about ICU need, treatment benefit or deployment
readiness is made.

## Headline results (partition E, after C-fitted Platt updating)

| Cohort | Operations | Events | Full XGBoost AUROC | Full LR AUROC | Refit-R16 ridge AUROC | Masked-R16 XGBoost AUROC |
|---|---|---|---|---|---|---|
| INSPIRE | 14,470 | 1,539 (10.6%) | 0.908 | 0.916 | 0.883 | 0.801 |
| MOVER | 7,473 | 3,490 (46.7%) | 0.797 | 0.755 | 0.758 | 0.705 |

Full Brier scores, paired configuration contrasts with 95% intervals, raw versus
updated calibration, and the SAS-evaluable subset analyses are reported in the
manuscript and its additional files.

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
R/figures/              Figure-generating scripts (incl. fig1_svg.R for the
                        Figure 1 design schematic, restyle_fig4_composite.R for
                        the final Figure 4 styling, single-panel regeneration
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
| 42–51 | `R/42_mover_dedup_recompute.R` → `51_consolidate_paired.R`; `python/47–51` | MOVER de-duplication, current (v4/E-evaluation) tables & figures, paired SAS comparisons, TRIPOD v5 |
| B1–B6 | `R/revision/` | Revision analyses |
| — | `R/figures/regen_single_panels.R`, `regen_fig7.R`, `split_panels.py`, `fig1_svg.R`, `restyle_fig4_composite.R` | Final figure files for submission |
| — | `audit/` | 290-check numeric audit of every quantitative claim in the manuscript |

Note: there is intentionally no script numbered 07 (numbering gap in the original
analysis log). Scripts up to the v3 era document the earlier whole-cohort and
partition-T analyses; the current partition-E evaluation corresponds to the
v4-era scripts (`43_tables_v4.R` → `51_consolidate_paired.R`, `python/47–51`).

## Environment

- R 4.4.3 — see `environment/r_packages.txt` (key packages: xgboost, tidymodels,
  pROC, PRROC, dcurves, torch, ggplot2/ggprism/svglite)
- Python 3.11 — see `environment/requirements.txt` (pandas, python-docx)

## Citation

If you use this code, please cite the manuscript (citation to be added upon
publication).

## License

MIT (see `LICENSE`).
