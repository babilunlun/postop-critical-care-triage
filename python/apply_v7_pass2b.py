#!/usr/bin/env python3
"""Pass 2b: final ~85 words of compression."""
import sys

PATH = '/workspace/manuscript_v7.md'
txt = open(PATH, encoding='utf-8').read()

EDITS = [
# ¶7: drop duplicated stricter-threshold clause (same operating point detailed later)
("(negative predictive value for true need 98.76%); a stricter <2% threshold covered 57.8% of operations with 0.113% missed escalations and 0.022% mortality. In MOVER,",
 "(negative predictive value for true need 98.76%). In MOVER,"),
("(2.0% in INSPIRE, the stricter threshold above, and 4.6% in MOVER; Table 5, Fig. 7)",
 "(2.0% in INSPIRE and 4.6% in MOVER; Table 5, Fig. 7)"),
# ¶7 deployment
("the pathway identifies a large majority of patients who can safely proceed to ward care, with residual mortality below 0.1%",
 "the pathway identifies a large majority who can safely proceed to ward care, with residual mortality below 0.1%"),
# ¶4 calibration decomposition
("nearly all of the intercept shift was attributable to the lower composite prevalence, with case mix nearly identical and a small residual, which explains why intercept-only recalibration sufficed.",
 "nearly all of the intercept shift reflected the lower composite prevalence, with case mix nearly identical and a small residual, explaining why intercept-only recalibration sufficed."),
# ¶3 first message
("This gap is expected, since the MOVER event definition is broader and event prevalence more than four times higher than in INSPIRE, and it argues",
 "This gap is expected, since the MOVER event definition is broader and prevalence more than four times higher, and it argues"),
# ¶12 limitations
("reflecting 2016–2017 practice at one Korean institution; development on multi-country data might further improve transport.",
 "reflecting 2016–2017 practice at one Korean institution; multi-country development might further improve transport."),
("triage thresholds were pre-specified, so the refinement cannot have inflated performance.",
 "triage thresholds were pre-specified, so performance cannot have been inflated."),
# ¶10 arterial-line
("Such process-of-care variables are powerful and legitimate predictors when the prediction time is after the decision is made, but their meaning is institution-specific:",
 "Such process-of-care variables are powerful and legitimate predictors when prediction occurs after the decision, but their meaning is institution-specific:"),
# ¶5 nested gradient
("the within-institution drift shows that a decade of practice change is enough to distort absolute risks:",
 "the within-institution drift shows that a decade of practice change can distort absolute risks:"),
# ¶13: delete Eleventh (triplicated in Results triage para and Discussion deployment para), renumber Twelfth
("Eleventh, because triage bands are defined on risk recalibrated within each cohort, band event rates approximate the band definitions by construction; the cross-cohort transport evidence is the ranking (low-tier safety and middle-band capture), not the band rates themselves. Twelfth, the decision-impact simulation",
 "Eleventh, the decision-impact simulation"),
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
