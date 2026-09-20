#!/usr/bin/env python3
"""Build manuscript v8 from v7:
  A. insert 3 new supplementary-table citations (using OLD numbers)
  B. global renumber of main-fig (6<->7), supp-fig, supp-table citations
  C. reorder figure-legend blocks + supplementary-material list to new order;
     add a new Supplementary Figure S2 (GRU) legend.
Filenames inside parentheses (lowercase, e.g. table_s4_...) are left untouched.
"""
import re, sys

SRC = "/mnt/results/04_manuscript/manuscript_v7.md"
DST = "/workspace/manuscript_v8.md"
text = open(SRC, encoding="utf-8").read()

# ---------- Phase A: insert 3 new citations (OLD numbers) ----------
insertions = [
    ("slope 1.001, 0.962–1.041).",
     "slope 1.001, 0.962–1.041; Supplementary Table S3)."),
    ("(Table 2, Supplementary Fig. S1).",
     "(Table 2, Supplementary Table S1, Supplementary Fig. S1)."),
    ("(procedures with ≥ 20 cases),",
     "(procedures with ≥ 20 cases; Supplementary Table S6),"),
]
for old, new in insertions:
    n = text.count(old)
    assert n == 1, f"insertion target not unique ({n}x): {old!r}"
    text = text.replace(old, new)
print("Phase A: 3 citations inserted")

# ---------- Phase B: global renumber ----------
suppfig_map = {1: 2, 2: 5, 3: 6, 4: 1, 5: 3, 6: 4}
supptab_map = {4: 1, 13: 2, 7: 3, 3: 4, 1: 5, 17: 6, 8: 7, 11: 8, 15: 9,
               12: 10, 9: 11, 2: 12, 14: 13, 16: 14, 5: 15, 10: 16, 6: 17}

def _renum(nums, mp):
    return re.sub(r'S(\d+)', lambda mm: 'S@@%d@@' % mp[int(mm.group(1))], nums)

# supplementary figures (Fig. / Figure / Figs. forms), possibly multi-citation
text = re.sub(r'(Supplementary\s+Fig\w*\.?\s+)(S\d+(?:(?:\s*,\s*|\s+and\s+)S\d+)*)',
              lambda m: m.group(1) + _renum(m.group(2), suppfig_map), text)
# supplementary tables (singular/plural), possibly "S5 and S10"
text = re.sub(r'(Supplementary\s+Tables?\s+)(S\d+(?:(?:\s*,\s*|\s+and\s+)S\d+)*)',
              lambda m: m.group(1) + _renum(m.group(2), supptab_map), text)
text = text.replace('@@', '')  # strip placeholders

# main figures: swap 6 <-> 7 in both "Fig. N" and "Figure N" forms
for ab in ('Fig. ', 'Figure '):
    text = text.replace(ab + '6', ab + '@@SWAP@@')
    text = text.replace(ab + '7', ab + '6')
    text = text.replace(ab + '@@SWAP@@', ab + '7')
assert '@@' not in text, "placeholder leaked"
print("Phase B: renumber applied")

# ---------- Phase C: reorder legends + supp list ----------
lines = text.split('\n')
def find(sub):
    return next(i for i, l in enumerate(lines) if l.strip().startswith(sub))
i_leg   = find('## Figure legends')
i_tab   = find('## Tables')
i_supp  = find('## Supplementary material')

NEW_S2_LEGEND = ("**Supplementary Figure S2. ROC comparison of GRU ensembles versus "
    "XGBoost on the internal test set.** Receiver operating characteristic curves on the "
    "VitalDB temporal hold-out test set (n = 1,797) for XGBoost (full), the "
    "sequence-plus-static GRU ensemble, logistic regression (full), the sequences-only "
    "GRU ensemble, and the SASA reference, with AUROC in parentheses.")

# --- figure legends: blocks separated by blank lines ---
leg_blocks = [b.strip() for b in '\n'.join(lines[i_leg+1:i_tab]).split('\n\n') if b.strip()]
main_blocks, supp_blocks = [], []
for b in leg_blocks:
    m_main = re.match(r'\*\*Figure (\d+)\.', b)
    m_supp = re.match(r'\*\*Supplementary Figure S(\d+)\.', b)
    if m_main:
        main_blocks.append((int(m_main.group(1)), b))
    elif m_supp:
        supp_blocks.append((int(m_supp.group(1)), b))
    else:
        sys.exit("unparsed legend block: " + b[:80])
main_blocks.sort(key=lambda x: x[0])
supp_blocks.sort(key=lambda x: x[0])
# insert new S2 (GRU) legend in numeric position
supp_blocks.append((2, NEW_S2_LEGEND))
supp_blocks.sort(key=lambda x: x[0])
assert [n for n, _ in main_blocks] == [1, 2, 3, 4, 5, 6, 7], [n for n, _ in main_blocks]
assert [n for n, _ in supp_blocks] == [1, 2, 3, 4, 5, 6], [n for n, _ in supp_blocks]
new_leg = ['## Figure legends', ''] + ['\n\n'.join(b for _, b in main_blocks + supp_blocks)]

# --- supplementary material list: reorder by new number, TRIPOD last ---
supp_lines = lines[i_supp+1:]
head = supp_lines[0]  # '## Supplementary material (to be compiled)' is at i_supp
items = [l for l in supp_lines if l.strip()]
fig_items, tab_items, other = [], [], []
for l in items:
    mf = re.match(r'- Supplementary Fig\. S(\d+)\.', l)
    mt = re.match(r'- Supplementary Table S(\d+)\.', l)
    if mf:   fig_items.append((int(mf.group(1)), l))
    elif mt: tab_items.append((int(mt.group(1)), l))
    elif l.strip().startswith('- TRIPOD'): other.append(l)
    elif not l.strip(): pass
    else: sys.exit("unparsed supp-list line: " + l[:80])
fig_items.sort(key=lambda x: x[0]); tab_items.sort(key=lambda x: x[0])
assert [n for n, _ in fig_items] == [1, 2, 3, 4, 5, 6], [n for n, _ in fig_items]
assert [n for n, _ in tab_items] == list(range(1, 18)), [n for n, _ in tab_items]
new_supp = [lines[i_supp], ''] + [l for _, l in fig_items] + [l for _, l in tab_items] + other

# --- splice back ---
out = lines[:i_leg] + new_leg + [''] + lines[i_tab:i_supp] + new_supp
open(DST, 'w', encoding='utf-8').write('\n'.join(out))
print("Phase C: legends + supp list reordered; v8 written to", DST)
print("main fig legend order:", [n for n, _ in main_blocks])
print("supp fig legend order:", [n for n, _ in supp_blocks])
print("supp fig list order:", [n for n, _ in fig_items])
print("supp tab list order:", [n for n, _ in tab_items])
