#!/usr/bin/env python3
"""Audit first-appearance order of all figure/table citations in manuscript_v7.md.
Handles compound citations like 'Supplementary Tables S5 and S10' and
'Figs. 6 and 7'. Excludes the figure-legends section and the supplementary
material list so only in-text citations drive the ordering.
"""
import re, json

MD = "/mnt/results/04_manuscript/manuscript_v7.md"
text = open(MD, encoding="utf-8").read()
lines = text.split("\n")

# Find section boundaries to exclude legends + supp list + references
legend_start = None
supp_list_start = None
ref_start = None
for i, ln in enumerate(lines):
    s = ln.strip().lower()
    if legend_start is None and re.match(r"^#+\s*figure legends", s):
        legend_start = i
    if supp_list_start is None and re.match(r"^#+\s*supplementary material", s):
        supp_list_start = i
    if ref_start is None and re.match(r"^#+\s*references", s):
        ref_start = i
print(f"sections: legends@{legend_start}, supplist@{supp_list_start}, refs@{ref_start}, total lines {len(lines)}")

# Body = everything before the figure legends section (legends come after refs usually;
# take min of the exclusion boundaries that exist)
boundaries = [b for b in [legend_start, supp_list_start, ref_start] if b is not None]
body_end = min(boundaries) if boundaries else len(lines)
# Also need to include anything after legends? No: in-text citations are all in main body.
body = lines[:body_end]

# Patterns. We scan line by line, record (line_no, category, number) in order.
# Categories: mainfig, maintab, suppfig, supptab
events = []

# Regex tokens:
#  - "Fig. 3", "Figs. 3 and 4", "Figure 2", "Fig. 3a"
#  - "Supplementary Fig. S1", "Supplementary Figs. S1 and S2", "Supplementary Figure S3"
#  - "Table 1", "Tables 1 and 2"
#  - "Supplementary Table S4", "Supplementary Tables S5 and S10"
pat = re.compile(r"""
    (?P<supp>Supplementary\s+)?                 # optional Supplementary
    (?P<kind>Figs?\.?|Figures?|Tables?)         # kind word
    \s*
    (?P<rest>                                   # the number list part
        S?\d+[a-z]?
        (?:\s*(?:,|and|&)\s*S?\d+[a-z]?)*
    )
""", re.VERBOSE)

for ln_no, ln in enumerate(body, start=1):
    for m in pat.finditer(ln):
        supp = bool(m.group("supp"))
        kind = m.group("kind").lower()
        rest = m.group("rest")
        nums = re.findall(r"S?(\d+)[a-z]?", rest)
        is_fig = kind.startswith("fig")
        is_tab = kind.startswith("tab")
        for n in nums:
            if is_fig and supp:
                cat = "suppfig"
            elif is_fig:
                cat = "mainfig"
            elif is_tab and supp:
                cat = "supptab"
            elif is_tab:
                cat = "maintab"
            else:
                continue
            events.append((ln_no, cat, int(n)))

# First appearance per category
first = {}
for ln_no, cat, n in events:
    key = (cat, n)
    if key not in first:
        first[key] = ln_no

report = {}
for cat in ["mainfig", "maintab", "suppfig", "supptab"]:
    items = sorted(((n, ln) for (c, n), ln in first.items() if c == cat), key=lambda x: x[1])
    report[cat] = items
    print(f"\n=== {cat} (first-citation order) ===")
    for rank, (n, ln) in enumerate(items, start=1):
        flag = "" if n == rank else f"  <-- renumber: old {n} -> new {rank}"
        print(f"  new {rank:2d}: old {cat}{n:2d}  (line {ln}){flag}")

# Which supp tables / figures exist but are never cited?
all_suppfig = set(range(1, 7))
all_supptab = set(range(1, 18))
cited_sf = {n for (c, n) in first if c == "suppfig"}
cited_st = {n for (c, n) in first if c == "supptab"}
print(f"\nSupp figures never cited: {sorted(all_suppfig - cited_sf)}")
print(f"Supp tables  never cited: {sorted(all_supptab - cited_st)}")

json.dump({cat: [(n, ln) for n, ln in items] for cat, items in report.items()},
          open("/workspace/citation_order_v7.json", "w"), indent=1)
print("\nsaved /workspace/citation_order_v7.json")
