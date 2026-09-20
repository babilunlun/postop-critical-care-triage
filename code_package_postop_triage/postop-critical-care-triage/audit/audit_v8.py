#!/usr/bin/env python3
"""Verify manuscript_v8.md: (1) all 4 citation categories strictly increasing by
first appearance; (2) no supp fig/table left uncited; (3) the 3 inserted citations
landed with correct NEW numbers; (4) word count band; (5) reference count."""
import re

MD = "/workspace/manuscript_v8.md"
text = open(MD, encoding="utf-8").read()
lines = text.split("\n")

legend_start = supp_list_start = ref_start = None
for i, ln in enumerate(lines):
    s = ln.strip().lower()
    if legend_start is None and re.match(r"^#+\s*figure legends", s): legend_start = i
    if supp_list_start is None and re.match(r"^#+\s*supplementary material", s): supp_list_start = i
    if ref_start is None and re.match(r"^#+\s*references", s): ref_start = i
body = lines[:min(b for b in [legend_start, supp_list_start, ref_start] if b is not None)]

pat = re.compile(r"""
    (?P<supp>Supplementary\s+)?(?P<kind>Figs?\.?|Figures?|Tables?)\s*
    (?P<rest>S?\d+[a-z]?(?:\s*(?:,|and|&)\s*S?\d+[a-z]?)*)
""", re.VERBOSE)
events = []
for ln_no, ln in enumerate(body, start=1):
    for m in pat.finditer(ln):
        supp = bool(m.group("supp")); kind = m.group("kind").lower()
        nums = re.findall(r"S?(\d+)[a-z]?", m.group("rest"))
        is_fig = kind.startswith("fig"); is_tab = kind.startswith("tab")
        for n in nums:
            cat = ("suppfig" if supp else "mainfig") if is_fig else ("supptab" if supp else "maintab")
            events.append((ln_no, cat, int(n)))
first = {}
for ln_no, cat, n in events:
    first.setdefault((cat, n), ln_no)

ok = True
for cat, expect_max in [("mainfig", 7), ("maintab", 5), ("suppfig", 6), ("supptab", 17)]:
    items = sorted(((n, ln) for (c, n), ln in first.items() if c == cat), key=lambda x: x[1])
    order = [n for n, _ in items]
    good = order == list(range(1, expect_max + 1))
    ok &= good
    print(f"{cat:8s}: order={order}  lines={[ln for _, ln in items]}  {'OK' if good else 'BAD'}")

# uncited check
cited_sf = {n for (c, n) in first if c == "suppfig"}
cited_st = {n for (c, n) in first if c == "supptab"}
print("supp figs never cited:", sorted(set(range(1, 7)) - cited_sf))
print("supp tabs never cited:", sorted(set(range(1, 18)) - cited_st))

# 3 inserted citations -> expected NEW numbers
checks = [
    ("slope 1.001, 0.962–1.041; Supplementary Table S4).", "insert#1 MOVER recal -> S4"),
    ("(Table 2, Supplementary Table S5, Supplementary Fig. S2).", "insert#2 GRU table S5 + fig S2"),
    ("(procedures with ≥ 20 cases; Supplementary Table S17),", "insert#3 procedure rates -> S17"),
]
for s, label in checks:
    print(("OK  " if s in text else "MISS"), label, "->", repr(s[:60]))

# word count (body, excluding refs/legends/tables/supp list) + whole doc
body_text = "\n".join(body)
print("body words:", len(body_text.split()))
print("total words:", len(text.split()))
# references count
refs = [l for l in lines[ref_start:legend_start] if re.match(r"^\d+\.\s", l.strip())]
print("reference entries:", len(refs))
print("ALL CITATION ORDER OK" if ok else "CITATION ORDER PROBLEM")
