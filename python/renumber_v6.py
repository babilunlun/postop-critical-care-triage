#!/usr/bin/env python3
"""Renumber manuscript_v6 citations by first appearance (Vancouver) and rebuild the reference list."""
import re, sys

DRAFT = '/workspace/v6_draft.md'
OUT = '/workspace/manuscript_v6.md'

txt = open(DRAFT, encoding='utf-8').read()

# --- split body vs references section ---
ref_start = txt.index('## References')
# references section ends at the '\n---\n' that precedes '## Figure legends'
fig_pos = txt.index('## Figure legends')
sep_pos = txt.rindex('---', ref_start, fig_pos)
body = txt[:ref_start]
ref_section = txt[ref_start:sep_pos]
tail = txt[sep_pos:]  # starts with '---\n\n## Figure legends...'

# --- parse old numbered refs ---
old_refs = {}
for m in re.finditer(r'(?m)^(\d+)\.\s+(.*)$', ref_section):
    n = int(m.group(1))
    old_refs[n] = m.group(2).strip()
assert len(old_refs) == 28, f"expected 28 old refs, got {len(old_refs)}"

# --- parse new refs block ---
nb = re.search(r'<!-- NEW_REFS\n(.*?)\n-->', ref_section, re.S)
assert nb, "NEW_REFS block not found"
new_refs = {}
for line in nb.group(1).strip().split('\n'):
    m = re.match(r'\{C:(\w+)\}\s+(.*)$', line.strip())
    assert m, f"bad NEW_REFS line: {line}"
    new_refs['C:' + m.group(1)] = m.group(2).strip()
assert len(new_refs) == 11, f"expected 11 new refs, got {len(new_refs)}"

# --- citation scanning ---
CIT = re.compile(r'\[((?:\d+|\{C:\w+\})(?:\s*[,\u2013]\s*(?:\d+|\{C:\w+\}))*)\]')

def parse_cit_content(content):
    """Return list of keys: ints for old refs, 'C:Name' strings for new ones."""
    keys = []
    for part in content.split(','):
        part = part.strip()
        m = re.fullmatch(r'(\d+)\s*\u2013\s*(\d+)', part)
        if m:
            a, b = int(m.group(1)), int(m.group(2))
            keys.extend(range(a, b + 1))
        elif re.fullmatch(r'\d+', part):
            keys.append(int(part))
        elif re.fullmatch(r'\{C:\w+\}', part):
            keys.append('C:' + part[3:-1])
        else:
            raise ValueError(f"unparseable citation element: {part!r} in [{content}]")
    return keys

order = []  # first-appearance order of keys
def collect(m):
    for k in parse_cit_content(m.group(1)):
        if k not in order:
            order.append(k)
    return m.group(0)

CIT.sub(collect, body)

# --- assign new numbers ---
newnum = {k: i + 1 for i, k in enumerate(order)}
total = len(order)
print(f"total distinct cited references: {total}")

# sanity: every old ref cited, every new ref cited
uncited_old = [n for n in old_refs if n not in newnum]
uncited_new = [k for k in new_refs if k not in newnum]
print("uncited old refs:", uncited_old)
print("uncited new refs:", uncited_new)
if uncited_old or uncited_new:
    sys.exit("ERROR: dangling references")

# --- rewrite citations in body ---
def compress(nums):
    """Sort, dedupe, compress runs of >=3 into a\u2013b."""
    nums = sorted(set(nums))
    out, i = [], 0
    while i < len(nums):
        j = i
        while j + 1 < len(nums) and nums[j + 1] == nums[j] + 1:
            j += 1
        if j - i >= 2:
            out.append(f"{nums[i]}\u2013{nums[j]}")
        else:
            out.extend(str(nums[t]) for t in range(i, j + 1))
        i = j + 1
    return ', '.join(out)

def rewrite(m):
    keys = parse_cit_content(m.group(1))
    return '[' + compress([newnum[k] for k in keys]) + ']'

new_body = CIT.sub(rewrite, body)
assert '{C:' not in new_body, "leftover placeholder in body"

# --- rebuild reference list ---
def entry(k):
    if isinstance(k, int):
        return old_refs[k]
    return new_refs[k]

ref_lines = ['## References', '']
for i, k in enumerate(order):
    ref_lines.append(f"{i + 1}. {entry(k)}")
new_ref_section = '\n'.join(ref_lines) + '\n\n'

final = new_body + new_ref_section + tail
open(OUT, 'w', encoding='utf-8').write(final)
print("written:", OUT, len(final), "chars")

# --- report mapping ---
label = {k: (f"old[{k}]" if isinstance(k, int) else k) for k in order}
print("\nnew-number : source")
for i, k in enumerate(order):
    tag = label[k]
    src = entry(k)[:70]
    print(f"{i + 1:3d} : {tag:16s} {src}")
