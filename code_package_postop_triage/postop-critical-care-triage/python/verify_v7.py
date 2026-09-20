#!/usr/bin/env python3
"""Hard verification of manuscript_v7.md against all binding constraints."""
import re

txt = open('/workspace/manuscript_v7.md', encoding='utf-8').read()

print('=== (a) 21 audit phrases ===')
AUDIT = [
 "57.8% of INSPIRE operations (55,595",
 "negative predictive value 99.66%",
 "12 deaths, 0.22 per 1,000 operations",
 "28.0% of MOVER operations (negative predictive value 98.27%; 3 deaths)",
 "94.2% (INSPIRE) and 97.2% (MOVER) of missed escalations and 98.3% and 99.6% of deaths",
 "388 operations per 1,000 in INSPIRE and 624 per 1,000 in MOVER",
 "3.1 bed-days per 1,000 operations, 1.4% of the cohort total",
 "57 observational bed-days per 1,000 operations in the INSPIRE intermediate band, and 238 observational admissions per 1,000 operations in MOVER",
 "2.0% in INSPIRE",
 "4.6% in MOVER",
 "INSPIRE 20.2% vs 44.0%, negative predictive value 98.54% vs 99.51%",
 "MOVER 10.9% vs 27.7%, 96.26% vs 98.42%",
 "net benefit of 32.1 (INSPIRE) and 86.5 (MOVER) true-need equivalents per 1,000 operations",
 "approximately 4–50% in both cohorts",
 "90.5% of the shift (−0.733 of −0.809 log-odds) to the lower composite prevalence, with case mix as scored by the model nearly identical (+0.044) and only a small residual (−0.121",
 "only half of the +2.352 intercept shift reflected the higher composite prevalence (+1.178); the remainder was a large residual (+1.365) together with a shallow calibration slope (0.717)",
 "32.7% of MOVER operations were observational admissions versus 6.2% in INSPIRE",
 "0.791, 95% CI 0.779–0.804 in INSPIRE",
 "0.722, 0.714–0.729 in MOVER",
 "AUROCs ranged from 0.79 (thoracic surgery) to 0.92 (gynecology) in INSPIRE and from 0.72 (ASA 3–5) to 0.80 (age <65 years) in MOVER",
 "5.3% in INSPIRE, 12.2% in MOVER",
]
npass = 0
for p in AUDIT:
    ok = p in txt
    npass += ok
    if not ok:
        print(f'  MISSING: {p[:70]}')
print(f'  {npass}/21 audit phrases present')

print('=== (b) citation census: all 39 refs cited ===')
# reference list entries
refsec = txt.split('## References')[1].split('##')[0]
refs = re.findall(r'^\s*(\d+)\.\s', refsec, flags=re.M)
print(f'  reference list entries: {len(refs)}')
body = txt.split('## References')[0]
missing = []
for n in range(1, 40):
    # match [n], [n, ...], [n–m] ranges, [..., n]
    pat = re.compile(r'\[(?:[^\]]*\b)?' + str(n) + r'(?:\s*[–,-]|[^\d\]])')
    found = False
    for m in re.finditer(r'\[([^\]]+)\]', body):
        content = m.group(1)
        # split on comma, handle ranges
        for part in content.split(','):
            part = part.strip()
            if '–' in part or '-' in part:
                mm = re.match(r'^(\d+)\s*[–-]\s*(\d+)$', part)
                if mm and int(mm.group(1)) <= n <= int(mm.group(2)):
                    found = True
            elif part.isdigit() and int(part) == n:
                found = True
    if not found:
        missing.append(n)
print(f'  uncited refs: {missing if missing else "none — all 39 cited"}')

print('=== (c) abstract word count ===')
abssec = txt.split('## Abstract')[1].split('## ')[0]
w = sum(len(l.split()) for l in abssec.split('\n') if not l.startswith('#'))
print(f'  abstract: {w} words')

print('=== (d) em-dash count ===')
print(f'  em-dashes (—): {txt.count(chr(0x2014))}')

print('=== (e) stock-phrase scan ===')
for ph in ['deserves emphasis', 'deserves mention', 'Notably,', 'Importantly,', 'Furthermore,', 'Moreover,']:
    c = txt.count(ph)
    if c:
        print(f'  FOUND {c}x: {ph}')
print('  scan done (silence above = clean)')

print('=== (f) style invariants ===')
for ph, exp in [('triage aid, not an admission oracle', 2),
                ('automated gatekeeper for ICU beds', 1),
                ('just in case', 1),
                ('on the table', 1),
                ('silent-mode', 1),
                ('In conclusion', 1)]:
    c = txt.count(ph)
    flag = 'OK' if c == exp else f'EXPECTED {exp}'
    print(f'  "{ph}": {c}x {flag}')

print('=== (g) number-destination spot check (deleted from Discussion, must exist in Results/elsewhere) ===')
ressec = txt.split('## Results')[1].split('## Discussion')[0]
for label, pat, where in [
    ('5.0% vasoactive under-capture', '5.0%', 'Results'),
    ('36 missed escalations MOVER', '36 missed escalations', 'Results'),
    ('44.9% MOVER event rate', '44.9%', 'Results'),
    ('10.8% INSPIRE event rate', '10.8%', 'Results'),
    ('+0.0006 overlap inflation', '+0.0006', 'Results'),
    ('95.3%/97.7% subgroup NPV', '97.7%', 'Results'),
    ('98.9% BIS missingness', '98.9%', 'Limitations'),
    ('40.3% SASA missing', '40.3%', 'Discussion'),
    ('99.1% flowsheet coverage', '99.1%', 'both'),
    ('93.6% data-quality gate', '93.6%', 'Results'),
]:
    if where == 'Results':
        ok = pat in ressec
    elif where == 'Limitations':
        ok = pat in txt.split('Several limitations')[1]
    elif where == 'Discussion':
        ok = pat in txt.split('## Discussion')[1].split('## Methods')[0]
    else:
        ok = pat in txt
    print(f'  {label}: {"OK" if ok else "MISSING!"} ({where})')

print('=== (h) main-text word count ===')
lines = txt.split('\n')
secs, cur = {}, 'P'
for l in lines:
    if l.startswith('## '):
        cur = l.strip()
    secs.setdefault(cur, []).append(l)
main = sum(sum(len(x.split()) for x in secs[k] if not x.startswith('#'))
           for k in ['## Introduction', '## Results', '## Discussion', '## Methods'])
print(f'  main text: {main} (band 8,800-9,200: {"PASS" if 8800 <= main <= 9200 else "FAIL"})')
