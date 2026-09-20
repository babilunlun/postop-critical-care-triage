#!/usr/bin/env bash
# 24_inspire_download.sh — download INSPIRE v1.4.2 from PhysioNet (credentialed)
# Credentials read from /workspace/.netrc (chmod 600); never written to outputs.
set -euo pipefail
cd /workspace/inspire
export HOME=/workspace   # wget reads $HOME/.netrc

BASE="https://physionet.org/files/inspire/1.4.2"
for f in SHA256SUMS.txt CHANGELOG.txt LICENSE.txt icd10_excluded.csv \
         operations.csv.gz labs.csv.gz vitals.csv.gz medications.csv.gz diagnosis.csv.gz; do
  if [[ -f "$f" ]]; then echo "exists: $f"; else echo "downloading: $f"; wget -q "$BASE/$f"; fi
done
# ward_vitals.csv.gz intentionally skipped (postoperative ward data not used)

echo "== checksum verify =="
sha256sum -c SHA256SUMS.txt 2>&1 | grep -v ward_vitals || true
ls -la
