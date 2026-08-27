#!/usr/bin/env python3
"""Split multi-panel svglite SVGs into single-panel SVG+PNG files.
- Crops by setting a new viewBox and wrapping content in a crop clipPath.
- Physically drops elements whose bbox does not intersect the crop box.
- Removes panel-letter tags (a/b/c/d) so reassembled figures carry no letters.
- Rasterizes with rsvg-convert at 300 dpi.
"""
import re, os, base64, subprocess, sys
import xml.etree.ElementTree as ET

NS = "http://www.w3.org/2000/svg"
XLINK = "http://www.w3.org/1999/xlink"
ET.register_namespace("", NS)
ET.register_namespace("xlink", XLINK)

OUT = "/mnt/results/04_manuscript/figures_single_panels"
os.makedirs(OUT, exist_ok=True)

def local(tag):
    return tag.split("}")[1] if "}" in tag else tag

def decode_clip(cid):
    """svglite clipPath ids: 'cp' + base64('xmin|xmax|ymin|ymax')."""
    try:
        s = base64.b64decode(cid[2:] + "=" * (-len(cid[2:]) % 4)).decode()
        parts = [float(v) for v in s.split("|")]
        if len(parts) == 4:
            x0, x1, y0, y1 = parts
            return (x0, y0, x1 - x0, y1 - y0)
    except Exception:
        return None
    return None

def num(v, default=None):
    try:
        return float(v)
    except (TypeError, ValueError):
        return default

def text_bbox(el):
    x = num(el.get("x")); y = num(el.get("y"))
    if x is None or y is None:
        return None
    s = "".join(el.itertext())
    style = el.get("style", "")
    m = re.search(r"font-size:\s*([\d.]+)", style)
    fs = float(m.group(1)) if m else 10.0
    w = len(s) * fs * 0.62 + 4
    anc = el.get("text-anchor", "start")
    if anc == "middle":
        x0, x1 = x - w / 2, x + w / 2
    elif anc == "end":
        x0, x1 = x - w, x + 2
    else:
        x0, x1 = x - 2, x + w
    h = fs * 1.4
    if el.get("transform"):  # rotated: allow vertical extent too
        return (x0 - w / 2, y - w / 2, w, w) if False else (min(x0, x - 4), y - w, max(w, x1 - x0) + 8, w + h)
    return (x0, y - h, x1 - x0, h + 4)

def path_bbox(d):
    if re.search(r"[Aa]", d):
        return None
    vals = [float(v) for v in re.findall(r"-?\d+\.?\d*(?:e-?\d+)?", d)]
    if len(vals) < 2 or len(vals) % 2:
        return None
    xs, ys = vals[0::2], vals[1::2]
    return (min(xs), min(ys), max(xs) - min(xs), max(ys) - min(ys))

def elem_bbox(el, clipmap):
    tag = local(el.tag)
    cp = el.get("clip-path")
    if cp:
        m = re.search(r"#([^)]+)", cp)
        if m and m.group(1) in clipmap:
            return clipmap[m.group(1)]
    if tag == "text":
        return text_bbox(el)
    if tag == "rect":
        x, y = num(el.get("x"), 0.0), num(el.get("y"), 0.0)
        w, h = el.get("width"), el.get("height")
        if w and str(w).endswith("%"):
            return None  # full-canvas background: keep everywhere
        w, h = num(w), num(h)
        if None in (w, h):
            return None
        return (x, y, w, h)
    if tag == "line":
        x1, x2 = num(el.get("x1")), num(el.get("x2"))
        y1, y2 = num(el.get("y1")), num(el.get("y2"))
        if None in (x1, x2, y1, y2):
            return None
        return (min(x1, x2), min(y1, y2), abs(x2 - x1) + 1, abs(y2 - y1) + 1)
    if tag == "circle":
        cx, cy, r = num(el.get("cx")), num(el.get("cy")), num(el.get("r"), 1.0)
        if None in (cx, cy):
            return None
        return (cx - r, cy - r, 2 * r, 2 * r)
    if tag in ("polyline", "polygon"):
        pts = [float(v) for v in re.findall(r"-?\d+\.?\d*", el.get("points", ""))]
        if len(pts) < 2:
            return None
        xs, ys = pts[0::2], pts[1::2]
        return (min(xs), min(ys), max(xs) - min(xs), max(ys) - min(ys))
    if tag == "path":
        return path_bbox(el.get("d", ""))
    return None  # unknown -> keep

def intersects(bb, crop, margin=12.0):
    if bb is None:
        return True
    x, y, w, h = bb
    cx, cy, cw, ch = crop
    return not (x + w < cx - margin or x > cx + cw + margin or
                y + h < cy - margin or y > cy + ch + margin)

PANEL_TAG = re.compile(r"^[a-d]$")

def is_panel_tag(el):
    if local(el.tag) != "text":
        return False
    s = ("".join(el.itertext())).strip()
    if not PANEL_TAG.match(s):
        return False
    style = el.get("style", "")
    m = re.search(r"font-size:\s*([\d.]+)", style)
    fs = float(m.group(1)) if m else 0
    return fs >= 12  # panel tags: single lowercase a-d at large font size

def filter_tree(el, crop, clipmap, stats):
    """Recursively drop children outside crop; drop panel tags. Returns kept-child count."""
    kept = []
    for ch in list(el):
        tag = local(ch.tag)
        if tag == "defs":
            kept.append(ch); continue
        if is_panel_tag(ch):
            stats["tags"] += 1
            continue
        bb = elem_bbox(ch, clipmap)
        if not intersects(bb, crop):
            stats["dropped"] += 1
            continue
        if tag == "g":
            filter_tree(ch, crop, clipmap, stats)
        kept.append(ch)
    el[:] = kept

def collect_clips(root):
    clipmap = {}
    for cp in root.iter(f"{{{NS}}}clipPath"):
        cid = cp.get("id")
        r = cp.find(f"{{{NS}}}rect")
        if cid and r is not None:
            x, y = num(r.get("x"), 0.0), num(r.get("y"), 0.0)
            w, h = num(r.get("width")), num(r.get("height"))
            if w and h:
                clipmap[cid] = (x, y, w, h)
        elif cid:
            bb = decode_clip(cid)
            if bb:
                clipmap[cid] = bb
    return clipmap

def split_svg(src, panels, out_prefix, dpi=300):
    """panels: list of (suffix, (x, y, w, h)) in SVG pt units."""
    raw = open(src, encoding="utf-8").read()
    outputs = []
    for suffix, crop in panels:
        root = ET.fromstring(raw)
        clipmap = collect_clips(root)
        stats = {"tags": 0, "dropped": 0}
        for ch in list(root):
            if local(ch.tag) == "defs":
                continue
            filter_tree(ch, crop, clipmap, stats)
        # wrap everything (except defs) in a crop-clip group
        x, y, w, h = crop
        defs = [ch for ch in list(root) if local(ch.tag) == "defs"]
        body = [ch for ch in list(root) if local(ch.tag) != "defs"]
        for ch in list(root):
            root.remove(ch)
        newdefs = ET.SubElement(root, f"{{{NS}}}defs")
        for d in defs:
            for sub in list(d):
                newdefs.append(sub)
        clip = ET.SubElement(newdefs, f"{{{NS}}}clipPath", {"id": "cropclip"})
        ET.SubElement(clip, f"{{{NS}}}rect", {"x": str(x), "y": str(y),
                                              "width": str(w), "height": str(h)})
        g = ET.SubElement(root, f"{{{NS}}}g", {"clip-path": "url(#cropclip)"})
        for b in body:
            g.append(b)
        root.set("width", f"{w:.2f}pt")
        root.set("height", f"{h:.2f}pt")
        root.set("viewBox", f"{x:.2f} {y:.2f} {w:.2f} {h:.2f}")
        svg_out = os.path.join(OUT, f"{out_prefix}{suffix}.svg")
        png_out = os.path.join(OUT, f"{out_prefix}{suffix}.png")
        ET.ElementTree(root).write(svg_out, encoding="unicode", xml_declaration=True)
        zoom = dpi / 72.0
        # rsvg-convert uses random-access writes: render locally, then cp to S3 mount
        tmp_png = f"/workspace/tmp_{out_prefix}{suffix}.png"
        subprocess.run(["rsvg-convert", "-z", f"{zoom:.4f}", "-f", "png",
                        "-o", tmp_png, svg_out], check=True)
        subprocess.run(["cp", tmp_png, png_out], check=True)
        os.remove(tmp_png)
        outputs.append((svg_out, png_out, stats.copy()))
        print(f"  {out_prefix}{suffix}: crop={crop} tags_removed={stats['tags']} "
              f"groups_dropped={stats['dropped']} svg={os.path.getsize(svg_out)//1024}KB")
    return outputs

# ---------------- figure configurations (crop boxes measured from SVG cells) ----
JOBS = [
    # Fig 2 (792 x 273.6): cells 5.5-211.85 | 211.85-418.96 | 418.96-786.5
    ("/mnt/results/01_internal_validation/fig_internal_validation.svg", "fig2", [
        ("a", (0, 0, 211.85, 273.6)),
        ("b", (211.85, 0, 207.11, 273.6)),
        ("c", (418.96, 0, 373.04, 273.6)),
    ]),
    # Fig 4 (648 x 590.4): 2x2, split x=334.1, y=306.6
    ("/mnt/results/04_manuscript/fig_validation_combined_v4.svg", "fig4", [
        ("a", (0, 0, 334.1, 306.6)),
        ("b", (334.1, 0, 313.9, 306.6)),
        ("c", (0, 306.6, 334.1, 283.8)),
        ("d", (334.1, 306.6, 313.9, 283.8)),
    ]),
    # Fig 5 (597.6 x 331.2): INSPIRE x-label 'Uncomplicated' ends at 322.7 -> split 323.5
    ("/mnt/results/06_icu_course/fig5_risk_by_category.svg", "fig5", [
        ("a", (0, 0, 323.5, 331.2)),
        ("b", (323.5, 0, 274.1, 331.2)),
    ]),
    # new Fig S1 = calibration decomposition (576 x 288): MOVER y-labels span 296-311 -> split 294.5
    ("/mnt/results/07_revision_analyses/figures/fig_b2_calibration_decomposition.svg", "figS1", [
        ("a", (0, 0, 294.5, 288.0)),
        ("b", (294.5, 0, 281.5, 288.0)),
    ]),
    # new Fig S4 = fairness (482.4 x 403.2): 3 rows x 2 cols
    ("/mnt/results/07_revision_analyses/figures/fig_b5_fairness.svg", "figS4", [
        ("a", (0, 0, 241.3, 136.25)),
        ("b", (241.3, 0, 241.1, 136.25)),
        ("c", (0, 136.25, 241.3, 122.15)),
        ("d", (241.3, 136.25, 241.1, 122.15)),
        ("e", (0, 258.4, 241.3, 144.8)),
        ("f", (241.3, 258.4, 241.1, 144.8)),
    ]),
    # figS5/figS6 are REGENERATED natively (regen_single_panels.R), not cropped:
    # figS5 panel d y-labels overlap panel c's cell; native regeneration is cleaner.
]

if __name__ == "__main__":
    only = sys.argv[1] if len(sys.argv) > 1 else None
    for src, prefix, panels in JOBS:
        if only and prefix != only:
            continue
        print(f"== {prefix} <- {os.path.basename(src)}")
        split_svg(src, panels, prefix)
    print("done ->", OUT)
