# Figure 1 (design schematic) as true vector SVG — replicates the accepted layout
# with A/C/E/T partition labels. All text is real SVG <text> (editable).
# Font: Liberation Sans (Arial-metric). Canvas 1536x1024 (3:2).

W <- 1536; H <- 1024
HDR_FILL <- "#A9CCEF"   # muted blue header bars
BAR_FILL <- "#D9E8FA"   # lighter blue partition bar
BOX_FILL <- "#FFFFFF"
BORDER   <- "#000000"
ARROW    <- "#666666"
FONT     <- "Liberation Sans, Arial, sans-serif"

svg <- character(0)
add <- function(...) svg <<- c(svg, paste0(...))

esc <- function(s) gsub("&", "&amp;", gsub("<", "&lt;", gsub(">", "&gt;", s)))

rect_el <- function(x, y, w, h, fill, stroke = BORDER, sw = 1.5)
  add(sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s" stroke="%s" stroke-width="%.1f"/>',
              x, y, w, h, fill, stroke, sw))

text_el <- function(x, y, s, size = 22, bold = FALSE, fill = "#000000")
  add(sprintf('<text x="%.1f" y="%.1f" text-anchor="middle" font-family="%s" font-size="%.1f" font-weight="%s" fill="%s">%s</text>',
              x, y, FONT, size, if (bold) "bold" else "normal", fill, esc(s)))

box_el <- function(x, y, w, h, title, subs = character(0), tsize = 25, ssize = 21) {
  rect_el(x, y, w, h, BOX_FILL)
  cx <- x + w / 2
  n <- length(subs)
  # vertical centering of the text block
  line_h <- c(tsize * 1.25, rep(ssize * 1.3, n))
  total_h <- sum(line_h)
  ty <- y + h / 2 - total_h / 2 + line_h[1] * 0.75
  text_el(cx, ty, title, tsize, TRUE)
  if (n) for (k in seq_len(n)) {
    ty <- ty + line_h[k + 1]
    text_el(cx, ty, subs[k], ssize, FALSE)
  }
}

arrow_el <- function(x1, y1, x2, y2)
  add(sprintf('<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1.8" marker-end="url(#arr)"/>',
              x1, y1, x2, y2, ARROW))

path_arrow <- function(d)
  add(sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="1.8" marker-end="url(#arr)"/>', d, ARROW))

# ---- header ----
add(sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">', W, H, W, H))
add('<defs><marker id="arr" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L7,3 L0,6 Z" fill="#666666"/></marker></defs>')
add(sprintf('<rect width="%d" height="%d" fill="#FFFFFF"/>', W, H))

# ---- column header bars ----
hdr <- function(x, w, label) {
  rect_el(x, 182, w, 58, HDR_FILL)
  text_el(x + w / 2, 182 + 38, label, 26, TRUE)
}
hdr(30,  460, "Source: VitalDB development")
hdr(525, 485, "Transport to target datasets")
hdr(1045, 460, "Locked evaluation")

# ---- left column: source boxes ----
LX <- 65; LW <- 390
box_el(LX, 270, LW, 85, "5987 eligible operations", "4190 train / 1797 test")
box_el(LX, 405, LW, 85, "Full models, 71 inputs", "XGBoost + ridge LR, frozen")
box_el(LX, 540, LW, 85, "Masked-R16", "frozen weights, 55 inputs masked")
box_el(LX, 675, LW, 85, "Refit-R16, 16 inputs", "source-only refit on 4190 train")
# vertical chain arrows
arrow_el(260, 355, 260, 400)
arrow_el(260, 490, 260, 535)
arrow_el(260, 625, 260, 670)

# ---- middle column: datasets ----
MX <- 600; MW <- 370
box_el(MX, 370, MW, 95, "INSPIRE", "same institution, overlapping period")
box_el(MX, 540, MW, 95, "MOVER", "geographically external")

# fan arrows: each model box -> both datasets
arrow_el(455, 447, 595, 405)   # Full -> INSPIRE
arrow_el(455, 460, 595, 575)   # Full -> MOVER
arrow_el(455, 575, 595, 435)   # Masked -> INSPIRE
arrow_el(455, 582, 595, 587)   # Masked -> MOVER
arrow_el(455, 700, 595, 455)   # Refit -> INSPIRE
arrow_el(455, 712, 595, 615)   # Refit -> MOVER

# ---- partition bar ----
BX <- 480; BW <- 600; BY <- 710; BH <- 80
seg_w <- BW / 4
seg_labels <- list(c("A · 50%", "source audits"),
                   c("C · 15%", "local updating"),
                   c("E · 15%", "this evaluation"),
                   c("T · 20%", "earlier evaluation"))
for (k in 0:3) {
  rect_el(BX + k * seg_w, BY, seg_w, BH, BAR_FILL)
  text_el(BX + k * seg_w + seg_w / 2, BY + 32, seg_labels[[k + 1]][1], 22, TRUE)
  text_el(BX + k * seg_w + seg_w / 2, BY + 60, seg_labels[[k + 1]][2], 16, FALSE)
}
# MOVER -> partition bar
arrow_el(785, 635, 785, 705)

# ---- right column: locked evaluation ----
RX <- 1095; RW <- 385
box_el(RX, 270, RW, 85, "Lock order:", "source models → C maps → E")
box_el(RX, 405, RW, 85, "C-only updating:", "intercept-only or Platt")
box_el(RX, 540, RW, 120, "E evaluation: 6 configurations",
       c("identical operations, paired differences", "patient-cluster bootstrap"))
box_el(RX, 700, RW, 85, "SAS reference", "identical evaluable subsets")

# MOVER -> C-only updating (elbow)
path_arrow("M 970 587 L 1032 587 L 1032 447 L 1090 447")
# C-only updating -> E evaluation
arrow_el(1287, 490, 1287, 535)

add('</svg>')

out <- "/workspace/figzip/Figure_1_BMC_v3.svg"
writeLines(svg, out, useBytes = TRUE)
cat("written", out, "\n")
