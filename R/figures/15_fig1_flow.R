#!/usr/bin/env Rscript
# Figure 1: cohort flow diagram (VitalDB development + MOVER external validation)
# Code-drawn with grid for exact numbers. Output: SVG (editable text) + PNG.
suppressPackageStartupMessages({ library(grid); library(svglite) })

FF <- "Liberation Sans"

draw_fig <- function() {
  grid.newpage()
  # vertical stretch: map content band [0.371, 0.965] -> [0.10, 0.96]
  ymap <- function(y) 0.10 + (y - 0.371) * (0.86 / 0.594)
  hmap <- function(h) h * (0.86 / 0.594)
  # ---------- helpers ----------
  box <- function(x, y, w, h, label, fill = "white", lwd = 1.1, fs = 9.5,
                  fontface = "plain", lineheight = 1.15) {
    grid.rect(x, y, w, h, just = c("center", "center"),
              gp = gpar(fill = fill, col = "black", lwd = lwd))
    grid.text(label, x, y, just = c("center", "center"),
              gp = gpar(fontfamily = FF, fontsize = fs, fontface = fontface,
                        lineheight = lineheight))
  }
  arrow <- function(x, y0, y1) {
    grid.segments(x, y0, x, y1,
                  arrow = grid::arrow(type = "closed", length = unit(0.09, "inches")),
                  gp = gpar(lwd = 1.1))
  }
  header <- function(x, y, label) {
    grid.text(label, x, y, just = c("center", "center"),
              gp = gpar(fontfamily = FF, fontsize = 10.5, fontface = "bold",
                        lineheight = 1.2))
  }

  # ---------- left column: VitalDB ----------
  lx <- 0.25; lw <- 0.40
  header(lx, ymap(0.965), "VitalDB — development cohort\nSeoul National University Hospital, Korea (Aug 2016–Jun 2017)")
  box(lx, ymap(0.880), lw, hmap(0.062), "Non-cardiac surgical cases in VitalDB\nn = 6,388")
  arrow(lx, ymap(0.849), ymap(0.826))
  box(lx, ymap(0.795), lw, hmap(0.056), "Excluded 399: age <18 yr or\nnon-general anesthesia",
      fill = "grey92", fs = 8.8)
  arrow(lx, ymap(0.767), ymap(0.744))
  box(lx, ymap(0.713), lw, hmap(0.062), "Adult (\u226518 yr) general anesthesia\nn = 5,989")
  arrow(lx, ymap(0.682), ymap(0.659))
  box(lx, ymap(0.624), lw, hmap(0.064), "Excluded 2: monitoring coverage <50%\nor anesthesia duration <30 min",
      fill = "grey92", fs = 8.8)
  arrow(lx, ymap(0.592), ymap(0.569))
  box(lx, ymap(0.534), lw, hmap(0.068), "Development cohort\nn = 5,987; events 1,184 (19.8%)",
      fontface = "bold")
  # split arrows
  grid.segments(lx, ymap(0.500), lx, ymap(0.478), gp = gpar(lwd = 1.1))
  grid.segments(0.145, ymap(0.478), 0.355, ymap(0.478), gp = gpar(lwd = 1.1))
  grid.segments(0.145, ymap(0.478), 0.145, ymap(0.462),
                arrow = grid::arrow(type = "closed", length = unit(0.09, "inches")),
                gp = gpar(lwd = 1.1))
  grid.segments(0.355, ymap(0.478), 0.355, ymap(0.462),
                arrow = grid::arrow(type = "closed", length = unit(0.09, "inches")),
                gp = gpar(lwd = 1.1))
  box(0.145, ymap(0.415), 0.19, hmap(0.088), "Training set\n(first 70%, temporal)\nn = 4,190\nevents 823 (19.6%)", fs = 9)
  box(0.355, ymap(0.415), 0.19, hmap(0.088), "Internal test set\n(last 30%, temporal)\nn = 1,797\nevents 361 (20.1%)", fs = 9)

  # ---------- right column: MOVER ----------
  rx <- 0.74; rw <- 0.44
  header(rx, ymap(0.965), "MOVER — external validation cohort\nUC Irvine Medical Center, USA (Nov 2017–Aug 2023)")
  box(rx, ymap(0.880), rw, hmap(0.062), "Operation records in MOVER EPIC extract\nn = 65,728")
  arrow(rx, ymap(0.849), ymap(0.826))
  box(rx, ymap(0.788), rw, hmap(0.070), paste0("Excluded 16,334: non-general anesthesia, age <18 yr,\n",
                                   "cardiac surgery, anesthesia duration <30 min,\n",
                                   "\u226524 h, or missing"),
      fill = "grey92", fs = 8.8)
  arrow(rx, ymap(0.753), ymap(0.730))
  box(rx, ymap(0.688), rw, hmap(0.078), paste0("External validation cohort\n",
                                   "n = 49,394 (34,144 unique patients)\n",
                                   "events 22,159 (44.9%)"),
      fontface = "bold")
  arrow(rx, ymap(0.649), ymap(0.626))
  box(rx, ymap(0.588), rw, hmap(0.070), paste0("Usable intraoperative time series: 46,971 (95.1%)\n",
                                   "remaining 2,423 (4.9%) imputed at model level"),
      fill = "grey98", fs = 8.8)

  # ---------- footnote ----------
  grid.text("Event = composite of postoperative intensive care unit admission or in-hospital death.",
            0.5, 0.025, just = c("center", "center"),
            gp = gpar(fontfamily = FF, fontsize = 9, fontface = "italic"))
}

W <- 9; H <- 7.2
svglite("/workspace/fig1_flow.svg", width = W, height = H, fix_text_size = FALSE)
draw_fig(); dev.off()
png("/workspace/fig1_flow.png", width = W, height = H, units = "in", res = 300)
draw_fig(); dev.off()
cat("done\n")
