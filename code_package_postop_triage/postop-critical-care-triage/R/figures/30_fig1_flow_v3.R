#!/usr/bin/env Rscript
# Figure 1 (v3): cohort flow diagram — VitalDB development, INSPIRE temporal
# validation, MOVER external validation. Code-drawn with grid. SVG + PNG.
suppressPackageStartupMessages({ library(grid); library(svglite) })

FF <- "Liberation Sans"

draw_fig <- function() {
  grid.newpage()
  box <- function(x, y, w, h, label, fill = "white", lwd = 1.1, fs = 8.6,
                  fontface = "plain", lineheight = 1.15) {
    grid.rect(x, y, w, h, just = c("center", "center"),
              gp = gpar(fill = fill, col = "black", lwd = lwd))
    grid.text(label, x, y, just = c("center", "center"),
              gp = gpar(fontfamily = FF, fontsize = fs, fontface = fontface,
                        lineheight = lineheight))
  }
  arrow <- function(x, y0, y1) {
    grid.segments(x, y0, x, y1,
                  arrow = grid::arrow(type = "closed", length = unit(0.08, "inches")),
                  gp = gpar(lwd = 1.1))
  }
  header <- function(x, y, label) {
    grid.text(label, x, y, just = c("center", "center"),
              gp = gpar(fontfamily = FF, fontsize = 9.6, fontface = "bold",
                        lineheight = 1.2))
  }

  cw <- 0.295
  x1 <- 0.168; x2 <- 0.500; x3 <- 0.832

  # ---------- column 1: VitalDB (development) ----------
  header(x1, 0.955, "VitalDB — development\nSNUH, Korea (Aug 2016–Jun 2017)")
  box(x1, 0.855, cw, 0.068, "Non-cardiac surgical cases in VitalDB\nn = 6,388")
  arrow(x1, 0.821, 0.799)
  box(x1, 0.756, cw, 0.068, "Excluded 399: age <18 yr or\nnon-general anesthesia",
      fill = "grey92", fs = 8.2)
  arrow(x1, 0.722, 0.700)
  box(x1, 0.657, cw, 0.068, "Adult (\u226518 yr) general anesthesia\nn = 5,989")
  arrow(x1, 0.623, 0.601)
  box(x1, 0.552, cw, 0.080, "Excluded 2: monitoring coverage <50%\nor anesthesia duration <30 min",
      fill = "grey92", fs = 8.2)
  arrow(x1, 0.512, 0.490)
  box(x1, 0.442, cw, 0.078, "Development cohort\nn = 5,987; events 1,184 (19.8%)",
      fontface = "bold", fs = 8.8)
  # split arrows
  grid.segments(x1, 0.403, x1, 0.385, gp = gpar(lwd = 1.1))
  grid.segments(x1 - 0.075, 0.385, x1 + 0.075, 0.385, gp = gpar(lwd = 1.1))
  grid.segments(x1 - 0.075, 0.385, x1 - 0.075, 0.371,
                arrow = grid::arrow(type = "closed", length = unit(0.08, "inches")),
                gp = gpar(lwd = 1.1))
  grid.segments(x1 + 0.075, 0.385, x1 + 0.075, 0.371,
                arrow = grid::arrow(type = "closed", length = unit(0.08, "inches")),
                gp = gpar(lwd = 1.1))
  box(x1 - 0.075, 0.305, 0.145, 0.118,
      "Training set\n(first 70%, temporal)\nn = 4,190\nevents 823 (19.6%)", fs = 8.0)
  box(x1 + 0.075, 0.305, 0.145, 0.118,
      "Internal test set\n(last 30%, temporal)\nn = 1,797\nevents 361 (20.1%)", fs = 8.0)

  # ---------- column 2: INSPIRE (temporal validation) ----------
  header(x2, 0.955, "INSPIRE — temporal validation\nSNUH, Korea (Jan 2011–Dec 2020)")
  box(x2, 0.855, cw, 0.068, "Operation records in INSPIRE\nn = 130,960")
  arrow(x2, 0.821, 0.799)
  box(x2, 0.756, cw, 0.068,
      "Excluded 32,327: non-general anesthesia,\nage <18 yr, cardiac surgery, or\nanesthesia duration <30 min",
      fill = "grey92", fs = 8.2)
  arrow(x2, 0.722, 0.700)
  box(x2, 0.652, cw, 0.078,
      "Adult general-anesthesia\nnon-cardiac surgeries\nn = 98,633; events 15,186 (15.4%)")
  arrow(x2, 0.613, 0.591)
  box(x2, 0.542, cw, 0.080,
      "Excluded 2,437: operations linked to the\nVitalDB development cohort\n(case_id 1\u20136,388)",
      fill = "grey92", fs = 8.2)
  arrow(x2, 0.502, 0.480)
  box(x2, 0.432, cw, 0.078,
      "Temporal validation cohort\nn = 96,196 (78,305 unique patients)\nevents 14,627 (15.2%)",
      fontface = "bold", fs = 8.8)

  # ---------- column 3: MOVER (external validation) ----------
  header(x3, 0.955, "MOVER — external validation\nUC Irvine Medical Center, USA (Nov 2017–Aug 2023)")
  box(x3, 0.855, cw, 0.068, "Operation records in MOVER EPIC extract\nn = 65,728")
  arrow(x3, 0.821, 0.799)
  box(x3, 0.748, cw, 0.084,
      "Excluded 16,334: non-general anesthesia,\nage <18 yr, cardiac surgery, anesthesia\nduration <30 min, \u226524 h, or missing",
      fill = "grey92", fs = 8.2)
  arrow(x3, 0.706, 0.684)
  box(x3, 0.628, cw, 0.094,
      "External validation cohort\nn = 49,394 (34,144 unique patients)\nevents 22,159 (44.9%)",
      fontface = "bold", fs = 8.8)
  arrow(x3, 0.581, 0.559)
  box(x3, 0.510, cw, 0.080,
      "Usable intraoperative time series: 46,971\n(95.1%); remaining 2,423 (4.9%)\nimputed at model level",
      fill = "grey98", fs = 8.2)

  # ---------- footnote ----------
  grid.text("Event = composite of postoperative intensive care unit admission or in-hospital death.",
            0.5, 0.030, just = c("center", "center"),
            gp = gpar(fontfamily = FF, fontsize = 9, fontface = "italic"))
}

W <- 10; H <- 6.8
svglite("/workspace/figures/fig1_flow_v3.svg", width = W, height = H, fix_text_size = FALSE)
draw_fig(); dev.off()
png("/workspace/figures/fig1_flow_v3.png", width = W, height = H, units = "in", res = 300)
draw_fig(); dev.off()
cat("fig1 v3 saved\n")
