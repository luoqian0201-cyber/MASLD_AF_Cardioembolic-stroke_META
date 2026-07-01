# ============================================================
# Figure 6 — Evidence for an AF-associated pathway linking
#             MASLD and stroke risk (3-panel vertical)
# Panel A: AF adjustment explains part of between-study
#          heterogeneity (meta-regression evidence)
# Panel B: Interaction by baseline AF (Jang 2026)
# Panel C: AF-associated pathway: mechanism to evidence
# Nature Communications / Lancet specialty target
# ============================================================
library(meta)
library(metafor)
library(readxl)

# ============================================================
# Color palette (project standard)
# ============================================================
org      <- "#E8611A"   # No-AF / MASLD / liver
org_l    <- "#FEF0E6"
blue     <- "#2878B5"   # With-AF / AF pathway / heart
blue_l   <- "#EBF3FA"
red      <- "#C0392B"   # stroke / brain
red_l    <- "#FDEDEC"
green    <- "#27AE60"   # protective
green_l  <- "#E9F7EF"
purple   <- "#8E44AD"   # meta-regression / attenuation
purple_l <- "#F4ECF7"
grey_d   <- "#2C3E50"   # dark text
grey_m   <- "#7F8C8D"   # medium text
grey_l   <- "#E5E7E9"   # light border
grey_bg  <- "#F8F9FA"   # panel background
col_text <- "#1A1A1A"

# ============================================================
# Helper functions (from graphical_abstract.R)
# ============================================================

# Rounded rectangle
rrect <- function(x, y, w, h, r = 1.2, col = "white", border = NA, lwd = 1) {
  n <- 15
  angles <- seq(0, pi/2, length.out = n)
  xa <- cos(angles) * r
  ya <- sin(angles) * r
  xx <- c(x + r, x + w - r, x + w - r + xa,
          x + w, x + w, x + w - r + rev(ya),
          x + w - r, x + r, x + r - ya,
          x, x, x + r - rev(xa))
  yy <- c(y, y, y + r - ya,
          y + r, y + h - r, y + h - r + rev(xa),
          y + h, y + h, y + h - r + xa,
          y + h - r, y + r, y + r - rev(ya))
  polygon(xx, yy, col = col, border = border, lwd = lwd)
}

# Organ icons (polished, scalable)
draw_liver <- function(cx, cy, s = 1, col = org) {
  x <- cx + s * c(-3, -1.5, 0.5, 3.5, 3.5, 2, 0, -2.5, -3.5, -3.5) * 0.9
  y <- cy + s * c(0.5, 2.5, 3, 1, -1, -3, -3.5, -2.5, -1, 0.5) * 0.9
  polygon(x, y, col = org_l, border = col, lwd = 2.2)
  set.seed(42)
  points(cx + s * runif(10, -2.2, 2.2), cy + s * runif(10, -2.2, 2.2),
         pch = 16, col = "#F4C430", cex = runif(10, 0.4, 1.0))
  text(cx, cy, "MASLD", font = 2, cex = 0.9, col = org)
}

draw_heart <- function(cx, cy, s = 1, col = blue) {
  t <- seq(0, 2*pi, length.out = 200)
  hx <- cx + s * 3.5 * 16 * sin(t)^3 / 16
  hy <- cy + s * 3.5 * (13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)) / 16
  polygon(hx, hy, col = blue_l, border = col, lwd = 2.2)
  ex <- seq(cx - s*1.8, cx + s*2, length.out = 80)
  ey <- cy + s * sin(ex * 2.5) * 1.5
  lines(ex, ey, col = col, lwd = 2.5)
  text(cx, cy - s*0.5, "AF", font = 2, cex = 0.9, col = col)
}

draw_brain <- function(cx, cy, s = 1, col = red) {
  x <- cx + s * c(-3, -2.8, -1.5, 0.5, 2, 3, 3, 1.5, 0, -1.5, -3) * 1.1
  y <- cy + s * c(0, 1.5, 3, 3.3, 2.5, 1, -1, -3, -3.5, -3, -1) * 1.1
  polygon(x, y, col = red_l, border = col, lwd = 2.2)
  for (gy in seq(cy + s*0.5, cy + s*2.5, length.out = 3)) {
    lines(c(cx - s*2.2, cx + s*2.2), c(gy, gy), col = col, lwd = 0.8, lty = 3)
  }
  text(cx, cy - s*0.3, "⚡", cex = 2, col = red)
}

small_liver <- function(cx, cy, col = org)    { draw_liver(cx, cy, s = 0.6, col = col) }
small_heart <- function(cx, cy, col = blue)   { draw_heart(cx, cy, s = 0.6, col = col) }
small_brain <- function(cx, cy, col = red)    { draw_brain(cx, cy, s = 0.6, col = col) }

# ============================================================
# Data pipeline — k=21 standard (LOCKED)
# ============================================================
data <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx",
                   sheet = "Main Meta Data")
data$HR <- as.numeric(data$HR)

data_main <- data
data_main <- subset(data_main, !grepl("MAFLD only", data_main$study))
data_main <- subset(data_main, !(study == "Park et al." & year == 2022))
data_main <- subset(data_main, !(study == "Kim et al. (B.S. Kim)" & year == 2025))
data_main$study_label <- make.unique(
  paste0(data_main$study, " (", data_main$year, ")"), sep = " #")
data_main$TE  <- log(data_main$HR)
data_main$seTE <- (log(data_main$upperCI) - log(data_main$lowerCI)) / 3.92

# Study characteristics
dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx",
                 sheet = "Study_Characteristics")
dc <- dc[-1, ]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label), ]

# Merge
merged <- merge(data_main[, c("study_label", "TE", "seTE")],
                dc[, c("study_label", "adjusted_AF", "HR_MASLD_to_AF",
                       "Mediation_Value", "AF_excluded")],
                by = "study_label", all.x = TRUE)
merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)

# Meta-regression model
rma_af <- rma(yi = TE, sei = seTE, mods = ~ adj_af,
              data = merged, method = "REML")

# Predicted HRs (ALL computed — no hardcoded HR values)
pred_no  <- predict(rma_af, newmods = c(0))   # Without AF adjustment
pred_yes <- predict(rma_af, newmods = c(1))   # With AF adjustment

# Group counts
n_no_af  <- sum(!merged$adj_af, na.rm = TRUE)
n_yes_af <- sum(merged$adj_af, na.rm = TRUE)

# Study bubble weights
merged$weight_af <- 1 / (merged$seTE^2 + rma_af$tau2)
merged$cex_af <- sqrt(merged$weight_af) / max(sqrt(merged$weight_af)) * 2.5 + 0.7

# ============================================================
# Hardcoded values — only from published paper, not in Excel
# ============================================================
jang_noaf  <- 1.10    # Jang 2026: No baseline AF subgroup
jang_af    <- 1.03    # Jang 2026: With baseline AF subgroup
jang_af_lo <- 0.95
jang_af_up <- 1.12

ohno_hr    <- 1.51    # Ohno 2023: MASLD → incident AF
ohno_lo    <- 1.46
ohno_up    <- 1.57

# ============================================================
# FIGURE 6 — Single multi-panel TIFF
# ============================================================
tiff("figures/Figure6_AF_pathway.tiff",
     width = 14, height = 20, units = "in", res = 600)

layout(matrix(c(1, 2, 3), nrow = 3), heights = c(1, 0.75, 1.1))
par(bg = "#FFFFFF")

# ============================================================
# PANEL A — Meta-regression Evidence
# "AF adjustment explains part of between-study heterogeneity"
# ============================================================
par(mar = c(5, 8, 6, 6))

plot(NA, NA,
     xlim = c(0.65, 2.3), ylim = c(0.5, 8),
     xlab = "", ylab = "", axes = FALSE)

# Background
rect(0.63, 0.3, 2.32, 8.05, col = grey_bg, border = "gray90", lwd = 0.5)

# Reference line
abline(v = 1, col = "gray80", lty = 2, lwd = 1.5)

# ---- Panel label + Title ----
text(0.66, 7.88, "A", cex = 2.3, font = 2, col = org, xpd = NA)
text(1.48, 7.88, "AF adjustment explains part of between-study heterogeneity",
     cex = 1.2, font = 2, col = col_text)
text(1.48, 7.35, "Meta-regression: predicted HRs from random-effects model with AF adjustment as moderator",
     cex = 0.62, col = grey_m)
segments(1.48, 7.15, 1.48, 7.22, col = grey_m, lwd = 0.5)

# ---- Stats Box (prominent, top-center) ----
rrect(0.70, 5.55, 1.55, 1.5, r = 1.5, col = "white", border = purple, lwd = 2)
text(1.475, 6.78, "Meta-Regression: AF Adjustment", cex = 0.88, font = 2, col = purple)
text(1.475, 6.28, sprintf("R² = %.1f%%", rma_af$R2), cex = 1.3, font = 2, col = purple)
text(1.475, 5.90, sprintf("p = %.3f", rma_af$QMp), cex = 0.95, font = 2, col = grey_d)
text(1.475, 5.65, sprintf("β = %.3f", rma_af$beta[2]), cex = 0.72, col = grey_m)

# ---- WITHOUT AF group (y = 4.6) ----
y_noaf <- 4.6
# Diamond: 7-point polygon
d_x_no <- c(exp(pred_no$ci.lb), exp(pred_no$pred), exp(pred_no$pred),
            exp(pred_no$ci.ub), exp(pred_no$pred), exp(pred_no$pred),
            exp(pred_no$ci.lb))
d_y_no <- c(y_noaf, y_noaf + 0.22, y_noaf, y_noaf,
            y_noaf - 0.22, y_noaf, y_noaf)
polygon(d_x_no, d_y_no, col = adjustcolor(org, 0.55),
        border = org, lwd = 2.5)
segments(exp(pred_no$ci.lb), y_noaf, exp(pred_no$ci.ub), y_noaf,
         col = org, lwd = 3.5, lend = 1)

text(0.66, y_noaf, "Without AF\nAdjustment",
     cex = 1.0, font = 2, col = org, pos = 4)
text(2.28, y_noaf,
     sprintf("k=%d   HR=%.2f [%.2f–%.2f]", n_no_af,
             exp(pred_no$pred), exp(pred_no$ci.lb), exp(pred_no$ci.ub)),
     cex = 0.78, col = grey_d, pos = 2)

# Study bubbles (No AF)
set.seed(42)
noaf_idx <- which(!merged$adj_af)
for (i in noaf_idx) {
  points(exp(merged$TE[i]), y_noaf + jitter(0, amount = 0.20),
         pch = 21, cex = merged$cex_af[i],
         col = adjustcolor(org, 0.40),
         bg = adjustcolor(org, 0.18), lwd = 0.7)
}

# ---- WITH AF group (y = 2.4) ----
y_yesaf <- 2.4
d_x_yes <- c(exp(pred_yes$ci.lb), exp(pred_yes$pred), exp(pred_yes$pred),
             exp(pred_yes$ci.ub), exp(pred_yes$pred), exp(pred_yes$pred),
             exp(pred_yes$ci.lb))
d_y_yes <- c(y_yesaf, y_yesaf + 0.22, y_yesaf, y_yesaf,
             y_yesaf - 0.22, y_yesaf, y_yesaf)
polygon(d_x_yes, d_y_yes, col = adjustcolor(blue, 0.55),
        border = blue, lwd = 2.5)
segments(exp(pred_yes$ci.lb), y_yesaf, exp(pred_yes$ci.ub), y_yesaf,
         col = blue, lwd = 3.5, lend = 1)

text(0.66, y_yesaf, "With AF\nAdjustment",
     cex = 1.0, font = 2, col = blue, pos = 4)
text(2.28, y_yesaf,
     sprintf("k=%d   HR=%.2f [%.2f–%.2f]", n_yes_af,
             exp(pred_yes$pred), exp(pred_yes$ci.lb), exp(pred_yes$ci.ub)),
     cex = 0.78, col = grey_d, pos = 2)

# Study bubbles (Yes AF)
yesaf_idx <- which(merged$adj_af)
for (i in yesaf_idx) {
  points(exp(merged$TE[i]), y_yesaf + jitter(0, amount = 0.20),
         pch = 21, cex = merged$cex_af[i] + 0.5,
         col = blue,
         bg = adjustcolor(blue, 0.30), lwd = 1.0)
}

# ---- Attenuation annotation (right side, between groups) ----
rrect(1.55, 2.92, 0.70, 1.35, r = 1.2,
      col = adjustcolor("white", 0.90), border = purple, lwd = 1.2)
text(1.90, 4.07, "AF adjustment", cex = 0.65, font = 2, col = purple)
text(1.90, 3.80, "explains part of", cex = 0.60, font = 3, col = grey_m)
text(1.90, 3.55, "between-study", cex = 0.60, font = 3, col = grey_m)
text(1.90, 3.30, "heterogeneity", cex = 0.60, font = 3, col = grey_m)
text(1.90, 3.05, sprintf("β = %.3f", rma_af$beta[2]), cex = 0.55, col = grey_m)

# ---- X-axis ----
axis(1, at = c(0.9, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0),
     labels = c("0.9", "1.0", "1.2", "1.4", "1.6", "1.8", "2.0"),
     cex.axis = 0.85, col = "gray60", col.axis = col_text)
mtext("Hazard Ratio", side = 1, line = 3.2, cex = 1.0, col = col_text)

# ---- Group labels on Y-axis ----
mtext("Not adjusted for AF", side = 2, at = y_noaf, line = 5.5,
      cex = 0.72, col = org, font = 2, las = 1)
mtext("Adjusted for AF", side = 2, at = y_yesaf, line = 5.5,
      cex = 0.72, col = blue, font = 2, las = 1)

# Legend
legend("bottomleft",
       legend = c("Study (not AF-adjusted)", "Study (AF-adjusted)"),
       pch = 21, pt.bg = c(adjustcolor(org, 0.18), adjustcolor(blue, 0.30)),
       col = c(adjustcolor(org, 0.40), blue),
       pt.cex = 1.5, pt.lwd = c(0.7, 1.0),
       bty = "n", cex = 0.62, inset = c(0.01, 0.02))

# ============================================================
# PANEL B — AF Subgroup Interaction (Jang 2026)
# "Interaction by baseline AF"
# NO ARROWS — this is effect modification, not causation
# ============================================================
par(mar = c(5, 6, 6, 4))

plot(NA, NA,
     xlim = c(0.68, 1.55), ylim = c(0.5, 5.5),
     xlab = "", ylab = "", axes = FALSE)

# Background
rect(0.66, 0.3, 1.57, 5.55, col = grey_bg, border = "gray90", lwd = 0.5)

# Reference line
abline(v = 1, col = "gray80", lty = 2, lwd = 1.5)

# ---- Panel label + Title ----
text(0.69, 5.35, "B", cex = 2.3, font = 2, col = blue, xpd = NA)
text(1.12, 5.35, "Interaction by baseline AF",
     cex = 1.2, font = 2, col = col_text)
text(1.12, 4.95, "Jang 2026  |  Effect modification by baseline AF status — patient-level evidence",
     cex = 0.60, col = grey_m)
segments(1.12, 4.80, 1.12, 4.86, col = grey_m, lwd = 0.5)

# ---- Row 1: Without Baseline AF (y = 3.9) ----
y1 <- 3.9
# Decorative diamond (fixed width, no CI available from source)
d1x <- c(1.06, 1.10, 1.10, 1.14, 1.10, 1.10, 1.06)
d1y <- c(y1, y1 + 0.22, y1, y1, y1 - 0.22, y1, y1)
polygon(d1x, d1y, col = adjustcolor(org, 0.55),
        border = org, lwd = 2.5)

text(0.70, y1, "Without baseline AF", cex = 1.05, font = 2, col = org, pos = 4)
text(1.52, y1, "HR = 1.10", cex = 0.90, col = org, font = 2, pos = 2)
# Note if CI unavailable
text(1.52, y1 - 0.35, "(CI unavailable)", cex = 0.48, col = grey_m, pos = 2, font = 3)

# ---- Row 2: With Baseline AF (y = 2.0) ----
y2 <- 2.0
d2x <- c(jang_af_lo, jang_af, jang_af, jang_af_up,
         jang_af, jang_af, jang_af_lo)
d2y <- c(y2, y2 + 0.22, y2, y2, y2 - 0.22, y2, y2)
polygon(d2x, d2y, col = adjustcolor(blue, 0.55),
        border = blue, lwd = 2.5)
# Full CI whisker
segments(jang_af_lo, y2, jang_af_up, y2, col = blue, lwd = 3.5, lend = 1)

text(0.70, y2, "With baseline AF", cex = 1.05, font = 2, col = blue, pos = 4)
text(1.52, y2,
     sprintf("HR = %.2f [%.2f–%.2f]", jang_af, jang_af_lo, jang_af_up),
     cex = 0.90, col = blue, font = 2, pos = 2)

# ---- Effect Modification annotation (NO arrow, NO "1.10→1.03") ----
rrect(1.16, 2.35, 0.35, 1.1, r = 1.2,
      col = adjustcolor("white", 0.90), border = purple, lwd = 1.2)
text(1.335, 3.25, "Effect", cex = 0.68, font = 2, col = purple)
text(1.335, 3.00, "modification", cex = 0.68, font = 2, col = purple)
text(1.335, 2.72, "by baseline AF", cex = 0.55, font = 3, col = grey_m)
text(1.335, 2.50, "p-interaction", cex = 0.50, col = grey_m)

# ---- X-axis ----
axis(1, at = c(0.9, 1.0, 1.1, 1.2, 1.3, 1.4),
     labels = c("0.9", "1.0", "1.1", "1.2", "1.3", "1.4"),
     cex.axis = 0.90, col = "gray60", col.axis = col_text)
mtext("Hazard Ratio (95% CI)", side = 1, line = 3.2, cex = 1.0, col = col_text)

# Y-axis group labels
mtext("Without\nbaseline AF", side = 2, at = y1, line = 3.5,
      cex = 0.68, col = org, font = 2, las = 1)
mtext("With\nbaseline AF", side = 2, at = y2, line = 3.5,
      cex = 0.68, col = blue, font = 2, las = 1)

# ============================================================
# PANEL C — AF-associated pathway: mechanism to evidence
# Left: mechanism cascade | Right: 3 evidence boxes
# ============================================================
par(mar = c(2, 2, 6, 2))

plot(NA, NA,
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "", ylab = "", axes = FALSE)

# Background
rect(0, 0, 100, 100, col = grey_bg, border = "gray90", lwd = 0.5)

# ---- Panel label + Title ----
text(2, 98, "C", cex = 2.3, font = 2, col = red, xpd = NA)
text(50, 98, "AF-associated pathway: mechanism to evidence",
     cex = 1.2, font = 2, col = col_text)
text(50, 94.5, "Integrating mechanistic hypothesis with meta-analytic and patient-level evidence",
     cex = 0.60, col = grey_m)

# ============================================================
# LEFT SIDE — Mechanism Cascade (x=3–48)
# ============================================================

# Organ icons along a vertical axis at cx=26
cx_m <- 26

# --- MASLD (Liver) at top ---
draw_liver(cx_m, 84, s = 1.1, col = org)
# Label underneath
text(cx_m, 74, "Metabolic dysfunction-associated", cex = 0.52, col = grey_m, font = 3)
text(cx_m, 71.5, "steatotic liver disease", cex = 0.52, col = grey_m, font = 3)

# Down arrow from liver
arrows(cx_m, 69.5, cx_m, 65, length = 0.10, col = org, lwd = 2.5)

# --- Branch: Incident AF box ---
rrect(cx_m - 11, 56, 22, 9, r = 1.5, col = "white", border = blue, lwd = 1.5)
text(cx_m, 62.5, "Incident AF", cex = 0.82, font = 2, col = blue)
text(cx_m, 59.5, sprintf("HR %.2f [%.2f–%.2f]", ohno_hr, ohno_lo, ohno_up),
     cex = 0.70, font = 2, col = blue)
text(cx_m, 57, "(Ohno 2023)", cex = 0.52, col = grey_m, font = 3)

# Branch connector (liver → AF box)
segments(cx_m, 69.5, cx_m, 65, col = org, lwd = 2.5)
# Small horizontal branch marker
segments(cx_m - 4, 65, cx_m + 4, 65, col = blue, lwd = 1.5, lty = 2)

# Down arrow from AF box
arrows(cx_m, 56, cx_m, 50.5, length = 0.10, col = blue, lwd = 2.5)

# --- "Potential AF-associated pathway" label ---
text(cx_m, 47.5, "Potential AF-associated", cex = 0.68, col = blue, font = 3)
text(cx_m, 45, "pathway", cex = 0.68, col = blue, font = 3)

# Down arrow
arrows(cx_m, 42.5, cx_m, 37, length = 0.10, col = blue, lwd = 2.5)

# --- Stroke Risk (Brain) at bottom ---
draw_brain(cx_m, 28, s = 1.1, col = red)
text(cx_m, 18, "Cardioembolic", cex = 0.65, font = 2, col = red)
text(cx_m, 15, "Stroke", cex = 0.65, font = 2, col = red)

# Vertical pathway label on far left
text(5, 52, "MASLD", cex = 0.55, col = org, font = 2, srt = 90)
text(5, 28, "Stroke", cex = 0.55, col = red, font = 2, srt = 90)
# Small vertical bracket
arrows(8, 80, 8, 31, length = 0.06, col = grey_l, lwd = 1.5, code = 3)

# ============================================================
# RIGHT SIDE — Evidence Chain Boxes (x=50–97)
# ============================================================

# Outer container
rrect(50, 4, 47, 86, r = 2, col = adjustcolor("white", 0.85),
      border = purple, lwd = 1.5)

# Header
text(73.5, 86.5, "Evidence Chain", cex = 0.85, font = 2, col = purple)
segments(55, 84.5, 92, 84.5, col = purple, lwd = 1.2)

# --- Evidence Box 1: AF adjustment (y=64–82) ---
ev1_y <- 64; ev1_h <- 18
rrect(52, ev1_y, 43, ev1_h, r = 1.5, col = grey_bg, border = org, lwd = 1.2)
# Number badge
points(55, ev1_y + ev1_h - 3, pch = 21, cex = 1.6, bg = org, col = org)
text(55, ev1_y + ev1_h - 3, "1", cex = 0.72, font = 2, col = "white")
# Content
text(73.5, ev1_y + ev1_h - 3, "AF adjustment", cex = 0.78, font = 2, col = org)
text(73.5, ev1_y + ev1_h - 6,
     sprintf("R² = %.1f%%   p = %.3f", rma_af$R2, rma_af$QMp),
     cex = 0.75, font = 2, col = grey_d)
text(73.5, ev1_y + ev1_h - 9,
     "Meta-regression (k=21): AF adjustment",
     cex = 0.55, col = grey_m)
text(73.5, ev1_y + ev1_h - 11.5,
     "explains part of between-study heterogeneity",
     cex = 0.55, col = grey_m)
text(73.5, ev1_y + 1.5,
     sprintf("β = %.3f", rma_af$beta[2]),
     cex = 0.52, col = grey_m, font = 3)

# --- Evidence Box 2: AF interaction (y=41–60) ---
ev2_y <- 41; ev2_h <- 20
rrect(52, ev2_y, 43, ev2_h, r = 1.5, col = grey_bg, border = blue, lwd = 1.2)
points(55, ev2_y + ev2_h - 3, pch = 21, cex = 1.6, bg = blue, col = blue)
text(55, ev2_y + ev2_h - 3, "2", cex = 0.72, font = 2, col = "white")
text(73.5, ev2_y + ev2_h - 3, "AF subgroup interaction", cex = 0.78, font = 2, col = blue)
text(73.5, ev2_y + ev2_h - 6.5,
     "Jang 2026: Patient-level evidence",
     cex = 0.58, col = grey_m)
text(73.5, ev2_y + ev2_h - 9.5,
     "Without baseline AF: HR = 1.10",
     cex = 0.65, col = org)
text(73.5, ev2_y + ev2_h - 12.5,
     "With baseline AF:    HR = 1.03 [0.95–1.12]",
     cex = 0.65, col = blue)
text(73.5, ev2_y + 2,
     "Effect modification by baseline AF status",
     cex = 0.55, col = grey_m, font = 3)

# --- Evidence Box 3: Incident AF (y=17–38) ---
ev3_y <- 17; ev3_h <- 21
rrect(52, ev3_y, 43, ev3_h, r = 1.5, col = grey_bg, border = blue, lwd = 1.2)
points(55, ev3_y + ev3_h - 3, pch = 21, cex = 1.6, bg = blue, col = blue)
text(55, ev3_y + ev3_h - 3, "3", cex = 0.72, font = 2, col = "white")
text(73.5, ev3_y + ev3_h - 3, "Incident AF", cex = 0.78, font = 2, col = blue)
text(73.5, ev3_y + ev3_h - 7,
     sprintf("HR = %.2f [%.2f–%.2f]", ohno_hr, ohno_lo, ohno_up),
     cex = 0.80, font = 2, col = grey_d)
text(73.5, ev3_y + ev3_h - 11,
     "Ohno 2023: MASLD independently",
     cex = 0.55, col = grey_m)
text(73.5, ev3_y + ev3_h - 13.5,
     "associated with incident AF in a",
     cex = 0.55, col = grey_m)
text(73.5, ev3_y + ev3_h - 16,
     "large general-population cohort",
     cex = 0.55, col = grey_m)
text(73.5, ev3_y + 1.5,
     "Establishes the MASLD→AF link in",
     cex = 0.52, col = grey_m, font = 3)
text(73.5, ev3_y - 0.8,
     "the AF-associated pathway",
     cex = 0.52, col = grey_m, font = 3)

# ---- Bottom synthesis text ----
text(73.5, 10,
     "The AF-associated pathway is supported by converging",
     cex = 0.52, col = grey_m, font = 3)
text(73.5, 7.5,
     "evidence from meta-regression, patient-level effect",
     cex = 0.52, col = grey_m, font = 3)
text(73.5, 5,
     "modification, and longitudinal AF incidence data.",
     cex = 0.52, col = grey_m, font = 3)

# ============================================================
# Global figure title (outer margin)
# ============================================================
mtext("Figure 6  |  Evidence for an AF-associated pathway linking MASLD and stroke risk",
      side = 3, line = -1.2, cex = 1.1, font = 2, col = col_text, outer = TRUE)
mtext("Panel A: Meta-regression evidence  |  Panel B: AF subgroup interaction (Jang 2026)  |  Panel C: Pathway-to-evidence integration",
      side = 3, line = -2.4, cex = 0.62, col = grey_m, outer = TRUE)

dev.off()
message("Figure 6: figures/Figure6_AF_pathway.tiff")

# ============================================================
# Console summary
# ============================================================
cat("\n========================================\n")
cat(" Figure 6 — AF-associated pathway (3 panels)\n")
cat("========================================\n")

cat(sprintf("\nPanel A — Meta-regression Evidence:\n"))
cat(sprintf("  Without AF: HR=%.2f [%.2f–%.2f]  k=%d\n",
    exp(pred_no$pred), exp(pred_no$ci.lb), exp(pred_no$ci.ub), n_no_af))
cat(sprintf("  With AF:    HR=%.2f [%.2f–%.2f]  k=%d\n",
    exp(pred_yes$pred), exp(pred_yes$ci.lb), exp(pred_yes$ci.ub), n_yes_af))
cat(sprintf("  R² = %.1f%%  |  p = %.3f  |  β = %.3f\n",
    rma_af$R2, rma_af$QMp, rma_af$beta[2]))

cat(sprintf("\nPanel B — AF Subgroup Interaction (Jang 2026):\n"))
cat(sprintf("  Without baseline AF: HR = %.2f\n", jang_noaf))
cat(sprintf("  With baseline AF:    HR = %.2f [%.2f–%.2f]\n",
    jang_af, jang_af_lo, jang_af_up))
cat(sprintf("  Effect modification — no causal arrow implied\n"))

cat(sprintf("\nPanel C — AF-associated pathway: mechanism to evidence\n"))
cat(sprintf("  Evidence 1: AF adjustment R²=%.1f%%, p=%.3f (meta-regression, k=%d)\n",
    rma_af$R2, rma_af$QMp, n_no_af + n_yes_af))
cat(sprintf("  Evidence 2: AF subgroup interaction (Jang 2026, patient-level)\n"))
cat(sprintf("  Evidence 3: Incident AF HR %.2f [%.2f–%.2f] (Ohno 2023)\n",
    ohno_hr, ohno_lo, ohno_up))

cat("\nOutput: figures/Figure6_AF_pathway.tiff\n")
cat("========================================\n")
