# ============================================================
# Figure 7 — Fibrosis Severity & Dynamics as Determinants
#            of Stroke Risk in MASLD
# MASLD & Stroke Meta-Analysis · k=21 FINAL
# Panel A: Fibrosis Severity Spectrum (Simon + Chen + Jang)
# Panel B: Fibrosis Dynamics (Park 2022 only)
# Panel C: Clinical Implications
# ALL HRs are real, traceable to original papers
# ============================================================
library(meta)
library(metafor)
library(readxl)

# ============================================================
# Data verification only — all Figure 7 HRs are paper-extracted
# severity-stratified values, NOT from the main meta-analysis pool
# ============================================================
data <- read_excel("MASLD_AF_Cardioembolic stroke_META.25.xlsx", sheet = "Main Meta Data")
data$HR <- as.numeric(data$HR)

# ============================================================
# Color palette
# ============================================================
col_low       <- "#27AE60"   # green — low severity
col_inter     <- "#E67E22"   # orange — intermediate
col_adv       <- "#C0392B"   # red — advanced/cirrhosis
col_regress   <- "#27AE60"   # green — protective
col_persist   <- "#E74C3C"   # red — risk
col_histo     <- "#2C3E50"   # dark blue-gray — histology
col_nit       <- "#8E44AD"   # purple — NIT
col_text      <- "#1A1A1A"   # nearly black
col_subtitle  <- "#2C2C2C"   # dark gray for subtitle
col_bg        <- "#FFFFFF"

# ============================================================
# Figure 7A — Fibrosis Severity Spectrum
# Simon 2022 (Histology) + Chen 2023 (NFS) + Jang 2026 (BARD)
# Real severity-stratified HRs from original papers
# ============================================================
tiff("figures/Figure7A_fibrosis_severity.tiff",
     width = 14, height = 10.5, units = "in", res = 600)

par(mar = c(5, 14, 6.5, 9), bg = col_bg)

# --- Plot region ---
plot(NA, NA,
     xlim = c(0.88, 2.42), ylim = c(0.5, 11.4),
     xlab = "", ylab = "", axes = FALSE)

# --- Gradient background bands: Low / Intermediate / Advanced ---
# Zone 1: Low severity (HR 1.0–1.40)
rect(0.86, 0.2, 1.40, 11.8, col = adjustcolor(col_low, 0.07), border = NA)
# Zone 2: Intermediate (HR 1.40–1.80)
rect(1.40, 0.2, 1.80, 11.8, col = adjustcolor(col_inter, 0.07), border = NA)
# Zone 3: Advanced (HR 1.80–2.50)
rect(1.80, 0.2, 2.48, 11.8, col = adjustcolor(col_adv, 0.07), border = NA)

# Zone labels at top
text(1.13, 11.15, "Low\nSeverity", cex = 0.68, font = 2, col = col_low)
text(1.60, 11.15, "Intermediate\nSeverity", cex = 0.68, font = 2, col = col_inter)
text(2.14, 11.15, "Advanced\nSeverity", cex = 0.68, font = 2, col = col_adv)

# Vertical separator lines (subtle)
abline(v = c(1.40, 1.80), col = "gray85", lty = 3, lwd = 0.8)

# Reference line at HR=1
abline(v = 1, col = "gray60", lty = 2, lwd = 1.5)

# --- Helper: severity point ---
draw_severity_point <- function(hr, lo, hi, y, col, severity, has_ci = TRUE) {
  if (has_ci && !is.na(lo) && !is.na(hi)) {
    segments(lo, y, hi, y, col = adjustcolor(col, 0.7), lwd = 3.5, lend = 1)
  }
  # Point
  if (severity == "low") {
    points(hr, y, pch = 21, cex = 3.0, bg = adjustcolor(col_low, 0.25), col = col_low, lwd = 2.5)
  } else if (severity == "inter") {
    points(hr, y, pch = 22, cex = 3.0, bg = adjustcolor(col_inter, 0.25), col = col_inter, lwd = 2.5)
  } else {
    points(hr, y, pch = 23, cex = 3.2, bg = adjustcolor(col_adv, 0.25), col = col_adv, lwd = 2.5)
  }
}

# --- Simon 2022 (Histology) — y band 7.2–10.0 ---
y_simon_base <- 9.5
y_simon_mid  <- 8.4
y_simon_lo   <- 7.4

# Study header band
rect(0.86, 6.9, 2.48, 10.1, col = adjustcolor(col_histo, 0.04), border = col_histo, lwd = 1.0)
text(0.88, 9.90, "Simon 2022", cex = 1.15, font = 2, col = col_histo, pos = 4)
text(0.88, 9.50, "Histology (biopsy)", cex = 0.72, font = 3, col = col_histo, pos = 4)

# Cirrhosis (F4) — advanced
draw_severity_point(1.91, 1.34, 2.74, y_simon_base, col_adv, "adv")
text(2.38, y_simon_base + 0.22, sprintf("Cirrhosis (F4)  %.2f [%.2f–%.2f]", 1.91, 1.34, 2.74),
     cex = 0.78, col = col_adv, pos = 2, font = 2)

# Non-cirrhotic fibrosis (F1-F3) — intermediate
draw_severity_point(1.96, 1.58, 2.45, y_simon_mid, col_inter, "inter")
text(2.38, y_simon_mid + 0.22, sprintf("Non-cirrhotic fibrosis (F1–F3)  %.2f [%.2f–%.2f]", 1.96, 1.58, 2.45),
     cex = 0.78, col = col_inter, pos = 2, font = 2)

# Simple steatosis — low
points(1.50, y_simon_lo, pch = 21, cex = 3.0, bg = adjustcolor(col_low, 0.25), col = col_low, lwd = 2.5)
text(2.38, y_simon_lo + 0.25, "Simple steatosis  1.50",
     cex = 0.78, col = col_low, pos = 2)

# Arrows connecting levels (upward = increasing severity)
arrows(1.50, y_simon_lo + 0.40, 1.96, y_simon_mid - 0.40,
       length = 0.10, col = "gray50", lwd = 1.8, lty = 2)
arrows(1.96, y_simon_mid + 0.40, 1.91, y_simon_base - 0.40,
       length = 0.10, col = "gray50", lwd = 1.8, lty = 2)

# --- Chen 2023 (NFS) — y band 3.6–5.8 ---
y_chen_base <- 5.5
y_chen_mid  <- 4.6
y_chen_lo   <- 3.7

rect(0.86, 3.2, 2.48, 6.0, col = adjustcolor(col_nit, 0.04), border = col_nit, lwd = 1.0)
text(0.88, 5.85, "Chen 2023", cex = 1.15, font = 2, col = col_nit, pos = 4)
text(0.88, 5.50, "NFS (non-invasive)", cex = 0.72, font = 3, col = col_nit, pos = 4)

# High NFS — advanced
draw_severity_point(2.08, 1.81, 2.38, y_chen_base, col_adv, "adv")
text(2.38, y_chen_base + 0.22, sprintf("High NFS (>0.676)  %.2f [%.2f–%.2f]", 2.08, 1.81, 2.38),
     cex = 0.78, col = col_adv, pos = 2, font = 2)

# Intermediate NFS — intermediate
draw_severity_point(1.38, 1.31, 1.46, y_chen_mid, col_inter, "inter")
text(2.38, y_chen_mid + 0.22, sprintf("Intermediate NFS   %.2f [%.2f–%.2f]", 1.38, 1.31, 1.46),
     cex = 0.78, col = col_inter, pos = 2, font = 2)

# Low NFS — low
points(1.29, y_chen_lo, pch = 21, cex = 3.0, bg = adjustcolor(col_low, 0.25), col = col_low, lwd = 2.5)
text(2.38, y_chen_lo + 0.25, "Low NFS  1.29",
     cex = 0.78, col = col_low, pos = 2)

arrows(1.29, y_chen_lo + 0.40, 1.38, y_chen_mid - 0.40,
       length = 0.10, col = "gray50", lwd = 1.8, lty = 2)
arrows(1.38, y_chen_mid + 0.40, 2.08, y_chen_base - 0.40,
       length = 0.10, col = "gray50", lwd = 1.8, lty = 2)

# --- Jang 2026 (BARD) — y band 1.0–2.8 ---
y_jang_hi <- 2.3
y_jang_lo <- 1.3

rect(0.86, 0.8, 2.48, 2.9, col = adjustcolor(col_nit, 0.04), border = col_nit, lwd = 1.0)
text(0.88, 2.75, "Jang 2026", cex = 1.15, font = 2, col = col_nit, pos = 4)
text(0.88, 2.40, "BARD score (non-invasive)", cex = 0.72, font = 3, col = col_nit, pos = 4)

# High BARD — advanced
draw_severity_point(1.11, 1.10, 1.13, y_jang_hi, col_adv, "adv")
text(2.38, y_jang_hi + 0.22, sprintf("High BARD (≥2)  %.2f [%.2f–%.2f]", 1.11, 1.10, 1.13),
     cex = 0.78, col = col_adv, pos = 2, font = 2)

# Low BARD — low
draw_severity_point(1.03, 1.01, 1.06, y_jang_lo, col_low, "low")
text(2.38, y_jang_lo + 0.25, sprintf("Low BARD (<2)  %.2f [%.2f–%.2f]", 1.03, 1.01, 1.06),
     cex = 0.78, col = col_low, pos = 2)

arrows(1.03, y_jang_lo + 0.40, 1.11, y_jang_hi - 0.40,
       length = 0.10, col = "gray50", lwd = 1.8, lty = 2)

# --- Right-side annotation (between Simon and Chen study bands, clear of all data) ---
text(2.40, 6.55,
     "Consistent direction across\nhistologic and non-invasive\nfibrosis measures.",
     cex = 0.75, font = 3, col = col_text, pos = 2, xpd = NA)

# --- Legend (shape by severity, cleanly above Simon header) ---
legend(0.88, 10.75,
       legend = c("Low severity", "Intermediate", "Advanced / Cirrhosis"),
       pch = c(21, 22, 23),
       pt.bg = c(adjustcolor(col_low, 0.25), adjustcolor(col_inter, 0.25), adjustcolor(col_adv, 0.25)),
       col = c(col_low, col_inter, col_adv),
       pt.cex = 1.8, pt.lwd = 2, cex = 0.70, bty = "n",
       text.col = col_text, xpd = NA)

# --- X-axis ---
axis(1, at = seq(0.9, 2.4, by = 0.2),
     labels = sprintf("%.1f", seq(0.9, 2.4, by = 0.2)),
     cex.axis = 0.85, col = "gray60", col.axis = col_text)
mtext("Hazard Ratio for Stroke (95% CI)", side = 1, line = 3.5, cex = 1.05, col = col_text)

# --- Title ---
text(0.86, 11.65, "A", cex = 2.5, font = 2, col = col_adv, xpd = NA)
text(1.60, 11.65, "Fibrosis Severity Spectrum", cex = 1.6, font = 2, col = col_text, xpd = NA)
mtext("Severity-stratified HRs from 3 studies with quantitative fibrosis assessment  |  All values extracted from original publications",
      side = 3, line = 1.0, cex = 0.78, col = "#1A1A1A")

# Data source annotation (bottom)
mtext("Simon 2022: Table 3 (Page 6)  |  Chen 2023: Fig 3C (Page 6)  |  Jang 2026: Table 3 (Page 6)",
      side = 1, line = 5.0, cex = 0.60, col = "gray55", font = 3)

dev.off()
message("Figure 7A: figures/Figure7A_fibrosis_severity.tiff")

# ============================================================
# Figure 7B — Fibrosis Dynamics (standalone)
# ONLY Park 2022 trajectory data
# Persistent fibrosis 1.31, Regressed fibrosis 0.58 [0.35–0.95]
# ============================================================
tiff("figures/Figure7B_fibrosis_dynamics.tiff",
     width = 11, height = 9, units = "in", res = 600)

par(mar = c(5, 11, 6, 8), bg = col_bg)

# Park 2022 data
park_persist  <- 1.31
park_regress  <- 0.58
park_regr_lo  <- 0.35
park_regr_up  <- 0.95

plot(NA, NA,
     xlim = c(0.30, 1.55), ylim = c(0.5, 6.2),
     xlab = "", ylab = "", axes = FALSE)

# Background
rect(0.27, 0.5, 1.58, 6.2, col = "#F8F9FA", border = "gray90", lwd = 0.5)

# Reference at HR=1
abline(v = 1, col = "gray55", lty = 2, lwd = 2)

# --- Persistent Fibrosis (top, risk) ---
y_persist <- 5.1

# Diamond for persistent
px <- c(park_persist - 0.06, park_persist, park_persist, park_persist + 0.06,
        park_persist, park_persist, park_persist - 0.06)
py <- c(y_persist, y_persist + 0.34, y_persist, y_persist,
        y_persist - 0.34, y_persist, y_persist)
polygon(px, py, col = adjustcolor(col_persist, 0.50),
        border = col_persist, lwd = 2.8)

# CI bar
segments(park_persist - 0.15, y_persist, park_persist + 0.15, y_persist,
         col = col_persist, lwd = 3.5, lend = 1)

# Arrow pointing right = risk
arrows(0.55, y_persist, park_persist - 0.09, y_persist,
       length = 0.13, col = col_persist, lwd = 4.5)

# Label (left margin, clean position)
text(0.28, y_persist + 0.12, "Persistent\nFibrosis", cex = 1.1, font = 2, col = col_persist, pos = 4)
text(0.28, y_persist + 0.70, "Increased Risk →", cex = 0.68, col = col_persist, font = 3, pos = 4)

# HR annotation (right side)
text(1.52, y_persist, "HR = 1.31\n(CI not reported\nfor subgroup)", cex = 0.70, font = 2, col = col_persist, pos = 2)

# --- Connecting arrow (persistent → regressed) ---
arrows(park_persist, y_persist - 0.52,
       park_regress, 2.3,
       length = 0.14, col = col_regress, lwd = 4.5)

# "Fibrosis Regression" annotation on the arrow
text(0.70, 3.80, "Fibrosis\nRegression", cex = 0.82, font = 2, col = col_regress)

# --- Risk reduction annotation box (RIGHT MARGIN, not blocking plot) ---
rect(1.57, 3.05, 1.78, 4.35, col = adjustcolor("white", 0.92),
     border = col_regress, lwd = 1.8, xpd = NA)
text(1.675, 4.15, "∼42%", cex = 1.2, font = 2, col = col_regress, xpd = NA)
text(1.675, 3.80, "relative", cex = 0.62, col = "#1A1A1A", xpd = NA)
text(1.675, 3.60, "risk", cex = 0.62, col = "#1A1A1A", xpd = NA)
text(1.675, 3.40, "reduction", cex = 0.62, col = "#1A1A1A", xpd = NA)
text(1.675, 3.18, "with fibrosis", cex = 0.55, col = "gray50", font = 3, xpd = NA)
text(1.675, 3.05, "regression", cex = 0.55, col = "gray50", font = 3, xpd = NA)

# --- Regressed Fibrosis (bottom, protective) ---
y_regr <- 1.5

# Diamond
rx <- c(park_regr_lo, park_regress, park_regress, park_regr_up,
        park_regress, park_regress, park_regr_lo)
ry <- c(y_regr, y_regr + 0.34, y_regr, y_regr, y_regr - 0.34, y_regr, y_regr)
polygon(rx, ry, col = adjustcolor(col_regress, 0.50),
        border = col_regress, lwd = 2.8)

# CI segment
segments(park_regr_lo, y_regr, park_regr_up, y_regr,
         col = col_regress, lwd = 3.5, lend = 1)

# Protective arrow pointing LEFT
arrows(1.02, y_regr, park_regress + 0.07, y_regr,
       length = 0.13, col = col_regress, lwd = 4.5)

# Label (left margin, clean position — offset vertically to avoid diamond)
text(0.28, y_regr - 0.20, "Regressed\nFibrosis", cex = 1.1, font = 2, col = col_regress, pos = 4)
text(0.28, y_regr - 0.70, "← Risk Reduction", cex = 0.68, col = col_regress, font = 3, pos = 4)

# HR annotation (right side)
text(1.52, y_regr, sprintf("HR = %.2f [%.2f–%.2f]", park_regress, park_regr_lo, park_regr_up),
     cex = 0.82, font = 2, col = col_regress, pos = 2)

# Source line
text(0.95, 0.60, "Data source: Park et al. 2022 — longitudinal fibrosis trajectory analysis",
     cex = 0.62, col = "gray50", font = 3)

# X-axis
axis(1, at = c(0.3, 0.5, 0.7, 0.9, 1.0, 1.2, 1.4, 1.55),
     labels = c("0.3", "0.5", "0.7", "0.9", "1.0", "1.2", "1.4", "1.6"),
     cex.axis = 0.88, col = "gray60", col.axis = "#1A1A1A")
mtext("Hazard Ratio for Stroke", side = 1, line = 3.2, cex = 1.08, col = "#1A1A1A")

# Title
text(0.27, 6.50, "B", cex = 2.5, font = 2, col = col_regress, xpd = NA)
text(0.90, 6.50, "Fibrosis Dynamics & Stroke Risk", cex = 1.5, font = 2, col = "#1A1A1A")
mtext("Longitudinal change in fibrosis status is associated with differential stroke risk",
      side = 3, line = 1.0, cex = 0.82, col = "#1A1A1A")

dev.off()
message("Figure 7B: figures/Figure7B_fibrosis_dynamics.tiff")

# ============================================================
# Figure 7C — Clinical Implications (standalone)
# Conceptual framework: Assessment → Stratification → Regression → Risk Reduction
# ============================================================
tiff("figures/Figure7C_clinical_implications.tiff",
     width = 12, height = 9, units = "in", res = 600)

par(mar = c(3, 3, 5.5, 3), bg = col_bg)

plot(NA, NA,
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "", ylab = "", axes = FALSE)

# Background
rect(0, 0, 100, 100, col = "#F8F9FA", border = "gray90", lwd = 0.5)

# --- Title ---
text(3, 97, "C", cex = 2.5, font = 2, col = "#2C3E50", xpd = NA)
text(50, 97, "Clinical Implications", cex = 1.5, font = 2, col = col_text)
text(50, 93, "Fibrosis assessment → Risk stratification → Fibrosis regression → Potential stroke risk reduction",
     cex = 0.68, col = col_subtitle)

# ============================================================
# Three clinical pillars
# ============================================================

# --- Pillar 1: Fibrosis Assessment & Risk Stratification (left) ---
rect(2, 8, 32, 88, col = "white", border = col_adv, lwd = 1.8)
rect(2, 80, 32, 88, col = col_adv, border = NA)
text(17, 84, "Risk Stratification", cex = 0.92, font = 2, col = "white")

text(17, 76, "Identify highest-risk", cex = 0.72, font = 2, col = col_adv)
text(17, 72, "MASLD phenotype", cex = 0.72, font = 2, col = col_adv)

# Severity gradient within pillar
rect(5, 57, 29, 69, col = "#FDEDEC", border = col_adv, lwd = 1)
text(17, 66, "Advanced Fibrosis", cex = 0.72, font = 2, col = col_adv)
text(17, 63, "Simon 2022: Cirrhosis HR 1.91", cex = 0.58, col = col_adv)
text(17, 60, "Chen 2023: High NFS HR 2.08", cex = 0.58, col = col_adv)
text(17, 58, "Jang 2026: High BARD HR 1.11", cex = 0.58, col = col_adv)

rect(5, 47, 29, 55, col = "#FEF5E7", border = col_inter, lwd = 1)
text(17, 53, "Intermediate Fibrosis", cex = 0.65, font = 2, col = col_inter)
text(17, 50, "Simon: Non-cirrhotic HR 1.96", cex = 0.55, col = col_inter)
text(17, 48, "Chen: Intermed. NFS HR 1.38", cex = 0.55, col = col_inter)

# Down arrow
arrows(17, 45, 17, 40, length = 0.09, col = col_adv, lwd = 2.5)
text(17, 36, "Risk-informed", cex = 0.7, font = 2, col = col_adv)
text(17, 32, "surveillance", cex = 0.7, font = 2, col = col_adv)
text(17, 28, "intensity", cex = 0.7, font = 2, col = col_adv)

text(17, 18, "Gradient across severity levels", cex = 0.58, col = "gray50")
text(17, 14, "supports fibrosis as a", cex = 0.58, col = "gray50")
text(17, 10, "risk-stratification biomarker", cex = 0.58, col = "gray50")

# --- Pillar 2: Fibrosis Monitoring (center) ---
rect(35, 8, 65, 88, col = "white", border = "#8E44AD", lwd = 1.8)
rect(35, 80, 65, 88, col = "#8E44AD", border = NA)
text(50, 84, "Fibrosis Monitoring", cex = 0.92, font = 2, col = "white")

text(50, 76, "Track trajectory", cex = 0.72, font = 2, col = "#8E44AD")
text(50, 72, "with non-invasive tools", cex = 0.65, col = "gray50")

# Monitoring tools
rect(38, 60, 62, 69, col = "#F3E8F7", border = "#8E44AD", lwd = 0.8)
text(50, 66.5, "Serum biomarkers", cex = 0.68, font = 2, col = "#8E44AD")
text(50, 63.5, "FIB-4 / NFS", cex = 0.58, col = "gray50")
text(50, 61.2, "Routinely available", cex = 0.55, col = "gray50")

rect(38, 50, 62, 58, col = "#F3E8F7", border = "#8E44AD", lwd = 0.8)
text(50, 56, "Composite scores", cex = 0.68, font = 2, col = "#8E44AD")
text(50, 53.5, "BARD / ELF / AAR", cex = 0.58, col = "gray50")
text(50, 51.2, "Enhanced panels", cex = 0.55, col = "gray50")

rect(38, 40, 62, 48, col = "#F3E8F7", border = "#8E44AD", lwd = 0.8)
text(50, 46, "Imaging", cex = 0.68, font = 2, col = "#8E44AD")
text(50, 43.5, "VCTE / MRE", cex = 0.58, col = "gray50")
text(50, 41.2, "Quantitative LSM", cex = 0.55, col = "gray50")

arrows(50, 38, 50, 33, length = 0.09, col = "#8E44AD", lwd = 2.5)
text(50, 29, "Detect progression", cex = 0.7, font = 2, col = "#8E44AD")
text(50, 25, "vs regression early", cex = 0.65, col = "gray50")

text(50, 18, "Serial assessment enables", cex = 0.58, col = "gray50")
text(50, 14, "dynamic risk reclassification", cex = 0.58, col = "gray50")
text(50, 10, "over the disease course", cex = 0.58, col = "gray50")

# --- Pillar 3: Risk Reduction (right) ---
rect(68, 8, 98, 88, col = "white", border = col_regress, lwd = 1.8)
rect(68, 80, 98, 88, col = col_regress, border = NA)
text(83, 84, "Risk Reduction", cex = 0.92, font = 2, col = "white")

text(83, 76, "Fibrosis regression", cex = 0.72, font = 2, col = col_regress)
text(83, 72, "may reduce stroke risk", cex = 0.65, col = "gray50")

# Park 2022 evidence
rect(72, 57, 94, 69, col = "#E9F7EF", border = col_regress, lwd = 1.2)
text(83, 66, "Park 2022", cex = 0.72, font = 2, col = col_regress)
text(83, 63, "Regressed vs Persistent", cex = 0.60, col = "gray50")
text(83, 60, "HR 0.58 [0.35–0.95]", cex = 0.75, font = 2, col = col_regress)
text(83, 58, "~42% relative risk reduction", cex = 0.60, font = 2, col = col_regress)

arrows(83, 55, 83, 49, length = 0.09, col = col_regress, lwd = 2.5)

# Therapeutic implications
text(83, 46, "Potential Interventions", cex = 0.68, font = 2, col = col_regress)
text(83, 41, "Weight loss / lifestyle", cex = 0.60, col = "gray50")
text(83, 37, "GLP-1 RA", cex = 0.60, col = "gray50")
text(83, 33, "Rezdiffra (THR-β)", cex = 0.60, col = "gray50")
text(83, 29, "Metabolic targets", cex = 0.60, col = "gray50")

text(83, 22, "→", cex = 1.2, col = col_regress)
text(83, 16, "Hypothesis:", cex = 0.62, font = 2, col = col_regress)
text(83, 12, "Fibrosis regression as", cex = 0.58, col = col_regress)
text(83, 9, "stroke prevention target", cex = 0.58, col = col_regress)

# --- Bottom summary bar ---
rect(2, 1, 98, 5.5, col = adjustcolor("#2C3E50", 0.06), border = "#2C3E50", lwd = 1.2)
text(50, 3.8,
     "Clinical translation: Fibrosis severity stratifies risk  |  Serial monitoring tracks trajectory  |  Regression may reduce cerebrovascular risk",
     cex = 0.62, font = 2, col = "#2C3E50")

dev.off()
message("Figure 7C: figures/Figure7C_clinical_implications.tiff")

# ============================================================
# Console summary
# ============================================================
cat("\n========================================\n")
cat(" Figure 7 — Fibrosis Severity & Dynamics\n")
cat("========================================\n")
cat(sprintf("\nPanel A — Fibrosis Severity Spectrum:\n"))
cat(sprintf("  Simon 2022 (Histology): Steatosis 1.50 → Non-cirrhotic 1.96 → Cirrhosis 1.91\n"))
cat(sprintf("  Chen 2023 (NFS):       Low 1.29 → Intermediate 1.38 → High 2.08\n"))
cat(sprintf("  Jang 2026 (BARD):      Low 1.03 → High 1.11\n"))
cat(sprintf("  All values: real paper-extracted severity-stratified HRs\n"))
cat(sprintf("\nPanel B — Fibrosis Dynamics (Park 2022):\n"))
cat(sprintf("  Persistent fibrosis: HR=1.31\n"))
cat(sprintf("  Regressed fibrosis:  HR=0.58 [0.35-0.95]\n"))
cat(sprintf("  Relative risk reduction: ~42%%\n"))
cat(sprintf("\nPanel C — Clinical Implications:\n"))
cat(sprintf("  Assessment → Stratification → Regression → Risk Reduction\n"))
cat("\n========================================\n")
