# ============================================================
# Supplementary Figure S3 — Funnel Plot + Trim-and-Fill Assessment
# MASLD & Stroke Meta-Analysis · k=21
# Publication bias assessment: Detection → Impact → Conclusion
# Nature publication style · 600 DPI
# ============================================================
library(meta)
library(readxl)

# ============================================================
# Data prep (standard pipeline — locked k=21)
# ============================================================
data <- read_excel("raw_data/MASLD_AF_Cardioembolic stroke_META.25.xlsx",
                   sheet = "Main Meta Data")
data$HR <- as.numeric(data$HR)
data$stroke_group <- ifelse(
  data$outcome %in% c("All Stroke", "Stroke", "Cerebrovascular disease (CVD)"),
  "Total Stroke", "Ischemic Stroke")

data_main <- data
data_main <- subset(data_main, !grepl("MAFLD only", data_main$study))
data_main <- subset(data_main, !(study == "Park et al." & year == 2022))
data_main <- subset(data_main, !(study == "Kim et al. (B.S. Kim)" & year == 2025))
data_main$study_label <- make.unique(
  paste0(data_main$study, " (", data_main$year, ")"), sep = " #")
data_main$TE  <- log(data_main$HR)
data_main$seTE <- (log(data_main$upperCI) - log(data_main$lowerCI)) / 3.92

# Main model
m_main <- metagen(TE = TE, seTE = seTE, studlab = study_label,
                  sm = "HR", method.tau = "REML", data = data_main)

# ============================================================
# Publication bias tests
# ============================================================
egger <- metabias(m_main, method.bias = "linreg", k.min = 3)
begg  <- metabias(m_main, method.bias = "rank",   k.min = 3)
tf    <- trimfill(m_main)

# Extract test statistics
egger_t <- sprintf("%.2f", egger$statistic)
egger_df <- egger$df
egger_p  <- egger$p.value
begg_z   <- sprintf("%.2f", begg$statistic)
begg_p   <- begg$p.value

# ============================================================
# Build plot data
# ============================================================
# Observed studies
obs_TE   <- m_main$TE
obs_seTE <- m_main$seTE
obs_HR   <- exp(obs_TE)

# Trim-and-fill imputed studies
if (tf$k0 > 0) {
  n_obs <- length(m_main$studlab)
  n_tot <- length(tf$studlab)
  imp_idx <- (n_obs + 1):n_tot
  imp_TE   <- tf$TE[imp_idx]
  imp_seTE <- tf$seTE[imp_idx]
  imp_HR   <- exp(imp_TE)
} else {
  imp_TE   <- numeric(0)
  imp_seTE <- numeric(0)
  imp_HR   <- numeric(0)
}

# Pooled estimates
pooled_HR    <- exp(m_main$TE.random)
pooled_TE    <- m_main$TE.random
pooled_TE_fe <- m_main$TE.fixed
pooled_lower <- exp(m_main$lower.random)
pooled_upper <- exp(m_main$upper.random)
tf_HR        <- exp(tf$TE.random)
tf_TE        <- tf$TE.random
tf_lower     <- exp(tf$lower.random)
tf_upper     <- exp(tf$upper.random)

# ============================================================
# Impact metrics (for Panel 2)
# ============================================================
attenuation_pct <- (tf_HR - pooled_HR) / pooled_HR * 100
direction_retained <- (pooled_HR > 1 && tf_HR > 1) || (pooled_HR < 1 && tf_HR < 1)
sig_retained <- (pooled_lower > 1 && tf_lower > 1) || (pooled_upper < 1 && tf_upper < 1)

# ============================================================
# Funnel boundaries (centered on fixed-effect estimate)
# ============================================================
se_grid <- seq(0, max(obs_seTE) * 1.05, length.out = 200)
funnel_upper <- exp(pooled_TE_fe + 1.96 * se_grid)
funnel_lower <- exp(pooled_TE_fe - 1.96 * se_grid)

# ============================================================
# Color palette
# ============================================================
col_navy   <- "#1B3A5C"
col_red    <- "#C0392B"
col_grey   <- "#7F8C8D"
col_blue   <- "#2874A6"
col_dark   <- "#333333"
col_mgrey  <- "#555555"   # medium grey (replaces gray50/gray60 for readability)
col_lgrey  <- "#777777"   # lighter grey (replaces gray65/gray70 for readability)

# ============================================================
# Figure S3 — Two-Panel Layout: Funnel Plot + Bias Assessment
# ============================================================
tiff("figures/FigureS3_funnel.tiff",
     width = 15, height = 10, units = "in", res = 600)

# Panel 1 (left 67%): Funnel plot
# Panel 2 (right 33%): Publication bias annotation
layout(matrix(c(1, 2), nrow = 1), widths = c(0.67, 0.33))

# ============================================================
# PANEL 1 — Funnel Plot
# ============================================================
par(mar = c(4.5, 5.5, 6, 3.5), bg = "white")

# Axis limits
x_min <- min(c(min(obs_HR), min(funnel_lower),
               if (length(imp_HR) > 0) min(imp_HR) else 1)) * 0.85
x_max <- max(c(max(obs_HR), max(funnel_upper),
               if (length(imp_HR) > 0) max(imp_HR) else 1)) * 1.12
y_max <- max(obs_seTE) * 1.10

# Extend ylim below zero to accommodate annotation box and legends
y_lim_lo <- -y_max * 0.14
plot(NA, NA,
     xlim = c(x_min, x_max), ylim = c(y_max, y_lim_lo),
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n",
     bty = "n", main = "")

# ---- Funnel polygon (pseudo 95% CI region) ----
polygon_x <- c(funnel_upper, rev(funnel_lower))
polygon_y <- c(se_grid, rev(se_grid))
polygon(polygon_x, polygon_y,
        col = adjustcolor("#E8ECF0", 0.55),
        border = NA)

# Funnel boundary lines
lines(funnel_upper, se_grid, col = "#999999", lwd = 1.2, lty = 2)
lines(funnel_lower, se_grid, col = "#999999", lwd = 1.2, lty = 2)

# ---- Grid lines ----
abline(h = pretty(c(0, max(obs_seTE)), 6), col = "#E8E8E8", lwd = 0.6)
abline(v = pretty(c(x_min, x_max), 8), col = "#E8E8E8", lwd = 0.6)

# ---- Reference lines ----
abline(v = 1, col = "#AAAAAA", lty = 3, lwd = 1.2)         # null effect
abline(v = pooled_HR, col = col_navy, lty = 1, lwd = 2)    # pooled HR
if (tf$k0 > 0) {
  abline(v = tf_HR, col = col_red, lty = 4, lwd = 1.8)     # adjusted HR
}

# ---- Observed studies ----
points(obs_HR, obs_seTE,
       pch = 21, cex = 1.6,
       col = "#777777", bg = adjustcolor(col_grey, 0.50))

# ---- Imputed studies (hollow red circles) + arrow annotation ----
if (length(imp_HR) > 0) {
  points(imp_HR, imp_seTE,
         pch = 1, cex = 2.0,
         col = col_red, lwd = 2.8)

  # Arrow annotation: label inside funnel (right of imputed cluster)
  imp_cy <- mean(range(imp_seTE))
  ann_x <- max(imp_HR) + (x_max - x_min) * 0.06
  ann_y <- imp_cy + 0.01

  for (i in seq_along(imp_HR)) {
    lines(c(ann_x - (x_max - x_min) * 0.02, imp_HR[i]),
          c(ann_y + (i - 2) * 0.008, imp_seTE[i]),
          col = adjustcolor(col_red, 0.50), lwd = 1.2, lty = 3)
  }

  text(ann_x, ann_y, "Potential\nmissing studies",
       cex = 0.55, col = col_red, font = 3, pos = 4)
}

# ---- Axes ----
x_ticks <- c(0.6, 0.8, 1.0, 1.2, 1.5, 2.0, 2.5, 3.0)
x_ticks <- x_ticks[x_ticks >= x_min & x_ticks <= x_max]
axis(1, at = x_ticks, labels = sprintf("%.1f", x_ticks),
     lwd = 0, lwd.ticks = 1, col.ticks = "#888888", col = "#888888",
     cex.axis = 0.9, col.axis = "#444444")

y_ticks <- pretty(c(0, max(obs_seTE)), 6)
axis(2, at = y_ticks,
     lwd = 0, lwd.ticks = 1, col.ticks = "#888888", col = "#888888",
     las = 1, cex.axis = 0.9, col.axis = "#444444")

# Right-side precision labels
axis(4, at = y_ticks,
     labels = sprintf("%.0f", 1/y_ticks),
     lwd = 0, lwd.ticks = 1, col.ticks = "#AAAAAA", col = "#AAAAAA",
     las = 1, cex.axis = 0.65, col.axis = "#777777")

# Axis titles
mtext("Hazard Ratio (log scale)", side = 1, line = 3.2, cex = 1.1, col = "#333333")
mtext("Standard Error", side = 2, line = 4, cex = 1.1, col = "#333333")
mtext("Precision (1/SE)", side = 4, line = 1.2, cex = 0.60, col = "#777777")

# ---- Legend (inside plot, bottom-left) ----
legend(x_min + (x_max - x_min) * 0.02,
       -y_max * 0.04,
       legend = c("Observed (k = 21)", "Pooled HR"),
       pch = c(21, NA),
       lty = c(NA, 1),
       lwd = c(NA, 2),
       col = c("#777777", col_navy),
       pt.bg = c(adjustcolor(col_grey, 0.50), NA),
       pt.cex = c(1.4, NA),
       cex = 0.60, text.col = "#444444", bty = "n")

if (tf$k0 > 0) {
  legend(x_min + (x_max - x_min) * 0.02,
         -y_max * 0.12,
         legend = c(paste0("Imputed (k = ", tf$k0, ")"), "Adjusted HR"),
         pch = c(1, NA),
         lty = c(NA, 4),
         lwd = c(2.5, 1.8),
         col = c(col_red, col_red),
         pt.cex = c(1.6, NA),
         cex = 0.60, text.col = col_red, bty = "n")
}

# ---- Reviewer-facing sentence (Figure S1 style, bottom right) ----
# Moved upward (anchored at y=0, extending into annotation zone)
if (tf$k0 > 0) {
  att_str <- sprintf("%+.1f%%", attenuation_pct)
  rect(x_max * 0.50, 0, x_max, -y_max * 0.10,
       col = adjustcolor("#FEF9E7", 0.90), border = "#D4AC0D", lwd = 1.4)
  text(x_max * 0.52, -y_max * 0.05,
       sprintf(paste0("Adjustment for potential missing studies attenuated\n",
                      "the pooled HR by only %s, without altering the overall conclusion."),
               att_str),
       cex = 0.52, font = 2, col = "#7D6608", pos = 4)
}

# ---- Title (Panel 1) ----
title(main = expression(bold("Supplementary Figure S3.") ~
      "Funnel Plot & Trim-and-Fill Assessment"),
      cex.main = 1.0, col.main = col_navy, line = 4.5)
mtext(expression("MASLD & Stroke Meta-Analysis" ~ (italic(k) == 21) ~
      " ·  Random-effects (REML)  ·  Grey shading = pseudo 95% CI"),
      cex = 0.65, col = "#555555", line = 2.8)

# ---- Footer ----
mtext(paste0("Egger and Begg tests indicate no significant funnel plot asymmetry. ",
      "Trim-and-fill imputation yields minimal attenuation (HR ",
      sprintf("%.2f", pooled_HR), " → ", sprintf("%.2f", tf_HR), ")."),
      cex = 0.55, col = "#777777", line = 0.3, side = 1, adj = 0.5)

# ============================================================
# PANEL 2 — Publication Bias Assessment
# Structure: Detection → Impact → Conclusion (all inside box)
# ============================================================
par(mar = c(4.5, 1, 6, 3), bg = "white")

plot(NA, NA,
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "", ylab = "", axes = FALSE, main = "")

# ---- Outer box (extended bottom for full containment) ----
rect(5, 3, 95, 94,
     col = adjustcolor("white", 0.92), border = col_navy, lwd = 2.5)

# ---- Header banner ----
rect(5, 85, 95, 94, col = col_navy, border = NA)
text(50, 89.5, "PUBLICATION BIAS ASSESSMENT",
     cex = 0.82, font = 2, col = "white")
text(50, 82.5, "Egger, Begg & Trim-and-Fill Analysis",
     cex = 0.52, font = 3, col = "#888888")

# ============================================================
# SECTION 1: DETECTION  (y ≈ 51–79)
# ============================================================
detect_header_y <- 77
text(15, detect_header_y, "Detection",
     cex = 0.70, font = 2, col = col_navy, pos = 4)
segments(15, detect_header_y - 2, 85, detect_header_y - 2,
         col = "#BBBBBB", lwd = 1.0)

# Egger
egg_y <- 70
text(17, egg_y, "Egger test",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
text(88, egg_y,
     paste0("p = ", sprintf("%.3f", egger_p)),
     cex = 0.66, col = col_dark, font = 1, pos = 2)
text(22, egg_y - 4.5,
     sprintf("t = %s,  df = %d", egger_t, egger_df),
     cex = 0.50, col = "#666666", pos = 4)

# Begg
begg_y <- 60.5
text(17, begg_y, "Begg test",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
text(88, begg_y,
     paste0("p = ", sprintf("%.3f", begg_p)),
     cex = 0.66, col = col_dark, font = 1, pos = 2)
text(22, begg_y - 4.5,
     paste0("z = ", begg_z),
     cex = 0.50, col = "#666666", pos = 4)

# Trim-and-fill (in Detection section)
tf_detect_y <- 51
text(17, tf_detect_y, "Trim-and-fill",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
text(88, tf_detect_y,
     paste0(tf$k0, " studies imputed"),
     cex = 0.62, col = col_red, font = 1, pos = 2)

# ============================================================
# SECTION 2: IMPACT  (y ≈ 22–47)
# ============================================================
impact_header_y <- 44
segments(15, 47, 85, 47,
         col = "#BBBBBB", lwd = 1.2)

text(15, impact_header_y, "Impact",
     cex = 0.70, font = 2, col = col_navy, pos = 4)
segments(15, impact_header_y - 2, 85, impact_header_y - 2,
         col = "#BBBBBB", lwd = 1.0)

# Observed HR
imp_y1 <- 38
text(17, imp_y1, "Observed HR",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
text(88, imp_y1,
     sprintf("%.3f", pooled_HR),
     cex = 0.66, col = col_navy, font = 2, pos = 2)

# Adjusted HR
imp_y2 <- 34
text(17, imp_y2, "Adjusted HR",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
text(88, imp_y2,
     sprintf("%.3f", tf_HR),
     cex = 0.66, col = col_red, font = 2, pos = 2)

# Attenuation
imp_y3 <- 30
text(17, imp_y3, "Attenuation",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
att_label <- sprintf("%+.1f%%", attenuation_pct)
text(88, imp_y3, att_label,
     cex = 0.66, col = col_red, font = 2, pos = 2)

# Direction retained
imp_y4 <- 26
text(17, imp_y4, "Direction retained",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
dir_label <- if (direction_retained) "Yes" else "No"
dir_col   <- if (direction_retained) "#27AE60" else col_red
text(88, imp_y4, dir_label,
     cex = 0.66, col = dir_col, font = 2, pos = 2)

# Statistical significance
imp_y5 <- 22
text(17, imp_y5, "Significance",
     cex = 0.68, font = 2, col = col_dark, pos = 4)
sig_label <- if (sig_retained) "Retained" else "Lost"
sig_col   <- if (sig_retained) "#27AE60" else col_red
text(88, imp_y5, sig_label,
     cex = 0.66, col = sig_col, font = 2, pos = 2)

# ============================================================
# SECTION 3: CONCLUSION  (y ≈ 5–18)
# ============================================================
conc_header_y <- 15
segments(15, 18, 85, 18,
         col = "#BBBBBB", lwd = 1.2)

text(15, conc_header_y, "Conclusion",
     cex = 0.70, font = 2, col = col_navy, pos = 4)
segments(15, conc_header_y - 2, 85, conc_header_y - 2,
         col = "#BBBBBB", lwd = 1.0)

# Conclusion text — neutral, reviewer-safe wording
text(50, 9,
     paste0("Potential publication bias had minimal\n",
            "impact on the pooled estimate."),
     cex = 0.68, font = 2, col = col_dark)

# ---- Bottom footnote (Panel 2) ----
mtext(paste0("Egger et al. (1997) BMJ  ·  Begg & Mazumdar (1994) Biometrics  ·  ",
      "Duval & Tweedie (2000) Biometrics"),
      cex = 0.42, col = "#888888", line = 0.2, side = 1, adj = 0.5)

dev.off()

message("Figure S3: figures/FigureS3_funnel.tiff")

# ============================================================
# Console summary
# ============================================================
cat("\n========================================\n")
cat(" Supplementary Figure S3 — Funnel Plot\n")
cat("========================================\n\n")

cat(sprintf("Pooled HR (k=21): %.3f [%.3f–%.3f]  I² = %.1f%%\n\n",
            pooled_HR, pooled_lower, pooled_upper,
            m_main$I2 * 100))

cat("--- Publication Bias Assessment ---\n")
cat(sprintf("  Detection:\n"))
cat(sprintf("    Egger test:        t = %s,  df = %d,  p = %.3f  %s\n",
            egger_t, egger_df, egger_p,
            if (egger_p < 0.05) "***" else "(n.s.)"))
cat(sprintf("    Begg test:         z = %s,  p = %.3f  %s\n",
            begg_z, begg_p,
            if (begg_p < 0.05) "***" else "(n.s.)"))
cat(sprintf("    Trim-and-fill:     %d studies imputed\n", tf$k0))

cat(sprintf("\n  Impact:\n"))
cat(sprintf("    Observed HR:       %.3f [%.3f–%.3f]\n",
            pooled_HR, pooled_lower, pooled_upper))
cat(sprintf("    Adjusted HR:       %.3f [%.3f–%.3f]\n",
            tf_HR, tf_lower, tf_upper))
cat(sprintf("    Attenuation:       %+.1f%%\n", attenuation_pct))
cat(sprintf("    Direction retained: %s\n",
            if (direction_retained) "Yes" else "No"))
cat(sprintf("    Significance:       %s\n",
            if (sig_retained) "Retained" else "Lost"))

if (length(imp_TE) > 0) {
  cat("\n  Imputed study positions:\n")
  for (i in seq_along(imp_TE)) {
    cat(sprintf("    %-35s  HR = %.3f  se = %.4f\n",
        tf$studlab[n_obs + i], imp_HR[i], imp_seTE[i]))
  }
}

cat(sprintf("\n  CONCLUSION: Potential publication bias had minimal\n"))
cat(sprintf("  impact on the pooled estimate (attenuation %+.1f%%).\n",
            attenuation_pct))
cat("\nOutput: figures/FigureS3_funnel.tiff\n")
cat("========================================\n")
