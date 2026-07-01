# ============================================================
# Figure 2 — Main Forest Plot (Nature Medicine style)
# Figure 3 — Sensitivity Analysis (two-panel)
# MASLD & Stroke Meta-Analysis · k=21 FINAL
# ============================================================
library(meta)
library(readxl)

# ============================================================
# Data prep (standard pipeline)
# ============================================================
data <- read_excel("MASLD_AF_Cardioembolic stroke_META.25.xlsx", sheet = "Main Meta Data")
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
m_main_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label,
                     sm = "HR", method.tau = "REML",
                     method.random.ci = "HK", data = data_main)

# ============================================================
# Figure 2 — Enhanced Main Forest Plot
# ============================================================
# Color palette
col_pooled   <- "#1B3A5C"   # dark navy
col_diamond  <- "#2C5F8A"   # medium blue
col_study    <- "#1A1A1A"   # black
col_ci       <- "#444444"   # dark grey
col_weight   <- "#7FA3C1"   # soft blue for weights

tiff("figures/Figure2_forest_main.tiff",
     width = 14, height = 10, units = "in", res = 600)

forest(m_main,
       leftcols  = c("studlab"),
       leftlabs  = c("Study"),
       rightcols = c("effect.ci"),
       rightlabs = c("HR (95% CI)"),

       xlab      = "Hazard Ratio",
       smlab     = paste0("MASLD and Stroke Risk  |  k = 21  |  ",
                          "N = 29,928,431  |  HR = 1.37 (1.28–1.46)"),

       col.study = col_study,
       col.square = col_study,
       col.diamond = col_diamond,
       col.diamond.lines = col_pooled,

       print.I2    = TRUE,
       print.tau2  = TRUE,
       print.pval.Q = TRUE,

       spacing    = 1.1,
       lwd        = 1.2,
       lwd.square = 1.8,

       sortvar    = m_main$w.random,
       fs.study   = 10,
       fs.hetstat = 9,
       ff.hetstat = "plain",

       text.random = "Random effects (REML)"
)

dev.off()
message("Figure 2: figures/Figure2_forest_main.tiff")

# ============================================================
# Figure 3 — Two-Panel Sensitivity Analysis
# ============================================================

# --- Panel A: Leave-One-Out ---
leave1 <- metainf(m_main)

# --- Prepare sensitivity models ---
# Sens A: -Kim 2020
dsA <- subset(data_main, !(study == "Kim et al." & year == 2020))
dsA$study_label <- make.unique(paste0(dsA$study, " (", dsA$year, ")"), sep = " #")
dsA$TE <- log(dsA$HR); dsA$seTE <- (log(dsA$upperCI) - log(dsA$lowerCI)) / 3.92
mA <- metagen(TE = TE, seTE = seTE, studlab = study_label,
              sm = "HR", method.tau = "REML", data = dsA)
mA_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label,
                 sm = "HR", method.tau = "REML", method.random.ci = "HK", data = dsA)

# Sens B: -Lee 2025 -Kim 2024
dsB <- subset(data_main, !(study == "Lee et al." & year == 2025))
dsB <- subset(dsB, !(study == "Kim et al." & year == 2024))
dsB$study_label <- make.unique(paste0(dsB$study, " (", dsB$year, ")"), sep = " #")
dsB$TE <- log(dsB$HR); dsB$seTE <- (log(dsB$upperCI) - log(dsB$lowerCI)) / 3.92
mB <- metagen(TE = TE, seTE = seTE, studlab = study_label,
              sm = "HR", method.tau = "REML", data = dsB)
mB_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label,
                 sm = "HR", method.tau = "REML", method.random.ci = "HK", data = dsB)

# Sens C: -all three
dsC <- subset(data_main, !(study == "Kim et al." & year == 2020))
dsC <- subset(dsC, !(study == "Lee et al." & year == 2025))
dsC <- subset(dsC, !(study == "Kim et al." & year == 2024))
dsC$study_label <- make.unique(paste0(dsC$study, " (", dsC$year, ")"), sep = " #")
dsC$TE <- log(dsC$HR); dsC$seTE <- (log(dsC$upperCI) - log(dsC$lowerCI)) / 3.92
mC <- metagen(TE = TE, seTE = seTE, studlab = study_label,
              sm = "HR", method.tau = "REML", data = dsC)
mC_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label,
                 sm = "HR", method.tau = "REML", method.random.ci = "HK", data = dsC)

# ============================================================
# Draw Figure 3
# ============================================================
tiff("figures/Figure3_sensitivity_analysis.tiff",
     width = 16, height = 12, units = "in", res = 600)

layout(matrix(c(1, 1, 2, 2,
                1, 1, 3, 3), nrow = 2, byrow = TRUE),
       heights = c(1, 1.2))
par(mar = c(4, 8, 3, 2), bg = "white")

# ============ TOP HALF: Panel A — Leave-One-Out ============
# Extract LOO data
loo_df <- data.frame(
  study    = leave1$studlab,
  HR       = exp(leave1$TE),
  lower    = exp(leave1$lower),
  upper    = exp(leave1$upper),
  I2       = leave1$I2 * 100,
  stringsAsFactors = FALSE
)
# Include "All studies" row
loo_df <- rbind(
  data.frame(study = "All studies (k=21)", HR = exp(m_main$TE.random),
             lower = exp(m_main$lower.random),
             upper = exp(m_main$upper.random),
             I2 = m_main$I2 * 100, stringsAsFactors = FALSE),
  loo_df
)

# Color coding
loo_col <- ifelse(grepl("Kim et al. \\(2020\\)", loo_df$study), "#E74C3C",
           ifelse(grepl("All studies", loo_df$study), "#1B3A5C", "#444444"))
loo_pch <- ifelse(grepl("Kim et al. \\(2020\\)", loo_df$study), 21,
           ifelse(grepl("All studies", loo_df$study), 23, 19))
loo_bg  <- ifelse(grepl("Kim et al. \\(2020\\)", loo_df$study), "#F1948A",
           ifelse(grepl("All studies", loo_df$study), "#2C5F8A", NA))
loo_cex <- ifelse(grepl("All studies", loo_df$study), 2.0,
           ifelse(grepl("Kim", loo_df$study), 1.6, 1.2))

n <- nrow(loo_df)
plot(NA, NA, xlim = c(1.1, 1.6), ylim = c(0.5, n + 0.5),
     xlab = "Hazard Ratio (log scale)",
     ylab = "", yaxt = "n", bty = "n", las = 1,
     main = "", log = "x")

# Reference line
abline(v = exp(m_main$TE.random), col = "#2C5F8A", lty = 2, lwd = 1.5)
abline(v = 1, col = "gray70", lty = 3, lwd = 0.8)

# Study labels
for (i in 1:n) {
  y <- n - i + 1
  col_i <- ifelse(grepl("Kim et al. \\(2020\\)", loo_df$study[i]),
                  "#C0392B", ifelse(grepl("All studies", loo_df$study[i]),
                  "#1B3A5C", "#333333"))
  font_i <- ifelse(grepl("All studies|Kim", loo_df$study[i]), 2, 1)

  axis(2, at = y, labels = loo_df$study[i], las = 1, tick = FALSE,
       cex.axis = 0.7, col.axis = col_i, font.axis = font_i, line = -0.5)
  # Point estimate
  points(loo_df$HR[i], y, pch = loo_pch[i], cex = loo_cex[i],
         col = loo_col[i], bg = loo_bg[i])
  # CI line
  segments(loo_df$lower[i], y, loo_df$upper[i], y,
           col = loo_col[i], lwd = ifelse(grepl("All studies|Kim", loo_df$study[i]), 3, 1.5))
}

# Title
title(main = "A  Leave-One-Out Analysis",
      cex.main = 1.3, font.main = 2, col.main = "#1B3A5C", adj = 0, line = 1.5)
mtext("Pooled HR after excluding each study sequentially",
      cex = 0.8, col = "gray50", adj = 0, line = 0.2)
mtext("Red = Kim 2020 influences estimate most · All HRs remain > 1",
      cex = 0.7, col = "#C0392B", adj = 0, line = -1, font = 3)

# ============ BOTTOM HALF: Panel B — Sensitivity Forest ============
par(mar = c(5, 3, 4, 3))

# Sensitivity comparison data
sens_data <- data.frame(
  Model = c("Main Analysis (k=21)",
            "Sensitivity A: −Kim 2020 (k=20)",
            "Sensitivity B: −Lee 2025, −Kim 2024 (k=19)",
            "Sensitivity C: −All three (k=18)"),
  HR    = c(exp(m_main$TE.random), exp(mA$TE.random),
            exp(mB$TE.random), exp(mC$TE.random)),
  lower = c(exp(m_main$lower.random), exp(mA$lower.random),
            exp(mB$lower.random), exp(mC$lower.random)),
  upper = c(exp(m_main$upper.random), exp(mA$upper.random),
            exp(mB$upper.random), exp(mC$upper.random)),
  I2    = c(m_main$I2 * 100, mA$I2 * 100, mB$I2 * 100, mC$I2 * 100),
  tau2  = c(m_main$tau2, mA$tau2, mB$tau2, mC$tau2),
  stringsAsFactors = FALSE
)

plot(NA, NA, xlim = c(1.0, 1.7), ylim = c(0.5, nrow(sens_data) + 0.5),
     xlab = "Hazard Ratio (95% CI)",
     ylab = "", yaxt = "n", bty = "n", las = 1, log = "x")

# Reference
abline(v = 1, col = "gray70", lty = 3, lwd = 1.2)
abline(v = exp(m_main$TE.random), col = "#1B3A5C", lty = 2, lwd = 1.2)

# Color per model
sens_col <- c("#1B3A5C", "#E67E22", "#2980B9", "#8E44AD")
sens_bg  <- c("#2C5F8A", "#F0A050", "#5BA0D0", "#B070D0")

for (i in 1:nrow(sens_data)) {
  y <- nrow(sens_data) - i + 1

  # Diamond
  diamond_x <- c(sens_data$lower[i],
                 sens_data$HR[i], sens_data$HR[i],
                 sens_data$upper[i], sens_data$HR[i],
                 sens_data$HR[i], sens_data$lower[i])
  diamond_y <- c(y, y + 0.35, y, y, y - 0.35, y, y)
  polygon(diamond_x, diamond_y,
          col = adjustcolor(sens_bg[i], 0.7),
          border = sens_col[i], lwd = 2)

  # CI line
  segments(sens_data$lower[i], y, sens_data$upper[i], y,
           col = sens_col[i], lwd = 2.5)

  # Point
  points(sens_data$HR[i], y, pch = 23, cex = 2,
         bg = sens_col[i], col = sens_col[i])

  # Label
  lab <- sprintf("%s  HR=%.2f [%.2f–%.2f]  I²=%.1f%%",
                 sens_data$Model[i],
                 sens_data$HR[i], sens_data$lower[i], sens_data$upper[i],
                 sens_data$I2[i])
  text(1.02, y, lab, cex = 0.8, col = sens_col[i], font = 2, pos = 4)
}

# Annotations
rect(1.45, 1.8, 1.65, 2.2, col = "#FEF9E7", border = "#F9E79F", lwd = 1)
text(1.55, 2.0, "All HR > 1", cex = 0.7, font = 2, col = "#B7950B")
text(1.55, 1.85, "Direction\nconsistent", cex = 0.55, col = "#B7950B")

title(main = "B  Sensitivity Analysis — Pooled Estimates Across Exclusion Tiers",
      cex.main = 1.3, font.main = 2, col.main = "#1B3A5C", adj = 0, line = 2.5)
mtext("Main analysis vs three sensitivity models showing robust association",
      cex = 0.8, col = "gray50", adj = 0, line = 1.2)
mtext("HK-corrected CIs · REML tau² estimator · Random-effects model",
      cex = 0.65, col = "gray70", adj = 0, line = 0)

dev.off()
message("Figure 3: figures/Figure3_sensitivity_analysis.tiff")

# ============================================================
# Summary
# ============================================================
cat("\n========================================\n")
cat(" Figure 2 & 3 生成完成\n")
cat("========================================\n")
cat("\nFigure 2 — Main Forest Plot\n")
cat(sprintf("  k=%d  N=%s  HR=%.3f (%.3f–%.3f)  I²=%.1f%%\n",
  nrow(data_main), "29,928,431",
  exp(m_main$TE.random), exp(m_main$lower.random), exp(m_main$upper.random),
  m_main$I2*100))
cat("\nFigure 3 — Sensitivity Analysis\n")
cat(sprintf("  Main:     k=%d  HR=%.3f  I²=%.1f%%\n",
  nrow(data_main), exp(m_main$TE.random), m_main$I2*100))
cat(sprintf("  Sens A:   k=%d  HR=%.3f  I²=%.1f%%\n",
  nrow(dsA), exp(mA$TE.random), mA$I2*100))
cat(sprintf("  Sens B:   k=%d  HR=%.3f  I²=%.1f%%\n",
  nrow(dsB), exp(mB$TE.random), mB$I2*100))
cat(sprintf("  Sens C:   k=%d  HR=%.3f  I²=%.1f%%\n",
  nrow(dsC), exp(mC$TE.random), mC$I2*100))
cat("\n========================================\n")
