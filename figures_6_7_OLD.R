# ============================================================
# Figure 6: AF Attenuation Plot
# Figure 7: Fibrosis Severity + Dynamics (Panel A + Panel B)
# 2026-06-20
# ============================================================
library(readxl)

# ============================================================
# Figure 6 — AF Attenuation Plot
# ============================================================
# Data: within-study HR before vs after AF adjustment
# 用户提供的四项示例数据；后续可替换为完整提取数据

af_data <- data.frame(
  Study     = c("Lee 2021", "Kim 2025", "Chen 2023", "Jang 2026"),
  HR_before = c(1.52, 1.26, 1.28, 1.14),
  HR_after  = c(1.43, 1.21, 1.26, 1.10),
  stringsAsFactors = FALSE
)

# Reverse order for top-down display
af_data$Study <- factor(af_data$Study, levels = rev(af_data$Study))

# --- Figure 6: Attnuation plot ---
tiff("figures/Figure6_AF_attenuation_OLD.tiff",
     width = 10, height = 5, units = "in", res = 300)

par(mar = c(5, 8, 3, 2))

plot(NA, NA,
     xlim = c(1.0, 1.7), ylim = c(0.7, nrow(af_data) + 0.3),
     xlab = "Hazard Ratio (95% CI)",
     ylab = "", yaxt = "n",
     main = "Figure 6. AF Attenuation Plot",
     bty = "n", las = 1)

# Reference line at HR = 1
abline(v = 1, col = "gray50", lty = 2, lwd = 0.8)

# Plot each study
cols_before <- "#E74C3C"  # red (before AF)
cols_after  <- "#2980B9"  # blue (after AF)

for (i in 1:nrow(af_data)) {
  y <- nrow(af_data) - i + 1

  # Connecting line
  segments(af_data$HR_before[i], y, af_data$HR_after[i], y,
           col = "gray60", lwd = 2)

  # Before AF point (red, filled)
  points(af_data$HR_before[i], y, pch = 21, cex = 2.2,
         bg = cols_before, col = cols_before)

  # After AF point (blue, filled)
  points(af_data$HR_after[i], y, pch = 21, cex = 2.2,
         bg = cols_after, col = cols_after)

  # Small arrow indicator
  arrows(af_data$HR_before[i], y,
         mean(c(af_data$HR_before[i], af_data$HR_after[i])), y,
         length = 0.08, col = "gray40", lwd = 0.5)
}

# Y-axis labels
axis(2, at = 1:nrow(af_data), labels = levels(af_data$Study),
     las = 1, tick = FALSE, cex.axis = 1.1)

# Legend
legend("bottomright",
       legend = c("Before AF adjustment", "After AF adjustment"),
       pch = 21, pt.bg = c(cols_before, cols_after),
       col = c(cols_before, cols_after),
       pt.cex = 1.8, bty = "n", cex = 1.0)

# Annotation
mtext("Each pair shows within-study attenuation\nof stroke risk after AF adjustment",
      side = 3, line = -1.2, cex = 0.8, col = "gray40")

dev.off()
message("Figure 6: figures/Figure6_AF_attenuation_OLD.tiff")

cat("\n========== Figure 6 (AF Attenuation) 完成 ==========\n")
cat("使用数据:\n")
print(af_data)
cat("如需更新数据，修改 af_data 后重新运行\n")

# ============================================================
# Figure 7 — Fibrosis Severity + Dynamics (2 panels)
# ============================================================

# --- Panel A: Fibrosis Severity Gradient ---
fibrosis_data <- data.frame(
  Tool   = c("Histology", "Histology", "Histology",
             "NFS",      "NFS",      "NFS",
             "AAR",      "AAR",      "AAR",
             "BARD",     "BARD"),
  Level  = c("Low", "Intermediate", "High",
             "Low", "Intermediate", "High",
             "Ref", "Intermediate", "High",
             "Low", "High"),
  HR     = c(1.50, 1.96, 1.91,
             1.29, 1.38, 2.08,
             1.00, 1.14, 1.29,
             1.03, 1.11),
  stringsAsFactors = FALSE
)

# --- Panel B: Fibrosis Dynamics (Park 2022) ---
park_dynamics <- data.frame(
  Status = c("Regressed fibrosis", "Persistent fibrosis"),
  HR     = c(0.58, 1.31),
  stringsAsFactors = FALSE
)

# ============================================================
# Draw Figure 7
# ============================================================
tiff("figures/Figure7_fibrosis_severity_dynamics_OLD.tiff",
     width = 12, height = 7, units = "in", res = 300)

layout(matrix(1:2, nrow = 1), widths = c(1.6, 1))
par(mar = c(4, 6, 3, 1))

# ---------- Panel A: Severity Gradient ----------
par(mar = c(4, 6, 3, 1))

# Assign Y positions by tool group
tools <- c("BARD", "AAR", "NFS", "Histology")
tool_y <- setNames(seq(4, 1, by = -1), tools)

# Level offsets within each tool
level_offset <- c("Low" = -0.25, "Ref" = -0.25, "Intermediate" = 0, "High" = 0.25)

fibrosis_data$y <- tool_y[fibrosis_data$Tool] + level_offset[fibrosis_data$Level]

# Color by tool
tool_cols <- c("Histology" = "#8E44AD", "NFS" = "#E67E22",
               "AAR" = "#27AE60", "BARD" = "#2980B9")
fibrosis_data$col <- tool_cols[fibrosis_data$Tool]

# Level shapes
level_pch <- c("Low" = 21, "Ref" = 21, "Intermediate" = 22, "High" = 24)
fibrosis_data$pch <- level_pch[fibrosis_data$Level]

plot(NA, NA,
     xlim = c(0.4, 2.4), ylim = c(0.5, 4.5),
     xlab = "Stroke HR",
     ylab = "", yaxt = "n",
     main = "A  Fibrosis Severity",
     bty = "n", las = 1)

# Reference line
abline(v = 1, col = "gray50", lty = 2, lwd = 0.8)

# Tool group labels
axis(2, at = tool_y, labels = names(tool_y),
     las = 1, tick = FALSE, cex.axis = 1.1, font = 2)

# Level labels (slightly offset left)
text(x = 0.45, y = fibrosis_data$y,
     labels = fibrosis_data$Level,
     cex = 0.75, col = "gray40", pos = 2)

# Plot points
for (i in 1:nrow(fibrosis_data)) {
  points(fibrosis_data$HR[i], fibrosis_data$y[i],
         pch = fibrosis_data$pch[i], cex = 2.2,
         bg = fibrosis_data$col[i], col = fibrosis_data$col[i])
}

# Connecting lines within each tool
for (t in tools) {
  sub <- fibrosis_data[fibrosis_data$Tool == t, ]
  sub <- sub[order(sub$HR), ]
  if (nrow(sub) > 1) {
    lines(sub$HR, sub$y, col = "gray60", lwd = 1.5, lty = 3)
  }
}

# Legend
legend("bottomright",
       legend = c("Low/Ref", "Intermediate", "High"),
       pch = c(21, 22, 24),
       pt.bg = "gray50", col = "gray50",
       pt.cex = 1.5, bty = "n", cex = 0.85,
       title = "Severity Level")

# ---------- Panel B: Fibrosis Dynamics (Park 2022) ----------
par(mar = c(4, 4, 3, 1))

park_dynamics$y <- c(1.5, 2.5)

plot(NA, NA,
     xlim = c(0, 2), ylim = c(1, 3),
     xlab = "Hazard Ratio",
     ylab = "", yaxt = "n",
     main = "B  Fibrosis Dynamics (Park 2022)",
     bty = "n", las = 1)

# Reference line
abline(v = 1, col = "gray50", lty = 2, lwd = 1.5)

# Regressed: green arrow pointing left (protective)
arrows(1.2, 1.5, 0.62, 1.5, length = 0.15, col = "#27AE60", lwd = 4, lty = 1)
points(0.58, 1.5, pch = 21, cex = 3, bg = "#27AE60", col = "#27AE60")
text(0.58, 1.5, "0.58", pos = 1, cex = 1.2, font = 2, col = "#27AE60")

# Persistent: red arrow pointing right (risk)
arrows(0.9, 2.5, 1.28, 2.5, length = 0.15, col = "#E74C3C", lwd = 4, lty = 1)
points(1.31, 2.5, pch = 21, cex = 3, bg = "#E74C3C", col = "#E74C3C")
text(1.31, 2.5, "1.31", pos = 1, cex = 1.2, font = 2, col = "#E74C3C")

# Labels
text(0.3, 1.5, "Regressed\nfibrosis", cex = 1.1, font = 3, col = "#27AE60", pos = 2)
text(0.3, 2.5, "Persistent\nfibrosis", cex = 1.1, font = 3, col = "#E74C3C", pos = 2)

# Annotation
text(1.0, 0.8, "← Protective    |    Risk →", cex = 0.85, col = "gray50")

dev.off()
message("Figure 7: figures/Figure7_fibrosis_severity_dynamics_OLD.tiff")

cat("\n========== Figure 7 (Fibrosis) 完成 ==========\n")
cat("Panel A 数据:\n")
print(fibrosis_data)
cat("\nPanel B 数据:\n")
print(park_dynamics)
cat("\n如需更新数据，修改 fibrosis_data / park_dynamics 后重新运行\n")
cat("\n全部完成。\n")
