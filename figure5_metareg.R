# ============================================================
# Figure 5 — Meta-Regression Bubble Plots
# MASLD & Stroke Meta-Analysis · k=21 FINAL
# Panel A: Mean Age vs log(HR)
# Panel B: AF Adjustment vs log(HR)
# Circulation / Nature Medicine publication quality
# ============================================================
library(meta)
library(metafor)
library(readxl)

# ============================================================
# Data prep (standard k=21 pipeline)
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

# Merge covariates
dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.25.xlsx", sheet = "Study_Characteristics")
dc <- dc[-1, ]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label), ]

merged <- merge(
  data_main[, c("study_label", "TE", "seTE", "stroke_group")],
  dc[, c("study_label", "Age_Mean_SD", "adjusted_AF", "Liver_Definition",
         "sample", "Study_Design")],
  by = "study_label", all.x = TRUE)

# Covariate coding
extract_first_num <- function(x) {
  m <- regexpr("[0-9]+.[0-9]*|[0-9]+", as.character(x))
  as.numeric(regmatches(as.character(x), m))
}
merged$mean_age <- extract_first_num(merged$Age_Mean_SD)
merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)

# ============================================================
# Fit ALL meta-regression models upfront
# ============================================================

# --- k=21 (locked main analysis) ---
rma_age <- rma(yi = TE, sei = seTE, mods = ~ mean_age,
               data = merged, method = "REML")
rma_af  <- rma(yi = TE, sei = seTE, mods = ~ adj_af,
               data = merged, method = "REML")

# --- k=20 (without Kim 2020) ---
merged_no_kim <- subset(merged, study_label != "Kim et al. (2020)")
rma_age_nk <- rma(yi = TE, sei = seTE, mods = ~ mean_age,
                  data = merged_no_kim, method = "REML")
rma_af_nk  <- rma(yi = TE, sei = seTE, mods = ~ adj_af,
                  data = merged_no_kim, method = "REML")

# --- Predictions for Panel A (continuous) ---
age_range <- range(merged$mean_age, na.rm = TRUE)
age_seq <- seq(age_range[1] - 3, age_range[2] + 3, length.out = 200)
pred_age <- predict(rma_age, newmods = cbind(age_seq))

# --- Predictions for Panel B (binary) ---
pred_af_0 <- predict(rma_af, newmods = c(0))  # Not adjusted for AF
pred_af_1 <- predict(rma_af, newmods = c(1))  # Adjusted for AF

# --- Weights for bubble size ---
merged$weight_age <- 1 / (merged$seTE^2 + rma_age$tau2)
merged$weight_af  <- 1 / (merged$seTE^2 + rma_af$tau2)
merged$cex_age <- sqrt(merged$weight_age) / max(sqrt(merged$weight_age)) * 6.5 + 1.2
merged$cex_af  <- sqrt(merged$weight_af)  / max(sqrt(merged$weight_af))  * 6.5 + 1.2

# --- Identify Kim 2020 ---
kim_idx <- which(grepl("Kim et al. \\(2020\\)", merged$study_label))

# --- Group indices ---
adj_no_idx  <- which(!merged$adj_af)
adj_yes_idx <- which(merged$adj_af)
n_no  <- sum(!merged$adj_af, na.rm = TRUE)
n_yes <- sum(merged$adj_af, na.rm = TRUE)

# ============================================================
# Color palette
# ============================================================
col_age       <- "#1B3A5C"   # dark navy
col_af        <- "#2878B5"   # steel blue
col_kim       <- "#E74C3C"   # red
col_kim_bg    <- "#F1948A"
col_bubble    <- "#5A8BB5"
col_bubble_bg <- adjustcolor("#A0C4E0", 0.5)
col_regline   <- "#2C3E50"
col_band      <- adjustcolor("#95A5A6", 0.15)
col_text      <- "#1A1A1A"
col_grey      <- "#666666"

# ============================================================
# Generate Figure 5 — 600 DPI TIFF
# ============================================================
tiff("figures/Figure5_metareg_bubble.tiff",
     width = 14, height = 14, units = "in", res = 600)

layout(matrix(c(1, 2), nrow = 2), heights = c(1, 1))
par(bg = "white")

# ============================================================
# PANEL A: Mean Age vs log(HR)
# ============================================================
par(mar = c(5, 5.5, 4, 3))

plot(NA, NA,
     xlim = c(28, 68), ylim = c(-0.08, 1.08),
     xlab = "", ylab = "", axes = FALSE)

# Grid
abline(h = 0, col = "gray75", lty = 2, lwd = 1.2)
abline(h = log(1.5), col = "gray92", lty = 1, lwd = 0.6)
abline(h = log(2.0), col = "gray92", lty = 1, lwd = 0.6)

# Confidence band
polygon(c(age_seq, rev(age_seq)),
        c(pred_age$ci.lb, rev(pred_age$ci.ub)),
        col = col_band, border = NA)

# Regression line
lines(age_seq, pred_age$pred, col = col_regline, lwd = 3)

# Axes
axis(1, at = seq(30, 65, 5), cex.axis = 1.05, col = "gray60", col.axis = col_text,
     mgp = c(3, 0.7, 0))
axis(2, at = log(c(1.0, 1.2, 1.5, 2.0, 2.5)),
     labels = c("1.0", "1.2", "1.5", "2.0", "2.5"),
     las = 1, cex.axis = 1.0, col = "gray60", col.axis = col_text,
     mgp = c(3, 0.7, 0))

mtext("Hazard Ratio (log scale)", side = 2, line = 3.5, cex = 1.15, col = col_text)
mtext("Mean Age (years)", side = 1, line = 3.2, cex = 1.15, col = col_text)

# --- Plot bubbles (Kim 2020 last so it renders on top) ---
for (i in 1:nrow(merged)) {
  if (i == kim_idx) next
  points(merged$mean_age[i], merged$TE[i],
         pch = 21,
         cex = merged$cex_age[i],
         col = col_bubble,
         bg = col_bubble_bg,
         lwd = 1.5)
}

# Kim 2020 highlighted
i <- kim_idx
points(merged$mean_age[i], merged$TE[i],
       pch = 21,
       cex = merged$cex_age[i] + 0.5,
       col = col_kim,
       bg = adjustcolor(col_kim_bg, 0.55),
       lwd = 3.5)
# Label
text(merged$mean_age[i] + 0.5, merged$TE[i] + 0.09,
     "Kim et al. 2020", cex = 0.8, font = 2, col = col_kim, pos = 3)

# --- Stats annotation box (top left) ---
rect(28.5, 0.83, 56, 1.04, col = adjustcolor("white", 0.90),
     border = col_age, lwd = 1.5)
text(29.2, 0.99, "Mean Age Meta-Regression", cex = 1.0, font = 2, col = col_age, pos = 4)
text(29.2, 0.90,
     sprintf("With Kim 2020 (k=%d): R² = %.1f%%, p = %.3f",
             rma_age$k, rma_age$R2, rma_age$QMp),
     cex = 0.70, col = col_grey, pos = 4)
text(29.2, 0.85,
     sprintf("Excluding Kim 2020 (k=%d): R² = %.1f%%, p = %.3f",
             rma_age_nk$k, rma_age_nk$R2, rma_age_nk$QMp),
     cex = 0.70, col = col_kim, font = 3, pos = 4)

# Equation
text(58, 0.10,
     bquote(beta == .(sprintf("%.4f", rma_age$beta[2])) * " per year"),
     cex = 0.72, col = col_grey, pos = 2)

# Bubble size legend
legend("topright",
       legend = c("Bubble size =", "Inverse-variance weight", "", "High precision", "Low precision"),
       pt.cex = c(0, 0, 0, 5.0, 1.8),
       pch = c(NA, NA, NA, 21, 21),
       col = c(NA, NA, NA, col_bubble, col_bubble),
       pt.bg = c(NA, NA, NA, col_bubble_bg, col_bubble_bg),
       pt.lwd = c(NA, NA, NA, 1.2, 1.2),
       bty = "n", cex = 0.60, text.col = col_grey,
       y.intersp = 1.15)

# Panel label
text(28.5, 1.06, "A", cex = 2.3, font = 2, col = col_age, xpd = NA)

# ============================================================
# PANEL B: AF Adjustment vs log(HR)
# ============================================================
par(mar = c(5, 5.5, 4, 3))

plot(NA, NA,
     xlim = c(-0.6, 1.6), ylim = c(-0.08, 1.08),
     xlab = "", ylab = "", axes = FALSE)

# Grid
abline(h = 0, col = "gray75", lty = 2, lwd = 1.2)
abline(h = log(1.5), col = "gray92", lty = 1, lwd = 0.6)
abline(h = log(2.0), col = "gray92", lty = 1, lwd = 0.6)

# --- Group mean diamonds ---
diamond <- function(x, y, s = 0.035, col = "black", bg = "white", lwd = 2) {
  dx <- x + c(0, s, 0, -s, 0)
  dy <- y + c(s, 0, -s, 0, s)
  polygon(dx, dy, col = bg, border = col, lwd = lwd)
}

# No AF group diamond
diamond(0, pred_af_0$pred, s = 0.045, col = col_af,
        bg = adjustcolor(col_af, 0.75), lwd = 2.5)
# Yes AF group diamond
diamond(1, pred_af_1$pred, s = 0.045, col = col_af,
        bg = adjustcolor(col_af, 0.75), lwd = 2.5)

# Vertical CI bars
segments(0, pred_af_0$ci.lb, 0, pred_af_0$ci.ub,
         col = col_af, lwd = 2.5, lend = 1)
segments(1, pred_af_1$ci.lb, 1, pred_af_1$ci.ub,
         col = col_af, lwd = 2.5, lend = 1)

# Regression line (connecting group means)
lines(c(0, 1), c(pred_af_0$pred, pred_af_1$pred),
      col = col_regline, lwd = 3)

# --- Jittered individual bubbles (pre-compute positions) ---
set.seed(42)
xj_no  <- jitter(rep(0, length(adj_no_idx)),  amount = 0.22)
xj_yes <- jitter(rep(1, length(adj_yes_idx)), amount = 0.22)

for (j in seq_along(adj_no_idx)) {
  i <- adj_no_idx[j]
  points(xj_no[j], merged$TE[i], pch = 21,
         cex = merged$cex_af[i],
         col = col_bubble,
         bg = col_bubble_bg,
         lwd = 1.3)
}
for (j in seq_along(adj_yes_idx)) {
  i <- adj_yes_idx[j]
  points(xj_yes[j], merged$TE[i], pch = 21,
         cex = merged$cex_af[i],
         col = col_bubble,
         bg = col_bubble_bg,
         lwd = 1.3)
}

# Label large studies
for (j in seq_along(adj_no_idx)) {
  i <- adj_no_idx[j]
  if (merged$cex_af[i] > 3) {
    text(xj_no[j], merged$TE[i], gsub(" .*", "", merged$study_label[i]),
         cex = 0.35, col = col_text, adj = c(0.5, -1.5))
  }
}
for (j in seq_along(adj_yes_idx)) {
  i <- adj_yes_idx[j]
  if (merged$cex_af[i] > 3) {
    text(xj_yes[j], merged$TE[i], gsub(" .*", "", merged$study_label[i]),
         cex = 0.35, col = col_text, adj = c(0.5, -1.5))
  }
}

# Axis
axis(1, at = c(0, 1),
     labels = c("Not Adjusted\nfor AF", "Adjusted\nfor AF"),
     cex.axis = 1.1, padj = 0.5, col = "gray60", col.axis = col_text,
     tick = FALSE, mgp = c(3, 0.5, 0))
axis(2, at = log(c(1.0, 1.2, 1.5, 2.0)),
     labels = c("1.0", "1.2", "1.5", "2.0"),
     las = 1, cex.axis = 1.0, col = "gray60", col.axis = col_text,
     mgp = c(3, 0.7, 0))

mtext("Hazard Ratio (log scale)", side = 2, line = 3.5, cex = 1.15, col = col_text)

# --- Group count + pooled HR annotations ---
text(0, 0.97, paste0("k = ", n_no), cex = 0.85, font = 2, col = col_af, pos = 3)
text(0, 0.90, sprintf("HR = %.2f", exp(pred_af_0$pred)),
     cex = 0.78, col = col_grey, pos = 3)
text(0, 0.84, sprintf("95%% CI [%.2f–%.2f]",
                      exp(pred_af_0$ci.lb), exp(pred_af_0$ci.ub)),
     cex = 0.62, col = col_grey, pos = 3)

text(1, 0.97, paste0("k = ", n_yes), cex = 0.85, font = 2, col = col_af, pos = 3)
text(1, 0.90, sprintf("HR = %.2f", exp(pred_af_1$pred)),
     cex = 0.78, col = col_grey, pos = 3)
text(1, 0.84, sprintf("95%% CI [%.2f–%.2f]",
                      exp(pred_af_1$ci.lb), exp(pred_af_1$ci.ub)),
     cex = 0.62, col = col_grey, pos = 3)

# --- Stats box ---
rect(-0.55, 0.78, 0.42, 0.98, col = adjustcolor("white", 0.90),
     border = col_af, lwd = 1.5)
text(-0.48, 0.93, "AF Adjustment", cex = 1.0, font = 2, col = col_af, pos = 4)
text(-0.48, 0.86, sprintf("Meta-Regression (k=%d)", rma_af$k),
     cex = 0.70, col = col_grey, pos = 4)
text(-0.48, 0.81,
     sprintf("R² = %.1f%%,  p = %.3f,  β = %.3f",
             rma_af$R2, rma_af$QMp, rma_af$beta[2]),
     cex = 0.68, col = col_grey, pos = 4)

# --- Mediation annotation ---
arrows(0.5, 0.30, 0.5, 0.18, length = 0.08, col = col_kim, lwd = 1.8)
text(0.5, 0.37,
     "AF adjustment attenuates\nstroke risk by ~21%",
     cex = 0.70, font = 3, col = col_kim)

# --- Sensitivity footnote ---
text(0.5, 0.04,
     sprintf("Sensitivity (k=%d, without Kim 2020): R²=%.1f%%, p=%.3f  ·  Strongest moderator of between-study heterogeneity",
             rma_af_nk$k, rma_af_nk$R2, rma_af_nk$QMp),
     cex = 0.55, col = "gray70", font = 3)

# Bubble size legend (compact, top-right)
legend("topright",
       legend = c("Bubble = weight", "Large = high precision"),
       pt.cex = c(0, 3.8),
       pch = c(NA, 21),
       col = c(NA, col_bubble),
       pt.bg = c(NA, col_bubble_bg),
       pt.lwd = c(NA, 1.2),
       bty = "n", cex = 0.55, text.col = col_grey,
       y.intersp = 1.0)

# Panel label
text(-0.6, 1.06, "B", cex = 2.3, font = 2, col = col_af, xpd = NA)

# ============================================================
# Overall title
# ============================================================
mtext("Meta-Regression — Sources of Between-Study Heterogeneity",
      side = 3, line = -2, outer = TRUE, cex = 1.5, font = 2, col = col_text)
mtext("MASLD and Stroke Risk  ·  REML Estimator  ·  Bubble size ∝ inverse-variance weight",
      side = 3, line = -3.5, outer = TRUE, cex = 0.75, col = col_grey)
mtext(paste0("k = 21 studies  |  N ≈ 29.9 million  |  Pooled HR = ",
             sprintf("%.2f", exp(rma_age$beta[1])),
             "  |  Mean Age: p = ", sprintf("%.3f", rma_age$QMp),
             "  |  AF Adjustment: p = ", sprintf("%.3f", rma_af$QMp)),
      side = 1, line = -1.5, outer = TRUE, cex = 0.6, col = "gray75")

dev.off()
message("Figure 5: figures/Figure5_metareg_bubble.tiff")

# ============================================================
# Console summary
# ============================================================
cat("\n========================================\n")
cat(" Figure 5 — Meta-Regression Bubble Plots\n")
cat("========================================\n")
cat("\nPanel A — Mean Age (continuous):\n")
cat(sprintf("  k=%d:  R² = %.1f%%,  p = %.4f,  β = %.4f per year\n",
    rma_age$k, rma_age$R2, rma_age$QMp, rma_age$beta[2]))
cat(sprintf("  k=%d:  R² = %.1f%%,  p = %.4f,  β = %.4f per year  (without Kim 2020)\n",
    rma_age_nk$k, rma_age_nk$R2, rma_age_nk$QMp, rma_age_nk$beta[2]))
cat(sprintf("  Kim 2020: logHR=%.3f, mean_age=%.1f, weight=%.1f\n",
    merged$TE[kim_idx], merged$mean_age[kim_idx],
    1/(merged$seTE[kim_idx]^2 + rma_age$tau2)))

cat("\nPanel B — AF Adjustment (binary):\n")
cat(sprintf("  k=%d:  R² = %.1f%%,  p = %.4f,  β = %.4f\n",
    rma_af$k, rma_af$R2, rma_af$QMp, rma_af$beta[2]))
cat(sprintf("  Not Adjusted (k=%d): HR=%.2f [%.2f–%.2f]\n",
    n_no, exp(pred_af_0$pred), exp(pred_af_0$ci.lb), exp(pred_af_0$ci.ub)))
cat(sprintf("  Adjusted (k=%d):     HR=%.2f [%.2f–%.2f]\n",
    n_yes, exp(pred_af_1$pred), exp(pred_af_1$ci.lb), exp(pred_af_1$ci.ub)))
cat(sprintf("  k=%d:  R² = %.1f%%,  p = %.4f  (without Kim 2020)\n",
    rma_af_nk$k, rma_af_nk$R2, rma_af_nk$QMp))
cat("\n========================================\n")
