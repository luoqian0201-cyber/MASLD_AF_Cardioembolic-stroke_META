# ============================================================
# Figure 4 — Subgroup Analysis Summary
# MASLD & Stroke Meta-Analysis · k=21 FINAL
# Nature style · Forest-style subgroup panel
# ============================================================
library(meta)
library(readxl)

# ============================================================
# Data prep
# ============================================================
data <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx", sheet = "Main Meta Data")
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

dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx", sheet = "Study_Characteristics")
dc <- dc[-1, ]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label), ]
merged <- merge(
  data_main[, c("study_label", "TE", "seTE", "stroke_group")],
  dc[, c("study_label", "Liver_Definition", "adjusted_AF", "fibrosis", "Study_Design")],
  by = "study_label", all.x = TRUE)
merged$liver_def <- ifelse(grepl("MASLD", merged$Liver_Definition), "MASLD",
                    ifelse(grepl("MAFLD", merged$Liver_Definition), "MAFLD", "NAFLD"))
merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)
merged$fibrosis_assessed <- ifelse(
  is.na(merged$fibrosis) | merged$fibrosis == "N/A" | merged$fibrosis == "",
  "Not Assessed", "Assessed")
merged$pop_based <- ifelse(
  merged$Study_Design %in% c("Nationwide Cohort", "Population Cohort"),
  "Population-Based", "Hospital/Clinic-Based")
data_main$liver_def        <- merged$liver_def[match(data_main$study_label, merged$study_label)]
data_main$adj_af           <- merged$adj_af[match(data_main$study_label, merged$study_label)]
data_main$fibrosis_assessed <- merged$fibrosis_assessed[match(data_main$study_label, merged$study_label)]
data_main$pop_based        <- merged$pop_based[match(data_main$study_label, merged$study_label)]

m_main <- metagen(TE = TE, seTE = seTE, studlab = study_label,
                  sm = "HR", method.tau = "REML", data = data_main)

# ============================================================
# Run all 5 subgroup analyses
# ============================================================
m_stroke  <- update(m_main, subgroup = stroke_group,      tau.common = FALSE)
m_liver   <- update(m_main, subgroup = liver_def,         tau.common = FALSE)
m_af      <- update(m_main, subgroup = adj_af,            tau.common = FALSE)
m_fib     <- update(m_main, subgroup = fibrosis_assessed, tau.common = FALSE)
m_pop     <- update(m_main, subgroup = pop_based,         tau.common = FALSE)

# ============================================================
# Build summary data frame
# ============================================================
build_rows <- function(m, subgroup_name, levels, reverse = FALSE) {
  rows <- data.frame(
    subgroup  = subgroup_name,
    level     = levels,
    k         = m$k.w,
    HR        = exp(m$TE.random.w),
    lower     = exp(m$lower.random.w),
    upper     = exp(m$upper.random.w),
    I2        = m$I2.w * 100,
    tau2      = m$tau2.w,
    p_between = m$pval.Q.b.random,
    stringsAsFactors = FALSE
  )
  if (reverse) rows <- rows[nrow(rows):1, ]
  return(rows)
}

df <- rbind(
  build_rows(m_stroke, "Stroke Type",
             c("Total Stroke", "Ischemic Stroke")),
  build_rows(m_liver,  "MASLD Definition",
             c("MASLD", "NAFLD", "MAFLD")),
  build_rows(m_af,     "AF Adjustment",
             c("No", "Yes")),
  build_rows(m_fib,    "Fibrosis Assessment",
             c("Not Assessed", "Assessed")),
  build_rows(m_pop,    "Study Population",
             c("Hospital/Clinic-Based", "Population-Based"))
)

# ============================================================
# Figure 4 — Subgroup Forest Summary
# ============================================================
tiff("figures/Figure4_subgroup_summary.tiff",
     width = 14, height = 11.5, units = "in", res = 600)

N <- nrow(df)
groups <- unique(df$subgroup)
n_groups <- length(groups)

# Generous y-range: each level 1.6u + group header 2u + bottom padding
ymax <- N * 1.6 + n_groups * 2.2 + 3

par(mar = c(4, 16, 5, 14), bg = "white")

plot(NA, NA,
     xlim = c(0.88, 2.0), ylim = c(0, ymax),
     xlab = "Hazard Ratio (95% CI)",
     ylab = "", yaxt = "n", bty = "n", las = 1, log = "x")

# Reference
abline(v = 1, col = "gray70", lty = 3, lwd = 1.2)
abline(v = exp(m_main$TE.random), col = "#2C5F8A", lty = 2, lwd = 1)

# Color palette for subgroups
sub_cols <- c("Stroke Type"        = "#1B3A5C",
              "MASLD Definition"   = "#E67E22",
              "AF Adjustment"      = "#C0392B",
              "Fibrosis Assessment" = "#8E44AD",
              "Study Population"   = "#27AE60")

# Build y positions
current_y <- ymax - 1.5  # start near top

for (g in seq_along(groups)) {
  gn <- groups[g]
  idx <- which(df$subgroup == gn)
  nlev <- length(idx)

  # Group header band
  y_header <- current_y
  rect(0.86, y_header - 0.7, 2.02, y_header + 0.7,
       col = adjustcolor(sub_cols[gn], 0.10), border = sub_cols[gn], lwd = 1)
  text(0.88, y_header, gn, cex = 1.1, font = 2, col = sub_cols[gn], pos = 4)

  # P-between annotation (right-aligned in header)
  pval <- df$p_between[idx[1]]
  sig_mark <- ifelse(pval < 0.05, "**",
              ifelse(pval < 0.10, "*", ""))
  text(2.0, y_header,
       paste0("P-between = ", sprintf("%.3f", pval), "  ", sig_mark),
       cex = 0.75, font = ifelse(pval < 0.05, 2, 1),
       col = ifelse(pval < 0.05, sub_cols[gn], "gray60"), pos = 2)

  # Draw levels below header
  for (i in seq_len(nlev)) {
    row <- idx[i]
    y <- y_header - i * 1.7

    # Diamond
    lr <- df$lower[row]; hr <- df$HR[row]; ur <- df$upper[row]
    dx <- c(lr, hr, hr, ur, hr, hr, lr)
    dy <- c(y, y + 0.30, y, y, y - 0.30, y, y)
    polygon(dx, dy, col = adjustcolor(sub_cols[gn], 0.45),
            border = sub_cols[gn], lwd = 2)

    # Level label (left side)
    lab <- sprintf("%s  (k=%d)", df$level[row], df$k[row])
    text(0.88, y, lab, cex = 0.85, col = "#222222", pos = 4)

    # Right-side stats
    stat <- sprintf("%.2f  [%.2f–%.2f]  I²=%.0f%%",
                    df$HR[row], df$lower[row], df$upper[row], df$I2[row])
    text(1.98, y, stat, cex = 0.7, col = "#555555", pos = 2)
  }

  # Gap after levels
  current_y <- y - 1.8
}

# Title
title(main = "Subgroup Analyses — MASLD and Stroke Risk",
      cex.main = 1.8, font.main = 2, col.main = "#1B3A5C", line = 3)
mtext(paste0("Random-effects (REML)  |  k = 21  |  Pooled HR = ",
             sprintf("%.2f", exp(m_main$TE.random)),
             "  |  ◆ = subgroup pooled estimate  |  ** P<0.05  * P<0.10"),
      cex = 0.8, col = "gray50", line = 1.5)

# Footer annotation
mtext(paste0("MAFLD > NAFLD > MASLD: Consistent gradient across disease definitions  |  ",
             "AF adjustment markedly attenuates risk  |  ",
             "Fibrosis assessment shows trend toward lower HR"),
      cex = 0.65, col = "gray70", line = 0.2)

dev.off()
message("Figure 4: figures/Figure4_subgroup_summary.tiff")

# ============================================================
# Console summary
# ============================================================
cat("\n========================================\n")
cat(" Figure 4 — Subgroup Analysis Summary\n")
cat("========================================\n")
for (g in groups) {
  sub <- df[df$subgroup == g, ]
  cat(sprintf("\n--- %s ---\n", g))
  for (i in 1:nrow(sub)) {
    cat(sprintf("  %-20s  k=%-2d  HR=%.2f (%.2f–%.2f)  I²=%.0f%%\n",
        sub$level[i], sub$k[i], sub$HR[i], sub$lower[i], sub$upper[i], sub$I2[i]))
  }
  cat(sprintf("  P-between = %.3f  %s\n", sub$p_between[1],
      ifelse(sub$p_between[1] < 0.05, "**",
      ifelse(sub$p_between[1] < 0.10, "*", ""))))
}
cat("\n========================================\n")
