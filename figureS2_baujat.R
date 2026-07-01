# ============================================================
# Supplementary Figure S2 — Baujat Plot
# MASLD & Stroke Meta-Analysis · k=21
# X-axis: Contribution to heterogeneity (Q)
# Y-axis: Influence on pooled effect size
# Highlights:
#   Lee 2025  (red)    — Effect-size driver
#   Kim 2020  (orange) — Age leverage (Meta-regression)
#   Jang 2026 (blue)   — Primary heterogeneity contributor
# Publication-quality · 600 DPI
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
# Compute Baujat coordinates
# ============================================================
TE_pooled <- m_main$TE.random
TE_i      <- m_main$TE
seTE_i    <- m_main$seTE

# Leave-one-out for Y-axis
leave1 <- metainf(m_main)
TE_loo   <- leave1$TE
seTE_loo <- leave1$seTE

# Baujat coordinates
xs <- (TE_i - TE_pooled)^2 / seTE_i^2
ys <- (TE_loo - TE_pooled)^2 / seTE_loo^2
studlab <- m_main$studlab

baujat_df <- data.frame(
  study     = studlab,
  x         = xs,
  y         = ys,
  TE        = TE_i,
  seTE      = seTE_i,
  TE_loo    = TE_loo,
  seTE_loo  = seTE_loo,
  stringsAsFactors = FALSE
)

# ============================================================
# Highlight mapping — three studies, three scientific roles
# ============================================================
# Pattern → (short_label, functional_role, color, line2)
highlight_defs <- list(
  list(pattern = "Lee et al. (2025)",
       label   = "Lee 2025",
       role    = "Effect-size driver",
       color   = "#C0392B",    # red
       pbg     = "#F1948A"),
  list(pattern = "Kim et al. (2020)",
       label   = "Kim 2020",
       role    = "Age leverage",
       role2   = "(Meta-regression)",
       color   = "#E67E22",    # orange
       pbg     = "#F5B041"),
  list(pattern = "Jang M et al. (2026)",
       label   = "Jang 2026",
       role    = "Primary heterogeneity",
       role2   = "contributor",
       color   = "#2874A6",    # navy-blue
       pbg     = "#7FB3D8")
)

# Tag studies
baujat_df$highlight <- FALSE
baujat_df$label     <- ""
baujat_df$role      <- ""
baujat_df$role2     <- ""
baujat_df$hl_color  <- "#95A5A6"
baujat_df$hl_pbg    <- "#E5E7E9"

for (hl in highlight_defs) {
  idx <- grep(hl$pattern, baujat_df$study, fixed = TRUE)
  if (length(idx) > 0) {
    baujat_df$highlight[idx] <- TRUE
    baujat_df$label[idx]     <- hl$label
    baujat_df$role[idx]      <- hl$role
    baujat_df$role2[idx]     <- if (is.null(hl$role2)) "" else hl$role2
    baujat_df$hl_color[idx]  <- hl$color
    baujat_df$hl_pbg[idx]    <- hl$pbg
  }
}

# ============================================================
# Color palette
# ============================================================
col_navy   <- "#1B3A5C"
col_red    <- "#C0392B"
col_orange <- "#E67E22"
col_blue   <- "#2874A6"
col_grey   <- "#95A5A6"
col_dark   <- "#555555"

# ============================================================
# Figure S2 — Two-Panel Layout: Baujat Plot + Annotations
# ============================================================
tiff("figures/FigureS2_baujat.tiff",
     width = 14, height = 10, units = "in", res = 600)

# Layout: Left (Baujat) 62% | Right (Legend + Key Findings) 38%
layout(matrix(c(1, 2), nrow = 1), widths = c(0.62, 0.38))

x_data_max <- max(baujat_df$x)
y_data_max <- max(baujat_df$y)
x_med <- median(baujat_df$x)
y_med <- median(baujat_df$y)

# ============================================================
# PANEL 1 — Baujat Plot (clean, no annotation boxes inside)
# ============================================================
par(mar = c(4.5, 6, 5, 3), bg = "white")

x_max <- x_data_max * 1.08
y_max <- y_data_max * 1.12

plot(baujat_df$x, baujat_df$y,
     xlim = c(0, x_max),
     ylim = c(0, y_max),
     xlab = "",
     ylab = "",
     type = "n",
     axes = FALSE,
     main = "")

# Grid
abline(h = pretty(c(0, y_data_max), 6), col = "#F0F0F0", lty = 1, lwd = 0.8)
abline(v = pretty(c(0, x_data_max), 6), col = "#F0F0F0", lty = 1, lwd = 0.8)

# Axes
axis(1, lwd = 0, lwd.ticks = 1, col.ticks = "gray60", col = "gray60")
axis(2, lwd = 0, lwd.ticks = 1, col.ticks = "gray60", col = "gray60", las = 1)

mtext(expression("Contribution to Heterogeneity" ~ (Q[italic(i)])),
      side = 1, line = 3.2, cex = 1.2, col = "#333333")
mtext(expression("Influence on Pooled Effect" ~ (italic(d)[italic(i)])),
      side = 2, line = 4.5, cex = 1.2, col = "#333333")

# ---- Non-highlighted studies (grey) ----
non_hl <- baujat_df[!baujat_df$highlight, ]
points(non_hl$x, non_hl$y,
       pch = 21, cex = 1.3,
       col = "#CCCCCC", bg = adjustcolor("#95A5A6", 0.30))

# ---- Highlighted studies — point + study name only (no role text inside plot) ----
hl_df <- baujat_df[baujat_df$highlight, ]
for (i in seq_len(nrow(hl_df))) {
  xi  <- hl_df$x[i]
  yi  <- hl_df$y[i]
  clr <- hl_df$hl_color[i]
  pbg <- hl_df$hl_pbg[i]

  # Large highlighted point
  points(xi, yi,
         pch = 21, cex = 2.8,
         col = clr, bg = adjustcolor(pbg, 0.35), lwd = 3)

  # Study name only (functional role moved to right panel)
  # Position: offset to avoid overlap with other points
  # Lee 2025: high y → label above-right
  # Kim 2020: mid → label right
  # Jang 2026: far right x → label above
  if (hl_df$label[i] == "Jang 2026") {
    text(xi, yi + y_data_max * 0.06, hl_df$label[i],
         cex = 0.95, font = 2, col = clr)
  } else {
    text(xi, yi, hl_df$label[i],
         pos = 4, offset = 0.9, cex = 0.95, font = 2, col = clr)
  }
}

# ---- Quadrant lines ----
segments(x_med, 0, x_med, y_data_max,
         col = "gray80", lty = 2, lwd = 1.2)
segments(0, y_med, x_data_max, y_med,
         col = "gray80", lty = 2, lwd = 1.2)

# ---- Quadrant labels (LARGER, DARKER, MORE VISIBLE) ----
quad_cex  <- 0.75
quad_col  <- "#444444"
quad_font <- 3  # italic

# Top-right
text(x_data_max * 0.98, y_data_max * 0.99,
     expression( atop("High Influence /",
                      "High Heterogeneity")),
     cex = quad_cex, col = quad_col, pos = 2, font = quad_font)

# Top-left
text(x_med * 0.03, y_data_max * 0.99,
     expression( atop("High Influence /",
                      "Low Heterogeneity")),
     cex = quad_cex, col = quad_col, pos = 4, font = quad_font)

# Bottom-right
text(x_data_max * 0.98, y_med * 0.01,
     "Heterogeneity\nContributors",
     cex = quad_cex, col = quad_col, pos = 2, font = quad_font)

# Bottom-left
text(x_med * 0.03, y_med * 0.01,
     "Stable\nContributors",
     cex = quad_cex, col = quad_col, pos = 4, font = quad_font)

# ---- Title (inside Panel 1) ----
title(main = expression(bold("Supplementary Figure S2.") ~
      "Baujat Plot — Influential & Heterogeneous Studies"),
      cex.main = 1.15, col.main = col_navy, line = 3.5)
mtext(expression("MASLD & Stroke Meta-Analysis" ~ (italic(k) == 21) ~
      " · Random-effects (REML)  ·  Dashed lines = median thresholds"),
      cex = 0.75, col = "gray50", line = 1.5)

# ---- Footer ----
mtext(paste0("Upper quadrants: disproportionate influence on pooled HR.  ",
      "Right quadrants: drive between-study heterogeneity."),
      cex = 0.60, col = "gray70", line = 0.5, side = 1, adj = 0.5)

# ============================================================
# PANEL 2 — Annotations (Functional Role Legend + Key Findings)
# Clean vertical separation: no overlap between sections
# ============================================================
par(mar = c(4.5, 1.5, 5, 2.5), bg = "white")

# Empty plot for annotation space
plot(NA, NA,
     xlim = c(0, 100), ylim = c(0, 100),
     xlab = "", ylab = "", axes = FALSE, main = "")

# ---- SECTION 1: HIGHLIGHTED STUDIES (upper: y = 55–100) ----

# Header banner
rect(5, 88, 95, 98, col = col_navy, border = NA)
text(50, 93, "HIGHLIGHTED STUDIES", cex = 0.90, font = 2, col = "white")
text(50, 84.5, "Functional Role Classification", cex = 0.58, font = 3, col = "gray50")

# Three study entries — compact, pushed upward
jq_q <- round(hl_df$x[hl_df$label == "Jang 2026"], 0)

role_entries <- list(
  list(y = 74, color = col_red,    pbg = "#F1948A",
       name = "Lee 2025", role = "Effect-size driver",
       detail = "Dominant influence on pooled HR (LOO HR = 1.230)"),
  list(y = 64, color = col_orange, pbg = "#F5B041",
       name = "Kim 2020", role = "Age leverage (Meta-regression)",
       detail = "Heterogeneity + age-related influence (Age R² = 26.5%)"),
  list(y = 55, color = col_blue,   pbg = "#7FB3D8",
       name = "Jang 2026", role = "Primary heterogeneity contributor",
       detail = paste0("Q-contrib = ", jq_q, "  ·  5.4× above #2"))
)

for (ent in role_entries) {
  # Color accent bar
  rect(8, ent$y - 3, 12, ent$y + 3, col = ent$color, border = NA)
  # Study name
  text(17, ent$y + 1.2, ent$name, cex = 0.80, font = 2, col = ent$color, pos = 4)
  # Functional role
  text(17, ent$y - 2.0, ent$role, cex = 0.58, font = 3, col = "#444444", pos = 4)
  # Detail
  text(17, ent$y - 4.8, ent$detail, cex = 0.47, col = "gray60", pos = 4)
}

# ---- VISUAL SEPARATOR: clear gap between sections ----
# Double separator lines in the empty zone
segments(5, 50.5, 95, 50.5, col = "gray80", lwd = 1.0, lty = 2)
segments(5, 49.0, 95, 49.0, col = "gray88", lwd = 0.6)

# ---- SECTION 2: KEY FINDINGS (lower: y = 0–46) ----
# Generous separation from HIGHLIGHTED STUDIES (Jang entry ends at y=52)

# Outer box
rect(3, 0.5, 97, 46,
     col = adjustcolor("#F8F9FA", 0.92),
     border = col_navy, lwd = 2.5)

# Header band (inside box)
rect(3, 37, 97, 46,
     col = adjustcolor(col_navy, 0.08), border = NA)
text(50, 41.5, "KEY FINDINGS", cex = 0.85, font = 2, col = col_navy)

# Separator below header
segments(14, 34, 86, 34, col = col_navy, lwd = 1.2)

# Findings — 4 merged bullet points
findings <- c(
  "• Lee 2025 exerted the greatest influence on the pooled HR (LOO HR = 1.230).",
  "• Jang 2026 contributed most substantially to between-study heterogeneity.",
  "• Kim 2020 demonstrated both heterogeneity contribution and age-related leverage.",
  "• No influential study altered the overall conclusion."
)

fy_first <- 30
fy_gap   <- 5.8

for (j in seq_along(findings)) {
  fy <- fy_first - (j - 1) * fy_gap
  text(10, fy, findings[j], cex = 0.58, col = "#444444", pos = 4)
}

# ---- Bottom footnote ----
mtext(paste0("Baujat diagnostic decomposition  ·  Random-effects (REML)  ·  k = 21  ·  ",
      "Grey points = other 18 studies"),
      cex = 0.48, col = "gray70", line = 0.5, side = 1, adj = 0.5)

dev.off()

message("Figure S2: figures/FigureS2_baujat.tiff")

# ============================================================
# Console summary
# ============================================================
cat("\n========================================\n")
cat(" Supplementary Figure S2 — Baujat Plot\n")
cat("========================================\n")
cat(sprintf("\nPooled HR (k=21): %.3f  |  I² = %.1f%%\n\n",
            exp(TE_pooled), m_main$I2 * 100))

cat("--- Highlighted Studies ---\n")
hl_df <- baujat_df[baujat_df$highlight, ]
for (i in seq_len(nrow(hl_df))) {
  cat(sprintf("  %-15s  Q-contrib = %7.2f  |  influence = %7.2f  |  LOO HR = %.3f  |  %s\n",
      hl_df$label[i], hl_df$x[i], hl_df$y[i], exp(hl_df$TE_loo[i]), hl_df$role[i]))
}

cat("\n--- Top 10 by Heterogeneity Contribution ---\n")
baujat_df_x <- baujat_df[order(-baujat_df$x), ]
for (i in 1:min(10, nrow(baujat_df_x))) {
  marker <- if (baujat_df_x$highlight[i]) "  ←" else ""
  cat(sprintf("  #%d  %-40s  Q-contrib = %.2f%s\n",
      i, baujat_df_x$study[i], baujat_df_x$x[i], marker))
}

cat("\n--- Top 10 by Influence on Pooled Effect ---\n")
baujat_df_y <- baujat_df[order(-baujat_df$y), ]
for (i in 1:min(10, nrow(baujat_df_y))) {
  marker <- if (baujat_df_y$highlight[i]) "  ←" else ""
  cat(sprintf("  #%d  %-40s  influence = %.2f%s\n",
      i, baujat_df_y$study[i], baujat_df_y$y[i], marker))
}

cat(sprintf("\n  Median Q-contribution: %.2f\n", median(baujat_df$x)))
cat(sprintf("  Median influence:      %.2f\n", median(baujat_df$y)))
cat("\nOutput: figures/FigureS2_baujat.tiff\n")
cat("========================================\n")
