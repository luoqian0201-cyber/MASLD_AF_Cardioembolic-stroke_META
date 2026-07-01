# ============================================================
# Graphical Abstract v3.1 — Post-Audit + Language Refinement
# MASLD and Stroke: Evidence for Fibrosis Severity
# and AF-Associated Pathways
# EN + ZH | Three mechanistic columns + Three evidence cards + Key Message
# ============================================================

# ============================================================
# Color palette
# ============================================================
bg       <- "#FFFFFF"
org      <- "#E8611A"   # MASLD / pathogenic
org_l    <- "#FEF0E6"   # light orange bg
blue     <- "#2878B5"   # AF / heart
blue_l   <- "#EBF3FA"   # light blue bg
red      <- "#C0392B"   # stroke
red_l    <- "#FDEDEC"   # light red bg
green    <- "#27AE60"   # protective
green_l  <- "#E9F7EF"   # light green bg
brown    <- "#B8733B"   # fibrosis parallel
brown_l  <- "#FDF2E9"   # light brown bg
grey_d   <- "#2C3E50"   # dark text
grey_m   <- "#7F8C8D"   # medium text
grey_l   <- "#E5E7E9"   # light border
grey_bg  <- "#F8F9FA"   # panel bg
card_bg  <- "#FCFCFC"   # card background
gold_msg <- "#7D6608"   # key message text

# ============================================================
# Helper functions
# ============================================================
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

arrow_right <- function(x0, y0, x1, y1, col = grey_m, lwd = 2, ah = 0.15) {
  arrows(x0, y0, x1, y1, length = ah, col = col, lwd = lwd)
}

arrow_down <- function(x0, y0, x1, y1, col = grey_m, lwd = 2, ah = 0.12) {
  arrows(x0, y0, x1, y1, length = ah, col = col, lwd = lwd)
}

dashed_arrow <- function(x0, y0, x1, y1, col = grey_m, lwd = 2, ah = 0.12) {
  arrows(x0, y0, x1, y1, length = ah, col = col, lwd = lwd, lty = 2)
}

dashed_arrow_diag <- function(x0, y0, x1, y1, col = grey_m, lwd = 2, ah = 0.10) {
  arrows(x0, y0, x1, y1, length = ah, col = col, lwd = lwd, lty = 2)
}

# Draw an organ icon: Liver
draw_liver <- function(cx, cy, s = 1, col = org) {
  x <- cx + s * c(-3, -1.5, 0.5, 3.5, 3.5, 2, 0, -2.5, -3.5, -3.5) * 0.9
  y <- cy + s * c(0.5, 2.5, 3, 1, -1, -3, -3.5, -2.5, -1, 0.5) * 0.9
  polygon(x, y, col = org_l, border = col, lwd = 2.2)
  set.seed(42)
  points(cx + s * runif(10, -2.2, 2.2), cy + s * runif(10, -2.2, 2.2),
         pch = 16, col = "#F4C430", cex = runif(10, 0.4, 1.0))
  text(cx, cy, "MASLD", font = 2, cex = 0.9, col = org)
}

# Draw an organ icon: Heart
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

# Draw an organ icon: Brain
draw_brain <- function(cx, cy, s = 1, col = red) {
  x <- cx + s * c(-3, -2.8, -1.5, 0.5, 2, 3, 3, 1.5, 0, -1.5, -3) * 1.1
  y <- cy + s * c(0, 1.5, 3, 3.3, 2.5, 1, -1, -3, -3.5, -3, -1) * 1.1
  polygon(x, y, col = red_l, border = col, lwd = 2.2)
  for (gy in seq(cy + s*0.5, cy + s*2.5, length.out = 3)) {
    lines(c(cx - s*2.2, cx + s*2.2), c(gy, gy), col = col, lwd = 0.8, lty = 3)
  }
  text(cx, cy - s*0.3, "⚡", cex = 2, col = red)
}

# Small organ icons
draw_liver_small <- function(cx, cy, col = org) {
  draw_liver(cx, cy, s = 0.7, col = col)
}

draw_heart_small <- function(cx, cy, col = blue) {
  draw_heart(cx, cy, s = 0.7, col = col)
}

draw_brain_small <- function(cx, cy, col = red) {
  draw_brain(cx, cy, s = 0.7, col = col)
}

# ============================================================
# Main draw function
# ============================================================
draw_abstract <- function(lang = "en") {

  plot(NA, NA, xlim = c(0, 120), ylim = c(0, 100),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  # ---- Language strings ----
  if (lang == "en") {
    title_main    <- "MASLD and Stroke"
    title_sub     <- "Evidence for Fibrosis Severity and AF-Associated Pathways"

    col1_header   <- "FIBROSIS SEVERITY"
    col1_sub      <- "Confirmed Association"
    col2_header   <- "AF-ASSOCIATED PATHWAY"
    col2_sub      <- "Tentative Mechanism"
    col3_header   <- "FIBROSIS REGRESSION"
    col3_sub      <- "Protective Signal"

    # Column 1
    c1_node1      <- "MASLD"
    c1_arrow1     <- "Fibrosis progression"
    c1_node2      <- "Higher\nStroke Risk"
    c1_anno_hdr   <- "Advanced fibrosis"
    c1_anno_arrow <- "↑ stroke risk"
    c1_anno_hr    <- "(HR ≈ 1.65)"

    # Column 2
    c2_liver      <- "MASLD"
    c2_af         <- "AF"
    c2_brain      <- "Stroke"
    c2_arrow_lbl1 <- "AF\nincidence"
    c2_arrow_lbl2 <- "Cardio-\nembolism"
    c2_metric1    <- "AF adjustment"
    c2_metric2    <- "R² = 16.4%"
    c2_metric3    <- "p = 0.040"
    c2_tag        <- "Potential AF-associated"
    c2_tag2       <- "pathway"

    # Column 3
    c3_node1      <- "Fibrosis\nRegression"
    c3_hr         <- "HR = 0.58"
    c3_node2      <- "Potential Stroke\nRisk Reduction"
    c3_risk_lbl   <- "Potential\nrisk reduction"
    c3_anno       <- "(Park 2022)"

    # Card 1
    card1_title   <- "MAIN META-ANALYSIS"
    card1_big     <- "HR 1.37"
    card1_ci      <- "95% CI 1.28 – 1.46"
    card1_k       <- "k = 21 studies"
    card1_sub     <- "Random-effects (REML)"

    # Card 2
    card2_title   <- "AF-ASSOCIATED"
    card2_title2  <- "ATTENUATION"
    card2_big     <- "R² = 16.4%"
    card2_p       <- "p = 0.040"
    card2_sub     <- "AF adjustment attenuates"
    card2_sub2    <- "between-study variance"

    # Card 3
    card3_title   <- "FIBROSIS SEVERITY"
    card3_title2  <- "& DYNAMICS"
    card3_line1   <- "Severity:  HR ≈ 1.65"
    card3_line2   <- "Regression:  HR = 0.58"

    # Key Message
    key_msg <- paste0(
      "Key Message:  MASLD is associated with increased stroke risk, ",
      "with fibrosis severity and AF-associated pathways ",
      "representing potential contributors to risk heterogeneity."
    )

    footnote <- "Graphical Abstract"

  } else {
    title_main    <- "MASLD与卒中"
    title_sub     <- "肝纤维化严重度与房颚相关通路的证据"

    col1_header   <- "肝纤维化严重度"
    col1_sub      <- "已确认的关联"
    col2_header   <- "房颚相关通路"
    col2_sub      <- "潜在机制"
    col3_header   <- "肝纤维化逆转"
    col3_sub      <- "保护性信号"

    c1_node1      <- "MASLD"
    c1_arrow1     <- "肝纤维化进展"
    c1_node2      <- "卒中风险\n升高"
    c1_anno_hdr   <- "晚期肝纤维化"
    c1_anno_arrow <- "↑ 卒中风险"
    c1_anno_hr    <- "(HR ≈ 1.65)"

    c2_liver      <- "MASLD"
    c2_af         <- "房颚"
    c2_brain      <- "卒中"
    c2_arrow_lbl1 <- "房颚\n发生"
    c2_arrow_lbl2 <- "心源性\n栓塞"
    c2_metric1    <- "房颚调整"
    c2_metric2    <- "R² = 16.4%"
    c2_metric3    <- "p = 0.040"
    c2_tag        <- "潜在的房颚"
    c2_tag2       <- "相关通路"

    c3_node1      <- "肝纤维化\n逆转"
    c3_hr         <- "HR = 0.58"
    c3_node2      <- "潜在的卒中\n风险降低"
    c3_risk_lbl   <- "潜在的\n风险降低"
    c3_anno       <- "(Park 2022)"

    card1_title   <- "主要 Meta 分析"
    card1_big     <- "HR 1.37"
    card1_ci      <- "95% CI 1.28 – 1.46"
    card1_k       <- "k = 21 项研究"
    card1_sub     <- "随机效应 (REML)"

    card2_title   <- "房颚相关"
    card2_title2  <- "衰减"
    card2_big     <- "R² = 16.4%"
    card2_p       <- "p = 0.040"
    card2_sub     <- "房颚调整削减了"
    card2_sub2    <- "研究间方差"

    card3_title   <- "肝纤维化严重度"
    card3_title2  <- "与动态变化"
    card3_line1   <- "严重度:  HR ≈ 1.65"
    card3_line2   <- "逆转:  HR = 0.58"

    key_msg <- paste0(
      "关键信息:  MASLD与卒中风险增加相关，",
      "肝纤维化严重度和房颚相关通路",
      "是风险差异性的潜在贡献因素。"
    )

    footnote <- "图形摘要"
  }

  # ============================================================
  # TITLE BAR (taller — prevents text overlap)
  # ============================================================
  rrect(2, 88.5, 116, 10.5, r = 1.8, col = grey_bg, border = grey_l, lwd = 1)
  text(60, 96.5, title_main, cex = 1.8, font = 2, col = grey_d)
  text(60, 91.5, title_sub, cex = 0.90, col = grey_m, font = 3)

  # ============================================================
  # UPPER PANEL — Three Mechanistic Columns
  # ============================================================
  upper_y_bot <- 41
  upper_y_top <- 86.5
  rrect(2, upper_y_bot, 116, upper_y_top - upper_y_bot, r = 2,
        col = "#FAFBFC", border = grey_l, lwd = 0.8)

  # Column boundaries
  col1_left  <- 3
  col1_right <- 41
  col2_left  <- 42
  col2_right <- 78
  col3_left  <- 79
  col3_right <- 117

  # Vertical dividers
  segments(col1_right + 0.5, upper_y_bot + 3, col1_right + 0.5, upper_y_top - 2,
           col = "#E0E0E0", lwd = 1, lty = 2)
  segments(col2_right + 0.5, upper_y_bot + 3, col2_right + 0.5, upper_y_top - 2,
           col = "#E0E0E0", lwd = 1, lty = 2)

  # ===========================================
  # COLUMN 1: FIBROSIS SEVERITY (Confirmed, Solid)
  # ===========================================
  col1_cx <- 22
  col1_header_y <- 82

  # Header bar
  rrect(col1_left + 2, col1_header_y, 34, 4.5, r = 1, col = org, border = NA)
  text(col1_cx, col1_header_y + 2.8, col1_header, cex = 0.70, font = 2, col = "white")
  text(col1_cx, col1_header_y + 0.8, col1_sub, cex = 0.45, font = 3, col = "#FFFFFFCC")

  # Liver icon
  draw_liver(col1_cx, 76, s = 0.85, col = org)
  text(col1_cx, 73.5, c1_node1, cex = 0.60, col = grey_m, font = 3)

  # Solid down arrow
  arrow_down(col1_cx, 72, col1_cx, 68, col = org, lwd = 2.5)

  # Fibrosis progression label
  text(col1_cx, 66, c1_arrow1, cex = 0.75, font = 2, col = grey_d)

  # Solid down arrow
  arrow_down(col1_cx, 63.5, col1_cx, 59.5, col = org, lwd = 2.5)

  # Higher Stroke Risk node
  symbols(col1_cx, 56, circles = 2.5, add = TRUE, inches = FALSE,
          fg = red, bg = red_l, lwd = 2.5)
  text(col1_cx, 56, c1_node2, cex = 0.65, font = 2, col = red)

  # Annotation: Advanced fibrosis + up arrow + HR
  rrect(col1_left + 2, 43, 34, 9, r = 1, col = brown_l, border = brown, lwd = 1.2)
  text(col1_cx, 50, c1_anno_hdr, cex = 0.65, font = 2, col = brown)
  text(col1_cx, 48, c1_anno_arrow, cex = 0.62, font = 1, col = red)
  text(col1_cx, 46, c1_anno_hr, cex = 0.55, font = 2, col = grey_d)

  # ===========================================
  # COLUMN 2: AF-ASSOCIATED PATHWAY (Dashed, Tentative)
  # ===========================================
  col2_cx <- 60

  # Header bar
  rrect(col2_left + 1, col1_header_y, 35, 4.5, r = 1, col = blue, border = NA)
  text(col2_cx, col1_header_y + 2.8, col2_header, cex = 0.70, font = 2, col = "white")
  text(col2_cx, col1_header_y + 0.8, col2_sub, cex = 0.45, font = 3, col = "#FFFFFFCC")

  # Three organ icons in diagonal cascade
  liver_x <- 47
  liver_y <- 74
  draw_liver_small(liver_x, liver_y, col = org)
  text(liver_x, liver_y - 3, c2_liver, cex = 0.52, col = grey_m, font = 3)

  heart_x <- 60
  heart_y <- 67
  draw_heart_small(heart_x, heart_y, col = blue)
  text(heart_x, heart_y - 3, c2_af, cex = 0.52, col = grey_m, font = 3)

  brain_x <- 73
  brain_y <- 60
  draw_brain_small(brain_x, brain_y, col = red)
  text(brain_x, brain_y - 3, c2_brain, cex = 0.52, col = grey_m, font = 3)

  # Dashed diagonal arrows
  dashed_arrow_diag(50, 72.5, 56.5, 68.5, col = blue, lwd = 2.2, ah = 0.10)
  dashed_arrow_diag(63.5, 65.5, 70, 61.5, col = blue, lwd = 2.2, ah = 0.10)

  # Arrow labels
  text(52.5, 73, c2_arrow_lbl1, cex = 0.42, font = 3, col = blue, pos = 3)
  text(67, 67, c2_arrow_lbl2, cex = 0.42, font = 3, col = blue, pos = 3)

  # AF metrics box
  rrect(col2_left + 2, 43, 34, 14, r = 1.5, col = blue_l, border = blue, lwd = 1.2)
  text(col2_cx, 55.5, c2_metric1, cex = 0.58, font = 2, col = blue)
  text(col2_cx, 53, c2_metric2, cex = 0.68, font = 2, col = grey_d)
  text(col2_cx, 50.5, c2_metric3, cex = 0.55, font = 2, col = grey_d)
  text(col2_cx, 47.5, c2_tag, cex = 0.48, font = 3, col = blue)
  text(col2_cx, 45.2, c2_tag2, cex = 0.48, font = 3, col = blue)

  # ===========================================
  # COLUMN 3: FIBROSIS REGRESSION (Protective, Green)
  # ===========================================
  col3_cx <- 98

  # Header bar
  rrect(col3_left + 1, col1_header_y, 35, 4.5, r = 1, col = green, border = NA)
  text(col3_cx, col1_header_y + 2.8, col3_header, cex = 0.70, font = 2, col = "white")
  text(col3_cx, col1_header_y + 0.8, col3_sub, cex = 0.45, font = 3, col = "#FFFFFFCC")

  # Node 1: Fibrosis Regression
  rrect(col3_left + 6, 75, 25, 8, r = 2, col = green_l, border = green, lwd = 2)
  text(col3_cx, 80.5, c3_node1, cex = 0.72, font = 2, col = green)

  # Down arrow
  arrow_down(col3_cx, 74.5, col3_cx, 69.5, col = green, lwd = 2.5)

  # "Potential risk reduction" label on the arrow
  text(col3_cx + 8, 72, c3_risk_lbl, cex = 0.42, font = 3, col = green, pos = 4)

  # HR big number
  rrect(col3_left + 6, 62.5, 25, 6.5, r = 2, col = "white", border = green, lwd = 1.8)
  text(col3_cx, 66.5, c3_hr, cex = 1.2, font = 2, col = grey_d)

  # Down arrow
  arrow_down(col3_cx, 62, col3_cx, 57, col = green, lwd = 2.5)

  # Node 2: Potential Stroke Risk Reduction
  symbols(col3_cx, 53.5, circles = 2.5, add = TRUE, inches = FALSE,
          fg = green, bg = green_l, lwd = 2.5)
  text(col3_cx, 53.5, c3_node2, cex = 0.60, font = 2, col = green)

  # Park 2022
  rrect(col3_left + 6, 43, 25, 4.5, r = 1, col = "#F0F8F0", border = green, lwd = 1)
  text(col3_cx, 46.2, c3_anno, cex = 0.55, font = 3, col = green)

  # ============================================================
  # LOWER PANEL — Three Evidence Cards
  # ============================================================
  card_y  <- 6
  card_h  <- 32
  card_w  <- 35
  card_gap <- 4.5

  card1_x <- 3
  card2_x <- card1_x + card_w + card_gap
  card3_x <- card2_x + card_w + card_gap

  # ---- CARD 1: Main Meta-Analysis ----
  rrect(card1_x, card_y, card_w, card_h, r = 2.5, col = card_bg, border = org, lwd = 2)
  rrect(card1_x + 1, card_y + card_h - 5.5, card_w - 2, 4.5, r = 2, col = org, border = NA)
  text(card1_x + card_w/2, card_y + card_h - 2.8, card1_title,
       cex = 0.78, font = 2, col = "white")
  # Big number
  text(card1_x + card_w/2, card_y + 22, card1_big,
       cex = 2.3, font = 2, col = grey_d)
  # 95% CI
  text(card1_x + card_w/2, card_y + 17.5, card1_ci,
       cex = 0.80, font = 1, col = grey_d)
  # k
  text(card1_x + card_w/2, card_y + 14.5, card1_k,
       cex = 0.65, font = 2, col = org)
  # Sub
  text(card1_x + card_w/2, card_y + 11.5, card1_sub,
       cex = 0.52, font = 3, col = grey_m)

  # ---- CARD 2: AF-Associated Attenuation ----
  rrect(card2_x, card_y, card_w, card_h, r = 2.5, col = card_bg, border = blue, lwd = 2)
  rrect(card2_x + 1, card_y + card_h - 6.5, card_w - 2, 6, r = 2.5, col = blue, border = NA)
  text(card2_x + card_w/2, card_y + card_h - 2.5, card2_title,
       cex = 0.72, font = 2, col = "white")
  text(card2_x + card_w/2, card_y + card_h - 4.8, card2_title2,
       cex = 0.60, font = 2, col = "white")
  # Big number
  text(card2_x + card_w/2, card_y + 22, card2_big,
       cex = 2.1, font = 2, col = grey_d)
  # p-value
  text(card2_x + card_w/2, card_y + 17.5, card2_p,
       cex = 0.80, font = 2, col = grey_d)
  # Sub
  text(card2_x + card_w/2, card_y + 14, card2_sub,
       cex = 0.55, font = 3, col = grey_m)
  text(card2_x + card_w/2, card_y + 11.5, card2_sub2,
       cex = 0.55, font = 3, col = grey_m)

  # ---- CARD 3: Fibrosis Severity & Dynamics ----
  rrect(card3_x, card_y, card_w, card_h, r = 2.5, col = card_bg, border = green, lwd = 2)
  rrect(card3_x + 1, card_y + card_h - 6.5, card_w - 2, 6, r = 2.5, col = green, border = NA)
  text(card3_x + card_w/2, card_y + card_h - 2.5, card3_title,
       cex = 0.72, font = 2, col = "white")
  text(card3_x + card_w/2, card_y + card_h - 4.8, card3_title2,
       cex = 0.60, font = 2, col = "white")
  # Two-line content
  text(card3_x + card_w/2, card_y + 21, card3_line1,
       cex = 0.85, font = 2, col = grey_d)
  text(card3_x + card_w/2, card_y + 16.5, card3_line2,
       cex = 0.85, font = 2, col = green)

  # ============================================================
  # KEY MESSAGE BAR
  # ============================================================
  rrect(8, 1.2, 104, 3.8, r = 1.2, col = "#FEF9E7", border = "#D4AC0D", lwd = 1.2)
  text(60, 3.1, key_msg, cex = 0.50, font = 3, col = gold_msg)

  # ============================================================
  # FOOTNOTE
  # ============================================================
  text(60, 0.3, footnote, cex = 0.50, col = "gray80", font = 3)

}

# ============================================================
# Generate both versions
# ============================================================
# --- English ---
tiff("figures/Graphical_Abstract_EN.tiff",
     width = 14, height = 11, units = "in", res = 600)
par(mar = c(0, 0, 0, 0), bg = bg)
draw_abstract("en")
dev.off()
message("EN version: figures/Graphical_Abstract_EN.tiff")

# --- Chinese ---
tiff("figures/Graphical_Abstract_ZH.tiff",
     width = 14, height = 11, units = "in", res = 600)
par(mar = c(0, 0, 0, 0), bg = bg)
draw_abstract("zh")
dev.off()
message("ZH version: figures/Graphical_Abstract_ZH.tiff")

cat("\nGraphical Abstract v3.1 complete.\n")
cat("  EN: figures/Graphical_Abstract_EN.tiff\n")
cat("  ZH: figures/Graphical_Abstract_ZH.tiff\n")
