# ============================================================
# Figure 1 — PRISMA 2020 Flow Diagram
# MASLD & Stroke Meta-Analysis
# Nature style · Clean layout · Editable parameters
# ============================================================

# ============================================================
# USER: Replace these numbers with actual search results
# ============================================================
N_identified    <- 2192  # PubMed + Embase + Web of Science total
N_pubmed        <- 386   # PubMed only
N_embase        <- 1386  # Embase only
N_wos           <- 420   # Web of Science only
N_duplicates    <- 577   # Duplicates removed
N_screened      <- 1615  # Title/abstract screened
N_excluded      <- 1025  # Records excluded at screening
N_ft_assessed   <- 590   # Full-text assessed
N_ft_excluded   <- 566   # Full-text excluded
N_qualitative   <- 24    # Studies in qualitative synthesis
N_quantitative  <- 21    # Studies in meta-analysis

# Exclusion reasons
excl_reasons <- c(
  "Meeting abstracts    = 403",
  "Reviews              = 91",
  "Protocols            = 15",
  "Cross-sectional      = 23",
  "No outcome           = 12",
  "No effect estimate   = 9",
  "Duplicate cohort     = 7",
  "Other                = 6"
)

# ============================================================
# PRISMA diagram
# ============================================================
prisma <- function() {
  # Colors — Nature style
  box_fill   <- "#F0F4F8"
  box_border <- "#7FA3C1"
  title_fill <- "#4A7BA7"
  excl_fill  <- "#FDF2F2"
  excl_border <- "#D4A0A0"
  final_fill <- "#E8F0E8"
  final_border <- "#7AAA7A"
  text_col  <- "#1A1A1A"
  grey_col  <- "#666666"

  plot(NA, NA, xlim = c(0, 100), ylim = c(0, 100),
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  # Rounded box helper
  draw_box <- function(x, y, w, h, label, sub = NULL,
                       fill = box_fill, border = box_border,
                       title = NULL, title_fill = NULL) {
    r <- 1.2; n <- 10
    angles <- seq(0, pi/2, length.out = n)
    xa <- cos(angles) * r; ya <- sin(angles) * r
    xx <- c(x - w/2 + r, x + w/2 - r, x + w/2 - r + xa,
            x + w/2, x + w/2, x + w/2 - r + rev(ya),
            x + w/2 - r, x - w/2 + r, x - w/2 + r - ya,
            x - w/2, x - w/2, x - w/2 + r - rev(xa))
    yy <- c(y - h/2, y - h/2, y - h/2 + r - ya,
            y - h/2 + r, y + h/2 - r, y + h/2 - r + rev(xa),
            y + h/2, y + h/2, y + h/2 - r + xa,
            y + h/2 - r, y - h/2 + r, y - h/2 + r - rev(ya))
    polygon(xx, yy, col = fill, border = border, lwd = 1.5)
    if (!is.null(title)) {
      rect(x - w/2, y + h/2 - 4, x + w/2, y + h/2,
           col = title_fill, border = NA)
      text(x, y + h/2 - 2, title, cex = 0.7, font = 2, col = "white")
    }
    text(x, y, label, cex = 0.8, font = 2, col = text_col)
    if (!is.null(sub)) {
      text(x, y - 2.2, sub, cex = 0.6, col = grey_col)
    }
  }

  my_arrow <- function(x0, y0, x1, y1, col = grey_col) {
    arrows(x0, y0, x1, y1, length = 0.08, col = col, lwd = 1.5)
  }

  # =========================================================
  # IDENTIFICATION
  # =========================================================
  text(8, 97, "Identification", cex = 1.2, font = 2, col = title_fill, pos = 4)

  # Three database boxes
  db_w <- 18; db_h <- 5; db_y <- 93.5
  draw_box(18, db_y, db_w, db_h,
           sprintf("PubMed\n(n = %s)", N_pubmed),
           fill = "#F7F9FC", border = "#B0C4D8")
  draw_box(50, db_y, db_w, db_h,
           sprintf("Embase\n(n = %s)", N_embase),
           fill = "#F7F9FC", border = "#B0C4D8")
  draw_box(82, db_y, db_w, db_h,
           sprintf("Web of Science\n(n = %s)", N_wos),
           fill = "#F7F9FC", border = "#B0C4D8")

  # Total identified
  draw_box(50, 86, 56, 6,
           sprintf("Records identified (n = %s)", N_identified))

  my_arrow(50, 83, 50, 80)

  # Duplicates removed
  draw_box(50, 77, 56, 5,
           sprintf("Duplicates removed (n = %s)", N_duplicates),
           fill = excl_fill, border = excl_border)

  my_arrow(50, 74.5, 50, 71.5)

  # =========================================================
  # SCREENING
  # =========================================================
  text(8, 70, "Screening", cex = 1.2, font = 2, col = title_fill, pos = 4)

  # Records screened
  draw_box(50, 67, 56, 6,
           sprintf("Records screened (n = %s)", N_screened))

  my_arrow(50, 64, 50, 61)

  # Records excluded at screening
  my_arrow(78, 64, 90, 64)
  draw_box(94, 64, 6, 6,
           sprintf("Excluded\n(n = %s)", N_excluded),
           fill = excl_fill, border = excl_border)

  # =========================================================
  # ELIGIBILITY
  # =========================================================
  text(8, 57, "Eligibility", cex = 1.2, font = 2, col = title_fill, pos = 4)

  my_arrow(50, 58, 50, 55)

  # Full-text assessed
  draw_box(50, 51, 56, 7,
           sprintf("Full-text articles assessed\nfor eligibility (n = %s)", N_ft_assessed))

  # FT exclusion box (right)
  my_arrow(78, 52, 88, 52)
  draw_box(93.5, 52, 7, 10,
           sprintf("Excluded\n(n = %s)", N_ft_excluded),
           fill = excl_fill, border = excl_border)

  # Exclusion reasons (below exclusion box, left-aligned)
  reasons_text <- paste(excl_reasons, collapse = "\n")
  text(93.5, 44, reasons_text, cex = 0.45, col = grey_col, pos = 1)

  # =========================================================
  # INCLUDED
  # =========================================================
  text(8, 35, "Included", cex = 1.2, font = 2, col = title_fill, pos = 4)

  my_arrow(50, 46.5, 50, 43)

  # Qualitative synthesis
  draw_box(50, 39, 56, 7,
           sprintf("Studies included in qualitative\nsynthesis (n = %s)", N_qualitative),
           fill = final_fill, border = final_border)

  my_arrow(50, 35.5, 50, 32.5)

  # Meta-analysis
  draw_box(50, 28, 56, 8,
           sprintf("Studies included in quantitative\nsynthesis (meta-analysis) (k = %s)", N_quantitative),
           fill = "#D5E8D5", border = "#5A9A5A")

  # =========================================================
  # FOOTER
  # =========================================================
  text(95, 2, "PRISMA 2020", cex = 0.7, font = 3, col = "gray75", pos = 2)
  text(50, 4, "Databases: PubMed, Embase, Web of Science  |  Search date: [DATE]",
       cex = 0.55, col = "gray80")
  text(50, 20, paste0("Note: 24 studies met inclusion criteria; 3 were excluded",
        " from quantitative synthesis\n(Kim B.S. Kim 2025, Lee 2021 MAFLD-only,",
        " Park 2022 — separate cohorts / subset analysis)"),
       cex = 0.5, col = "gray80")
}

# ============================================================
# Export TIFF (600 DPI)
# ============================================================
tiff("figures/Figure1_PRISMA.tiff",
     width = 10, height = 12, units = "in", res = 600)
par(mar = c(1, 1, 1, 1), bg = "white")
prisma()
dev.off()
message("Figure 1: figures/Figure1_PRISMA.tiff")

cat("\n========================================\n")
cat(" PRISMA 2020 — 已完成\n")
cat("========================================\n")
cat(" Identification:\n")
cat(sprintf("   Records identified:    %s (PubMed %s + Embase %s + WoS %s)\n",
    N_identified, N_pubmed, N_embase, N_wos))
cat(sprintf("   Duplicates removed:    %s\n", N_duplicates))
cat(" Screening:\n")
cat(sprintf("   Records screened:      %s\n", N_screened))
cat(sprintf("   Records excluded:      %s\n", N_excluded))
cat(" Eligibility:\n")
cat(sprintf("   Full-text assessed:    %s\n", N_ft_assessed))
cat(sprintf("   Full-text excluded:    %s\n", N_ft_excluded))
cat(" Included:\n")
cat(sprintf("   Qualitative synthesis: %s\n", N_qualitative))
cat(sprintf("   Meta-analysis:         k = %s\n", N_quantitative))
cat("\n   Flow check: 2192 - 577 = 1615; 1615 - 1025 = 590; 590 - 566 = 24; 24 - 3 = 21 ✓\n")
cat("========================================\n")
