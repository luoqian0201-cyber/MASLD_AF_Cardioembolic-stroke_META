# ============================================================
# Supplementary Table S1 — AF Evidence Hierarchy
# Evidence for an AF-associated pathway between MASLD and stroke
# Nature Medicine / Lancet specialty supplementary style
# ============================================================
library(metafor)
library(readxl)

# ============================================================
# Data pipeline — compute meta-regression values
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

dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx",
                 sheet = "Study_Characteristics")
dc <- dc[-1, ]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label), ]

merged <- merge(data_main[, c("study_label", "TE", "seTE")],
                dc[, c("study_label", "adjusted_AF", "HR_MASLD_to_AF")],
                by = "study_label", all.x = TRUE)
merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)

# Meta-regression
rma_af <- rma(yi = TE, sei = seTE, mods = ~ adj_af,
              data = merged, method = "REML")
pred_no  <- predict(rma_af, newmods = c(0))
pred_yes <- predict(rma_af, newmods = c(1))

n_no  <- sum(!merged$adj_af, na.rm = TRUE)
n_yes <- sum(merged$adj_af, na.rm = TRUE)

# ============================================================
# Compute attenuation percentages
# ============================================================
pct_attenuation <- function(hr_before, hr_after) {
  round((hr_before - hr_after) / (hr_before - 1) * 100, 1)
}

# ============================================================
# Build evidence hierarchy table
# ============================================================

# Helper: format HR (95% CI)
fmt_hr <- function(hr, lo = NA, up = NA) {
  if (is.na(lo) || is.na(up)) return(sprintf("%.2f", hr))
  sprintf("%.2f [%.2f–%.2f]", hr, lo, up)
}

# ---- Row data ----
# Each row: list(evidence_level, study, evidence_type, hr_before, hr_lo_before, hr_up_before,
#                hr_after, hr_lo_after, hr_up_after, note, color_group)

rows <- list()

# Row 1: Meta-regression (strongest between-study evidence)
pct_meta <- pct_attenuation(exp(pred_no$pred), exp(pred_yes$pred))
rows[[1]] <- list(
  level       = "I",
  study       = sprintf("Meta-regression (k=%d)", n_no + n_yes),
  evidence    = "Between-study AF moderation",
  hr_before   = exp(pred_no$pred),
  hr_lo_before = exp(pred_no$ci.lb),
  hr_up_before = exp(pred_no$ci.ub),
  hr_after    = exp(pred_yes$pred),
  hr_lo_after  = exp(pred_yes$ci.lb),
  hr_up_after  = exp(pred_yes$ci.ub),
  pct         = pct_meta,
  note        = sprintf("R²=%.1f%%, p=%.3f, β=%.3f", rma_af$R2, rma_af$QMp, rma_af$beta[2]),
  color       = if (pct_meta > 40) "red" else if (pct_meta > 20) "orange" else "green"
)

# Row 2: Jang 2026 — Patient-level AF interaction (Level C, Absolute)
jang_pct <- pct_attenuation(1.10, 1.03)
rows[[2]] <- list(
  level       = "II",
  study       = "Jang et al. (2026)",
  evidence    = "Patient-level AF subgroup interaction",
  hr_before   = 1.10,
  hr_lo_before = NA,
  hr_up_before = NA,
  hr_after    = 1.03,
  hr_lo_after  = 0.95,
  hr_up_after  = 1.12,
  pct         = jang_pct,
  note        = "Absolute AF independence; Level C evidence; MASLD excess risk nearly eliminated in AF subgroup",
  color       = if (jang_pct > 40) "red" else if (jang_pct > 20) "orange" else "green"
)

# Row 3: Jang 2026 — Within-study AF adjustment
jang_ws_pct <- pct_attenuation(1.14, 1.10)
rows[[3]] <- list(
  level       = "III",
  study       = "Jang et al. (2026)",
  evidence    = "Within-study AF adjustment",
  hr_before   = 1.14,
  hr_lo_before = NA,
  hr_up_before = NA,
  hr_after    = 1.10,
  hr_lo_after  = NA,
  hr_up_after  = NA,
  pct         = jang_ws_pct,
  note        = "Model-based AF covariate adjustment; consistent with patient-level interaction",
  color       = if (jang_ws_pct > 40) "red" else if (jang_ws_pct > 20) "orange" else "green"
)

# Row 4: Lee 2021 — Bundle adjustment (Level B)
lee_pct <- pct_attenuation(1.55, 1.43)
rows[[4]] <- list(
  level       = "IV",
  study       = "Lee et al. (2021)",
  evidence    = "Bundle adjustment (CV comorbidities incl. AF)",
  hr_before   = 1.55,
  hr_lo_before = NA,
  hr_up_before = NA,
  hr_after    = 1.43,
  hr_lo_after  = NA,
  hr_up_after  = NA,
  pct         = lee_pct,
  note        = "Level B evidence; bundle includes AF + cardiovascular comorbidities; AF-specific contribution not isolatable",
  color       = if (lee_pct > 40) "red" else if (lee_pct > 20) "orange" else "green"
)

# Row 5: Ohno 2023 — Supporting pathway evidence (MASLD → AF)
rows[[5]] <- list(
  level       = "S1",
  study       = "Ohno et al. (2023)",
  evidence    = "MASLD → incident AF (pathway link)",
  hr_before   = NA,
  hr_lo_before = NA,
  hr_up_before = NA,
  hr_after    = 1.51,
  hr_lo_after  = 1.46,
  hr_up_after  = 1.57,
  pct         = NA,
  note        = "Establishes MASLD→AF association; does not directly test AF→stroke attenuation; large general-population cohort (n=8.2M)",
  color       = "grey"
)

# ============================================================
# Console output — formatted table
# ============================================================
cat("\n")
cat("====================================================================================================================\n")
cat("  Supplementary Table S1. Evidence hierarchy for the AF-associated pathway between MASLD and stroke risk\n")
cat("====================================================================================================================\n\n")

# Header
cat(sprintf("%-5s %-28s %-38s %-18s %-18s %7s  %s\n",
    "Level", "Study", "Evidence Type",
    "Without AF HR (95% CI)", "With AF HR (95% CI)",
    "% Atten.", "Interpretation"))
cat(strrep("-", 156), "\n")

# Color maps
color_name <- c("red" = "Substantial", "orange" = "Moderate", "green" = "Minimal", "grey" = "Supporting")
color_icon <- c("red" = "●●●", "orange" = "●●○", "green" = "●○○", "grey" = "···")

for (r in rows) {
  hr_before_str <- if (is.na(r$hr_before)) "—" else fmt_hr(r$hr_before, r$hr_lo_before, r$hr_up_before)
  hr_after_str  <- if (is.na(r$hr_after)) "—" else fmt_hr(r$hr_after, r$hr_lo_after, r$hr_up_after)
  pct_str       <- if (is.na(r$pct)) "—" else sprintf("%.1f%%", r$pct)
  interp_str    <- sprintf("%s %s", color_icon[r$color], color_name[r$color])

  cat(sprintf("%-5s %-28s %-38s %-18s %-18s %7s  %s\n",
      r$level, r$study, r$evidence,
      hr_before_str, hr_after_str,
      pct_str, interp_str))
  cat(sprintf("       %s\n", r$note))
  cat("\n")
}

cat("----------------------------------------------------------------------------------------------------------------------\n")
cat("Color coding: ●●● Red (Substantial attenuation, >40%)  |  ●●○ Orange (Moderate, 20–40%)  |  ●○○ Green (Minimal, <20%)\n")
cat("Evidence levels: I = Between-study meta-regression  |  II = Patient-level interaction  |  III = Within-study AF adjustment\n")
cat("  IV = Bundle adjustment (AF non-isolatable)  |  S1 = Supporting pathway evidence (MASLD→AF link)\n")
cat("\n% Attenuation = (HR_without_AF − HR_with_AF) / (HR_without_AF − 1) × 100\n")
cat("======================================================================================================================\n\n")

# ============================================================
# Export as CSV (Excel-ready)
# ============================================================
csv_rows <- list()
for (r in rows) {
  csv_rows[[length(csv_rows) + 1]] <- data.frame(
    Level              = r$level,
    Study              = r$study,
    Evidence_Type      = r$evidence,
    HR_Without_AF      = if (is.na(r$hr_before)) "—" else fmt_hr(r$hr_before, r$hr_lo_before, r$hr_up_before),
    HR_With_AF         = if (is.na(r$hr_after)) "—" else fmt_hr(r$hr_after, r$hr_lo_after, r$hr_up_after),
    Pct_Attenuation    = if (is.na(r$pct)) "—" else sprintf("%.1f%%", r$pct),
    Attenuation_Level  = color_name[r$color],
    Note               = r$note,
    stringsAsFactors   = FALSE
  )
}
csv_df <- do.call(rbind, csv_rows)

write.csv(csv_df, "tables/TableS1_AF_evidence_hierarchy.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
message("CSV exported: tables/TableS1_AF_evidence_hierarchy.csv")

# ============================================================
# Also print the clean markdown-style table for manuscript
# ============================================================
cat("\n--- Markdown table (for manuscript) ---\n\n")
cat("| Level | Study | Evidence Type | Without AF HR (95% CI) | With AF HR (95% CI) | % Attenuation | Interpretation |\n")
cat("|-------|-------|--------------|----------------------|--------------------|--------------|----------------|\n")
for (r in rows) {
  hr_before_str <- if (is.na(r$hr_before)) "—" else fmt_hr(r$hr_before, r$hr_lo_before, r$hr_up_before)
  hr_after_str  <- if (is.na(r$hr_after)) "—" else fmt_hr(r$hr_after, r$hr_lo_after, r$hr_up_after)
  pct_str       <- if (is.na(r$pct)) "—" else sprintf("%.1f%%", r$pct)
  interp_str    <- sprintf("%s %s", color_icon[r$color], color_name[r$color])

  cat(sprintf("| %s | %s | %s | %s | %s | %s | %s |\n",
      r$level, r$study, r$evidence,
      hr_before_str, hr_after_str,
      pct_str, interp_str))
}
cat("\n")
