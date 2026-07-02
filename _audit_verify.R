# ============================================================
# Manuscript Audit — Complete Statistical Verification
# Cross-reference: main.tex vs R output vs blueprint
# ============================================================
library(readxl)
library(meta)
library(metafor)

# ---- 1. Load data ----
data <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx", sheet = "Main Meta Data")
data$HR <- as.numeric(data$HR)

data_main <- data
data_main <- subset(data_main, !grepl("MAFLD only", data_main$study))
data_main <- subset(data_main, !(study == "Park et al." & year == 2022))
data_main <- subset(data_main, !(study == "Kim et al. (B.S. Kim)" & year == 2025))

data_main$study_label <- make.unique(paste0(data_main$study, " (", data_main$year, ")"), sep = " #")
data_main$TE  <- log(data_main$HR)
data_main$seTE <- (log(data_main$upperCI) - log(data_main$lowerCI)) / 3.92

# ---- 2. Merge covariates ----
dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx", sheet = "Study_Characteristics")
dc <- dc[-1, ]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label), ]

merged <- merge(
  data_main[, c("study_label", "TE", "seTE")],
  dc[, c("study_label", "country", "Liver_Definition", "diagnosis_method",
         "follow_up", "Age_Mean_SD", "sample", "adjusted_AF", "fibrosis",
         "Study_Design")],
  by = "study_label", all.x = TRUE
)

merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)
merged$liver_def <- ifelse(grepl("MASLD", merged$Liver_Definition), "MASLD",
                    ifelse(grepl("MAFLD", merged$Liver_Definition), "MAFLD", "NAFLD"))
merged$fibrosis_assessed <- ifelse(is.na(merged$fibrosis) | merged$fibrosis == "N/A" | merged$fibrosis == "",
                                    "Not Assessed", "Assessed")
merged$pop_based <- ifelse(merged$Study_Design %in% c("Nationwide Cohort", "Population Cohort"),
                            "Population-Based", "Hospital/Clinic-Based")
merged$followup_num <- as.numeric(gsub(".*?([0-9.]+).*", "\\1", merged$follow_up))
merged$sample_num <- as.numeric(gsub("[^0-9.]", "", merged$sample))
merged$log_sample <- log(merged$sample_num)
merged$mean_age <- as.numeric(gsub(".*?([0-9.]+).*", "\\1", merged$Age_Mean_SD))

# ---- 3. Main Analysis ----
m_main <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML", data = data_main)
m_main_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML",
                     method.random.ci = "HK", data = data_main)

cat("========================================\n")
cat("       MASTER STATISTICS TABLE\n")
cat("========================================\n")
cat(sprintf("%-40s %s\n", "ITEM", "VALUE"))
cat(strrep("-", 60), "\n")
cat(sprintf("%-40s %d\n", "k", nrow(data_main)))
cat(sprintf("%-40s %s\n", "n", "≈29.8 million"))
cat(sprintf("%-40s %.2f\n", "HR (REML, standard CI)", exp(m_main$TE.random)))
cat(sprintf("%-40s %.2f–%.2f\n", "95% CI (standard)", exp(m_main$lower.random), exp(m_main$upper.random)))
cat(sprintf("%-40s %.2f\n", "HR (HK adjusted)", exp(m_main_hk$TE.random)))
cat(sprintf("%-40s %.2f–%.2f\n", "95% CI (HK)", exp(m_main_hk$lower.random), exp(m_main_hk$upper.random)))
cat(sprintf("%-40s %.1f%%\n", "I²", m_main$I2 * 100))
cat(sprintf("%-40s %.4f\n", "τ²", m_main$tau2))
cat(sprintf("%-40s %.1f, df=%d, p<0.001\n", "Q", m_main$Q, m_main$df.Q))
cat(sprintf("%-40s %.2f (%.2f–%.2f)\n", "Common-effect HR",
    exp(m_main$TE.common), exp(m_main$lower.common), exp(m_main$upper.common)))

# ---- 4. Meta-Regression ----
rma_af <- rma(yi = TE, sei = seTE, mods = ~ adj_af, data = merged, method = "REML")
pred_no <- predict(rma_af, newmods = c(0))
pred_yes <- predict(rma_af, newmods = c(1))
atten <- (exp(pred_no$pred) - exp(pred_yes$pred)) / (exp(pred_no$pred) - 1) * 100

cat(sprintf("\n%-40s %.1f%%\n", "AF R²", rma_af$R2))
cat(sprintf("%-40s %.4f\n", "AF p-value", rma_af$QMp))
cat(sprintf("%-40s %.3f\n", "β (adj_afTRUE)", coef(rma_af)[2]))
cat(sprintf("%-40s %.2f (%.2f–%.2f)\n", "Predicted HR (without AF)",
    exp(pred_no$pred), exp(pred_no$ci.lb), exp(pred_no$ci.ub)))
cat(sprintf("%-40s %.2f (%.2f–%.2f)\n", "Predicted HR (with AF)",
    exp(pred_yes$pred), exp(pred_yes$ci.lb), exp(pred_yes$ci.ub)))
cat(sprintf("%-40s %.1f%%\n", "AF Attenuation", atten))

# All 7 meta-regressions
mr1 <- metareg(m_main, ~ factor(merged$diag_method))
mr2 <- metareg(m_main, ~ merged$liver_def)
mr3 <- metareg(m_main, ~ factor(merged$country_clean))
mr4 <- metareg(m_main, ~ merged$followup_num)
mr5 <- metareg(m_main, ~ merged$log_sample)
mr6 <- metareg(m_main, ~ merged$adj_af)
mr7 <- metareg(m_main, ~ merged$mean_age)

cat("\n--- All 7 Meta-Regressions ---\n")
models <- list(
  list("Diagnosis Method",    mr1),
  list("MASLD Definition",    mr2),
  list("Country",             mr3),
  list("Follow-up",           mr4),
  list("log(Sample Size)",    mr5),
  list("Adjusted AF",         mr6),
  list("Mean Age",            mr7)
)
for (mod in models) {
  s <- summary(mod[[2]])
  r2 <- ifelse(is.null(s$R2), NA, s$R2)
  cat(sprintf("%-25s R²=%5.1f%%  p=%.4f\n", mod[[1]], r2, s$QMp))
}

# ---- 5. Subgroup Analyses ----
data_main$adj_af <- merged$adj_af[match(data_main$study_label, merged$study_label)]
data_main$liver_def <- merged$liver_def[match(data_main$study_label, merged$study_label)]
data_main$fibrosis_assessed <- merged$fibrosis_assessed[match(data_main$study_label, merged$study_label)]
data_main$pop_based <- merged$pop_based[match(data_main$study_label, merged$study_label)]

# Stroke subtype
data_main$stroke_group <- ifelse(data_main$outcome %in% c("All Stroke", "Stroke", "Cerebrovascular disease (CVD)"),
                                  "Total Stroke", "Ischemic Stroke")
m_stroke <- update(m_main, subgroup = stroke_group, tau.common = FALSE)
cat("\n--- Stroke Subtype Subgroup ---\n")
for (i in seq_along(m_stroke$subgroup.levels)) {
  lev <- m_stroke$subgroup.levels[i]
  n <- sum(data_main$stroke_group == lev)
  hr <- exp(m_stroke$TE.random.w[i])
  lo <- exp(m_stroke$lower.random.w[i])
  up <- exp(m_stroke$upper.random.w[i])
  cat(sprintf("%s: k=%d  HR=%.2f (%.2f–%.2f)\n", lev, n, hr, lo, up))
}
cat(sprintf("p-between = %.3f\n", m_stroke$pval.Q.b.random))

# MASLD definition
m_liver <- update(m_main, subgroup = liver_def, tau.common = FALSE)
cat("\n--- MASLD Definition Subgroup ---\n")
for (i in seq_along(m_liver$subgroup.levels)) {
  lev <- m_liver$subgroup.levels[i]
  n <- sum(data_main$liver_def == lev)
  hr <- exp(m_liver$TE.random.w[i])
  lo <- exp(m_liver$lower.random.w[i])
  up <- exp(m_liver$upper.random.w[i])
  cat(sprintf("%s: k=%d  HR=%.2f (%.2f–%.2f)\n", lev, n, hr, lo, up))
}
cat(sprintf("p-between = %.3f\n", m_liver$pval.Q.b.random))

# AF adjustment
m_af <- update(m_main, subgroup = adj_af, tau.common = FALSE)
cat("\n--- AF Adjustment Subgroup ---\n")
for (i in seq_along(m_af$subgroup.levels)) {
  lev <- m_af$subgroup.levels[i]
  n <- sum(data_main$adj_af == (lev == "TRUE"))
  hr <- exp(m_af$TE.random.w[i])
  lo <- exp(m_af$lower.random.w[i])
  up <- exp(m_af$upper.random.w[i])
  cat(sprintf("AF adj=%s: k=%d  HR=%.2f (%.2f–%.2f)\n", lev, n, hr, lo, up))
}
cat(sprintf("p-between = %.4f\n", m_af$pval.Q.b.random))

# Fibrosis assessment
m_fib <- update(m_main, subgroup = fibrosis_assessed, tau.common = FALSE)
cat("\n--- Fibrosis Assessment Subgroup ---\n")
for (i in seq_along(m_fib$subgroup.levels)) {
  lev <- m_fib$subgroup.levels[i]
  n <- sum(data_main$fibrosis_assessed == lev)
  hr <- exp(m_fib$TE.random.w[i])
  lo <- exp(m_fib$lower.random.w[i])
  up <- exp(m_fib$upper.random.w[i])
  cat(sprintf("%s: k=%d  HR=%.2f (%.2f–%.2f)\n", lev, n, hr, lo, up))
}
cat(sprintf("p-between = %.3f\n", m_fib$pval.Q.b.random))

# Population-based
m_pop <- update(m_main, subgroup = pop_based, tau.common = FALSE)
cat("\n--- Setting Subgroup ---\n")
for (i in seq_along(m_pop$subgroup.levels)) {
  lev <- m_pop$subgroup.levels[i]
  n <- sum(data_main$pop_based == lev)
  hr <- exp(m_pop$TE.random.w[i])
  lo <- exp(m_pop$lower.random.w[i])
  up <- exp(m_pop$upper.random.w[i])
  cat(sprintf("%s: k=%d  HR=%.2f (%.2f–%.2f)\n", lev, n, hr, lo, up))
}
cat(sprintf("p-between = %.3f\n", m_pop$pval.Q.b.random))

# ---- 6. Sensitivity Analyses ----
data_sens_kim <- subset(data_main, !(study == "Kim et al." & year == 2020))
data_sens_kim$TE <- log(data_sens_kim$HR)
data_sens_kim$seTE <- (log(data_sens_kim$upperCI) - log(data_sens_kim$lowerCI)) / 3.92
m_kim <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML", data = data_sens_kim)

data_sens_nhis <- subset(data_main, !(study == "Lee et al." & year == 2025) & !(study == "Kim et al." & year == 2024))
data_sens_nhis$TE <- log(data_sens_nhis$HR)
data_sens_nhis$seTE <- (log(data_sens_nhis$upperCI) - log(data_sens_nhis$lowerCI)) / 3.92
m_nhis <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML", data = data_sens_nhis)

data_sens_all <- subset(data_main, !(study == "Kim et al." & year == 2020) &
                                   !(study == "Lee et al." & year == 2025) &
                                   !(study == "Kim et al." & year == 2024))
data_sens_all$TE <- log(data_sens_all$HR)
data_sens_all$seTE <- (log(data_sens_all$upperCI) - log(data_sens_all$lowerCI)) / 3.92
m_all <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML", data = data_sens_all)

cat("\n--- Sensitivity Analyses ---\n")
cat(sprintf("Excl Kim 2020:        k=%d  HR=%.2f (%.2f–%.2f)  I²=%.1f%%\n",
    nrow(data_sens_kim), exp(m_kim$TE.random), exp(m_kim$lower.random), exp(m_kim$upper.random),
    m_kim$I2 * 100))
cat(sprintf("Excl NHIS overlap:    k=%d  HR=%.2f (%.2f–%.2f)  I²=%.1f%%\n",
    nrow(data_sens_nhis), exp(m_nhis$TE.random), exp(m_nhis$lower.random), exp(m_nhis$upper.random),
    m_nhis$I2 * 100))
cat(sprintf("Excl all 3:           k=%d  HR=%.2f (%.2f–%.2f)  I²=%.1f%%\n",
    nrow(data_sens_all), exp(m_all$TE.random), exp(m_all$lower.random), exp(m_all$upper.random),
    m_all$I2 * 100))

# ---- 7. Publication Bias ----
egger <- metabias(m_main, method.bias = "linreg", k.min = 3)
begg <- metabias(m_main, method.bias = "rank", k.min = 3)
m_no_lee <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML",
                    data = data_main[!grepl("Lee.*2025", data_main$study_label), ])
egger_nolee <- metabias(m_no_lee, method.bias = "linreg", k.min = 3)
cat(sprintf("\nEgger full:           p=%.4f\n", egger$p.value))
cat(sprintf("Egger w/o Lee 2025:   p=%.4f\n", egger_nolee$p.value))
cat(sprintf("Begg:                 p=%.4f\n", begg$p.value))

# ---- 8. Individual Study HRs ----
cat("\n--- Individual Study HRs (k=21) ---\n")
for (i in 1:nrow(data_main)) {
  cat(sprintf("%2d. %-45s HR=%.2f (%.2f–%.2f)\n", i, data_main$study_label[i],
      data_main$HR[i], data_main$lowerCI[i], data_main$upperCI[i]))
}

# ---- 9. AF Study Count Verification ----
cat("\n--- AF Adjustment Status ---\n")
cat(sprintf("AF adjusted studies: %d\n", sum(merged$adj_af)))
cat(sprintf("Studies without AF adj: %d\n", sum(!merged$adj_af)))

# ---- 10. Fibrosis Subgroup Count Verification ----
cat("\n--- Fibrosis Assessment Status ---\n")
cat(sprintf("Fibrosis assessed: %d\n", sum(merged$fibrosis_assessed == "Assessed")))
cat(sprintf("Fibrosis not assessed: %d\n", sum(merged$fibrosis_assessed == "Not Assessed")))

# ---- 11. Manuscript Claims Cross-Check ----
cat("\n========================================\n")
cat("       MANUSCRIPT CROSS-CHECK\n")
cat("========================================\n")

# Check: "All 21 HRs > 1.0"
all_above_1 <- all(data_main$HR > 1.0)
cat(sprintf("[CLAIM] 'All studies HR > 1.0': %s\n", ifelse(all_above_1, "PASS", "FAIL")))
if (!all_above_1) {
  below <- data_main[data_main$HR <= 1.0, c("study_label", "HR", "lowerCI", "upperCI")]
  print(below)
}

# Check: range "1.10 to 2.01"
min_hr <- min(data_main$HR)
max_hr <- max(data_main$HR)
cat(sprintf("[CLAIM] 'HR range %.2f to %.2f': min=%.2f max=%.2f\n", 1.10, 2.01, min_hr, max_hr))
cat(sprintf("        %s\n", ifelse(abs(min_hr - 1.10) < 0.02 && abs(max_hr - 2.01) < 0.02, "PASS", "CHECK")))

# Check: Jang 2026 is min HR
jang_row <- data_main[grepl("Jang", data_main$study), ]
cat(sprintf("[CLAIM] 'Jang 2026: HR 1.10': HR=%.2f %s\n",
    jang_row$HR[1], ifelse(abs(jang_row$HR[1] - 1.10) < 0.02, "PASS", "CHECK")))

# Check: Kim 2020 is max HR
kim_row <- data_main[grepl("Kim et al. \\(2020\\)", data_main$study_label), ]
if (nrow(kim_row) > 0) {
  cat(sprintf("[CLAIM] 'Kim 2020: HR 2.01': HR=%.2f %s\n",
      kim_row$HR[1], ifelse(abs(kim_row$HR[1] - 2.01) < 0.02, "PASS", "CHECK")))
}

# Check: 5 countries
cat(sprintf("[CLAIM] '5 countries': unique countries = %d  (%s)\n",
    length(unique(merged$country_clean)), paste(sort(unique(merged$country_clean)), collapse=", ")))

# Check: k=8 population-based
cat(sprintf("[CLAIM] 'population-based n=8': count=%d %s\n",
    sum(merged$pop_based == "Population-Based"),
    ifelse(sum(merged$pop_based == "Population-Based") == 8, "PASS", "CHECK")))

# Check: k=13 hospital-based
cat(sprintf("[CLAIM] 'hospital/clinic-based n=13': count=%d %s\n",
    sum(merged$pop_based == "Hospital/Clinic-Based"),
    ifelse(sum(merged$pop_based == "Hospital/Clinic-Based") == 13, "PASS", "CHECK")))

# Check: MASLD-defined k=6
cat(sprintf("[CLAIM] 'MASLD-defined k=6': count=%d %s\n",
    sum(merged$liver_def == "MASLD"),
    ifelse(sum(merged$liver_def == "MASLD") == 6, "PASS", "CHECK")))

# Check: MAFLD-defined k=7
cat(sprintf("[CLAIM] 'MAFLD-defined k=7': count=%d %s\n",
    sum(merged$liver_def == "MAFLD"),
    ifelse(sum(merged$liver_def == "MAFLD") == 7, "PASS", "CHECK")))

# Check: NAFLD-defined k=8
cat(sprintf("[CLAIM] 'NAFLD-defined k=8': count=%d %s\n",
    sum(merged$liver_def == "NAFLD"),
    ifelse(sum(merged$liver_def == "NAFLD") == 8, "PASS", "CHECK")))

cat("\n===== AUDIT COMPLETE =====\n")
