# Quick fix: run remaining checks
library(readxl)
library(meta)
library(metafor)

data <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx", sheet = "Main Meta Data")
data$HR <- as.numeric(data$HR)

data_main <- data
data_main <- subset(data_main, !grepl("MAFLD only", data_main$study))
data_main <- subset(data_main, !(study == "Park et al." & year == 2022))
data_main <- subset(data_main, !(study == "Kim et al. (B.S. Kim)" & year == 2025))

data_main$study_label <- make.unique(paste0(data_main$study, " (", data_main$year, ")"), sep = " #")
data_main$TE  <- log(data_main$HR)
data_main$seTE <- (log(data_main$upperCI) - log(data_main$lowerCI)) / 3.92

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

data_main$adj_af <- merged$adj_af[match(data_main$study_label, merged$study_label)]
data_main$liver_def <- merged$liver_def[match(data_main$study_label, merged$study_label)]
data_main$fibrosis_assessed <- merged$fibrosis_assessed[match(data_main$study_label, merged$study_label)]
data_main$pop_based <- merged$pop_based[match(data_main$study_label, merged$study_label)]
data_main$stroke_group <- ifelse(data_main$outcome %in% c("All Stroke", "Stroke", "Cerebrovascular disease (CVD)"),
                                  "Total Stroke", "Ischemic Stroke")

m_main <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML", data = data_main)
m_main_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML",
                     method.random.ci = "HK", data = data_main)

# All subgroup analyses
cat("===== SUBGROUP ANALYSES (COMPLETE) =====\n")

# Stroke subtype
m_stroke <- update(m_main, subgroup = stroke_group, tau.common = FALSE)
cat("\n[Stroke Subtype]\n")
for (i in seq_along(m_stroke$subgroup.levels)) {
  lev <- m_stroke$subgroup.levels[i]
  n <- sum(data_main$stroke_group == lev)
  cat(sprintf("%s: k=%d HR=%.2f (%.2f-%.2f) I²=%.1f%%\n", lev, n,
    exp(m_stroke$TE.random.w[i]), exp(m_stroke$lower.random.w[i]), exp(m_stroke$upper.random.w[i]),
    m_stroke$I2.w[i]*100))
}
cat(sprintf("p-between = %.4f\n", m_stroke$pval.Q.b.random))

# MASLD definition
m_liver <- update(m_main, subgroup = liver_def, tau.common = FALSE)
cat("\n[MASLD Definition]\n")
for (i in seq_along(m_liver$subgroup.levels)) {
  lev <- m_liver$subgroup.levels[i]
  n <- sum(data_main$liver_def == lev)
  cat(sprintf("%s: k=%d HR=%.2f (%.2f-%.2f) I²=%.1f%%\n", lev, n,
    exp(m_liver$TE.random.w[i]), exp(m_liver$lower.random.w[i]), exp(m_liver$upper.random.w[i]),
    m_liver$I2.w[i]*100))
}
cat(sprintf("p-between = %.4f\n", m_liver$pval.Q.b.random))

# AF adjustment
m_af <- update(m_main, subgroup = adj_af, tau.common = FALSE)
cat("\n[AF Adjustment]\n")
for (i in seq_along(m_af$subgroup.levels)) {
  lev <- m_af$subgroup.levels[i]
  n <- sum(data_main$adj_af == (lev == "TRUE"), na.rm=TRUE)
  cat(sprintf("adj_af=%s: k=%d HR=%.2f (%.2f-%.2f) I²=%.1f%%\n", lev, n,
    exp(m_af$TE.random.w[i]), exp(m_af$lower.random.w[i]), exp(m_af$upper.random.w[i]),
    m_af$I2.w[i]*100))
}
cat(sprintf("p-between = %.4f\n", m_af$pval.Q.b.random))

# Fibrosis
m_fib <- update(m_main, subgroup = fibrosis_assessed, tau.common = FALSE)
cat("\n[Fibrosis Assessment]\n")
for (i in seq_along(m_fib$subgroup.levels)) {
  lev <- m_fib$subgroup.levels[i]
  n <- sum(data_main$fibrosis_assessed == lev)
  cat(sprintf("%s: k=%d HR=%.2f (%.2f-%.2f) I²=%.1f%%\n", lev, n,
    exp(m_fib$TE.random.w[i]), exp(m_fib$lower.random.w[i]), exp(m_fib$upper.random.w[i]),
    m_fib$I2.w[i]*100))
}
cat(sprintf("p-between = %.4f\n", m_fib$pval.Q.b.random))

# Setting
m_pop <- update(m_main, subgroup = pop_based, tau.common = FALSE)
cat("\n[Setting]\n")
for (i in seq_along(m_pop$subgroup.levels)) {
  lev <- m_pop$subgroup.levels[i]
  n <- sum(data_main$pop_based == lev)
  cat(sprintf("%s: k=%d HR=%.2f (%.2f-%.2f) I²=%.1f%%\n", lev, n,
    exp(m_pop$TE.random.w[i]), exp(m_pop$lower.random.w[i]), exp(m_pop$upper.random.w[i]),
    m_pop$I2.w[i]*100))
}
cat(sprintf("p-between = %.4f\n", m_pop$pval.Q.b.random))

# ---- Sensitivity ----
data_sens_kim <- subset(data_main, !(study == "Kim et al." & year == 2020))
data_sens_kim$TE <- log(data_sens_kim$HR)
data_sens_kim$seTE <- (log(data_sens_kim$upperCI) - log(data_sens_kim$lowerCI)) / 3.92
m_kim <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=data_sens_kim)

data_sens_nhis <- subset(data_main, !(study=="Lee et al." & year==2025) & !(study=="Kim et al." & year==2024))
data_sens_nhis$TE <- log(data_sens_nhis$HR)
data_sens_nhis$seTE <- (log(data_sens_nhis$upperCI) - log(data_sens_nhis$lowerCI)) / 3.92
m_nhis <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=data_sens_nhis)

data_sens_all <- subset(data_main, !(study=="Kim et al." & year==2020) &
                                   !(study=="Lee et al." & year==2025) &
                                   !(study=="Kim et al." & year==2024))
data_sens_all$TE <- log(data_sens_all$HR)
data_sens_all$seTE <- (log(data_sens_all$upperCI) - log(data_sens_all$lowerCI)) / 3.92
m_all <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=data_sens_all)

cat("\n[Sensitivity Analyses]\n")
cat(sprintf("Excl Kim 2020 (k=%d):       HR=%.2f (%.2f-%.2f) I²=%.1f%%\n",
    nrow(data_sens_kim), exp(m_kim$TE.random), exp(m_kim$lower.random), exp(m_kim$upper.random), m_kim$I2*100))
cat(sprintf("Excl NHIS overlap (k=%d):   HR=%.2f (%.2f-%.2f) I²=%.1f%%\n",
    nrow(data_sens_nhis), exp(m_nhis$TE.random), exp(m_nhis$lower.random), exp(m_nhis$upper.random), m_nhis$I2*100))
cat(sprintf("Excl all 3 (k=%d):          HR=%.2f (%.2f-%.2f) I²=%.1f%%\n",
    nrow(data_sens_all), exp(m_all$TE.random), exp(m_all$lower.random), exp(m_all$upper.random), m_all$I2*100))

# Publication bias
egger <- metabias(m_main, method.bias="linreg", k.min=3)
begg <- metabias(m_main, method.bias="rank", k.min=3)
m_no_lee <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML",
                    data=data_main[!grepl("Lee.*2025", data_main$study_label),])
egger_nolee <- metabias(m_no_lee, method.bias="linreg", k.min=3)

cat("\n[Publication Bias]\n")
cat(sprintf("Egger full:              p=%.4f\n", egger$p.value))
cat(sprintf("Egger w/o Lee 2025:      p=%.4f\n", egger_nolee$p.value))
cat(sprintf("Begg:                    p=%.4f\n", begg$p.value))

# ---- AF studies identification ----
cat("\n[AF-Adjusted Studies Identification]\n")
af_studies <- merged[merged$adj_af, c("study_label", "adjusted_AF")]
cat(sprintf("Count: %d\n", nrow(af_studies)))
print(af_studies)

# ---- Key Cross-Check ----
cat("\n===== MANUSCRIPT CLAIMS vs R OUTPUT =====\n")
# Most critical check: CI reporting
cat(sprintf("\n⚠️  HK CI: %.2f–%.2f  vs  Standard CI: %.2f–%.2f\n",
    exp(m_main_hk$lower.random), exp(m_main_hk$upper.random),
    exp(m_main$lower.random), exp(m_main$upper.random)))
cat(sprintf("Manuscript reports: 1.28–1.46\n"))
cat(sprintf("→ If HK is primary (per Methods), CI should be: %.2f–%.2f\n",
    exp(m_main_hk$lower.random), exp(m_main_hk$upper.random)))

# Country check
cat(sprintf("\nCountries: %s\n", paste(sort(unique(merged$country_clean)), collapse=", ")))
cat(sprintf("Korea count: %d\n", sum(merged$country_clean == "Korea")))

# median follow-up range
cat(sprintf("\nFollow-up range: %.1f–%.1f\n", min(merged$followup_num, na.rm=TRUE), max(merged$followup_num, na.rm=TRUE)))

cat("\n===== COMPLETE =====\n")
