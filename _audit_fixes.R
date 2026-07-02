# ============================================================
# Audit Fixes: PI, Trim-and-Fill, Attenuation verification
# ============================================================
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

m_main <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML", data = data_main)
m_main_hk <- metagen(TE = TE, seTE = seTE, studlab = study_label, sm = "HR", method.tau = "REML",
                     method.random.ci = "HK", data = data_main)

# ============================================================
# 1. 95% Prediction Interval
# ============================================================
cat("===== 95% PREDICTION INTERVAL =====\n")
# PI = pooled_HR ± t_{k-2, 0.975} × sqrt(τ² + SE²)
k <- nrow(data_main)
tau2 <- m_main$tau2
se_pooled <- m_main$seTE.random
t_crit <- qt(0.975, df = k - 2)

pi_lo <- exp(m_main$TE.random - t_crit * sqrt(tau2 + se_pooled^2))
pi_up <- exp(m_main$TE.random + t_crit * sqrt(tau2 + se_pooled^2))

cat(sprintf("k = %d\n", k))
cat(sprintf("τ² = %.4f\n", tau2))
cat(sprintf("SE of pooled estimate = %.4f\n", se_pooled))
cat(sprintf("t-critical (df=%d) = %.4f\n", k-2, t_crit))
cat(sprintf("95%% PI: %.2f – %.2f\n", pi_lo, pi_up))
cat(sprintf("PI > 1.0: %s\n", ifelse(pi_lo > 1.0, "YES — direction robust", "NO — crosses null")))

# Also compute PI using metafor::predict()
rma_main <- rma(yi = TE, sei = seTE, data = data_main, method = "REML")
pi_rma <- predict(rma_main)
cat(sprintf("\nmetafor::predict() PI: %.2f – %.2f\n", exp(pi_rma$cr.lb), exp(pi_rma$cr.ub)))

# ============================================================
# 2. Trim-and-Fill
# ============================================================
cat("\n===== TRIM-AND-FILL =====\n")
# Using meta::trimfill
tf <- trimfill(m_main)
cat(sprintf("Trim-and-fill method: %s\n", tf$method))
cat(sprintf("Number of imputed studies: %d\n", tf$k0))
cat(sprintf("Original k: %d\n", tf$k))
cat(sprintf("Original HR: %.4f (%.4f–%.4f)\n",
    exp(m_main$TE.random), exp(m_main$lower.random), exp(m_main$upper.random)))
cat(sprintf("Trim-and-fill HR: %.4f (%.4f–%.4f)\n",
    exp(tf$TE.random), exp(tf$lower.random), exp(tf$upper.random)))
cat(sprintf("Attenuation: HR %.2f → %.2f\n", exp(m_main$TE.random), exp(tf$TE.random)))

# Also try L0 estimator (most commonly reported)
tf_lo <- trimfill(m_main, method = "L0")
cat(sprintf("\nL0 method:\n"))
cat(sprintf("Number of imputed studies: %d\n", tf_lo$k0))
cat(sprintf("L0 HR: %.4f (%.4f–%.4f)\n",
    exp(tf_lo$TE.random), exp(tf_lo$lower.random), exp(tf_lo$upper.random)))

# ============================================================
# 3. Attenuation formula verification
# ============================================================
cat("\n===== ATTENUATION FORMULA VERIFICATION =====\n")

# Current formula in manuscript (line 114):
# (HR_without_AF - HR_with_AF) / (HR_without_AF - 1) × 100%

# Merge AF adjustment
dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.最新版xlsx_.xlsx", sheet = "Study_Characteristics")
dc <- dc[-1,]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label),]

merged <- merge(
  data_main[, c("study_label", "TE", "seTE")],
  dc[, c("study_label", "adjusted_AF")],
  by = "study_label", all.x = TRUE
)
merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)

rma_af <- rma(yi = TE, sei = seTE, mods = ~ adj_af, data = merged, method = "REML")
pred_no <- predict(rma_af, newmods = c(0))
pred_yes <- predict(rma_af, newmods = c(1))

# Current formula
atten_current <- (exp(pred_no$pred) - exp(pred_yes$pred)) / (exp(pred_no$pred) - 1) * 100

# Alternative log-scale formula
atten_log <- (1 - log(exp(pred_yes$pred)) / log(exp(pred_no$pred))) * 100

cat(sprintf("Predicted HR (without AF): %.4f\n", exp(pred_no$pred)))
cat(sprintf("Predicted HR (with AF):    %.4f\n", exp(pred_yes$pred)))
cat(sprintf("Current formula:  (HR_no - HR_yes) / (HR_no - 1) × 100 = %.1f%%\n", atten_current))
cat(sprintf("Log-scale formula: [1 - ln(HR_yes) / ln(HR_no)] × 100  = %.1f%%\n", atten_log))
cat(sprintf("Difference: %.2f pp\n", abs(atten_current - atten_log)))

# Also show the user's suggested formula result
cat(sprintf("\nUser suggested: [1 - ln(HR_adj) / ln(HR_unadj)] × 100 = %.1f%%\n", atten_log))

# ============================================================
# 4. Verification: Jang 2026 within-study AF attenuation
# ============================================================
cat("\n===== JANG 2026 WITHIN-STUDY ATTENUATION =====\n")
# HR 1.14 (before AF) → 1.10 (after AF)
atten_jang_current <- (1.14 - 1.10) / (1.14 - 1) * 100
atten_jang_log <- (1 - log(1.10) / log(1.14)) * 100
cat(sprintf("Jang within-study: 1.14 → 1.10\n"))
cat(sprintf("Current formula: %.1f%%\n", atten_jang_current))
cat(sprintf("Log formula:      %.1f%%\n", atten_jang_log))

# ============================================================
# 5. ALSO: Check I² WITH prediction interval context
# ============================================================
cat("\n===== SUMMARY FOR MANUSCRIPT =====\n")
cat(sprintf("MAIN: HR %.2f (%.2f–%.2f), I²=%.1f%%, τ²=%.4f, k=%d\n",
    exp(m_main$TE.random), exp(m_main$lower.random), exp(m_main$upper.random),
    m_main$I2*100, m_main$tau2, k))
cat(sprintf("PI:   %.2f–%.2f\n", pi_lo, pi_up))
cat(sprintf("TAF:  k0=%d, HR %.2f (%.2f–%.2f) [L0: k0=%d, HR %.2f]\n",
    tf$k0, exp(tf$TE.random), exp(tf$lower.random), exp(tf$upper.random),
    tf_lo$k0, exp(tf_lo$TE.random)))
cat(sprintf("ATTN: %.1f%% (current formula) vs %.1f%% (log formula)\n", atten_current, atten_log))

cat("\n===== COMPLETE =====\n")
