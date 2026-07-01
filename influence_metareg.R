library(meta)
library(metafor)
library(readxl)

# === Load & prep ===
data <- read_excel("MASLD_AF_Cardioembolic stroke_META.25.xlsx", sheet = "Main Meta Data")
data$HR <- as.numeric(data$HR)
data$stroke_group <- ifelse(data$outcome %in% c("All Stroke","Stroke","Cerebrovascular disease (CVD)"), "Total Stroke", "Ischemic Stroke")
data_main <- data
data_main <- subset(data_main, !grepl("MAFLD only", data_main$study))
data_main <- subset(data_main, !(study == "Kim et al. (B.S. Kim)" & year == 2025))
# Kim 2020 KEPT in
data_main$study_label <- make.unique(paste0(data_main$study, " (", data_main$year, ")"), sep=" #")
data_main$TE  <- log(data_main$HR)
data_main$seTE <- (log(data_main$upperCI) - log(data_main$lowerCI)) / 3.92

dc <- read_excel("MASLD_AF_Cardioembolic stroke_META.25.xlsx", sheet = "Study_Characteristics")
dc <- dc[-1,]
dc$study_clean <- gsub("^[①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳]+", "", dc$study)
dc$study_clean <- trimws(dc$study_clean)
dc$study_label <- paste0(dc$study_clean, " (", dc$year, ")")
dc <- dc[!duplicated(dc$study_label),]
merged <- merge(data_main[,c("study_label","TE","seTE","stroke_group")],
                dc[,c("study_label","Liver_Definition","Age_Mean_SD","adjusted_AF")],
                by="study_label", all.x=TRUE)
merged$liver_def <- ifelse(grepl("MASLD", merged$Liver_Definition), "MASLD",
                    ifelse(grepl("MAFLD", merged$Liver_Definition), "MAFLD", "NAFLD"))
merged$adj_af <- grepl("yes|Yes|1=yes", merged$adjusted_AF)

# === Mean age extraction ===
extract_age <- function(x) {
  m <- regexpr("[0-9]+(\\.[0-9]+)?", x)
  as.numeric(regmatches(x, m))
}
merged$mean_age <- extract_age(merged$Age_Mean_SD)

cat("=== mean_age 提取结果 ===\n")
for(i in 1:nrow(merged)) {
  cat(sprintf("%-35s Age_Mean_SD=%-40s mean_age=%s\n",
      merged$study_label[i], merged$Age_Mean_SD[i], merged$mean_age[i]))
}

# === Full model (k=22) ===
m_mr <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=merged)
mr_full <- metareg(m_mr, ~ mean_age)
cat(sprintf("\n========== 全部22篇: R²=%.1f%%, p=%.3f (k=%d) ==========\n",
    mr_full$R2, mr_full$QMp, mr_full$k))

# === Targeted LOO ===
targets <- c("Kim et al. (2020)", "Simon et al. (2022)", "Chen et al. (2023)")
cat("\n========== 定向 Leave-One-Out Meta-Regression (Mean Age) ==========\n")
cat(sprintf("%-30s %5s %8s %10s\n", "排除", "k", "R²", "p"))
cat(strrep("-", 58), "\n")

for(tgt in targets) {
  sub <- subset(merged, study_label != tgt)
  m_sub <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=sub)
  mr <- metareg(m_sub, ~ mean_age)
  cat(sprintf("%-30s %5d %7.1f%% %10.3f\n", tgt, mr$k, mr$R2, mr$QMp))
}

# === Full LOO (all 22) ===
cat("\n========== 全部 LOO Meta-Regression (Mean Age) ==========\n")
cat(sprintf("%-35s %5s %8s %8s\n", "排除研究", "k", "R²", "p"))
cat(strrep("-", 62), "\n")
for(i in 1:nrow(merged)) {
  tgt <- merged$study_label[i]
  sub <- subset(merged, study_label != tgt)
  m_sub <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=sub)
  mr <- tryCatch(metareg(m_sub, ~ mean_age), error=function(e) list(R2=NA, QMp=NA, k=NA))
  sig <- ifelse(is.na(mr$QMp), "", ifelse(mr$QMp < 0.05, " **", ifelse(mr$QMp < 0.10, " .", "")))
  cat(sprintf("%-35s %5d %7.1f%% %8.3f%s\n", tgt, mr$k, mr$R2, mr$QMp, sig))
}

# === Also do for AF adjustment (for comparison) ===
cat("\n========== 定向 LOO Meta-Regression (Adjusted AF) ==========\n")
cat(sprintf("%-30s %5s %8s %10s\n", "排除", "k", "R²", "p"))
cat(strrep("-", 58), "\n")
for(tgt in targets) {
  sub <- subset(merged, study_label != tgt)
  m_sub <- metagen(TE=TE, seTE=seTE, studlab=study_label, sm="HR", method.tau="REML", data=sub)
  mr <- metareg(m_sub, ~ adj_af)
  cat(sprintf("%-30s %5d %7.1f%% %10.3f\n", tgt, mr$k, mr$R2, mr$QMp))
}
