# Module 1p Post check the kappa and accuracy for main-text filtering
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")

# import data sheet
my.ans <- read.csv("Dp.50.paper.for.postcheck_edited.csv")
llm.ans <- read.csv("E3.5.2e.pdf.filter.left.results_20260127_165257.csv")

# check if there are exactly 50 paper id matches
common_paper_ids <- intersect(my.ans$paper_id, llm.ans$paper_id)
cat("common paper id:", length(common_paper_ids), "\n")

# integrate them
answer <- merge(my.ans, 
                     llm.ans[, c("paper_id", "Criteria.met")], 
                     by = "paper_id", 
                     all.x = TRUE)
names(answer)[names(answer) == "Criteria.met"] <- "llm.gpt5.2"
df <- answer[,c('human.coder_HL', 'llm.gpt5.2')]

################## calculate Kappa ##################
library(psych)
library(readr)
kappa_results <- c()
reference_col <- df$human.coder_HL
for(i in 2:ncol(df)) {
  col_name <- colnames(df)[i]
  rater_data <- data.frame(
    rater1 = reference_col,
    rater2 = df[[i]]
  )
  # Calculate Cohen's kappa
  kappa_result <- cohen.kappa(rater_data)
  # meaning this in my case: weighted kappa
  kappa_value <- kappa_result$weighted.kappa
  if(is.na(kappa_value)) {
    kappa_value <- kappa_result$kappa
  }
  kappa_results <- c(kappa_results, kappa_value)
  
  cat("The Cohen's kappa of human.coder_HL vs", col_name, ":", round(kappa_value, 4), "\n")
}
kappa_row <- c("Kappa", round(kappa_results, 4), "%")

################## calculate consistency and false negative ##################
# --- Total number of rows used for evaluation ---
total_rows <- nrow(df)

consistency_results <- c()
false_negative_results <- c()

reference_col <- df$human.coder_HL

for (i in 2:ncol(df)) {
  
  col_name <- colnames(df)[i]
  llm_col <- df[[i]]
  
  # 1. Consistency: percentage of rows where answers match
  consistency <- sum(reference_col == llm_col, na.rm = TRUE) / total_rows * 100
  
  # 2. False negative rate
  # Definition: human = "yes" AND llm = "no"
  false_negative <- sum(
    reference_col == "yes" & llm_col == "no",
    na.rm = TRUE
  ) / total_rows * 100
  
  consistency_results <- c(consistency_results, consistency)
  false_negative_results <- c(false_negative_results, false_negative)
  
  cat(
    "Human vs", col_name,
    "| Consistency:", round(consistency, 2), "%",
    "| False negative:", round(false_negative, 2), "%\n"
  )
}

# Bind results to df (same style as your kappa output)
consistency_row <- c("Consistency (%)", round(consistency_results, 2))
false.negative_row <- c("False negative (%)", round(false_negative_results, 2))

df_with_result <- rbind(
  df,
  kappa_row,
  consistency_row,
  false.negative_row
)

# Check result
print(df_with_result)

# Output
write_csv(df_with_result, "Ep.post.check.result.csv")
