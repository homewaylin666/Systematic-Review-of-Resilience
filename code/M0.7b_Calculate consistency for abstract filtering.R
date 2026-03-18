# Module 0.7b Calculate Cohen's Kappa
rm(list=ls())
library(here)

#import data sheet that was edited by human filter
# df <- read.csv(here("data", "C1a.50.for.abstract.filtering_edited.csv")) #for training
df <- read.csv(here("data", "C1b.100.for.abstract.filtering.validation_edited.csv")) #for validation


# Identify all LLM columns (any column name containing 'llm.')
llm_cols <- grep("llm\\.", names(df), value = TRUE)

# Create a new df with standardized columns:
df <- df[, c("human.coder_HL", llm_cols), drop = FALSE]


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


################## out put ##################
# write_csv(df_with_result, here("data", "C2a.training.result.csv")) #for training
write_csv(df_with_result, here("data", "C2b.validation.result.csv")) #for validation
