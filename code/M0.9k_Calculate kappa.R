# Module 0.9k Calculate Cohen's Kappa and consistency
rm(list=ls())
library(here)

#import data sheet
#for training
#ref <- read.csv(here("data", "D1.100.paper.list.for.training.csv"))
#df.5.1a <- read.csv(here("data", "E1.llm.output", "E1.5.1a.pdf.filter.train.results_20260123_192018.csv"))
#df.5.1b <- read.csv(here("data", "E1.llm.output", "E1.5.1b.pdf.filter.train.results_20260124_010439.csv"))
#df.5.2a <- read.csv(here("data", "E1.llm.output", "E1.5.2a.pdf.filter.train.results_20260123_174651.csv"))
#df.5.2b <- read.csv(here("data", "E1.llm.output", "E1.5.2b.pdf.filter.train.results_20260124_092725.csv"))
#df.5.2c <- read.csv(here("data", "E1.llm.output", "E1.5.2c.pdf.filter.train.results_20260124_165229.csv"))
#df.5.2d <- read.csv(here("data", "E1.llm.output", "E1.5.2d.pdf.filter.train.results_20260124_173601.csv"))
#df.5.2e <- read.csv(here("data", "E1.llm.output", "E1.5.2e.pdf.filter.train.results_20260125_223604.csv"))

#for validation
ref <- read.csv(here("data","D2.100.paper.list.for.validation.csv")) 
df.5.2c <- read.csv(here("data", "E2.llm.output", "E2.5.2c.pdf.filter.validate.results_20260124_203358.csv"))
df.5.2d <- read.csv(here("data", "E2.llm.output", "E2.5.2d.pdf.filter.validate.results_20260124_204037.csv"))
df.5.2e <- read.csv(here("data", "E2.llm.output", "E2.5.2e.pdf.filter.validate.results_20260125_224232.csv"))

################## prepare the df ##################
# Load required packages
library(dplyr)
library(purrr)

# --- 1) Identify all data frames whose names start with "df" except "df" itself ---
# (We exclude 'ref' because it does not start with "df")
all_objs <- ls()

# Select object names that start with "df" but exclude the eventual target 'df' and 'df.num' if they exist
# (This finds df5.2a, df5.1a, etc.)
other_df_names <- all_objs[grepl("^df", all_objs) & !(all_objs %in% c("df", "df.num"))]

# --- 2) Rename Criteria.met -> llm<suffix> in each of these other data frames ---
# Example: df5.2a$Criteria.met -> df5.2a$llm5.2a
for (nm in other_df_names) {
  # get the data frame
  tmp_df <- get(nm)
  
  # compute new column name by removing the leading "df" from object name and prefixing with "llm"
  new_llm_name <- paste0("llm", sub("^df", "", nm))
  
  # rename Criteria.met to new name
  # (assumption per your instruction: Criteria.met exists)
  tmp_df <- tmp_df %>% rename(!!new_llm_name := Criteria.met)
  
  # assign back to the original variable name in the global environment
  assign(nm, tmp_df, envir = .GlobalEnv)
}

# --- 3) Build a list of data frames to join ---
# Start with ref: keep paper_id and human.coder_HL
ref_df <- ref %>% select(paper_id, human.coder_HL)

# For each other df, keep paper_id and the llm* column(s)
# We'll create a list of data frames where each element has paper_id + its llm column(s)
other_dfs_for_join <- map(other_df_names, function(nm) {
  tmp <- get(nm)
  # select paper_id and any columns that start with "llm" (should be the renamed Criteria.met)
  tmp %>% select(paper_id, matches("^llm"))
})

# Prepend ref to the list so ref is the left-most (base) for left-joining
join_list <- c(list(ref_df), other_dfs_for_join)

# --- 4) Left-join all data frames by paper_id, preserving ref's rows ---
# Use purrr::reduce with dplyr::left_join
df.num <- reduce(join_list, function(x, y) left_join(x, y, by = "paper_id"))

# --- 5) Remove paper_id column and save to df ---
df <- df.num %>% select(-paper_id)


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
    reference_col == "IN" & llm_col == "OUT",
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

#out put
#write_csv(df_with_result, here("data", "E1k.train.result.csv")) #for training
write_csv(df_with_result, here("data", "E2k.validate.result.csv")) #for validation
