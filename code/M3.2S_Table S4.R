#### Chi-square analysis (one representative value per study; exports tables)
rm(list=ls())
library(here)
setwd(here())

# ---- Load required packages ----
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---- Import the data ----
df <- read.csv("data/Gz.full.data.sheet_594.csv")

# ---- Define priority orders (must match your plotting orders) ----
level_order    <- c("Landscape", "Ecosystem", "Community", "Population", "Individual")
approach_map   <- c("field observation"="FO", "field experiment"="FM",
                    "indoor experiment"="LM", "modeling and simulation"="MD")
approach_order <- c("MD","LM","FM","FO")
target_order_lc <- c("environmental context","functional response","structure",
                     "process based indicator","quantity") # lower-case form for matching
target_order_tc <- str_to_title(target_order_lc)           # title case for final labels

# ---- Create table directory if it doesn't exist ----
if (!dir.exists("table")) {
  dir.create("table")
}

# ---- Random pick_one function with fixed seed ----
set.seed(406)  # fixed random seed for reproducibility

pick_one <- function(x, order_vec_lc = NULL, map_fun = identity, to_title = FALSE) {
  # Return NA if input is missing or empty
  if (is.na(x) || x == "") return(NA_character_)
  
  # Split by semicolon and trim whitespace
  vals <- str_split(x, ";")[[1]] %>% trimws()
  vals <- vals[vals != ""]
  if (length(vals) == 0) return(NA_character_)
  
  # Randomly choose one value
  chosen <- sample(vals, 1)
  
  # Apply mapping function
  out <- map_fun(chosen)
  
  # Convert to title case if requested
  if (isTRUE(to_title)) out <- str_to_title(out)
  
  return(out)
}

# ---- Build single-row-per-study dataframe with representative values ----
single_data <- df %>%
  transmute(
    # Level: choose by level_order; Title Case
    Level = sapply(level,
                   pick_one,
                   order_vec_lc = level_order,
                   map_fun = function(z) stringr::str_to_title(z),
                   to_title = FALSE),
    # Approach: choose by names(approach_map); then map to FO/FM/LM/MD
    Approach = sapply(approach,
                      pick_one,
                      order_vec_lc = names(approach_map),
                      map_fun = function(z) unname(approach_map[tolower(z)]),
                      to_title = FALSE),
    # Target: choose by target_order_lc; Title Case for display
    Target_variable = sapply(target_variable_group,
                             pick_one,
                             order_vec_lc = target_order_lc,
                             map_fun = identity,
                             to_title = TRUE)
  ) %>%
  # drop NAs
  filter(!is.na(Level), !is.na(Approach), !is.na(Target_variable)) %>%
  # set factor levels
  mutate(
    Level = factor(Level, levels = level_order),
    Approach = factor(Approach, levels = approach_order),
    Target_variable = factor(Target_variable, levels = target_order_tc)
  )


# ---- Chi-square analysis function ----
chi_square_analysis <- function(data, var1, var2) {
  # Create contingency table
  ct <- table(data[[var1]], data[[var2]])
  # Perform chi-square test
  chi_test <- suppressWarnings(chisq.test(ct))
  # Standardized residuals
  std_residuals <- chi_test$stdres
  
  # Convert to a tidy result dataframe
  result_df <- expand.grid(
    var1 = rownames(std_residuals),
    var2 = colnames(std_residuals),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      observed = as.vector(ct),
      expected = as.vector(chi_test$expected),
      std_residual = as.vector(std_residuals),
      significant = abs(std_residual) >= 1.96,
      direction = dplyr::case_when(
        std_residual >= 1.96  ~ "EXCESS",
        std_residual <= -1.96 ~ "DEFICIT",
        TRUE ~ "NS"
      )
    )
  names(result_df)[1:2] <- c(var1, var2)
  
  return(list(
    data = result_df,
    chi_test = chi_test,
    p_value = chi_test$p.value,
    contingency_table = ct,
    expected = chi_test$expected,
    std_residuals = std_residuals
  ))
}

# ---- Pretty print function (optional console output) ----
display_chi_square_results <- function(results, var1_name, var2_name) {
  cat("=== CHI-SQUARE ANALYSIS: ", var1_name, " vs ", var2_name, " ===\n")
  cat("Chi-square statistic:", round(results$chi_test$statistic, 4), "\n")
  cat("Degrees of freedom:", results$chi_test$parameter, "\n")
  cat("P-value:", format.pval(results$p_value, digits = 4), "\n")
  cat("Overall significance:", ifelse(results$p_value < 0.001, "***", 
                                      ifelse(results$p_value < 0.01, "**",
                                             ifelse(results$p_value < 0.05, "*", "NS"))), "\n\n")
  cat("Contingency Table (Observed):\n"); print(results$contingency_table); cat("\n")
  cat("Expected frequencies:\n"); print(round(results$expected, 2)); cat("\n")
  cat("Standardized residuals:\n"); print(round(results$std_residuals, 3)); cat("\n")
}

# ---- Run all three pairwise analyses ----
cat("=== DATA SUMMARY (one representative per study) ===\n")
cat("Total studies used:", nrow(single_data), "\n")
cat("Level:", paste(levels(single_data$Level), collapse = ", "), "\n")
cat("Approach:", paste(levels(single_data$Approach), collapse = ", "), "\n")
cat("Target_variable:", paste(levels(single_data$Target_variable), collapse = ", "), "\n\n")

# 1) Level vs Approach
res_LA <- chi_square_analysis(single_data, "Level", "Approach")
display_chi_square_results(res_LA, "Level", "Approach")

# 2) Level vs Target_variable
res_LT <- chi_square_analysis(single_data, "Level", "Target_variable")
display_chi_square_results(res_LT, "Level", "Target_variable")

# 3) Approach vs Target_variable
res_AT <- chi_square_analysis(single_data, "Approach", "Target_variable")
display_chi_square_results(res_AT, "Approach", "Target_variable")

# ---- Export helper: write matrices and tidy results to csv ----
write_matrix_csv <- function(mat, file) {
  df_out <- as.data.frame.matrix(mat)
  df_out <- tibble::rownames_to_column(df_out, var = "Row")
  write.csv(df_out, file, row.names = FALSE)
}

write_results <- function(prefix, results, var1, var2) {
  # Tidy full table
  write.csv(results$data, paste0(prefix, "_", var1, "_", var2, "_full_results.csv"), row.names = FALSE)
  # Significant subset
  sig <- results$data %>% filter(significant)
  write.csv(sig, paste0(prefix, "_", var1, "_", var2, "_significant_only.csv"), row.names = FALSE)
  # Observed / Expected / StdResid matrices
  write_matrix_csv(results$contingency_table, paste0(prefix, "_", var1, "_", var2, "_observed.csv"))
  write_matrix_csv(round(results$expected, 4), paste0(prefix, "_", var1, "_", var2, "_expected.csv"))
  write_matrix_csv(round(results$std_residuals, 4), paste0(prefix, "_", var1, "_", var2, "_stdres.csv"))
}

# ---- Write all outputs ----
# (optional) keep original separate exports - uncomment if you still want them
# write_results("table/prefix", res_LA, "Level", "Approach")
# write_results("table/prefix", res_LT, "Level", "Target_variable")
# write_results("table/prefix", res_AT, "Approach", "Target_variable")

# Helper to add a Pair column and rename first two cols to Var1/Var2 for consistency
add_pair_full <- function(results, pair_name) {
  df <- results$data
  # ensure the first two columns are the factor names (they already are in results$data)
  names(df)[1:2] <- c("Var1", "Var2")
  df <- df %>%
    mutate(Pair = pair_name) %>%
    dplyr::select(Pair, dplyr::everything())  # place Pair first, use dplyr::select explicitly
  return(df)
}

# Build combined full table (all three pairwise full-results concatenated)
combined_full <- bind_rows(
  add_pair_full(res_LA, "Level vs Approach"),
  add_pair_full(res_LT, "Level vs Target_variable"),
  add_pair_full(res_AT, "Approach vs Target_variable")
) %>%
  # optionally order columns in a convenient way, use dplyr::select explicitly
  dplyr::select(Pair, Var1, Var2, observed, expected, std_residual, significant, direction, dplyr::everything())

# Write single Table S4.csv to table directory
write.csv(combined_full, "table/Table S4.csv", row.names = FALSE)

# (optional) also write the previous summary of only significant cells if you still want it
sig_LA <- res_LA$data %>% filter(significant)
sig_LT <- res_LT$data %>% filter(significant)
sig_AT <- res_AT$data %>% filter(significant)

summary_sig <- bind_rows(
  if (nrow(sig_LA)>0) add_pair_full(list(data=sig_LA), "Level vs Approach") else NULL,
  if (nrow(sig_LT)>0) add_pair_full(list(data=sig_LT), "Level vs Target_variable") else NULL,
  if (nrow(sig_AT)>0) add_pair_full(list(data=sig_AT), "Approach vs Target_variable") else NULL
)

if (nrow(summary_sig) > 0) {
  write.csv(summary_sig, "table/Table S4_significant_only.csv", row.names = FALSE)
}
