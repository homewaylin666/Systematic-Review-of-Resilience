# Module 4 Conditional Inference Forest
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# ---- Import the data and package (Load only one package to avoid conflicts)----
library(party)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(caret)

df <- read.csv("data/Gz.full.data.sheet_594_edited.csv")
df <- df %>%
  dplyr::select(id, measurement, quantification, target_variable_type, target_variable_group, level, approach, institution_country,
                habitat_type, taxon, disturbance_type, disturbance_pattern, observation_duration, journal, disturbance_both)

# ---- Only keep studies that observe direct-response ----
df <- df %>%
  filter(measurement == "observed")
# ---- Functions that help data processing ----
# Function to count frequencies after splitting multi-value entries
table_split <- function(data, column, sep = ";", useNA = "ifany") {
  # Extract the column as a vector
  col_data <- data[[column]]
  # Split by separator and unlist
  split_data <- unlist(strsplit(col_data, sep))
  # Trim whitespace
  split_data <- trimws(split_data)
  # Return frequency table
  table(split_data, useNA = useNA)
}
# Function to merge categories in multi-value columns
merge_categories <- function(data, column, merge_list, sep = ";") {
  # merge_list is a named list where names are new categories
  # and values are vectors of old categories to be merged
  
  data <- data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      # Handle NA
      if (is.na(x)) return(NA)
      
      # Split values
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      
      # Replace old categories with new ones
      for (new_cat in names(merge_list)) {
        old_cats <- merge_list[[new_cat]]
        values[values %in% old_cats] <- new_cat
      }
      
      # Remove duplicates and collapse
      unique_values <- unique(values)
      
      # If only one value remains, return it without separator
      if (length(unique_values) == 1) {
        return(unique_values)
      } else {
        return(paste(unique_values, collapse = paste0(sep, " ")))
      }
    }))
  
  # Return the modified data frame
  return(data)
}
# Function to rename values in column Y based on condition in column X
rename_conditional <- function(data, check_column, check_value,target_column,
                               old_value, new_value, sep = ";") {
  data <- data %>%
    mutate(!!target_column := sapply(1:n(), function(i) {
      # Get values from both columns
      check_val <- .data[[check_column]][i]
      target_val <- .data[[target_column]][i]
      
      # Handle NA
      if (is.na(target_val)) return(NA)
      
      # Check if check_column contains the check_value (even in multi-value)
      check_values <- trimws(unlist(strsplit(as.character(check_val), sep)))
      has_check_value <- check_value %in% check_values
      
      # If condition is met, perform replacement in target_column
      if (has_check_value) {
        # Split target column values
        target_values <- trimws(unlist(strsplit(as.character(target_val), sep)))
        
        # Replace old_value with new_value
        target_values[target_values == old_value] <- new_value
        
        # Remove duplicates
        unique_values <- unique(target_values)
        
        # Return single value or concatenated multi-values
        if (length(unique_values) == 1) {
          return(unique_values)
        } else {
          return(paste(unique_values, collapse = paste0(" ", sep, " ")))
        }
      } else {
        # If condition not met, keep original
        return(target_val)
      }
    }))
  
  return(data)
}
# Function to remove a category only from multi-value samples
remove_from_multivalue <- function(data, column, category_to_remove, sep = ";") {
  # Remove category only if there are multiple values
  # Keep it if it's the only value
  
  data <- data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      # Handle NA
      if (is.na(x)) return(NA)
      
      # Split values
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      
      # If only one value, keep it (even if it's the target category)
      if (length(values) == 1) {
        return(x)
      }
      
      # If multiple values, remove the target category
      values <- values[values != category_to_remove]
      
      # Remove duplicates
      unique_values <- unique(values)
      
      # Return single value or concatenated values
      if (length(unique_values) == 1) {
        return(unique_values)
      } else {
        return(paste(unique_values, collapse = paste0(sep, " ")))
      }
    }))
  
  return(data)
}
# Function to compare how two categories in predictor X distribute across response Y
compare_category_distribution <- function(data, predictor, cat_a, cat_b, response = "quantification", sep = "; ") {
  
  # Expand multi-value rows: split predictor column by separator, one row per value
  expanded <- data %>%
    mutate(.pred_original = .data[[predictor]]) %>%
    separate_rows(!!sym(predictor), sep = sep) %>%
    mutate(!!sym(predictor) := trimws(.data[[predictor]]))
  
  # Subset data for each category from the expanded data
  data_a <- expanded[expanded[[predictor]] == cat_a, ]
  data_b <- expanded[expanded[[predictor]] == cat_b, ]
  
  cat(sprintf("\n=== Comparing '%s' vs '%s' in [%s] ===\n", cat_a, cat_b, predictor))
  cat(sprintf("  n(%s) = %d,  n(%s) = %d\n\n", cat_a, nrow(data_a), cat_b, nrow(data_b)))
  
  # Frequency and proportion tables
  freq_a <- table(data_a[[response]])
  freq_b <- table(data_b[[response]])
  
  # Align levels
  all_levels <- sort(unique(c(names(freq_a), names(freq_b))))
  freq_a <- freq_a[all_levels]; freq_a[is.na(freq_a)] <- 0; names(freq_a) <- all_levels
  freq_b <- freq_b[all_levels]; freq_b[is.na(freq_b)] <- 0; names(freq_b) <- all_levels
  
  prop_a <- round(freq_a / sum(freq_a) * 100, 1)
  prop_b <- round(freq_b / sum(freq_b) * 100, 1)
  
  # Build comparison table
  comp <- data.frame(
    response_category = all_levels,
    n_a = as.integer(freq_a),
    pct_a = as.numeric(prop_a),
    n_b = as.integer(freq_b),
    pct_b = as.numeric(prop_b),
    pct_diff = as.numeric(prop_a - prop_b)
  )
  colnames(comp)[2:5] <- c(
    paste0("n_", cat_a), paste0("pct_", cat_a),
    paste0("n_", cat_b), paste0("pct_", cat_b)
  )
  
  print(comp, row.names = FALSE)
  
  # Fisher's exact test (handles small cells better than chi-square)
  cont <- rbind(freq_a, freq_b)
  rownames(cont) <- c(cat_a, cat_b)
  test <- fisher.test(cont, simulate.p.value = TRUE, B = 5000)
  
  cat(sprintf("\nFisher's exact test p-value: %.4f\n", test$p.value))
  
  if (test$p.value > 0.05) {
    cat("→ Distributions are NOT significantly different. Merging is reasonable.\n")
  } else {
    cat("→ Distributions ARE significantly different. Merging may lose information.\n")
  }
  
  invisible(comp)
}
# Function to keep only the highest priority category from multi-value entries
keep_priority <- function(data, column, priority_list, sep = ";") {
  # priority_list: vector of categories in priority order (highest to lowest)
  # Example: c("fire", "physical template", "chemical", ...)
  
  data <- data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      # Handle NA
      if (is.na(x)) return(NA)
      
      # Split values
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      
      # If only one value, keep it
      if (length(values) == 1) return(x)
      
      # Find which values exist in priority list
      matching_values <- values[values %in% priority_list]
      
      # If no match in priority list, keep original
      if (length(matching_values) == 0) return(x)
      
      # Find the highest priority value
      # (lowest index in priority_list = highest priority)
      priorities <- match(matching_values, priority_list)
      highest_priority_value <- matching_values[which.min(priorities)]
      
      return(highest_priority_value)
    }))
  
  return(data)
}
# Function to create summary for one variable
create_var_summary <- function(data, var_name, var_type) {
  if (var_type == "categorical") {
    freq_table <- table(data[[var_name]], useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    summary_df <- data.frame(
      Variable = var_name,
      Type = "Categorical",
      Category = names(freq_table),
      Frequency = as.vector(freq_table),
      Percentage = round(as.vector(prop_table), 2),
      stringsAsFactors = FALSE
    )
  } else {  # numeric
    summary_df <- data.frame(
      Variable = var_name,
      Type = "Numeric",
      Category = c("Min", "Q1", "Median", "Mean", "Q3", "Max", "NA"),
      Frequency = c(
        min(data[[var_name]], na.rm = TRUE),
        quantile(data[[var_name]], 0.25, na.rm = TRUE),
        median(data[[var_name]], na.rm = TRUE),
        mean(data[[var_name]], na.rm = TRUE),
        quantile(data[[var_name]], 0.75, na.rm = TRUE),
        max(data[[var_name]], na.rm = TRUE),
        sum(is.na(data[[var_name]]))
      ),
      Percentage = NA,
      stringsAsFactors = FALSE
    )
  }
  
  return(summary_df)
}


# ---- data processing: merge categories: Quantification ----
# check the distribution first
table(df$quantification, useNA = "ifany")
table_split(df, "quantification")
# create the category 'multidimensional quantification' and merge small categories
df <- df %>%
  mutate(quantification = ifelse(str_detect(quantification, ";"), "multidimensional quantification", quantification)) %>%
  mutate(quantification = case_when(
    quantification %in% c("recovery rate", "recovery degree", "recovery time", "multidimensional quantification", "latitude") ~ quantification,
    # Convert all other values to "others"
    TRUE ~ "others"
  ))
# 'others' includes resistance(34), invariability(7), and other metrics(19).
# check the distribution again
table(df$quantification) #rate:degree:time:other:multi:latitude = 135:132:69:60:44:39

# ---- data processing: merge categories: Disturbance pattern ----
# check the distribution first
table(df$disturbance_pattern, useNA = "ifany")
# manually check the 4 NA, assign them a value
df <- df %>%
  mutate(disturbance_pattern = case_when(
    id == 543 ~ "press", id == 551 ~ "pulse",
    id == 1016 ~ "pulse", id == 1488 ~ "pulse",
    TRUE ~ disturbance_pattern
  ))
# merge small categories
df <- df %>%
  mutate(disturbance_pattern = case_when(
    # Convert values with semicolons to "pulse" (n=14)
    str_detect(disturbance_pattern, ";") ~ "pulse",
    # Keep other values as they are
    TRUE ~ disturbance_pattern
  ))
# check the distribution again
table(df$disturbance_pattern, useNA = "ifany") # 144(press):335(pulse)

# ---- data processing: merge categories: Observation time ----
# check how many NA are there
sum(is.na(df$observation_duration))
# manually check the 21 NA, assign them a value (only if they do have a value)
df <- df %>%
  mutate(observation_duration = case_when(
    id == 551 ~ 10585, id == 1426 ~ 36500, id == 1488 ~ 365, id == 1662 ~ 14600,
    TRUE ~ observation_duration
  )) # There are still 17 NA left (3.5% samples)

# given limited NA, imputed missing values using the median of the corresponding quantification category
df <- df %>%
  group_by(quantification) %>%
  mutate(observation_duration = ifelse(
    is.na(observation_duration), median(observation_duration, na.rm = TRUE), observation_duration)) %>%
  ungroup()

# log the observation time
df <- df %>%
  mutate(
    observation_duration = case_when(
      observation_duration > 0 ~ log10(observation_duration),
      TRUE ~ 0
    ))


# ---- data processing: merge categories: Target variable ----
# check the distribution first (split the multi-values)
table_split(df, "target_variable_group")
# manually check the 2 NA and 1 other, assign them a value
df <- df %>%
  mutate(target_variable_group = case_when(
    id == 586 ~ "environmental context", id == 1260 ~ "process based indicator", 
    id == 1977 ~ "environmental context",
    TRUE ~ target_variable_group),
    target_variable_type = case_when(
      id == 586 ~ "regional network parameter", id == 1260 ~ "interaction", 
      id == 1977 ~ "regional network parameter",
      TRUE ~ target_variable_type))

# merge environmental context into other target variable groups according to their target variable type
df <- df %>% 
  rename_conditional("target_variable_type", "abiotic parameter", "target_variable_group", "environmental context", "functional response") %>%
  rename_conditional("target_variable_type", "regional network parameter", "target_variable_group", "environmental context", "process based indicator") %>%
  rename_conditional("target_variable_type", "land use", "target_variable_group", "environmental context", "structure")

# check the distribution (not split multi-value)
table(df$target_variable_group, useNA = "ifany")
# deal with multi-value
  # define priority rankings for target variable group
target_variable_priority <- c("process based indicator", "structure", "functional response", "quantify")
  # for each multi-value sample, only keep the prioritized disturbance type
df <- df %>%
  keep_priority("target_variable_group", target_variable_priority)
# check the distribution again
table(df$target_variable_group, useNA = "ifany")

# ---- data processing: merge categories: Approach ----
# check the distribution first (split the multi-values)
table_split(df, "approach")
# test whether modeling and indoor experiment are similar
compare_category_distribution(df, "approach", "modeling and simulation", "indoor experiment")
# Fisher's exact test p-value: 0.2138
# merge small but relatively similar categories
df <- merge_categories(df, "approach", merge_list = list(
  "model/indoor" = c("modeling and simulation", "indoor experiment")
))
# check the distribution (not split multi-value)
table(df$approach, useNA = "ifany")
# deal with multi-value
  # define priority rankings for approach
approach_priority <- c("model/indoor", "field experiment", "field observation")
  # for each multi-value sample, only keep the prioritized disturbance type
df <- df %>%
  keep_priority("approach", approach_priority)
# check the distribution again
table(df$approach, useNA = "ifany")

# ---- data processing: merge categories: Level ----
# check the distribution first (split the multi-values)
table_split(df, "level")
# manually check the only NA, assign them a value
df <- df %>%
  mutate(level = case_when(
    id == 586 ~ "ecosystem",
    TRUE ~ level))
# test whether landscape and ecosystem are similar
compare_category_distribution(df, "level", "landscape", "ecosystem")
# Fisher's exact test p-value: 0.0220, they are different
# merge small but relatively similar categories
df <- merge_categories(df, "level", merge_list = list(
  "ecosystem/landscape" = c("landscape", "ecosystem")
))
  # check the distribution (not split multi-value)
table(df$level, useNA = "ifany")
  # deal with multi-value
# define priority rankings for level
level_priority <- c("ecosystem/landscape", "community", "population", "individual")
# for each multi-value sample, only keep the prioritized level
df <- df %>%
  keep_priority("level", level_priority)
# check the distribution again
table(df$level, useNA = "ifany")


# ---- data processing: merge categories: Disturbance type ----
# check the distribution first (split the multi-values)
table_split(df, "disturbance_type")
# manually check the 2 NA, assign them a value
df <- df %>%
  mutate(disturbance_type = case_when(
    id == 543 ~ "biotic", id == 551 ~ "climatic",
    TRUE ~ disturbance_type
  ))
# test whether landscape and ecosystem are similar
compare_category_distribution(df, "disturbance_type", "geophysical", "hydrological")
# Fisher's exact test p-value: 0.6021, they are NOT significantly different
compare_category_distribution(df, "disturbance_type", "hydrological", "landuse and infrastructure development")
# Fisher's exact test p-value: 0.3957, they are NOT significantly different
compare_category_distribution(df, "disturbance_type", "geophysical", "landuse and infrastructure development")
# Fisher's exact test p-value: 0.9226, they are NOT significantly different
compare_category_distribution(df, "disturbance_type", "biotic", "biological resource use")
# Fisher's exact test p-value: 0.7149, they are NOT significantly different
compare_category_distribution(df, "disturbance_type", "resource", "chemical")
# Fisher's exact test p-value: 0.0588, they are NOT significantly different
# merge small but relatively similar categories
df <- merge_categories(df, "disturbance_type", merge_list = list(
  "physical template/land use" = c("geophysical", "hydrological", "landuse and infrastructure development"),
  "biotic pressure" = c("biotic", "biological resource use"),
  "chemical/resource" = c("chemical", "resource")
))

# check the distribution (not split multi-value)
table(df$disturbance_type, useNA = "ifany")
# deal with multi-value
df <- remove_from_multivalue(df, "disturbance_type", "structural") # remove structural disturbances that with other type of disturbances.
  # define priority rankings for disturbance type
pulse_priority <- c("fire", "chemical/resource", "physical template/land use", "biotic pressure", "climatic")
press_priority <- c("physical template/land use", "biotic pressure", "climatic", "chemical/resource", "fire")
  # for each multi-value sample, only keep the prioritized disturbance type
df <- df %>%
  mutate(disturbance_type = case_when(
    disturbance_pattern == "pulse" ~ 
      keep_priority(df, "disturbance_type", pulse_priority)$disturbance_type,
    disturbance_pattern == "press" ~ 
      keep_priority(df, "disturbance_type", press_priority)$disturbance_type,
    TRUE ~ disturbance_type
  ))

# check the distribution (not split multi-value) again.
table(df$disturbance_type, useNA = "ifany")

# ---- data processing: merge categories: Taxon ----
# check the distribution (not split multi-value)
table(df$taxon, useNA = "ifany")
# merge categories
df <- df %>%
  mutate(taxon = case_when(
    # Convert NA values to "large.scale"
    is.na(taxon) ~ "large scale no specific taxon",
    # Merge kingdoms that are not Plantae or Animalia into "microbe"
    str_detect(taxon, "bacteria|fungi|archaea|chromista|protozoa|phytoplankton|zooplankton") ~ "microbe included",
    # Convert values with both Plantae and Animalia to "both"
    str_detect(taxon, "plantae") & str_detect(taxon, "animalia") ~ "plantae and animalia",
    # Rest of the algae are kelp forest, view them like plant
    str_detect(taxon, "algae") ~ "plantae",
    # Keep Plantae and Animalia as they are
    taxon %in% c("plantae", "animalia") ~ taxon,
    # Convert all other values to "others"
    TRUE ~ "others/virtual"
  ))
# check the distribution again
table(df$taxon, useNA = "ifany")
# test whether "plantae and animalia" is closer to plantae or animalia
compare_category_distribution(df, "taxon", "plantae and animalia", "plantae")
#Fisher's exact test p-value: 0.0244
compare_category_distribution(df, "taxon", "plantae and animalia", "animalia")
#Fisher's exact test p-value: 0.0732
# merge the small categories (n<20)
df <- merge_categories(df, "taxon", merge_list = list(
  "no specific" = c("others/virtual", "large scale no specific taxon"),
  "animalia/both" = c("plantae and animalia", "animalia")
))

# ---- data processing: merge categories: Habitat type ----
# check the distribution
table(df$habitat_type, useNA = "ifany")
# give NA a name
df <- df %>%
  mutate(habitat_type = case_when(
    # Convert NA values to "no.habitat"
    is.na(habitat_type) ~ "no specific",
    # Keep other values as they are
    TRUE ~ habitat_type
  ))
# test whether "coastal", "marine.neritic" are similar
compare_category_distribution(df, "habitat_type", "coastal", "marine.neritic") #p-value: 0.9980
# test whether "desert", "shrubland", "other" are similar
compare_category_distribution(df, "habitat_type", "desert", "shrubland") #p-value: 0.0146
compare_category_distribution(df, "habitat_type", "desert", "other") #p-value: 0.1814
compare_category_distribution(df, "habitat_type", "other", "shrubland") #p-value: 0.5817
# test whether "savanna", "grassland", "agricultural" are similar
compare_category_distribution(df, "habitat_type", "savanna", "grassland") #p-value: 0.1098
compare_category_distribution(df, "habitat_type", "savanna", "agricultural") #p-value: 0.3039
compare_category_distribution(df, "habitat_type", "grassland", "agricultural") #p-value: 0.0262
# test whether "multi", "no specific/major" are similar
compare_category_distribution(df, "habitat_type", "multi", "no specific") #p-value: 0.3385

# merge some small cats into similar groups
df <- merge_categories(df, "habitat_type", merge_list = list(
  "marine/coastal" = c("oceanic", "marine.neritic", "coastal"),
  "open barren" = c("desert", "shrubland", "other"),
  "open herbaceous" = c("savanna", "grassland", "agricultural"),
  "no specific/major" = c("multi", "no specific")
))

# ---- data processing: merge categories: Institution country ----
# check the distribution
table(df$institution_country, useNA = "ifany")
# First convert all country codes to uppercase
df <- df %>%
  mutate(institution_country = toupper(institution_country))
# Merge with continent mapping based on ISO 3166-1 alpha-2 codes
df <- merge_categories(df, "institution_country", merge_list = list(
  "north america" = c("US", "CA", "MX"),
  "central & south america" = c("GT", "BZ", "SV", "HN", "NI", "CR", "PA", "CU", "JM", "HT", "DO", "TT", "BB", "GD", "VC", "LC", "DM", "AG", "KN", "BS", "GL", "BR", "AR", "CL", "PE", "CO", "VE", "EC", "BO", "PY", "UY", "GY", "SR", "GF"),
  "asia" = c("CN", "IN", "JP", "KR", "ID", "MY", "TH", "VN", "PH", "SG", "TW", "MM", "KH", "LA", "BN", "TL", "MN", "KZ", "UZ", "TM", "TJ", "KG", "AF", "PK", "BD", "LK", "MV", "NP", "BT", "IR", "IQ", "SY", "LB", "JO", "IL", "PS", "SA", "YE", "OM", "AE", "QA", "BH", "KW", "AM", "AZ", "GE"),
  "europe" = c("AL", "AD", "AT", "BY", "BE", "BA", "BG", "HR", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU", "IS", "IE", "IT", "LV", "LI", "LT", "LU", "MT", "MD", "MC", "ME", "NL", "MK", "NO", "PL", "PT", "RO", "RU", "SM", "RS", "SK", "SI", "ES", "SE", "CH", "UA", "GB", "VA", "CY", "TR", "AX", "FO", "GI", "GG", "JE", "IM"),
  "africa" = c("NG", "ET", "EG", "ZA", "KE", "UG", "DZ", "SD", "MA", "AO", "GH", "MZ", "MG", "CM", "CI", "NE", "BF", "ML", "MW", "ZM", "SO", "SN", "TD", "ZW", "GN", "RW", "BJ", "TN", "BI", "ER", "SL", "TG", "CF", "LR", "LY", "MR", "NA", "GM", "BW", "GA", "LS", "GW", "GQ", "MU", "SZ", "DJ", "RE", "KM", "CV", "ST", "SC", "YT"),
  "oceania" = c("AU", "NZ", "PG", "FJ", "SB", "VU", "NC", "PF", "WS", "GU", "TO", "KI", "PW", "MH", "FM", "TV", "NR", "CK", "NU", "TK", "WF", "AS", "MP", "UM", "HM", "CC", "CX", "NF")
))
# check the distribution again
table(df$institution_country, useNA = "ifany")
# test whether "north america", "central & south america" are similar
compare_category_distribution(df, "institution_country", "north america", "central & south america") #p-value: 0.9860
# test whether "europe", "africa" are similar
compare_category_distribution(df, "institution_country", "europe", "africa") #p-value: 0.0352
# Merge small categories (<20)
df <- merge_categories(df, "institution_country", merge_list = list(
  "america" = c("north america", "central & south america"),
  "europe & africa" = c("europe", "africa")
))



# ---- data processing: merge categories: Journal ----
# check the distribution first
table(df$journal, useNA = "ifany")
# merge small but relatively similar categories
df <- merge_categories(df, "journal", merge_list = list(
  "general journal" = c("science", "nature", "proceedings of the national academy of sciences of the united states of america")
))


# =====================================================================
# CONDITIONAL INFERENCE FOREST ANALYSIS
# =====================================================================
# ===== 1. Prepare data =====
predictors_cat <- c("disturbance_pattern", "target_variable_group", "approach", 
                    "level", "disturbance_type", "taxon", "habitat_type")
predictors_num <- "observation_duration"

analysis_data <- df %>%
  dplyr::select(quantification, all_of(c(predictors_cat, predictors_num)))

# Convert to factors
for (var in predictors_cat) {
  analysis_data[[var]] <- as.factor(analysis_data[[var]])
}
analysis_data$quantification <- as.factor(analysis_data$quantification)

# ===== 2. Data quality checks =====
cat("=== DATA QUALITY CHECKS ===\n")
cat("Sample size:", nrow(analysis_data), "\n")
class_dist <- table(analysis_data$quantification)
imbalance_ratio <- max(class_dist) / min(class_dist)
cat("Response distribution:\n")
print(class_dist)
cat(sprintf("Imbalance ratio: %.2f:1\n", imbalance_ratio))

# Check for missing values
missing_summary <- sapply(analysis_data, function(x) sum(is.na(x)))
if (any(missing_summary > 0)) {
  cat("\n⚠️  Variables with missing values:\n")
  print(missing_summary[missing_summary > 0])
} else {
  cat("\n✓ No missing values\n")
}

# Check for near-zero variance predictors
nzv <- nearZeroVar(analysis_data[, -1], saveMetrics = TRUE)
if (any(nzv$nzv)) {
  cat("\n⚠️  Near-zero variance predictors:\n")
  print(rownames(nzv)[nzv$nzv])
} else {
  cat("✓ No near-zero variance predictors\n")
}

# ===== 3. Export variable summary =====
outcome_summary <- create_var_summary(analysis_data, "quantification", "categorical")
cat_summaries <- lapply(predictors_cat, function(var) {
  create_var_summary(analysis_data, var, "categorical")
})
num_summary <- create_var_summary(analysis_data, predictors_num, "numeric")

all_summaries <- bind_rows(outcome_summary, bind_rows(cat_summaries), num_summary)
overall_info <- data.frame(
  Variable = c("Total Sample Size", "Number of Predictors", "Outcome Variable"),
  Type = c("Overall", "Overall", "Overall"),
  Category = c(as.character(nrow(analysis_data)), 
               as.character(length(c(predictors_cat, predictors_num))),
               "quantification"), 
  Frequency = NA, Percentage = NA, stringsAsFactors = FALSE
)
final_summary <- bind_rows(overall_info, all_summaries)
write.csv(final_summary, "table/Table CIF_variable_summary.csv", row.names = FALSE)


# ===== 4. Build formula =====
set.seed(207)
predictor_names <- setdiff(names(analysis_data), "quantification")
fml <- as.formula(paste("quantification ~", paste(predictor_names, collapse = " + ")))
cat("\n=== MODEL FORMULA ===\n")
print(fml)


# ===== 5. Define core functions =====
# Strict macro-F1
macroF1_strict <- function(obs, pred) {
  obs  <- factor(obs)
  pred <- factor(pred, levels = levels(obs))
  cm   <- table(obs, pred)
  f1 <- sapply(levels(obs), function(cl) {
    tp    <- cm[cl, cl]
    fp    <- sum(cm[, cl]) - tp
    fn    <- sum(cm[cl, ]) - tp
    denom <- 2 * tp + fp + fn
    if (denom == 0) 0 else 2 * tp / denom
  })
  mean(f1)
}
# Permutation importance for a single predictor
perm_drop_predictor <- function(fit, valid_df, ycol, pred_col, metric_fun, 
                                nperm = 20, seed = 207) {
  if (!is.null(seed)) set.seed(seed)
  base_pred <- predict(fit, newdata = valid_df, type = "response")
  base      <- metric_fun(valid_df[[ycol]], base_pred)
  
  drops <- replicate(nperm, {
    Xp <- valid_df
    Xp[[pred_col]] <- Xp[[pred_col]][sample.int(nrow(valid_df))]
    pred_p <- predict(fit, newdata = Xp, type = "response")
    base - metric_fun(valid_df[[ycol]], pred_p)
  })
  mean(drops)
}
# Comprehensive CV function: returns both importance AND performance metrics
cv_full_analysis <- function(formula, data, 
                             cv_folds = 5, cv_repeats = 20, nperm = 50, mtry_fixed = NULL, 
                             mincriterion = 0, ntree = 2000,seed  = 207) {
  all_vars      <- all.vars(formula)
  response_var  <- all_vars[1]
  predictor_names <- all_vars[-1]
  p <- length(predictor_names)
  
  if (is.null(mtry_fixed)) mtry_fixed <- floor(sqrt(p))
  mtry_fixed <- min(mtry_fixed, p)
  
  cat(sprintf("\n=== CV ANALYSIS (mtry=%d, folds=%d, repeats=%d, mincriterion=%.2f) ===\n", 
              mtry_fixed, cv_folds, cv_repeats, mincriterion))
  
  ctrl <- cforest_control(
    teststat = "quad",
    testtype = "Univ",
    mincriterion = mincriterion,
    ntree = ntree,
    mtry = mtry_fixed)
  
  set.seed(seed)
  
  y_all <- data[[response_var]]
  lev   <- levels(y_all)
  
  # Storage
  importance_list <- list()
  oof_preds       <- list()
  cv_scores       <- c()
  run_id          <- 0L
  
  rep_seeds <- seed + seq_len(cv_repeats) * 100L
  
  for (r in seq_len(cv_repeats)) {
    set.seed(rep_seeds[r])
    folds <- createFolds(y_all, k = cv_folds, list = TRUE, returnTrain = FALSE)
    
    for (k in seq_along(folds)) {
      run_id   <- run_id + 1L
      test_idx <- folds[[k]]
      train_df <- data[-test_idx, , drop = FALSE]
      valid_df <- data[ test_idx, , drop = FALSE]
      
      # Fit model
      fit <- cforest(formula, data = train_df, controls = ctrl)
      
      # Out-of-fold predictions
      pred <- predict(fit, newdata = valid_df, type = "response")
      obs  <- factor(valid_df[[response_var]], levels = lev)
      pred <- factor(pred, levels = lev)
      
      # CV score
      fold_f1 <- macroF1_strict(obs, pred)
      cv_scores <- c(cv_scores, fold_f1)
      
      # Store OOF predictions
      oof_preds[[length(oof_preds) + 1L]] <- data.frame(
        obs = obs, pred = pred, repeat_id = r, fold = k, stringsAsFactors = FALSE
      )
      
      # Variable importance (permutation)
      for (pred_var in predictor_names) {
        drop_val <- perm_drop_predictor(
          fit, valid_df, response_var, pred_var,
          metric_fun = macroF1_strict, nperm = nperm,
          seed = rep_seeds[r] + k
        )
        importance_list[[length(importance_list) + 1L]] <- data.frame(
          run = paste0("r", r, "_k", k),
          predictor = pred_var,
          drop = drop_val,
          stringsAsFactors = FALSE
        )
      }
    }
    if (r %% 5 == 0) cat(sprintf("Completed repeat %d/%d\n", r, cv_repeats))
  }
  
  # === Aggregate importance ===
  imp_df <- bind_rows(importance_list)
  imp_summary <- imp_df %>%
    group_by(predictor) %>%
    summarise(
      median_drop = median(drop, na.rm = TRUE),
      mean_drop   = mean(drop, na.rm = TRUE),
      p05         = quantile(drop, 0.05, na.rm = TRUE),
      p95         = quantile(drop, 0.95, na.rm = TRUE),
      pos_frac    = mean(drop > 0),
      .groups     = "drop"
    ) %>%
    arrange(desc(median_drop))
  
  # Top-3 frequency
  rank_df <- imp_df %>%
    group_by(run) %>%
    mutate(rank = rank(-drop, ties.method = "average")) %>%
    ungroup() %>%
    group_by(predictor) %>%
    summarise(top3_freq = mean(rank <= 3), .groups = "drop")
  
  imp_final <- left_join(imp_summary, rank_df, by = "predictor")
  
  # === Aggregate performance ===
  oof_df <- bind_rows(oof_preds)
  
  # Pooled metrics
  pooled_cm       <- table(factor(oof_df$obs, levels = lev),
                           factor(oof_df$pred, levels = lev))
  pooled_macroF1  <- macroF1_strict(oof_df$obs, oof_df$pred)
  pooled_accuracy <- mean(oof_df$obs == oof_df$pred)
  
  # Per-class F1
  per_class <- do.call(rbind, lapply(lev, function(cl) {
    tp    <- pooled_cm[cl, cl]
    fp    <- sum(pooled_cm[, cl]) - tp
    fn    <- sum(pooled_cm[cl, ]) - tp
    prec  <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    rec   <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1    <- ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
    data.frame(class = cl, Precision = prec, Recall = rec, F1 = f1,
               TP = tp, FP = fp, FN = fn)
  }))
  
  # CV score distribution
  cv_summary <- c(
    mean = mean(cv_scores),
    sd   = sd(cv_scores),
    p05  = quantile(cv_scores, 0.05),
    p95  = quantile(cv_scores, 0.95)
  )
  
  cat("\n=== CV PERFORMANCE SUMMARY ===\n")
  cat(sprintf("Mean macro-F1: %.4f (SD: %.4f)\n", cv_summary["mean"], cv_summary["sd"]))
  cat(sprintf("5th-95th percentile: [%.4f, %.4f]\n", cv_summary["p05"], cv_summary["p95"]))
  cat(sprintf("Pooled macro-F1: %.4f\n", pooled_macroF1))
  cat(sprintf("Pooled accuracy: %.4f\n", pooled_accuracy))
  
  list(
    importance       = imp_final,
    importance_raw   = imp_df,
    cv_scores        = cv_summary,
    cv_scores_vec    = cv_scores,
    pooled_metrics   = c(macroF1 = pooled_macroF1, accuracy = pooled_accuracy),
    confusion_matrix = pooled_cm,
    per_class        = per_class,
    oof_predictions  = oof_df
  )
}
# Lightweight CV for mtry tuning (performance only)
cv_performance_only <- function(formula, data,
                                cv_folds   = 5, cv_repeats = 10, mtry_fixed = NULL, 
                                mincriterion = 0, ntree = 500, seed = 207) {
  all_vars      <- all.vars(formula)
  response_var  <- all_vars[1]
  p <- length(all_vars) - 1
  
  if (is.null(mtry_fixed)) mtry_fixed <- floor(sqrt(p))
  mtry_fixed <- min(mtry_fixed, p)
  
  ctrl <- cforest_control(teststat = "quad",
                          testtype = "Univ",
                          ntree = ntree, 
                          mtry = mtry_fixed, 
                          mincriterion = mincriterion) 
  set.seed(seed)
  
  y_all <- data[[response_var]]
  cv_scores <- c()
  
  rep_seeds <- seed + seq_len(cv_repeats) * 100L
  
  for (r in seq_len(cv_repeats)) {
    set.seed(rep_seeds[r])
    folds <- createFolds(y_all, k = cv_folds, list = TRUE, returnTrain = FALSE)
    
    for (k in seq_along(folds)) {
      test_idx <- folds[[k]]
      train_df <- data[-test_idx, , drop = FALSE]
      valid_df <- data[ test_idx, , drop = FALSE]
      
      # Retry logic for LAPACK SVD failures
      fit        <- NULL
      attempt    <- 0L
      fold_seed  <- rep_seeds[r] + k * 13L
      
      while (is.null(fit) && attempt < 5L) {
        attempt <- attempt + 1L
        set.seed(fold_seed + attempt * 7L)
        fit <- tryCatch(
          cforest(formula, data = train_df, controls = ctrl),
          error = function(e) { NULL }
        )
      }
      
      if (is.null(fit)) { next }  # skip fold if all 5 attempts failed
      
      pred <- predict(fit, newdata = valid_df, type = "response")
      
      cv_scores <- c(cv_scores, macroF1_strict(valid_df[[response_var]], pred))
    }
  }
  
  list(
    cv_scores_vec = cv_scores,
    mean_f1 = mean(cv_scores),
    sd_f1   = sd(cv_scores)
  )
}
# mtry tuning helper
suggest_mtry_grid <- function(p) {
  cand <- unique(round(c(sqrt(p), 1.5*sqrt(p), 2*sqrt(p), 
                         0.08*p, 0.10*p, 0.12*p)))
  cand <- cand[cand > 0 & cand < p]
  cand <- sort(unique(c(cand, 2, 3, 4, 5)))
  cand[cand < p]
}
# Fast mtry tuning 
tune_mtry_fast <- function(fml, data, candidates,
                           cv_folds = 5, cv_repeats = 10, mincriterion = 0,
                           ntree = 500, seed = 207) {
  p <- length(all.vars(fml)) - 1
  candidates <- sort(unique(pmin(p - 1, candidates)))
  
  cat("\n=== FAST MTRY TUNING (performance only) ===\n")
  cat(sprintf("Candidates: %s\n", paste(candidates, collapse = ", ")))
  
  results <- list()
  for (m in candidates) {
    cat(sprintf("[Tuning] mtry = %d ... ", m))
    
    out <- cv_performance_only(fml, data, cv_folds, cv_repeats, m, mincriterion, ntree, seed)
    
    se <- out$sd_f1 / sqrt(cv_folds * cv_repeats)
    
    results[[length(results) + 1]] <- data.frame(
      mtry = m, 
      f1_mean = out$mean_f1, 
      f1_sd = out$sd_f1,
      se = se
    )
    cat(sprintf("F1 = %.4f (SD = %.4f)\n", out$mean_f1, out$sd_f1))
  }
  
  tab <- bind_rows(results) %>% arrange(mtry)
  
  cat("\n=== TUNING RESULTS ===\n")
  print(tab, digits = 4)
  
  # 1-SE rule
  best_mean <- max(tab$f1_mean)
  best_se   <- tab$se[which.max(tab$f1_mean)]
  thresh    <- best_mean - best_se
  
  pick <- tab %>% 
    filter(f1_mean >= thresh) %>%
    arrange(mtry) %>%  # Simpler model when tied
    slice(1)
  
  cat(sprintf("\n✓ Chosen mtry: %d (F1=%.4f ± %.4f)\n", 
              pick$mtry, pick$f1_mean, pick$f1_sd))
  
  list(summary = tab, chosen_mtry = pick$mtry, chosen_row = pick)
}


# ===== 6. Test mincriterion sensitivity =====
cat("\n=== MINCRITERION SENSITIVITY ANALYSIS ===\n")

mincriterion_values <- c(0, 0.9, 0.95)
sensitivity_results <- list()

for (mc in mincriterion_values) {
  cat(sprintf("\nTesting mincriterion = %.2f ...\n", mc))
  
  out <- cv_performance_only(fml, analysis_data, 
                             cv_folds = 5, cv_repeats = 5,
                             mtry_fixed = floor(sqrt(length(predictor_names))),
                             mincriterion = mc,
                             ntree = 500, seed = 207)
  
  sensitivity_results[[length(sensitivity_results) + 1]] <- data.frame(
    mincriterion = mc,
    mean_f1 = out$mean_f1,
    sd_f1 = out$sd_f1
  )
}

sens_table <- bind_rows(sensitivity_results)
print(sens_table, digits = 4)

best_mc_idx <- which.max(sens_table$mean_f1)
f1_range <- max(sens_table$mean_f1) - min(sens_table$mean_f1)

if (f1_range < 0.01) {
  cat("\n✓ Performance insensitive to mincriterion (Δ < 0.01)\n")
  cat("  Using default (0) for maximum flexibility\n")
  mincriterion_final <- 0
} else {
  cat("\n⚠️  Performance varies with mincriterion\n")
  mincriterion_final <- sens_table$mincriterion[best_mc_idx]
  cat(sprintf("  Using best-performing value: %.2f\n", mincriterion_final))
}

write.csv(sens_table, "table/Table_CIF_mincriterion_sensitivity.csv", row.names = FALSE)

# ===== 7. Tune mtry =====
p_full <- length(predictor_names)
grid   <- suggest_mtry_grid(p_full)

tuning <- tune_mtry_fast(fml, analysis_data, grid,
                    cv_folds = 5, cv_repeats = 10, 
                    mincriterion = mincriterion_final, ntree = 500, seed = 207)
write.csv(tuning$summary, "table/Table_CIF_mtry_sensitivity.csv", row.names = FALSE) # manual check
mtry_final <- 3 # following "1-se rule" and closest to √p heuristic

# ===== 8. Tune tree sizes =====
# Detailed version with all CV fold results
tree_sizes <- c(250, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)

ntree_results <- data.frame(
  ntree = numeric(),
  mean_f1 = numeric(),
  sd_f1 = numeric(),
  stringsAsFactors = FALSE
)

for (nt in tree_sizes) {
  cat(sprintf("Testing ntree = %d...\n", nt))
  
  tree.test <- cv_performance_only(fml, analysis_data, 
                                   cv_folds = 5, cv_repeats = 10,
                                   mtry_fixed = mtry_final, mincriterion = 0,
                                   ntree = nt, seed = 207)
  
  # Add row to results
  new_row <- data.frame(
    ntree = nt,
    mean_f1 = tree.test$mean_f1,
    sd_f1 = if(!is.null(tree.test$sd_f1)) tree.test$sd_f1 else NA
  )
  
  ntree_results <- rbind(ntree_results, new_row)
  
  cat(sprintf("ntree = %d: F1 = %.4f\n", nt, tree.test$mean_f1))
}

# Export
write.csv(ntree_results, "table/Table_CIF_ntree_tuning.csv", row.names = FALSE)
print(ntree_results)
cat("\n=== BEST NTREE ===\n")
best_idx <- which.max(ntree_results$mean_f1)
cat(sprintf("Optimal ntree: %d\n", ntree_results$ntree[best_idx]))
cat(sprintf("F1 Score: %.4f ± %.4f\n", 
            ntree_results$mean_f1[best_idx], 
            ntree_results$sd_f1[best_idx]))

# ===== 9. Final analysis =====
cat("\n\n" %+% paste(rep("=", 60), collapse="") %+% "\n")
cat("FINAL ANALYSIS (mtry = ", mtry_final, ")\n")
cat(paste(rep("=", 60), collapse="") %+% "\n")

final_results <- cv_full_analysis(
  fml, analysis_data,
  cv_folds = 5, cv_repeats = 30, nperm = 50, mincriterion = mincriterion_final,
  mtry_fixed = mtry_final, ntree = 2000, seed = 207
)

# ===== 10. Class imbalance assessment =====
cat("\n=== CLASS IMBALANCE HANDLING ===\n")

smallest_class <- names(which.min(class_dist))
smallest_perf <- final_results$per_class %>%
  filter(class == smallest_class)

cat(sprintf("\nSmallest class (%s, n=%d) performance:\n", 
            smallest_class, min(class_dist)))
print(smallest_perf, digits = 3)

if (smallest_perf$F1 < 0.5) {
  cat("\n⚠️  Smallest class F1 < 0.5, performance may be inadequate\n")
} else {
  cat("\n✓ Smallest class adequately predicted (F1 ≥ 0.5)\n")
}

weighted_f1 <- weighted.mean(
  final_results$per_class$F1,
  table(analysis_data$quantification)[final_results$per_class$class]
)

cat(sprintf("\nMacro-F1: %.3f (equal weight, primary metric)\n", 
            final_results$pooled_metrics["macroF1"]))
cat(sprintf("Weighted-F1: %.3f (weighted by class size)\n", weighted_f1))

cat("\nImbalance handling summary:\n")
cat("  ✓ Stratified CV ensures proportional representation\n")
cat("  ✓ Macro-F1 treats all classes equally\n")
cat("  ✓ Per-class metrics monitored\n")
if (imbalance_ratio < 3) {
  cat("  ✓ Imbalance is moderate (< 3:1), no resampling needed\n")
}


# ===== 11. Model diagnostics =====
cat("\n=== MODEL DIAGNOSTICS ===\n")

# Confusion matrix
cat("\nConfusion Matrix (Pooled OOF Predictions):\n")
print(final_results$confusion_matrix)

# Per-class metrics
cat("\nPer-Class Performance:\n")
print(final_results$per_class, digits = 3)

# Identify problematic samples (optional: if you want per-sample analysis)
oof_summary <- final_results$oof_predictions %>%
  group_by(obs) %>%
  summarise(
    n = n(),
    n_correct = sum(obs == pred),
    accuracy = mean(obs == pred),
    .groups = "drop"
  ) %>%
  arrange(accuracy)

cat("\nAccuracy by True Class:\n")
print(oof_summary)

# Check if any class is systematically misclassified
cat("\nMost Common Misclassifications:\n")
misclass <- final_results$oof_predictions %>%
  filter(obs != pred) %>%
  count(obs, pred) %>%
  arrange(desc(n)) %>%
  head(5)
print(misclass)


# ===== 12. Export results =====
write.csv(final_results$cv_scores, 
          "table/Table_overall_metrics.csv", row.names = TRUE)
write.csv(final_results$importance, 
          "table/Table_CIF_importance.csv", row.names = FALSE)
write.csv(tuning$summary, 
          "table/Table_CIF_mtry_tuning.csv", row.names = FALSE)
write.csv(data.frame(cv_macroF1 = final_results$cv_scores_vec),
          "table/Table_CIF_cv_scores_distribution.csv", row.names = FALSE)
write.csv(final_results$per_class,
          "table/Table_CIF_per_class_metrics.csv", row.names = FALSE)
write.csv(as.data.frame.matrix(final_results$confusion_matrix),
          "table/Table_CIF_confusion_matrix.csv")

cat("\n✓ All tables exported to table/ directory\n")


# ===== 12.5. Save all results to Rdata (before plotting) =====
# Pre-compute heatmap data for top-3 predictors
top3_predictors <- final_results$importance %>%
  arrange(desc(median_drop)) %>%
  head(3) %>%
  pull(predictor)

heatmap_list <- setNames(
  lapply(top3_predictors, function(pred_var) {
    row_props <- prop.table(
      table(analysis_data[[pred_var]], analysis_data$quantification),
      margin = 1) * 100
    hm_df <- as.data.frame(row_props)
    colnames(hm_df) <- c("Category", "Metric", "Percentage")
    hm_df
  }),
  top3_predictors
)

plot_data <- list(
  final_results    = final_results,
  analysis_data    = analysis_data,
  class_dist       = class_dist,
  imbalance_ratio  = imbalance_ratio,
  weighted_f1      = weighted_f1,
  mtry_final       = mtry_final,
  mincriterion_final = mincriterion_final,
  predictor_names  = predictor_names,
  predictors_cat   = predictors_cat,
  predictors_num   = predictors_num,
  top3_predictors  = top3_predictors,
  heatmap_list     = heatmap_list,
  imp_plot_df      = final_results$importance %>%
    arrange(median_drop) %>%
    mutate(predictor = factor(predictor, levels = predictor)),
  pretty_names     = c(
    disturbance_pattern   = "Disturbance pattern",
    target_variable_group = "Target variable",
    approach              = "Approach",
    level                 = "Ecological Level",
    disturbance_type      = "Disturbance type",
    taxon                 = "Taxon",
    habitat_type          = "Habitat type",
    observation_duration  = "Observation duration (log)"
  )
)

save(plot_data, file = "data/H.CIF.Rdata")
cat("✓ All model results saved to data/H.CIF.Rdata\n")
# ===== 13. Visualization =====
load("data/H.CIF.Rdata")  # load saved results before plotting

# Unpack
final_results      <- plot_data$final_results
analysis_data      <- plot_data$analysis_data
top3_predictors    <- plot_data$top3_predictors
heatmap_list       <- plot_data$heatmap_list
pretty_names       <- plot_data$pretty_names

# --- Variable importance plot ---
plot_df <- final_results$importance %>%
  arrange(median_drop) %>%
  mutate(predictor = factor(predictor, levels = predictor))

plot_df$label <- recode(as.character(plot_df$predictor), !!!pretty_names)
plot_df$label <- factor(plot_df$label, levels = plot_df$label)

p_importance <- ggplot(plot_df, aes(x = label, y = median_drop)) +
  geom_col(fill = "#3F4788", alpha = 1.5) +
  geom_errorbar(aes(ymin = p05, ymax = p95), width = 0.25, linewidth = 0.5) +
  geom_text(aes(label = sprintf("Top-3: %d%%\nP(Δ>0): %d%%",
                                round(top3_freq*100), round(pos_frac*100))),
            hjust = -0.05, size = 2.5, color = "gray30") +
  coord_flip(ylim = c(min(plot_df$p05)*0.9, max(plot_df$p95)*1.2)) +
  labs(
    title = "Variable Importance (Permutation Drop in Macro-F1)",
    x = NULL,
    y = "Δ Macro-F1 (median with 5%-95% CI)"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

print(p_importance)
ggsave("figure/Fig_CIF_importance.png", p_importance, width = 7, height = 5, dpi = 300)
ggsave("figure/Fig_CIF_importance.pdf", p_importance, width = 7, height = 5)

# --- CV score distribution ---
p_cv <- ggplot(data.frame(f1 = final_results$cv_scores_vec), aes(x = f1)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = final_results$cv_scores["mean"], 
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = final_results$cv_scores["mean"], 
           y = Inf, vjust = 1.5, 
           label = sprintf("Mean = %.3f", final_results$cv_scores["mean"]),
           color = "red", size = 3.5) +
  labs(
    title = "Distribution of CV Macro-F1 Scores",
    x = "Macro-F1", 
    y = "Frequency"
  ) +
  theme_minimal(base_size = 11)

print(p_cv)
ggsave("figure/Fig_CIF_cv_distribution.png", p_cv, width = 6, height = 4, dpi = 300)

# --- Confusion matrix heatmap ---
cm_df <- as.data.frame(final_results$confusion_matrix)
names(cm_df) <- c("Observed", "Predicted", "Freq")
cm_df$Observed  <- gsub("multidimensional quantification", 
                        "multidimensional\nquantification", cm_df$Observed)
cm_df$Predicted <- gsub("multidimensional quantification", 
                        "multidimensional\nquantification", cm_df$Predicted)
p_cm <- ggplot(cm_df, aes(x = Predicted, y = Observed, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#440154", high = "#FDE724", name = "Count") +
  labs(title = "Confusion Matrix (Pooled OOF Predictions)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_cm)
ggsave("figure/Fig_CIF_confusion_matrix.png", p_cm, width = 6, height = 5, dpi = 300)

cat("\n✓ All figures saved to figure/ directory\n")


# ===== 14. Category-to-Metric Association Analysis =====
cat("\n=== ANALYZING CATEGORY-TO-METRIC ASSOCIATIONS ===\n")

# Function to calculate association between predictor categories and outcome
analyze_category_metric_association <- function(data, predictor_var, outcome_var = "quantification") {
  # Create contingency table
  cont_table <- table(data[[predictor_var]], data[[outcome_var]])
  
  # Calculate proportions (row percentages)
  row_props <- prop.table(cont_table, margin = 1) * 100
  
  # Convert to data frame
  assoc_df <- as.data.frame(cont_table)
  colnames(assoc_df) <- c("Category", "Metric", "Count")
  
  # Add row percentages
  assoc_df <- assoc_df %>%
    group_by(Category) %>%
    mutate(
      Percentage = Count / sum(Count) * 100,
      Total_in_Category = sum(Count)
    ) %>%
    ungroup() %>%
    arrange(Category, desc(Percentage))
  
  # Add predictor variable name
  assoc_df$Predictor <- predictor_var
  
  # Identify dominant metric for each category (highest percentage)
  dominant <- assoc_df %>%
    group_by(Category) %>%
    slice_max(Percentage, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr::select(Predictor, Category, Dominant_Metric = Metric, 
                  Dominant_Percentage = Percentage, Total_in_Category)
  
  list(
    full_table = assoc_df,
    dominant_metric = dominant,
    contingency_table = cont_table,
    row_proportions = row_props
  )
}

# Analyze all categorical predictors
all_associations <- list()
dominant_summaries <- list()

for (pred_var in predictors_cat) {
  cat(sprintf("\nAnalyzing: %s\n", pred_var))
  
  assoc <- analyze_category_metric_association(analysis_data, pred_var)
  
  all_associations[[pred_var]] <- assoc$full_table
  dominant_summaries[[pred_var]] <- assoc$dominant_metric
  
  # Print dominant metric for each category
  cat("\nDominant metric for each category:\n")
  print(assoc$dominant_metric, n = Inf)
}

# Combine all full tables
full_association_table <- bind_rows(all_associations)

# Combine all dominant summaries
dominant_summary_table <- bind_rows(dominant_summaries)

# Export tables
write.csv(full_association_table, 
          "table/Table_CIF_category_metric_associations_full.csv", 
          row.names = FALSE)

write.csv(dominant_summary_table, 
          "table/Table_CIF_category_metric_dominant.csv", 
          row.names = FALSE)

cat("\n✓ Category-to-metric association tables exported\n")

# --- Create a summary heatmap for key predictors ---
# Focus on top 3 most important predictors

top3_predictors <- final_results$importance %>%
  arrange(desc(median_drop)) %>%
  head(3) %>%
  pull(predictor)

cat("\n=== Creating association heatmaps for top 3 predictors ===\n")

for (pred_var in top3_predictors) {
  # Get contingency table
  cont_table <- table(analysis_data[[pred_var]], analysis_data$quantification)
  row_props <- prop.table(cont_table, margin = 1) * 100
  
  # Convert to data frame for ggplot
  heatmap_df <- as.data.frame(row_props)
  colnames(heatmap_df) <- c("Category", "Metric", "Percentage")
  
  # Create heatmap
  p_heatmap <- ggplot(heatmap_df, aes(x = Metric, y = Category, fill = Percentage)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
              color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient2(
      low = "#440154", 
      mid = "#21908C", 
      high = "#FDE724", 
      midpoint = 25,  # 25% is the baseline (random chance for 4 classes)
      name = "Percentage"
    ) +
    labs(
      title = sprintf("Association: %s → Resilience Metric", 
                      pretty_names[pred_var]),
      x = "Resilience Metric",
      y = gsub("_", " ", tools::toTitleCase(pred_var))
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  # Save
  filename <- sprintf("figure/Fig_CIF_association_%s.png", pred_var)
  ggsave(filename, p_heatmap, width = 7, height = 5, dpi = 300)
  
  cat(sprintf("✓ Saved: %s\n", filename))
}

cat("\n✓ Association heatmaps created for top predictors\n")

# ===== 14.5 Combined 4-panel figure (importance + top-3 heatmaps) =====
library(patchwork)

# Helper: capitalize first letter of each word
cap_words <- function(x) gsub("(^|\\s)(\\w)", "\\1\\U\\2", x, perl = TRUE)

# Helper: replace spaces with newlines for y-axis labels
space_to_newline <- function(x) gsub(" ", "\n", x)

clean_category <- function(x) {
  x <- cap_words(as.character(x))
  # handle Model/indoor FIRST before any space replacement
  x <- gsub("Model/indoor", "Model&\nIndoor\nExperiment", x, fixed = TRUE)
  # then replace remaining spaces with newlines
  x <- gsub(" ", "\n", x)
  x
}

# Rebuild importance plot
plot_df <- plot_data$imp_plot_df
plot_df$label <- dplyr::recode(as.character(plot_df$predictor), !!!pretty_names)
plot_df$label <- gsub("Target variable", "Measured variable", plot_df$label)
plot_df$label <- space_to_newline(plot_df$label)
plot_df$label <- factor(plot_df$label, levels = plot_df$label)

p_imp <- ggplot(plot_df, aes(x = label, y = median_drop)) +
  geom_col(fill = "#3F4788") +
  geom_errorbar(aes(ymin = p05, ymax = p95), width = 0.25, linewidth = 0.6) +
  geom_text(aes(label = sprintf("Top-3: %d%%\nP(Δ>0): %d%%",
                                round(top3_freq*100), round(pos_frac*100))),
            hjust = -0.05, size = 3, color = "gray30") +
  coord_flip(ylim = c(min(plot_df$p05)*0.9, max(plot_df$p95)*1)) +
  labs(title = "Variable Importance\n(Permutation Drop in Macro-F1)",
       x = NULL, y = "Δ Macro-F1 (median with 5%–95% CI)") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_text(hjust = 0.5, size = 15),
        axis.text         = element_text(size = 13),
        axis.title        = element_text(size = 13))

p_imp <- p_imp + theme(plot.tag = element_text(size = 16, face = "bold"))

# Shared fill scale
shared_fill <- scale_fill_gradient2(
  low = "#440154", mid = "#21908C", high = "#FDE724",
  midpoint = 25, name = "Percentage",
  limits = c(0, 50)
)

build_hm <- function(pred_var, show_x = FALSE, show_legend = FALSE) {
  hm_df <- heatmap_list[[pred_var]]
  
  # Clean labels
  hm_df$Category <- clean_category(hm_df$Category)
  hm_df$Metric   <- cap_words(as.character(hm_df$Metric))
  
  # Fix title
  title_label <- pretty_names[pred_var]
  title_label <- gsub("Target variable", "Measured variable", title_label)
  title_label <- sprintf("Association: %s → Resilience Metric", title_label)
  
  # Y-axis label
  y_label <- cap_words(gsub("_", " ", pred_var))
  y_label <- gsub("Target Variable Group", "Measured Variable", y_label)
  y_label <- space_to_newline(y_label)
  
  p <- ggplot(hm_df, aes(x = Metric, y = Category, fill = Percentage)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)),
              color = "white", size = 3.2, fontface = "bold") +
    shared_fill +
    labs(title = title_label,
         x = if (show_x) "Resilience Metric" else NULL,
         y = y_label) +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.x     = if (show_x) element_text(angle = 25, hjust = 1, size = 12) 
      else element_blank(),
      axis.text.y     = element_text(size = 12, lineheight = 0.85),
      axis.ticks.x    = if (show_x) element_line() else element_blank(),
      axis.title      = element_text(size = 13),
      panel.grid      = element_blank(),
      plot.title      = element_text(hjust = 0.5, size = 13),
      legend.position = if (show_legend) "right" else "none",
      legend.text     = element_text(size = 12),
      legend.title    = element_text(size = 13)
    )
  p
}

# Middle heatmap gets the legend
p_hm1 <- build_hm(top3_predictors[1], show_x = FALSE, show_legend = FALSE)
p_hm2 <- build_hm(top3_predictors[2], show_x = FALSE, show_legend = TRUE)
p_hm3 <- build_hm(top3_predictors[3], show_x = TRUE,  show_legend = FALSE)
p_hm1 <- p_hm1 + theme(plot.tag = element_text(size = 16, face = "bold"))
p_hm2 <- p_hm2 + theme(plot.tag = element_text(size = 16, face = "bold"))
p_hm3 <- p_hm3 + theme(plot.tag = element_text(size = 16, face = "bold"))

p_combined <- p_imp + (p_hm1 / p_hm2 / p_hm3) +
  plot_layout(widths = c(2, 1.3)) +
  plot_annotation(
    title = "Conditional Inference Forest: Variable Importance and Category Associations",
    tag_levels = "a",
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.tag   = element_text(size = 16, face = "bold")
    )
  )

ggsave("figure/Fig_CIF_combined.png", p_combined, width = 18, height = 10, dpi = 600)
cat("✓ Combined 4-panel figure saved\n")

# ===== 15. Summary for manuscript =====
cat("\n\n" %+% paste(rep("=", 60), collapse="") %+% "\n")
cat("MANUSCRIPT SUMMARY\n")
cat(paste(rep("=", 60), collapse="") %+% "\n")

cat(sprintf(
  "Sample size: %d observations across %d outcome classes (%s)\n",
  nrow(analysis_data),
  nlevels(analysis_data$quantification),
  paste(table(analysis_data$quantification), collapse = ", ")
))

cat(sprintf(
  "Predictors: %d (%d categorical, %d numeric)\n",
  length(predictor_names),
  length(predictors_cat),
  length(predictors_num)
))

cat(sprintf("\nModel: Conditional Inference Forest (ntree=3500, mtry=%d)\n", 3))

cat(sprintf(
  "\nPerformance: Mean macro-F1 = %.3f (5%%–95%% CI: %.3f–%.3f)\n",
  final_results$cv_scores["mean"],
  final_results$cv_scores["p05"],
  final_results$cv_scores["p95"]
))

cat(sprintf(
  "             Pooled macro-F1 = %.3f, Accuracy = %.3f\n",
  final_results$pooled_metrics["macroF1"],
  final_results$pooled_metrics["accuracy"]
))

cat("\nTop 3 most important predictors:\n")
print(head(final_results$importance %>% dplyr::select(predictor, median_drop, pos_frac), 3))

cat("\n" %+% paste(rep("=", 60), collapse="") %+% "\n\n")
