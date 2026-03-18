# Module 2f Post processing and calculating F1 for llm data collection
rm(list=ls())
library(here)
library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

#training
key <- read.csv(here("data", "F1.75.paper.sample.for.training.csv"))
df18 <- read.csv(here("data", "G1.llm.output", "G1_pdf_analysis_results_20260202_202338.csv"))
df19 <- read.csv(here("data", "G1.llm.output", "G1_pdf_analysis_results_20260203_152246.csv"))
df20 <- read.csv(here("data", "G1.llm.output", "G1_pdf_analysis_results_20260203_171737.csv"))
df21 <- read.csv(here("data", "G1.llm.output", "G1_pdf_analysis_results_20260203_191319.csv"))
df_names <- c("df18", "df19", "df20", "df21")
#validation
df20v <- read.csv(here("data", "G2.llm.output", "G2_pdf_analysis_results_20260203_193840.csv")) 
df21v <- read.csv(here("data", "G2.llm.output", "G2_pdf_analysis_results_20260203_185811.csv"))
key <- read.csv(here("data", "F2.75.paper.sample.for.validation.csv")) #unlock the # when validating
df_names <- c("df20v", "df21v") #unlock the # when validating

############################ Unify the categories name ############################
# Step 1: Convert all "Not.applicable" to NA
key <- key %>%
  mutate(across(everything(), ~ ifelse(tolower(.x) == "not.applicable", NA_character_, .x)))

# Step 2: Adjust specific values in each column
replace_with_other <- function(text_str) {
  if (is.na(text_str)) return(NA_character_)
  # Replace all occurrences of 'tolerance' with 'latitude' (substring-level replacement)
  text_str <- gsub("tolerance", "latitude", text_str)
  allowed_terms <- c(
    "recovery degree", "recovery rate", "recovery time",
    "latitude", "invariability", "inferred", "resistance"
  )
  # Split by semicolon and trim whitespace
  terms <- trimws(unlist(strsplit(text_str, ";")))
  # Replace terms not in allowed_terms with "other"
  terms <- ifelse(terms %in% allowed_terms, terms, "other")
  # Remove duplicates and ensure only one "other"
  terms <- unique(terms)
  # Join back with semicolon
  paste(terms, collapse = "; ")
}

# Step 3: Deal with specific columns
clean_key_data <- function(df) {
  df <- df %>%
    mutate(
      # Habitat type: remove Multi_ prefix and content after semicolon
      habitat_type = if("habitat_type" %in% names(df)) {
        habitat_type %>%
          str_replace_all("Multi_", "") %>%
          str_replace_all(";.*", "")
      } else {
        habitat_type
      },
      
      # framework: change NA to "none"
      framework = framework %>%
        str_replace_all("^(Ecosystem\\.health|Vulnerability|Adaptability)$", "Large Scale framework") %>%
        str_replace_all(".*Resilience=.*", "Resilience framework") %>%
        ifelse(is.na(.) | . == "NA", "none", .),
      
      # Quantification: multiple changes with string replacement
      quantification = quantification %>%
        str_replace_all("_.*", "") %>%
        str_replace_all("\\.", " ") %>%
        str_replace_all("\\bnone\\b", "inferred") %>%
        sapply(replace_with_other),  # Add the new operation here
      
      # Target variable: change "." to ""
      target_variable_type = target_variable_type %>%
        str_replace_all("\\.", " ") %>%
        str_replace_all("\\bfunction\\b", "ecosystem function") %>%
        str_replace_all("\\blanduse\\b", "land use"),
      
      target_variable_group = target_variable_group %>%
        str_replace_all("\\.", " "),
      
      # Location range: change "<2" to "2"
      location_range = ifelse(location_range == "<2", "2", location_range)
    )
  
  return(df)
}
# Usage
key <- clean_key_data(key)
#key$Taxon[key$Taxon == "NA"] <- NA
key <- key %>%
  mutate(across(where(is.character), tolower))

################################# Post processing ################################# 
# First custom function: Data processing
# Before any further processing, standardize all column names in key and all dfs:
standardize_names <- function(x) {
  names(x) <- gsub("\\.", "_", names(x))
  names(x) <- gsub(" ", "_", names(x))
  names(x) <- tolower(names(x))
  x
}

for(n in df_names) assign(n, standardize_names(get(n)), envir = .GlobalEnv)

#Split disturbance type
split_disturbance <- function(df) {
  if ("disturbance_type" %in% names(df)) {
    # Split multiple values by semicolon and process each row
    df$disturbance_manipulation <- sapply(df$disturbance_type, function(x) {
      if (is.na(x)) return(NA_character_)
      
      # Split by semicolon
      values <- trimws(unlist(strsplit(x, ";")))
      
      # Extract x part (before underscore) from each value
      x_parts <- sapply(values, function(val) {
        if (grepl("_", val)) {
          return(strsplit(val, "_")[[1]][1])
        } else {
          return(val)  # if no underscore, return the whole value
        }
      })
      
      # Remove duplicates and combine with semicolon
      x_parts <- unique(x_parts[!is.na(x_parts) & x_parts != ""])
      if (length(x_parts) == 0) return(NA_character_)
      paste(x_parts, collapse = "; ")
    })
    
    # Update original column to keep only y part (after underscore)
    df$disturbance_type <- sapply(df$disturbance_type, function(x) {
      if (is.na(x)) return(NA_character_)
      
      # Split by semicolon
      values <- trimws(unlist(strsplit(x, ";")))
      
      # Extract y part (after underscore) from each value
      y_parts <- sapply(values, function(val) {
        if (grepl("_", val)) {
          parts <- strsplit(val, "_")[[1]]
          if (length(parts) > 1) {
            return(parts[2])
          } else {
            return(NA_character_)
          }
        } else {
          return(NA_character_)  # if no underscore, return NA
        }
      })
      
      # Remove duplicates and NAs, combine with semicolon
      y_parts <- unique(y_parts[!is.na(y_parts) & y_parts != ""])
      if (length(y_parts) == 0) return(NA_character_)
      paste(y_parts, collapse = "; ")
    })
  }
  
  return(df)
}

# ---- Data processing ----
process_data <- function(df) {
  # 1. rename paper_id to id
  if ("paper_id" %in% names(df)) {
    names(df)[names(df) == "paper_id"] <- "id"
  }
  
  # 2. Deal with Field country
  if ("field_country" %in% names(df)) {
    df$field_country[df$field_country == "NAM"] <- NA_character_
    if ("location" %in% names(df)) {
      na_rows <- is.na(df$field_country) & !is.na(df$location)
      df$field_country[na_rows] <- "na"
    }
  }
  
  # 3. lower case、delete [] and''，, to ;
  char_cols <- sapply(df, is.character)
  for (col in names(df)[char_cols]) {
    df[[col]] <- tolower(df[[col]])
    df[[col]] <- gsub("\\[|\\]|'|\"", "", df[[col]])
    if (col != "location") {
      df[[col]] <- gsub(",", ";", df[[col]])
    }
  }
  
  # 4. framework standardize
  if ("framework" %in% names(df)) {
    df$framework <- ifelse((grepl("resilience =", df$framework) | grepl("resilience indices =", df$framework)) & grepl("+", df$framework) & (grepl("resist", df$framework) & grepl("recov", df$framework)),
                           "resilience framework",
                           ifelse(grepl("ecosystem health", df$framework) | grepl("vulnerability", df$framework) | grepl("adaptability", df$framework) | grepl("eh", df$framework),
                           "large scale framework",
                           ifelse(grepl("stability =", df$framework),
                                  "stability=resilience+resistance(+others)",
                                         ifelse(df$framework == "none",
                                                df$framework,
                                                ifelse(grepl("resilience", df$framework) & grepl("+", df$framework) & grepl("resist", df$framework) & !grepl("=", df$framework),
                                                       "others/null = resilience+resistance",
                                                              "none")))))
  }
  
  # 5. Quantification column standardize using replace_with_other function
  replace_with_other <- function(text_str) {
    if (is.na(text_str)) return(NA_character_)
    allowed_terms <- c("recovery rate", "recovery time", "recovery degree", "latitude", "invariability", "inferred", "resistance")
    # Split by semicolon and trim whitespace
    terms <- trimws(unlist(strsplit(text_str, ";")))
    # Replace terms not in allowed_terms with "other"
    terms <- ifelse(terms %in% allowed_terms, terms, "other")
    # Remove duplicates and ensure only one "other"
    terms <- unique(terms)
    # Join back with semicolon
    paste(terms, collapse="; ")
  }
  
  if ("quantification" %in% names(df)) {
    df$quantification <- sapply(df$quantification, replace_with_other)
  }
  
  # 6. Habitat type - remove 'Multi_' prefix
  if ("habitat_type" %in% names(df)) {
    # Set the allowed type
    allowed_habitats <- c(
      "forest", "grassland", "wetland", "marine.neritic", "savanna", "global", "coastal",
      "urban", "agricultural", "desert", "shrubland", "oceanic", "multi","other", NA)
    # Convert to lowercase
    df$habitat_type <- tolower(df$habitat_type)
    
    # Remove "multi_" prefix if exists
    df$habitat_type <- gsub("^multi_", "", df$habitat_type)
    
    # Remove underscore and everything after it
    df$habitat_type <- gsub(";.*$", "", df$habitat_type)
    
    # Change types not in allowed_habitats to 'other'
    df$habitat_type <- ifelse(df$habitat_type %in% allowed_habitats, 
                              df$habitat_type, "other")
    df$habitat_type <- gsub("multi_", "", df$habitat_type)
  }
  
  # 7. Target variable type and group standardize
  allowed_types <- c(
    "cover", "density", "abundance", "biomass", "frequency",
    "composition", "diversity","demography parameter", "interaction",
    "ecosystem function", "physiological indicator", "growth","abiotic parameter", 
    "land use", "regional network parameter", "socio-eco parameter", "other")
  
  type2group <- list(
    quantity = c("cover", "density", "abundance", "biomass", "frequency"),
    structure = c("composition", "diversity"),
    `process-based indicator` = c("demography parameter", "interaction"),
    `functional response` = c("ecosystem function", "physiological indicator", "growth"),
    `environmental context` = c("abiotic parameter", "land use", "regional network parameter", "socio-eco parameter"),
    other = c("other"))
  
  clean_type <- function(type_str) {
    if (is.na(type_str)) return(NA_character_)
    # unify ,/;
    type_str <- gsub(",", ";", tolower(type_str))
    # split
    vars <- trimws(unlist(strsplit(type_str, ";")))
    # change types not in allowed_types to 'other'
    vars <- ifelse(vars %in% allowed_types, vars, "")
    # remove empty strings if any
    vars <- vars[vars != ""]
    if (length(vars) == 0) return(NA_character_)
    vars <- unique(vars)
    paste(vars, collapse="; ")
  }
  
  type_to_group <- function(type_str) {
    if (is.na(type_str)) return(NA_character_)
    vars <- trimws(unlist(strsplit(type_str, ";")))
    groups <- c()
    for (group in names(type2group)) {
      if (any(vars %in% type2group[[group]])) groups <- c(groups, group)
    }
    groups <- unique(groups)
    if (length(groups) == 0) return(NA_character_)
    paste(groups, collapse="; ")
  }
  
  if ("target_variable_type" %in% names(df)) {
    df$target_variable_type <- sapply(df$target_variable_type, clean_type)
    df$target_variable_group <- sapply(df$target_variable_type, type_to_group)
  }
  
  # Fix the target variable group column name issue
  if ("target_variable_group" %in% names(df)) {
    df$target_variable_group <- gsub("-", " ", df$target_variable_group)
  }
  
  # Disturbance type separation
  if ("disturbance_type" %in% names(df)) {
    df <- split_disturbance(df)
  }
  
  return(df)
}
process_all_data <- function(df_names) {
  processed_count <- 0
  for (df_name in df_names) {
    if (exists(df_name)) {
      original_df <- get(df_name)
      processed_df <- process_data(original_df)
      assign(df_name, processed_df, envir = .GlobalEnv)
      processed_count <- processed_count + 1
      cat(paste("✓ Completed processing:", df_name, "\n"))
    } 
  }
}

# ---- Calculate accuracy ----
calculate_f1 <- function(pred, actual, sep = ";") {
  if (length(pred) != length(actual)) return(NA)
  pred_list <- strsplit(as.character(pred), sep)
  actual_list <- strsplit(as.character(actual), sep)
  scores <- numeric(length(pred_list))
  for (i in seq_along(pred_list)) {
    p <- trimws(pred_list[[i]])
    a <- trimws(actual_list[[i]])
    if (length(p) == 1 && is.na(p) && length(a) == 1 && is.na(a)) {
      scores[i] <- 1
      next
    }
    if ((length(p) == 1 && is.na(p)) || (length(a) == 1 && is.na(a))) {
      scores[i] <- 0
      next
    }
    TP <- sum(p %in% a)
    FP <- sum(!(p %in% a))
    FN <- sum(!(a %in% p))
    precision <- ifelse((TP+FP)==0, 0, TP/(TP+FP))
    recall <- ifelse((TP+FN)==0, 0, TP/(TP+FN))
    scores[i] <- ifelse((precision+recall)==0, 0, 2*precision*recall/(precision+recall))
  }
  mean(scores, na.rm=TRUE)
}

ordinal_f1 <- function(pred, actual, categories, sep=";") {
  pred_list <- strsplit(as.character(pred), sep)
  actual_list <- strsplit(as.character(actual), sep)
  scores <- numeric(length(pred_list))
  for (i in seq_along(pred_list)) {
    pred_vals <- trimws(pred_list[[i]])
    actual_vals <- trimws(actual_list[[i]])
    if (length(pred_vals)==1 && is.na(pred_vals) && length(actual_vals)==1 && is.na(actual_vals)) {
      scores[i] <- 1
      next
    }
    if ((length(pred_vals)==1 && is.na(pred_vals)) || (length(actual_vals)==1 && is.na(actual_vals))) {
      scores[i] <- 0
      next
    }
    pair_score <- outer(pred_vals, actual_vals, Vectorize(function(p,a){
      pred_idx <- match(p, categories)
      act_idx <- match(a, categories)
      if (is.na(pred_idx) || is.na(act_idx)) return(0)
      if (pred_idx == act_idx) return(1)
      if (abs(pred_idx - act_idx)==1) return(0.5)
      return(0)
    }))
    max_pred_score <- apply(pair_score, 1, max, na.rm=TRUE)
    max_actual_score <- apply(pair_score, 2, max, na.rm=TRUE)
    tp <- sum(max_pred_score)
    fp <- length(pred_vals) - tp
    fn <- length(actual_vals) - sum(max_actual_score)
    precision <- ifelse(length(pred_vals)==0, 0, tp / (tp+fp))
    recall <- ifelse(length(actual_vals)==0, 0, tp / (tp+fn))
    scores[i] <- ifelse(precision+recall==0, 0, 2 * precision * recall / (precision+recall))
  }
  mean(scores, na.rm=TRUE)
}

calculate_Location_accuracy <- function(pred, actual) {
  # Helper function to process coordinate strings
  process_coordinates <- function(coord_str) {
    # Return NA if input is NA
    if (is.na(coord_str)) return(NA)
    
    # Extract all numbers (including decimals) from the string
    numbers <- str_extract_all(coord_str, "\\d+\\.?\\d*")[[1]]
    
    # Return NA if no numbers found
    if (length(numbers) == 0) return(NA)
    
    # Convert to numeric and take absolute values
    numbers <- abs(as.numeric(numbers))
    
    # If we have multiple coordinate pairs (separated by ';'), process each pair
    coord_pairs <- str_split(coord_str, ";\\s*")[[1]]
    
    all_coords <- list()
    
    for (pair in coord_pairs) {
      # Extract numbers and directions for this pair
      pair_numbers <- abs(as.numeric(str_extract_all(pair, "\\d+\\.?\\d*")[[1]]))
      pair_directions <- str_extract_all(tolower(pair), "[nsew]")[[1]]
      
      # Skip if we don't have exactly 2 numbers
      if (length(pair_numbers) != 2) next
      
      # Process directions: n/e stay positive, s/w become negative
      if (length(pair_directions) >= 1) {
        if (pair_directions[1] %in% c("s", "w")) {
          pair_numbers[1] <- -pair_numbers[1]
        }
      }
      if (length(pair_directions) >= 2) {
        if (pair_directions[2] %in% c("s", "w")) {
          pair_numbers[2] <- -pair_numbers[2]
        }
      }
      
      all_coords[[length(all_coords) + 1]] <- pair_numbers
    }
    
    # If no valid coordinate pairs found, return NA
    if (length(all_coords) == 0) return(NA)
    
    # If multiple coordinate pairs, take the average
    if (length(all_coords) > 1) {
      coord_matrix <- do.call(rbind, all_coords)
      final_coords <- colMeans(coord_matrix)
    } else {
      final_coords <- all_coords[[1]]
    }
    
    return(final_coords)
  }
  
  # Process all predictions and actual values
  scores <- numeric(length(pred))
  
  for (i in seq_along(pred)) {
    pred_coords <- process_coordinates(pred[i])
    actual_coords <- process_coordinates(actual[i])
    
    # If both are NA, score is 1
    if (is.na(pred_coords[1]) && is.na(actual_coords[1])) {
      scores[i] <- 1
    }
    # If only one is NA, score is 0
    else if (is.na(pred_coords[1]) || is.na(actual_coords[1])) {
      scores[i] <- 0
    }
    # If both have valid coordinates, check if differences are within latitude
    else {
      diff1 <- abs(pred_coords[1] - actual_coords[1])
      diff2 <- abs(pred_coords[2] - actual_coords[2])
      scores[i] <- ifelse(diff1 <= 5 && diff2 <= 5, 1, 0)
    }
  }
  
  # Return average accuracy
  return(mean(scores))
}

calculate_Location_range_accuracy <- function(pred, actual) {
  if (length(pred) != length(actual)) return(NA)
  scores <- numeric(length(pred))
  for (i in seq_along(pred)) {
    if (is.na(pred[i]) && is.na(actual[i])) {
      scores[i] <- 1
    } else if (is.na(pred[i]) || is.na(actual[i])) {
      scores[i] <- 0
    } else {
      pred_num <- suppressWarnings(as.numeric(pred[i]))
      actual_num <- suppressWarnings(as.numeric(actual[i]))
      if (is.na(pred_num) || is.na(actual_num)) {
        scores[i] <- 0
      } else {
        scores[i] <- ifelse(abs(pred_num - actual_num) <= 5, 1, 0)
      }
    }
  }
  return(mean(scores))
}

calculate_log_similarity <- function(pred, actual) {
  if (length(pred) != length(actual)) return(NA)
  scores <- numeric(length(pred))
  for (i in seq_along(pred)) {
    if (is.na(pred[i]) && is.na(actual[i])) {
      scores[i] <- 1
    } else if (is.na(pred[i]) || is.na(actual[i])) {
      scores[i] <- 0
    } else {
      a <- as.numeric(actual[i])   # key
      b <- as.numeric(pred[i])     # df
      if (is.na(a) || is.na(b) || a <= 0 || b <= 0) {
        scores[i] <- 0
      } else {
        scores[i] <- 1 - abs(log10(b/a))
        scores[i] <- max(0, scores[i])
      }
    }
  }
  return(mean(scores))
}

calculate_accuracy <- function(df, key, df_name) {
  # Standardize column names to lower case and underscore for robust matching
  names(df) <- gsub("\\.", "_", names(df))
  names(df) <- tolower(names(df))
  names(key) <- gsub("\\.", "_", names(key))
  names(key) <- tolower(names(key))
  merged <- merge(df, key, by = "id", suffixes = c("_df", "_key"))
  names(merged) <- gsub("\\.", "_", names(merged))
  names(merged) <- tolower(names(merged))
  if (nrow(merged) == 0) return(NA)
  results <- list()
  f1_columns <- c("measurement", "framework", "quantification", 
                  "target_variable_type", "target_variable_group", 
                  "institution_country", "field_country", "habitat_type", "taxon")
  for (col in f1_columns) {
    col_df <- paste0(col, "_df")
    col_key <- paste0(col, "_key")
    if (col_df %in% names(merged) && col_key %in% names(merged)) {
      results[[col]] <- calculate_f1(merged[[col_df]], merged[[col_key]])
    } else {
      results[[col]] <- NA
    }
  }
  if ("level_df" %in% names(merged) && "level_key" %in% names(merged)) {
    level_categories <- c("individual", "population", "community", "ecosystem", "landscape")
    results[["level"]] <- ordinal_f1(merged[["level_df"]], merged[["level_key"]], level_categories)
  } else {
    results[["level"]] <- NA
  }
  if ("approach_df" %in% names(merged) && "approach_key" %in% names(merged)) {
    approach_categories <- c("modeling and simulation", "indoor experiment", "field experiment", "field observation")
    results[["approach"]] <- ordinal_f1(merged[["approach_df"]], merged[["approach_key"]], approach_categories)
  } else {
    results[["approach"]] <- NA
  }
  if ("location_df" %in% names(merged) && "location_key" %in% names(merged)) {
    results[["location"]] <- calculate_Location_accuracy(merged[["location_df"]], merged[["location_key"]])
  } else {
    results[["location"]] <- NA
  }
  if ("location_range_df" %in% names(merged) && "location_range_key" %in% names(merged)) {
    results[["location_range"]] <- calculate_Location_range_accuracy(merged[["location_range_df"]], merged[["location_range_key"]])
  } else {
    results[["location_range"]] <- NA
  }
  if ("measurement_key" %in% names(merged)) {
    non_inferred <- merged[
      merged[["measurement_key"]] != "inferred" & merged[["measurement_df"]] != "inferred",
    ]
    if ("disturbance_manipulation_df" %in% names(non_inferred) && "disturbance_manipulation_key" %in% names(non_inferred)) {
      results[["disturbance_manipulation"]] <- calculate_f1(non_inferred[["disturbance_manipulation_df"]], non_inferred[["disturbance_manipulation_key"]])
    } else {
      results[["disturbance_manipulation"]] <- NA
    }
    if ("disturbance_type_df" %in% names(non_inferred) && "disturbance_type_key" %in% names(non_inferred)) {
      results[["disturbance_type"]] <- calculate_f1(non_inferred[["disturbance_type_df"]], non_inferred[["disturbance_type_key"]])
    } else {
      results[["disturbance_type"]] <- NA
    }
    if ("disturbance_pattern_df" %in% names(non_inferred) && "disturbance_pattern_key" %in% names(non_inferred)) {
      results[["disturbance_pattern"]] <- calculate_f1(
        non_inferred[["disturbance_pattern_df"]], 
        non_inferred[["disturbance_pattern_key"]]
      )
    } else {
      results[["disturbance_pattern"]] <- NA
    }
    if ("observation_duration_df" %in% names(non_inferred) && "observation_duration_key" %in% names(non_inferred)) {
      results[["observation_duration"]] <- calculate_log_similarity(non_inferred[["observation_duration_df"]], non_inferred[["observation_duration_key"]])
    } else {
      results[["observation_duration"]] <- NA
    }
  } else {
    results[["disturbance_manipulation"]] <- NA
    results[["disturbance_type"]] <- NA
    results[["disturbance_pattern"]] <- NA
    results[["observation_duration"]] <- NA
  }
  result_row <- data.frame(id = df_name, stringsAsFactors = FALSE)
  for (col in names(results)) {
    result_row[[col]] <- results[[col]]
  }
  return(result_row)
}

calculate_accuracy_for_all <- function(df_names, key) {
  accuracy_results <- list()
  calculated_count <- 0
  cat("Starting accuracy calculation...\n")
  for (df_name in df_names) {
    if (exists(df_name)) {
      cat(paste("Calculating:", df_name, "\n"))
      df <- get(df_name)
      accuracy_results[[df_name]] <- calculate_accuracy(df, key, df_name)
      calculated_count <- calculated_count + 1
      cat(paste("✓ Completed calculation:", df_name, "\n"))
    } else {
      cat(paste("⚠ Data frame does not exist:", df_name, "\n"))
    }
  }
  all_results <- do.call(rbind, accuracy_results)
  cat(paste("\nAccuracy calculation completed! Calculated", calculated_count, "data frames.\n"))
  return(all_results)
}

# Step 1: Process data
process_all_data(df_names)
# After checking the data, Step 2: Calculate accuracy
results <- calculate_accuracy_for_all(df_names, key)

# ---- Output ----
combined_df <- rbind(key, results) %>% select(-target_variable_type)
#write.csv(combined_df, file = here("data", "G1f.training.result.csv"), row.names = FALSE) # unlock the # when training
write.csv(combined_df, file = here("data", "G2f.validation.result.csv"), row.names = FALSE) # unlock the # when validating
