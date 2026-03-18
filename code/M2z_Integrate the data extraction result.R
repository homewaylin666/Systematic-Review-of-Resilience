# Module 2z Integrate the data extraction results
rm(list=ls())
library(here)

# ---- Import dataframes ----
df.web.in <- read.csv(here("data", "Cc.abstract.screened.result_1227.csv")) #contents information from online database
df.train <- read.csv(here("data", "G1.llm.output", "G1_pdf_analysis_results_20260203_191319.csv"))
df.valid <- read.csv(here("data", "G2.llm.output", "G2_pdf_analysis_results_20260203_185811.csv"))
df.apply <- read.csv(here("data", "G3_pdf_analysis_results_20260204_012133.csv") )

# ---- Merge three dataframes ----
# Combine df.train, df.valid, and df.apply into df.data
df.data <- rbind(df.train, df.valid, df.apply)

# Sort df.data by paper_id in ascending order
df.data <- df.data[order(as.numeric(df.data$paper_id)), ]

# ---- Filter df.web.in to match df.data ----
# Keep only rows in df.web.in where Abstract_ID exists in df.data$paper_id
df.web.in <- df.web.in[df.web.in$Abstract_ID %in% df.data$paper_id, ]

# Check if row counts match
if (nrow(df.web.in) != nrow(df.data)) {
  stop("Error: Row count mismatch between df.web.in and df.data!")
}

# ---- Extract and merge metadata from df.web.in ----
# Extract Title, Year, and Journal columns from df.web.in
metadata <- df.web.in[, c("Abstract_ID", "Title", "Year", "Journal")]

# Convert Journal to uppercase
metadata$Journal <- toupper(metadata$Journal)

# Merge metadata into df.data by matching Abstract_ID with paper_id
df.data <- merge(df.data, metadata, 
                 by.x = "paper_id", by.y = "Abstract_ID", 
                 all.x = TRUE, sort = FALSE)

# Remove Justification column if it exists
if ("Justification" %in% names(df.data)) {
  df.data <- df.data[, !names(df.data) %in% "Justification"]
}

# Re-sort df.data by paper_id to maintain order
df.data <- df.data[order(as.numeric(df.data$paper_id)), ]

# ---- Create data processing functions ----
standardize_names <- function(x) {
  names(x) <- gsub("\\.", "_", names(x))
  names(x) <- gsub(" ", "_", names(x))
  names(x) <- tolower(names(x))
  x
}
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
# ---- Apply data processing functions ----
# Standardize column names
df.data <- standardize_names(df.data)

# Process all data cleaning and standardization
df.data$disturbance_both <- df.data$disturbance_type
df.ready <- process_data(df.data)

# ---- Output ----
row_count <- nrow(df.ready)
out_name <- paste0("Gz.full.data.sheet_", row_count, ".csv")
write.csv(df.ready, here("data", out_name), row.names = FALSE)
