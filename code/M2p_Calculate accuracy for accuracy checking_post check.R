# Module 2p Post processing and calculating F1 for llm data collection (post check)
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")
library(readxl)
library(openxlsx)
library(dplyr)
library(stringr)

key <- read.csv("Fp.50.paper.for.post.check.csv")
dfG3 <- read.csv("G3_pdf_analysis_results_20260204_012133.csv")

############################ Unify the categories name ############################
key <- key %>%
  mutate(across(everything(), ~ ifelse(tolower(.x) == "not.applicable", NA_character_, .x)))

replace_with_other <- function(text_str) {
  if (is.na(text_str)) return(NA_character_)
  text_str <- gsub("tolerance", "latitude", text_str)
  allowed_terms <- c(
    "recovery degree", "recovery rate", "recovery time",
    "latitude", "invariability", "inferred", "resistance"
  )
  terms <- trimws(unlist(strsplit(text_str, ";")))
  terms <- ifelse(terms %in% allowed_terms, terms, "other")
  terms <- unique(terms)
  paste(terms, collapse = "; ")
}

clean_key_data <- function(df) {
  df <- df %>%
    mutate(
      habitat_type = if("habitat_type" %in% names(df)) {
        habitat_type %>%
          str_replace_all("Multi_", "") %>%
          str_replace_all(";.*", "")
      } else {
        habitat_type
      },
      framework = framework %>%
        str_replace_all("^(Ecosystem\\.health|Vulnerability|Adaptability)$", "Large Scale framework") %>%
        str_replace_all(".*Resilience=.*", "Resilience framework") %>%
        ifelse(is.na(.) | . == "NA", "none", .),
      quantification = quantification %>%
        str_replace_all("_.*", "") %>%
        str_replace_all("\\.", " ") %>%
        str_replace_all("\\bnone\\b", "inferred") %>%
        sapply(replace_with_other),
      target_variable_type = target_variable_type %>%
        str_replace_all("\\.", " ") %>%
        str_replace_all("\\bfunction\\b", "ecosystem function") %>%
        str_replace_all("\\blanduse\\b", "land use"),
      target_variable_group = target_variable_group %>%
        str_replace_all("\\.", " "),
      location_range = ifelse(location_range == "<2", "2", location_range)
    )
  return(df)
}

key <- clean_key_data(key)
key <- key %>%
  mutate(across(where(is.character), tolower))

################################# Post processing #################################
standardize_names <- function(x) {
  names(x) <- gsub("\\.", "_", names(x))
  names(x) <- gsub(" ", "_", names(x))
  names(x) <- tolower(names(x))
  x
}

key  <- standardize_names(key)
dfG3 <- standardize_names(dfG3)

# Rename paper_id to id in dfG3
if ("paper_id" %in% names(dfG3)) {
  names(dfG3)[names(dfG3) == "paper_id"] <- "id"
}

# Match dfG3 to key by id (keep only rows present in key)
dfG3 <- dfG3[dfG3$id %in% key$id, ]
cat(paste("Matched samples:", nrow(dfG3), "(expected 50)\n"))

split_disturbance <- function(df) {
  if ("disturbance_type" %in% names(df)) {
    df$disturbance_manipulation <- sapply(df$disturbance_type, function(x) {
      if (is.na(x)) return(NA_character_)
      values <- trimws(unlist(strsplit(x, ";")))
      x_parts <- sapply(values, function(val) {
        if (grepl("_", val)) strsplit(val, "_")[[1]][1] else val
      })
      x_parts <- unique(x_parts[!is.na(x_parts) & x_parts != ""])
      if (length(x_parts) == 0) return(NA_character_)
      paste(x_parts, collapse = "; ")
    })
    df$disturbance_type <- sapply(df$disturbance_type, function(x) {
      if (is.na(x)) return(NA_character_)
      values <- trimws(unlist(strsplit(x, ";")))
      y_parts <- sapply(values, function(val) {
        if (grepl("_", val)) {
          parts <- strsplit(val, "_")[[1]]
          if (length(parts) > 1) parts[2] else NA_character_
        } else {
          NA_character_
        }
      })
      y_parts <- unique(y_parts[!is.na(y_parts) & y_parts != ""])
      if (length(y_parts) == 0) return(NA_character_)
      paste(y_parts, collapse = "; ")
    })
  }
  return(df)
}

process_data <- function(df) {
  if ("paper_id" %in% names(df)) {
    names(df)[names(df) == "paper_id"] <- "id"
  }
  if ("field_country" %in% names(df)) {
    df$field_country[df$field_country == "NAM"] <- NA_character_
    if ("location" %in% names(df)) {
      na_rows <- is.na(df$field_country) & !is.na(df$location)
      df$field_country[na_rows] <- "na"
    }
  }
  char_cols <- sapply(df, is.character)
  for (col in names(df)[char_cols]) {
    df[[col]] <- tolower(df[[col]])
    df[[col]] <- gsub("\\[|\\]|'|\"", "", df[[col]])
    if (col != "location") {
      df[[col]] <- gsub(",", ";", df[[col]])
    }
  }
  if ("framework" %in% names(df)) {
    df$framework <- ifelse(
      (grepl("resilience =", df$framework) | grepl("resilience indices =", df$framework)) &
        grepl("+", df$framework) & (grepl("resist", df$framework) & grepl("recov", df$framework)),
      "resilience framework",
      ifelse(grepl("ecosystem health", df$framework) | grepl("vulnerability", df$framework) |
               grepl("adaptability", df$framework) | grepl("eh", df$framework),
             "large scale framework",
             ifelse(grepl("stability =", df$framework),
                    "stability=resilience+resistance(+others)",
                    ifelse(df$framework == "none",
                           df$framework,
                           ifelse(grepl("resilience", df$framework) & grepl("+", df$framework) &
                                    grepl("resist", df$framework) & !grepl("=", df$framework),
                                  "others/null = resilience+resistance",
                                  "none")))))
  }
  replace_with_other_local <- function(text_str) {
    if (is.na(text_str)) return(NA_character_)
    allowed_terms <- c("recovery rate", "recovery time", "recovery degree",
                       "latitude", "invariability", "inferred", "resistance")
    terms <- trimws(unlist(strsplit(text_str, ";")))
    terms <- ifelse(terms %in% allowed_terms, terms, "other")
    terms <- unique(terms)
    paste(terms, collapse = "; ")
  }
  if ("quantification" %in% names(df)) {
    df$quantification <- sapply(df$quantification, replace_with_other_local)
  }
  if ("habitat_type" %in% names(df)) {
    allowed_habitats <- c(
      "forest", "grassland", "wetland", "marine.neritic", "savanna", "global", "coastal",
      "urban", "agricultural", "desert", "shrubland", "oceanic", "multi", "other", NA)
    df$habitat_type <- tolower(df$habitat_type)
    df$habitat_type <- gsub("^multi_", "", df$habitat_type)
    df$habitat_type <- gsub(";.*$", "", df$habitat_type)
    df$habitat_type <- ifelse(df$habitat_type %in% allowed_habitats, df$habitat_type, "other")
    df$habitat_type <- gsub("multi_", "", df$habitat_type)
  }
  allowed_types <- c(
    "cover", "density", "abundance", "biomass", "frequency",
    "composition", "diversity", "demography parameter", "interaction",
    "ecosystem function", "physiological indicator", "growth", "abiotic parameter",
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
    type_str <- gsub(",", ";", tolower(type_str))
    vars <- trimws(unlist(strsplit(type_str, ";")))
    vars <- ifelse(vars %in% allowed_types, vars, "")
    vars <- vars[vars != ""]
    if (length(vars) == 0) return(NA_character_)
    paste(unique(vars), collapse = "; ")
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
    paste(groups, collapse = "; ")
  }
  if ("target_variable_type" %in% names(df)) {
    df$target_variable_type <- sapply(df$target_variable_type, clean_type)
    df$target_variable_group <- sapply(df$target_variable_type, type_to_group)
  }
  if ("target_variable_group" %in% names(df)) {
    df$target_variable_group <- gsub("-", " ", df$target_variable_group)
  }
  if ("disturbance_type" %in% names(df)) {
    df <- split_disturbance(df)
  }
  return(df)
}

dfG3 <- process_data(dfG3)
cat("✓ Completed processing: dfG3\n")

################################# F1 calculation #################################
calculate_f1 <- function(pred, actual, sep = ";") {
  if (length(pred) != length(actual)) return(NA)
  pred_list <- strsplit(as.character(pred), sep)
  actual_list <- strsplit(as.character(actual), sep)
  scores <- numeric(length(pred_list))
  for (i in seq_along(pred_list)) {
    p <- trimws(pred_list[[i]])
    a <- trimws(actual_list[[i]])
    if (length(p) == 1 && is.na(p) && length(a) == 1 && is.na(a)) { scores[i] <- 1; next }
    if ((length(p) == 1 && is.na(p)) || (length(a) == 1 && is.na(a))) { scores[i] <- 0; next }
    TP <- sum(p %in% a)
    FP <- sum(!(p %in% a))
    FN <- sum(!(a %in% p))
    precision <- ifelse((TP+FP)==0, 0, TP/(TP+FP))
    recall    <- ifelse((TP+FN)==0, 0, TP/(TP+FN))
    scores[i] <- ifelse((precision+recall)==0, 0, 2*precision*recall/(precision+recall))
  }
  mean(scores, na.rm = TRUE)
}

ordinal_f1 <- function(pred, actual, categories, sep = ";") {
  pred_list   <- strsplit(as.character(pred), sep)
  actual_list <- strsplit(as.character(actual), sep)
  scores <- numeric(length(pred_list))
  for (i in seq_along(pred_list)) {
    pred_vals   <- trimws(pred_list[[i]])
    actual_vals <- trimws(actual_list[[i]])
    if (length(pred_vals)==1 && is.na(pred_vals) && length(actual_vals)==1 && is.na(actual_vals)) { scores[i] <- 1; next }
    if ((length(pred_vals)==1 && is.na(pred_vals)) || (length(actual_vals)==1 && is.na(actual_vals))) { scores[i] <- 0; next }
    pair_score <- outer(pred_vals, actual_vals, Vectorize(function(p, a) {
      pi <- match(p, categories); ai <- match(a, categories)
      if (is.na(pi) || is.na(ai)) return(0)
      if (pi == ai) return(1)
      if (abs(pi - ai) == 1) return(0.5)
      return(0)
    }))
    max_pred_score   <- apply(pair_score, 1, max, na.rm = TRUE)
    max_actual_score <- apply(pair_score, 2, max, na.rm = TRUE)
    tp <- sum(max_pred_score)
    fp <- length(pred_vals) - tp
    fn <- length(actual_vals) - sum(max_actual_score)
    precision <- ifelse(length(pred_vals)==0,   0, tp/(tp+fp))
    recall    <- ifelse(length(actual_vals)==0, 0, tp/(tp+fn))
    scores[i] <- ifelse(precision+recall==0, 0, 2*precision*recall/(precision+recall))
  }
  mean(scores, na.rm = TRUE)
}

calculate_Location_accuracy <- function(pred, actual) {
  process_coordinates <- function(coord_str) {
    if (is.na(coord_str)) return(NA)
    coord_pairs <- str_split(coord_str, ";\\s*")[[1]]
    all_coords <- list()
    for (pair in coord_pairs) {
      pair_numbers   <- abs(as.numeric(str_extract_all(pair, "\\d+\\.?\\d*")[[1]]))
      pair_directions <- str_extract_all(tolower(pair), "[nsew]")[[1]]
      if (length(pair_numbers) != 2) next
      if (length(pair_directions) >= 1 && pair_directions[1] %in% c("s", "w")) pair_numbers[1] <- -pair_numbers[1]
      if (length(pair_directions) >= 2 && pair_directions[2] %in% c("s", "w")) pair_numbers[2] <- -pair_numbers[2]
      all_coords[[length(all_coords) + 1]] <- pair_numbers
    }
    if (length(all_coords) == 0) return(NA)
    if (length(all_coords) > 1) colMeans(do.call(rbind, all_coords)) else all_coords[[1]]
  }
  scores <- numeric(length(pred))
  for (i in seq_along(pred)) {
    pc <- process_coordinates(pred[i])
    ac <- process_coordinates(actual[i])
    if (is.na(pc[1]) && is.na(ac[1])) { scores[i] <- 1 }
    else if (is.na(pc[1]) || is.na(ac[1])) { scores[i] <- 0 }
    else { scores[i] <- ifelse(abs(pc[1]-ac[1]) <= 5 && abs(pc[2]-ac[2]) <= 5, 1, 0) }
  }
  mean(scores)
}

calculate_Location_range_accuracy <- function(pred, actual) {
  if (length(pred) != length(actual)) return(NA)
  scores <- numeric(length(pred))
  for (i in seq_along(pred)) {
    if (is.na(pred[i]) && is.na(actual[i])) { scores[i] <- 1 }
    else if (is.na(pred[i]) || is.na(actual[i])) { scores[i] <- 0 }
    else {
      pn <- suppressWarnings(as.numeric(pred[i]))
      an <- suppressWarnings(as.numeric(actual[i]))
      scores[i] <- ifelse(is.na(pn) || is.na(an), 0, ifelse(abs(pn-an) <= 5, 1, 0))
    }
  }
  mean(scores)
}

calculate_log_similarity <- function(pred, actual) {
  if (length(pred) != length(actual)) return(NA)
  scores <- numeric(length(pred))
  for (i in seq_along(pred)) {
    if (is.na(pred[i]) && is.na(actual[i])) { scores[i] <- 1 }
    else if (is.na(pred[i]) || is.na(actual[i])) { scores[i] <- 0 }
    else {
      a <- as.numeric(actual[i]); b <- as.numeric(pred[i])
      scores[i] <- if (is.na(a) || is.na(b) || a <= 0 || b <= 0) 0 else max(0, 1 - abs(log10(b/a)))
    }
  }
  mean(scores)
}

calculate_accuracy <- function(df, key, df_name) {
  names(df)  <- tolower(gsub("\\.", "_", names(df)))
  names(key) <- tolower(gsub("\\.", "_", names(key)))
  merged <- merge(df, key, by = "id", suffixes = c("_df", "_key"))
  names(merged) <- tolower(gsub("\\.", "_", names(merged)))
  if (nrow(merged) == 0) return(NA)
  
  results <- list()
  
  # Standard F1 columns
  f1_columns <- c("measurement", "framework", "quantification",
                  "target_variable_type", "target_variable_group",
                  "institution_country", "field_country", "habitat_type", "taxon")
  for (col in f1_columns) {
    col_df  <- paste0(col, "_df")
    col_key <- paste0(col, "_key")
    results[[col]] <- if (col_df %in% names(merged) && col_key %in% names(merged))
      calculate_f1(merged[[col_df]], merged[[col_key]]) else NA
  }
  
  # Ordinal F1
  if ("level_df" %in% names(merged) && "level_key" %in% names(merged)) {
    results[["level"]] <- ordinal_f1(merged[["level_df"]], merged[["level_key"]],
                                     c("individual", "population", "community", "ecosystem", "landscape"))
  } else { results[["level"]] <- NA }
  
  if ("approach_df" %in% names(merged) && "approach_key" %in% names(merged)) {
    results[["approach"]] <- ordinal_f1(merged[["approach_df"]], merged[["approach_key"]],
                                        c("modeling and simulation", "indoor experiment", "field experiment", "field observation"))
  } else { results[["approach"]] <- NA }
  
  # Location
  if ("location_df" %in% names(merged) && "location_key" %in% names(merged))
    results[["location"]] <- calculate_Location_accuracy(merged[["location_df"]], merged[["location_key"]])
  else results[["location"]] <- NA
  
  if ("location_range_df" %in% names(merged) && "location_range_key" %in% names(merged))
    results[["location_range"]] <- calculate_Location_range_accuracy(merged[["location_range_df"]], merged[["location_range_key"]])
  else results[["location_range"]] <- NA
  
  # Disturbance columns — only for non-inferred rows
  if ("measurement_key" %in% names(merged)) {
    non_inferred <- merged[
      merged[["measurement_key"]] != "inferred" & merged[["measurement_df"]] != "inferred", ]
    for (col in c("disturbance_manipulation", "disturbance_type", "disturbance_pattern")) {
      col_df  <- paste0(col, "_df")
      col_key <- paste0(col, "_key")
      results[[col]] <- if (col_df %in% names(non_inferred) && col_key %in% names(non_inferred))
        calculate_f1(non_inferred[[col_df]], non_inferred[[col_key]]) else NA
    }
    results[["observation_duration"]] <- if (
      "observation_duration_df" %in% names(non_inferred) && "observation_duration_key" %in% names(non_inferred))
      calculate_log_similarity(non_inferred[["observation_duration_df"]], non_inferred[["observation_duration_key"]])
    else NA
  } else {
    results[["disturbance_manipulation"]] <- NA
    results[["disturbance_type"]]         <- NA
    results[["disturbance_pattern"]]      <- NA
    results[["observation_duration"]]     <- NA
  }
  
  result_row <- data.frame(id = df_name, stringsAsFactors = FALSE)
  for (col in names(results)) result_row[[col]] <- results[[col]]
  return(result_row)
}

################################# Run & Output #################################
accuracy_result <- calculate_accuracy(dfG3, key, "dfG3")
cat("✓ Accuracy calculation completed\n")
print(accuracy_result)

combined_df <- rbind(key, accuracy_result) %>% select(-target_variable_type)
write.csv(combined_df, file = "Gp.post.check.result.csv", row.names = FALSE)
cat("✓ Output saved: Gp.post.check.result.csv\n")