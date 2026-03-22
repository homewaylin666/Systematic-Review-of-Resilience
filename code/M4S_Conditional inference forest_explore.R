# Module 4 Conditional Inference Forest: add two factors for exploration
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# ---- Import packages (load only one forest package to avoid conflicts) ----
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

# ---- Keep only direct-response studies ----
df <- df %>%
  filter(measurement == "observed")

# ---- Helper functions ----

# Count frequencies after splitting multi-value entries
table_split <- function(data, column, sep = ";", useNA = "ifany") {
  col_data <- data[[column]]
  split_data <- unlist(strsplit(col_data, sep))
  split_data <- trimws(split_data)
  table(split_data, useNA = useNA)
}

# Merge categories in multi-value columns
merge_categories <- function(data, column, merge_list, sep = ";") {
  data <- data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      if (is.na(x)) return(NA)
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      for (new_cat in names(merge_list)) {
        old_cats <- merge_list[[new_cat]]
        values[values %in% old_cats] <- new_cat
      }
      unique_values <- unique(values)
      if (length(unique_values) == 1) {
        return(unique_values)
      } else {
        return(paste(unique_values, collapse = paste0(sep, " ")))
      }
    }))
  return(data)
}

# Rename values in column Y based on condition in column X
rename_conditional <- function(data, check_column, check_value, target_column,
                               old_value, new_value, sep = ";") {
  data <- data %>%
    mutate(!!target_column := sapply(1:n(), function(i) {
      check_val  <- .data[[check_column]][i]
      target_val <- .data[[target_column]][i]
      if (is.na(target_val)) return(NA)
      check_values   <- trimws(unlist(strsplit(as.character(check_val), sep)))
      has_check_value <- check_value %in% check_values
      if (has_check_value) {
        target_values <- trimws(unlist(strsplit(as.character(target_val), sep)))
        target_values[target_values == old_value] <- new_value
        unique_values <- unique(target_values)
        if (length(unique_values) == 1) return(unique_values)
        else return(paste(unique_values, collapse = paste0(" ", sep, " ")))
      } else {
        return(target_val)
      }
    }))
  return(data)
}

# Remove a category only from multi-value entries (keep if sole value)
remove_from_multivalue <- function(data, column, category_to_remove, sep = ";") {
  data <- data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      if (is.na(x)) return(NA)
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      if (length(values) == 1) return(x)
      values <- values[values != category_to_remove]
      unique_values <- unique(values)
      if (length(unique_values) == 1) return(unique_values)
      else return(paste(unique_values, collapse = paste0(sep, " ")))
    }))
  return(data)
}

# Compare how two categories distribute across response variable
compare_category_distribution <- function(data, predictor, cat_a, cat_b, response = "quantification", sep = "; ") {
  expanded <- data %>%
    mutate(.pred_original = .data[[predictor]]) %>%
    separate_rows(!!sym(predictor), sep = sep) %>%
    mutate(!!sym(predictor) := trimws(.data[[predictor]]))
  data_a <- expanded[expanded[[predictor]] == cat_a, ]
  data_b <- expanded[expanded[[predictor]] == cat_b, ]
  cat(sprintf("\n=== Comparing '%s' vs '%s' in [%s] ===\n", cat_a, cat_b, predictor))
  cat(sprintf("  n(%s) = %d,  n(%s) = %d\n\n", cat_a, nrow(data_a), cat_b, nrow(data_b)))
  freq_a <- table(data_a[[response]])
  freq_b <- table(data_b[[response]])
  all_levels <- sort(unique(c(names(freq_a), names(freq_b))))
  freq_a <- freq_a[all_levels]; freq_a[is.na(freq_a)] <- 0; names(freq_a) <- all_levels
  freq_b <- freq_b[all_levels]; freq_b[is.na(freq_b)] <- 0; names(freq_b) <- all_levels
  prop_a <- round(freq_a / sum(freq_a) * 100, 1)
  prop_b <- round(freq_b / sum(freq_b) * 100, 1)
  comp <- data.frame(
    response_category = all_levels,
    n_a  = as.integer(freq_a), pct_a = as.numeric(prop_a),
    n_b  = as.integer(freq_b), pct_b = as.numeric(prop_b),
    pct_diff = as.numeric(prop_a - prop_b)
  )
  colnames(comp)[2:5] <- c(paste0("n_", cat_a), paste0("pct_", cat_a),
                           paste0("n_", cat_b), paste0("pct_", cat_b))
  print(comp, row.names = FALSE)
  cont <- rbind(freq_a, freq_b)
  rownames(cont) <- c(cat_a, cat_b)
  test <- fisher.test(cont, simulate.p.value = TRUE, B = 5000)
  cat(sprintf("\nFisher's exact test p-value: %.4f\n", test$p.value))
  if (test$p.value > 0.05) {
    cat("-> Distributions are NOT significantly different. Merging is reasonable.\n")
  } else {
    cat("-> Distributions ARE significantly different. Merging may lose information.\n")
  }
  invisible(comp)
}

# Keep only the highest-priority category from multi-value entries
keep_priority <- function(data, column, priority_list, sep = ";") {
  data <- data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      if (is.na(x)) return(NA)
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      if (length(values) == 1) return(x)
      matching_values <- values[values %in% priority_list]
      if (length(matching_values) == 0) return(x)
      priorities <- match(matching_values, priority_list)
      matching_values[which.min(priorities)]
    }))
  return(data)
}

# Create a summary table for one variable
create_var_summary <- function(data, var_name, var_type) {
  if (var_type == "categorical") {
    freq_table <- table(data[[var_name]], useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    summary_df <- data.frame(
      Variable = var_name, Type = "Categorical",
      Category = names(freq_table),
      Frequency = as.vector(freq_table),
      Percentage = round(as.vector(prop_table), 2),
      stringsAsFactors = FALSE
    )
  } else {
    summary_df <- data.frame(
      Variable = var_name, Type = "Numeric",
      Category = c("Min","Q1","Median","Mean","Q3","Max","NA"),
      Frequency = c(min(data[[var_name]], na.rm=TRUE),
                    quantile(data[[var_name]], 0.25, na.rm=TRUE),
                    median(data[[var_name]], na.rm=TRUE),
                    mean(data[[var_name]], na.rm=TRUE),
                    quantile(data[[var_name]], 0.75, na.rm=TRUE),
                    max(data[[var_name]], na.rm=TRUE),
                    sum(is.na(data[[var_name]]))),
      Percentage = NA, stringsAsFactors = FALSE
    )
  }
  return(summary_df)
}


# ---- Data processing: Quantification ----
table(df$quantification, useNA = "ifany")
table_split(df, "quantification")
df <- df %>%
  mutate(quantification = ifelse(str_detect(quantification, ";"), "multidimensional quantification", quantification)) %>%
  mutate(quantification = case_when(
    quantification %in% c("recovery rate","recovery degree","recovery time",
                          "multidimensional quantification","latitude") ~ quantification,
    TRUE ~ "others"
  ))
table(df$quantification)

# ---- Data processing: Disturbance pattern ----
table(df$disturbance_pattern, useNA = "ifany")
df <- df %>%
  mutate(disturbance_pattern = case_when(
    id == 543 ~ "press", id == 551 ~ "pulse",
    id == 1016 ~ "pulse", id == 1488 ~ "pulse",
    TRUE ~ disturbance_pattern
  ))
df <- df %>%
  mutate(disturbance_pattern = case_when(
    str_detect(disturbance_pattern, ";") ~ "pulse",
    TRUE ~ disturbance_pattern
  ))
table(df$disturbance_pattern, useNA = "ifany")

# ---- Data processing: Observation duration ----
sum(is.na(df$observation_duration))
df <- df %>%
  mutate(observation_duration = case_when(
    id == 551 ~ 10585, id == 1426 ~ 36500,
    id == 1488 ~ 365,  id == 1662 ~ 14600,
    TRUE ~ observation_duration
  ))
# Impute remaining NAs with within-group median
df <- df %>%
  group_by(quantification) %>%
  mutate(observation_duration = ifelse(
    is.na(observation_duration),
    median(observation_duration, na.rm = TRUE),
    observation_duration)) %>%
  ungroup()
# Log-transform (days)
df <- df %>%
  mutate(observation_duration = case_when(
    observation_duration > 0 ~ log10(observation_duration),
    TRUE ~ 0
  ))

# ---- Data processing: Target variable ----
table_split(df, "target_variable_group")
df <- df %>%
  mutate(target_variable_group = case_when(
    id == 586  ~ "environmental context",
    id == 1260 ~ "process based indicator",
    id == 1977 ~ "environmental context",
    TRUE ~ target_variable_group),
    target_variable_type = case_when(
      id == 586  ~ "regional network parameter",
      id == 1260 ~ "interaction",
      id == 1977 ~ "regional network parameter",
      TRUE ~ target_variable_type))
df <- df %>%
  rename_conditional("target_variable_type","abiotic parameter","target_variable_group","environmental context","functional response") %>%
  rename_conditional("target_variable_type","regional network parameter","target_variable_group","environmental context","process based indicator") %>%
  rename_conditional("target_variable_type","land use","target_variable_group","environmental context","structure")
target_variable_priority <- c("process based indicator","structure","functional response","quantify")
df <- df %>% keep_priority("target_variable_group", target_variable_priority)
table(df$target_variable_group, useNA = "ifany")

# ---- Data processing: Approach ----
table_split(df, "approach")
compare_category_distribution(df, "approach", "modeling and simulation", "indoor experiment")
df <- merge_categories(df, "approach", merge_list = list(
  "model/indoor" = c("modeling and simulation","indoor experiment")
))
approach_priority <- c("model/indoor","field experiment","field observation")
df <- df %>% keep_priority("approach", approach_priority)
table(df$approach, useNA = "ifany")

# ---- Data processing: Level ----
table_split(df, "level")
df <- df %>% mutate(level = case_when(id == 586 ~ "ecosystem", TRUE ~ level))
compare_category_distribution(df, "level", "landscape", "ecosystem")
df <- merge_categories(df, "level", merge_list = list(
  "ecosystem/landscape" = c("landscape","ecosystem")
))
level_priority <- c("ecosystem/landscape","community","population","individual")
df <- df %>% keep_priority("level", level_priority)
table(df$level, useNA = "ifany")

# ---- Data processing: Disturbance type ----
table_split(df, "disturbance_type")
df <- df %>%
  mutate(disturbance_type = case_when(
    id == 543 ~ "biotic", id == 551 ~ "climatic",
    TRUE ~ disturbance_type
  ))
compare_category_distribution(df, "disturbance_type", "geophysical", "hydrological")
compare_category_distribution(df, "disturbance_type", "hydrological", "landuse and infrastructure development")
compare_category_distribution(df, "disturbance_type", "geophysical", "landuse and infrastructure development")
compare_category_distribution(df, "disturbance_type", "biotic", "biological resource use")
compare_category_distribution(df, "disturbance_type", "resource", "chemical")
df <- merge_categories(df, "disturbance_type", merge_list = list(
  "physical template/land use" = c("geophysical","hydrological","landuse and infrastructure development"),
  "biotic pressure"            = c("biotic","biological resource use"),
  "chemical/resource"          = c("chemical","resource")
))
df <- remove_from_multivalue(df, "disturbance_type", "structural")
pulse_priority <- c("fire","chemical/resource","physical template/land use","biotic pressure","climatic")
press_priority <- c("physical template/land use","biotic pressure","climatic","chemical/resource","fire")
df <- df %>%
  mutate(disturbance_type = case_when(
    disturbance_pattern == "pulse" ~
      keep_priority(df, "disturbance_type", pulse_priority)$disturbance_type,
    disturbance_pattern == "press" ~
      keep_priority(df, "disturbance_type", press_priority)$disturbance_type,
    TRUE ~ disturbance_type
  ))
table(df$disturbance_type, useNA = "ifany")

# ---- Data processing: Taxon ----
table(df$taxon, useNA = "ifany")
df <- df %>%
  mutate(taxon = case_when(
    is.na(taxon) ~ "large scale no specific taxon",
    str_detect(taxon, "bacteria|fungi|archaea|chromista|protozoa|phytoplankton|zooplankton") ~ "microbe included",
    str_detect(taxon, "plantae") & str_detect(taxon, "animalia") ~ "plantae and animalia",
    str_detect(taxon, "algae") ~ "plantae",
    taxon %in% c("plantae","animalia") ~ taxon,
    TRUE ~ "others/virtual"
  ))
compare_category_distribution(df, "taxon", "plantae and animalia", "plantae")
compare_category_distribution(df, "taxon", "plantae and animalia", "animalia")
df <- merge_categories(df, "taxon", merge_list = list(
  "no specific"    = c("others/virtual","large scale no specific taxon"),
  "animalia/both"  = c("plantae and animalia","animalia")
))

# ---- Data processing: Habitat type ----
table(df$habitat_type, useNA = "ifany")
df <- df %>%
  mutate(habitat_type = case_when(is.na(habitat_type) ~ "no specific", TRUE ~ habitat_type))
compare_category_distribution(df, "habitat_type", "coastal",  "marine.neritic")
compare_category_distribution(df, "habitat_type", "desert",   "shrubland")
compare_category_distribution(df, "habitat_type", "desert",   "other")
compare_category_distribution(df, "habitat_type", "other",    "shrubland")
compare_category_distribution(df, "habitat_type", "savanna",  "grassland")
compare_category_distribution(df, "habitat_type", "savanna",  "agricultural")
compare_category_distribution(df, "habitat_type", "grassland","agricultural")
compare_category_distribution(df, "habitat_type", "multi",    "no specific")
df <- merge_categories(df, "habitat_type", merge_list = list(
  "marine/coastal"   = c("oceanic","marine.neritic","coastal"),
  "open barren"      = c("desert","shrubland","other"),
  "open herbaceous"  = c("savanna","grassland","agricultural"),
  "no specific/major"= c("multi","no specific")
))

# ---- Data processing: Institution country -> continent ----
table(df$institution_country, useNA = "ifany")
df <- df %>% mutate(institution_country = toupper(institution_country))
df <- merge_categories(df, "institution_country", merge_list = list(
  "north america"            = c("US","CA","MX"),
  "central & south america"  = c("GT","BZ","SV","HN","NI","CR","PA","CU","JM","HT","DO","TT","BB","GD","VC","LC","DM","AG","KN","BS","GL","BR","AR","CL","PE","CO","VE","EC","BO","PY","UY","GY","SR","GF"),
  "asia"                     = c("CN","IN","JP","KR","ID","MY","TH","VN","PH","SG","TW","MM","KH","LA","BN","TL","MN","KZ","UZ","TM","TJ","KG","AF","PK","BD","LK","MV","NP","BT","IR","IQ","SY","LB","JO","IL","PS","SA","YE","OM","AE","QA","BH","KW","AM","AZ","GE"),
  "europe"                   = c("AL","AD","AT","BY","BE","BA","BG","HR","CZ","DK","EE","FI","FR","DE","GR","HU","IS","IE","IT","LV","LI","LT","LU","MT","MD","MC","ME","NL","MK","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","CY","TR","AX","FO","GI","GG","JE","IM"),
  "africa"                   = c("NG","ET","EG","ZA","KE","UG","DZ","SD","MA","AO","GH","MZ","MG","CM","CI","NE","BF","ML","MW","ZM","SO","SN","TD","ZW","GN","RW","BJ","TN","BI","ER","SL","TG","CF","LR","LY","MR","NA","GM","BW","GA","LS","GW","GQ","MU","SZ","DJ","RE","KM","CV","ST","SC","YT"),
  "oceania"                  = c("AU","NZ","PG","FJ","SB","VU","NC","PF","WS","GU","TO","KI","PW","MH","FM","TV","NR","CK","NU","TK","WF","AS","MP","UM","HM","CC","CX","NF")
))
compare_category_distribution(df, "institution_country", "north america", "central & south america")
compare_category_distribution(df, "institution_country", "europe", "africa")
df <- merge_categories(df, "institution_country", merge_list = list(
  "america"        = c("north america","central & south america"),
  "europe & africa"= c("europe","africa")
))

# ---- Data processing: Journal ----
table(df$journal, useNA = "ifany")
df <- merge_categories(df, "journal", merge_list = list(
  "general journal"       = c("science","nature","proceedings of the national academy of sciences of the united states of america"),
  "function & molecular"  = c("molecular ecology","functional ecology"),
  "NEE & GEB & ecography" = c("nature ecology & evolution","global ecology and biogeography","ecography")
))


# =====================================================================
# CONDITIONAL INFERENCE FOREST ANALYSIS
# =====================================================================

# ===== 1. Prepare analysis data =====
predictors_cat <- c("disturbance_pattern","target_variable_group","approach",
                    "level","disturbance_type","taxon","habitat_type",
                    "institution_country","journal")
predictors_num <- "observation_duration"

analysis_data <- df %>%
  dplyr::select(quantification, all_of(c(predictors_cat, predictors_num)))

for (var in predictors_cat) {
  analysis_data[[var]] <- as.factor(analysis_data[[var]])
}
analysis_data$quantification <- as.factor(analysis_data$quantification)

# ===== 2. Data quality checks =====
cat("=== DATA QUALITY CHECKS ===\n")
cat("Sample size:", nrow(analysis_data), "\n")
class_dist     <- table(analysis_data$quantification)
imbalance_ratio <- max(class_dist) / min(class_dist)
cat("Response distribution:\n"); print(class_dist)
cat(sprintf("Imbalance ratio: %.2f:1\n", imbalance_ratio))

missing_summary <- sapply(analysis_data, function(x) sum(is.na(x)))
if (any(missing_summary > 0)) {
  cat("\n  Variables with missing values:\n")
  print(missing_summary[missing_summary > 0])
} else {
  cat("\nNo missing values\n")
}

nzv <- nearZeroVar(analysis_data[, -1], saveMetrics = TRUE)
if (any(nzv$nzv)) {
  cat("\n  Near-zero variance predictors:\n")
  print(rownames(nzv)[nzv$nzv])
} else {
  cat("No near-zero variance predictors\n")
}

# ===== 3. Export variable summary =====
outcome_summary <- create_var_summary(analysis_data, "quantification", "categorical")
cat_summaries   <- lapply(predictors_cat, function(var) create_var_summary(analysis_data, var, "categorical"))
num_summary     <- create_var_summary(analysis_data, predictors_num, "numeric")

all_summaries <- bind_rows(outcome_summary, bind_rows(cat_summaries), num_summary)
overall_info  <- data.frame(
  Variable   = c("Total Sample Size","Number of Predictors","Outcome Variable"),
  Type       = c("Overall","Overall","Overall"),
  Category   = c(as.character(nrow(analysis_data)),
                 as.character(length(c(predictors_cat, predictors_num))),
                 "quantification"),
  Frequency  = NA, Percentage = NA, stringsAsFactors = FALSE
)
final_summary <- bind_rows(overall_info, all_summaries)
write.csv(final_summary, "table/cif.explore/Table CIF_variable_summary.csv", row.names = FALSE)

# ===== 4. Build model formula =====
set.seed(207)
predictor_names <- setdiff(names(analysis_data), "quantification")
fml <- as.formula(paste("quantification ~", paste(predictor_names, collapse = " + ")))
cat("\n=== MODEL FORMULA ===\n"); print(fml)


# ===== 5. Core model functions =====

# Strict macro-F1 (unweighted mean of per-class F1)
macroF1_strict <- function(obs, pred) {
  obs  <- factor(obs)
  pred <- factor(pred, levels = levels(obs))
  cm   <- table(obs, pred)
  f1   <- sapply(levels(obs), function(cl) {
    tp    <- cm[cl, cl]
    fp    <- sum(cm[, cl]) - tp
    fn    <- sum(cm[cl, ]) - tp
    denom <- 2 * tp + fp + fn
    if (denom == 0) 0 else 2 * tp / denom
  })
  mean(f1)
}

# Permutation importance: mean drop in macro-F1 when predictor is shuffled
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

# Full repeated CV: returns importance + performance; shows progress bar
cv_full_analysis <- function(formula, data,
                             cv_folds = 5, cv_repeats = 30, nperm = 50,
                             mtry_fixed = NULL, mincriterion = 0,
                             ntree = 1500, seed = 207) {
  all_vars        <- all.vars(formula)
  response_var    <- all_vars[1]
  predictor_names <- all_vars[-1]
  p               <- length(predictor_names)
  
  if (is.null(mtry_fixed)) mtry_fixed <- floor(sqrt(p))
  mtry_fixed <- min(mtry_fixed, p)
  
  cat(sprintf("\n=== CV ANALYSIS (mtry=%d, folds=%d, repeats=%d, mincriterion=%.2f) ===\n",
              mtry_fixed, cv_folds, cv_repeats, mincriterion))
  
  ctrl <- cforest_control(
    teststat     = "quad",
    testtype     = "Univ",
    mincriterion = mincriterion,
    ntree        = ntree,
    mtry         = mtry_fixed)
  
  y_all <- data[[response_var]]
  lev   <- levels(y_all)
  
  importance_list <- list()
  oof_preds       <- list()
  cv_scores       <- c()
  run_id          <- 0L
  skipped_folds   <- 0L
  total_runs      <- cv_folds * cv_repeats
  
  rep_seeds <- seed + seq_len(cv_repeats) * 100L
  
  # Progress bar across all folds × repeats
  pb <- txtProgressBar(min = 0, max = total_runs, style = 3,
                       label = "CV progress")
  
  for (r in seq_len(cv_repeats)) {
    set.seed(rep_seeds[r])
    folds <- createFolds(y_all, k = cv_folds, list = TRUE, returnTrain = FALSE)
    
    for (k in seq_along(folds)) {
      run_id   <- run_id + 1L
      test_idx <- folds[[k]]
      train_df <- data[-test_idx, , drop = FALSE]
      valid_df <- data[ test_idx, , drop = FALSE]
      
      train_df <- droplevels(train_df)
      valid_df <- droplevels(valid_df)
      
      # Align factor levels: valid must match train
      for (v in names(train_df)) {
        if (is.factor(train_df[[v]]) && v != response_var)
          valid_df[[v]] <- factor(valid_df[[v]], levels = levels(train_df[[v]]))
      }
      
      # Fit with retry logic to handle rare LAPACK SVD failures
      fit          <- NULL
      attempt      <- 0L
      attempt_seed <- rep_seeds[r] + k * 13L
      
      while (is.null(fit) && attempt < 5L) {
        attempt <- attempt + 1L
        set.seed(attempt_seed + attempt * 7L)
        fit <- tryCatch(
          cforest(formula, data = train_df, controls = ctrl),
          error = function(e) {
            message(sprintf("  [r%d k%d attempt%d] cforest error: %s – retrying...",
                            r, k, attempt, conditionMessage(e)))
            NULL
          }
        )
      }
      
      # Skip fold if all attempts failed
      if (is.null(fit)) {
        message(sprintf("  WARNING: skipping r%d_k%d after 5 failed attempts", r, k))
        skipped_folds <- skipped_folds + 1L
        setTxtProgressBar(pb, run_id)
        next
      }
      
      pred <- predict(fit, newdata = valid_df, type = "response")
      obs  <- factor(valid_df[[response_var]], levels = lev)
      pred <- factor(pred, levels = lev)
      
      fold_f1   <- macroF1_strict(obs, pred)
      cv_scores <- c(cv_scores, fold_f1)
      
      oof_preds[[length(oof_preds) + 1L]] <- data.frame(
        obs = obs, pred = pred, repeat_id = r, fold = k,
        stringsAsFactors = FALSE
      )
      
      # Permutation importance for every predictor
      for (pred_var in predictor_names) {
        drop_val <- perm_drop_predictor(
          fit, valid_df, response_var, pred_var,
          metric_fun = macroF1_strict, nperm = nperm,
          seed = rep_seeds[r] + k
        )
        importance_list[[length(importance_list) + 1L]] <- data.frame(
          run = paste0("r", r, "_k", k), predictor = pred_var,
          drop = drop_val, stringsAsFactors = FALSE
        )
      }
      
      # Advance progress bar
      setTxtProgressBar(pb, run_id)
    }
  }
  
  close(pb)
  cat(sprintf("\nTotal skipped folds: %d / %d\n", skipped_folds, total_runs))
  
  # --- Aggregate importance ---
  imp_df <- bind_rows(importance_list)
  imp_summary <- imp_df %>%
    group_by(predictor) %>%
    summarise(
      median_drop = median(drop, na.rm = TRUE),
      mean_drop   = mean(drop,   na.rm = TRUE),
      p05         = quantile(drop, 0.05, na.rm = TRUE),
      p95         = quantile(drop, 0.95, na.rm = TRUE),
      pos_frac    = mean(drop > 0),
      .groups     = "drop"
    ) %>%
    arrange(desc(median_drop))
  
  rank_df <- imp_df %>%
    group_by(run) %>%
    mutate(rank = rank(-drop, ties.method = "average")) %>%
    ungroup() %>%
    group_by(predictor) %>%
    summarise(top3_freq = mean(rank <= 3), .groups = "drop")
  
  imp_final <- left_join(imp_summary, rank_df, by = "predictor")
  
  # --- Aggregate performance ---
  oof_df <- bind_rows(oof_preds)
  
  pooled_cm      <- table(factor(oof_df$obs,  levels = lev),
                          factor(oof_df$pred, levels = lev))
  pooled_macroF1 <- macroF1_strict(oof_df$obs, oof_df$pred)
  pooled_accuracy <- mean(oof_df$obs == oof_df$pred)
  
  per_class <- do.call(rbind, lapply(lev, function(cl) {
    tp   <- pooled_cm[cl, cl]
    fp   <- sum(pooled_cm[, cl]) - tp
    fn   <- sum(pooled_cm[cl, ]) - tp
    prec <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    rec  <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1   <- ifelse(prec + rec == 0, 0, 2 * prec * rec / (prec + rec))
    data.frame(class = cl, Precision = prec, Recall = rec, F1 = f1,
               TP = tp, FP = fp, FN = fn)
  }))
  
  cv_summary <- c(
    mean = mean(cv_scores), sd = sd(cv_scores),
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
    oof_predictions  = oof_df,
    skipped_folds    = skipped_folds
  )
}


# ===== 6. Run CV analysis and save results =====
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

final_results <- cv_full_analysis(
  fml, analysis_data,
  cv_folds = 5, cv_repeats = 30, nperm = 50,
  mincriterion = 0, mtry_fixed = 3, ntree = 500, seed = 207
)

# Save model results to disk so figures can be reproduced without re-running CV
save(final_results, analysis_data, class_dist, imbalance_ratio, predictor_names,
     predictors_cat, predictors_num,
     file = "explore.CIF.Rdata")
cat("\nModel results saved to: explore.CIF.Rdata\n")


# ===== 7. Class imbalance assessment =====
cat("\n=== CLASS IMBALANCE HANDLING ===\n")

smallest_class <- names(which.min(class_dist))
smallest_perf  <- final_results$per_class %>% filter(class == smallest_class)

cat(sprintf("\nSmallest class (%s, n=%d) performance:\n", smallest_class, min(class_dist)))
print(smallest_perf, digits = 3)

if (smallest_perf$F1 < 0.5) {
  cat("\n  Smallest class F1 < 0.5, performance may be inadequate\n")
} else {
  cat("\nSmallest class adequately predicted (F1 >= 0.5)\n")
}

weighted_f1 <- weighted.mean(
  final_results$per_class$F1,
  table(analysis_data$quantification)[final_results$per_class$class]
)

cat(sprintf("\nMacro-F1:    %.3f (equal weight, primary metric)\n", final_results$pooled_metrics["macroF1"]))
cat(sprintf("Weighted-F1: %.3f (weighted by class size)\n", weighted_f1))

cat("\nImbalance handling summary:\n")
cat("  Stratified CV ensures proportional representation\n")
cat("  Macro-F1 treats all classes equally\n")
cat("  Per-class metrics monitored\n")
if (imbalance_ratio < 3) cat("  Imbalance is moderate (< 3:1), no resampling needed\n")


# ===== 8. Model diagnostics =====
cat("\n=== MODEL DIAGNOSTICS ===\n")

cat("\nConfusion Matrix (Pooled OOF Predictions):\n")
print(final_results$confusion_matrix)

cat("\nPer-Class Performance:\n")
print(final_results$per_class, digits = 3)

oof_summary <- final_results$oof_predictions %>%
  group_by(obs) %>%
  summarise(n = n(), n_correct = sum(obs == pred),
            accuracy = mean(obs == pred), .groups = "drop") %>%
  arrange(accuracy)

cat("\nAccuracy by True Class:\n"); print(oof_summary)

cat("\nMost Common Misclassifications:\n")
misclass <- final_results$oof_predictions %>%
  filter(obs != pred) %>% count(obs, pred) %>%
  arrange(desc(n)) %>% head(5)
print(misclass)


# ===== 9. Export result tables =====
write.csv(final_results$cv_scores,
          "table/cif.explore/Table_overall_metrics.csv", row.names = TRUE)
write.csv(final_results$importance,
          "table/cif.explore/Table_CIF_importance.csv", row.names = FALSE)
write.csv(data.frame(cv_macroF1 = final_results$cv_scores_vec),
          "table/cif.explore/Table_CIF_cv_scores_distribution.csv", row.names = FALSE)
write.csv(final_results$per_class,
          "table/cif.explore/Table_CIF_per_class_metrics.csv", row.names = FALSE)
write.csv(as.data.frame.matrix(final_results$confusion_matrix),
          "table/cif.explore/Table_CIF_confusion_matrix.csv")

cat("\nAll tables exported to table/cif.explore/\n")


# =====================================================================
# FIGURES  –  load saved results before plotting (skip re-running CV)
# =====================================================================
load("explore.CIF.Rdata")   # loads: final_results, analysis_data, class_dist,
#        imbalance_ratio, predictor_names,
#        predictors_cat, predictors_num
cat("Loaded explore.CIF.Rdata for plotting\n")

# Pretty axis labels
pretty_names <- c(
  disturbance_pattern   = "Disturbance pattern",
  target_variable_group = "Measured variable",
  approach              = "Approach",
  level                 = "Biological Level",
  disturbance_type      = "Disturbance type",
  taxon                 = "Taxon",
  habitat_type          = "Habitat type",
  institution_country   = "Institution continent",
  journal               = "Journal",
  observation_duration  = "Observation duration (log)"
)

# ===== 10. Visualisation =====

# --- Variable importance plot ---
plot_df <- final_results$importance %>%
  arrange(median_drop) %>%
  mutate(predictor = factor(predictor, levels = predictor))

plot_df$label <- dplyr::recode(as.character(plot_df$predictor), !!!pretty_names)
plot_df$label <- factor(plot_df$label, levels = plot_df$label)

p_importance <- ggplot(plot_df, aes(x = label, y = median_drop)) +
  geom_col(fill = "#3F4788", alpha = 1.5) +
  geom_errorbar(aes(ymin = p05, ymax = p95), width = 0.25, linewidth = 0.5) +
  geom_text(aes(label = sprintf("Top-3: %d%%\nP(D>0): %d%%",
                                round(top3_freq * 100), round(pos_frac * 100))),
            hjust = -0.05, size = 2.5, color = "gray30") +
  coord_flip(ylim = c(min(plot_df$p05) * 0.9, max(plot_df$p95) * 1.2)) +
  labs(title = "Variable Importance (Permutation Drop in Macro-F1)",
       x = NULL, y = "Delta Macro-F1 (median with 5%-95% CI)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

print(p_importance)
ggsave("figure/cif.explore/Fig_CIF_importance.png", p_importance, width = 7, height = 5, dpi = 300)
ggsave("figure/cif.explore/Fig_CIF_importance.pdf", p_importance, width = 7, height = 5)

# --- CV score distribution ---
p_cv <- ggplot(data.frame(f1 = final_results$cv_scores_vec), aes(x = f1)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7, color = "white") +
  geom_vline(xintercept = final_results$cv_scores["mean"],
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = final_results$cv_scores["mean"],
           y = Inf, vjust = 1.5,
           label = sprintf("Mean = %.3f", final_results$cv_scores["mean"]),
           color = "red", size = 3.5) +
  labs(title = "Distribution of CV Macro-F1 Scores",
       x = "Macro-F1", y = "Frequency") +
  theme_minimal(base_size = 11)

print(p_cv)
ggsave("figure/cif.explore/Fig_CIF_cv_distribution.png", p_cv, width = 6, height = 4, dpi = 300)

# --- Confusion matrix heatmap ---
cm_df <- as.data.frame(final_results$confusion_matrix)
names(cm_df) <- c("Observed","Predicted","Freq")

p_cm <- ggplot(cm_df, aes(x = Predicted, y = Observed, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), color = "white", size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#440154", high = "#FDE724", name = "Count") +
  labs(title = "Confusion Matrix (Pooled OOF Predictions)") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_cm)
ggsave("figure/cif.explore/Fig_CIF_confusion_matrix.png", p_cm, width = 6, height = 5, dpi = 300)

cat("\nAll figures saved to figure/cif.explore/\n")


# ===== 11. Category-to-metric association analysis =====
cat("\n=== ANALYZING CATEGORY-TO-METRIC ASSOCIATIONS ===\n")

analyze_category_metric_association <- function(data, predictor_var, outcome_var = "quantification") {
  cont_table <- table(data[[predictor_var]], data[[outcome_var]])
  row_props  <- prop.table(cont_table, margin = 1) * 100
  assoc_df   <- as.data.frame(cont_table)
  colnames(assoc_df) <- c("Category","Metric","Count")
  assoc_df <- assoc_df %>%
    group_by(Category) %>%
    mutate(Percentage = Count / sum(Count) * 100,
           Total_in_Category = sum(Count)) %>%
    ungroup() %>%
    arrange(Category, desc(Percentage))
  assoc_df$Predictor <- predictor_var
  dominant <- assoc_df %>%
    group_by(Category) %>%
    slice_max(Percentage, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr::select(Predictor, Category,
                  Dominant_Metric      = Metric,
                  Dominant_Percentage  = Percentage,
                  Total_in_Category)
  list(full_table = assoc_df, dominant_metric = dominant,
       contingency_table = cont_table, row_proportions = row_props)
}

all_associations  <- list()
dominant_summaries <- list()

for (pred_var in predictors_cat) {
  cat(sprintf("\nAnalyzing: %s\n", pred_var))
  assoc <- analyze_category_metric_association(analysis_data, pred_var)
  all_associations[[pred_var]]   <- assoc$full_table
  dominant_summaries[[pred_var]] <- assoc$dominant_metric
  cat("\nDominant metric for each category:\n")
  print(assoc$dominant_metric, n = Inf)
}

full_association_table    <- bind_rows(all_associations)
dominant_summary_table    <- bind_rows(dominant_summaries)

write.csv(full_association_table,
          "table/cif.explore/Table_CIF_category_metric_associations_full.csv",
          row.names = FALSE)
write.csv(dominant_summary_table,
          "table/cif.explore/Table_CIF_category_metric_dominant.csv",
          row.names = FALSE)
cat("\nCategory-to-metric association tables exported\n")

# Association heatmaps for top 3 most important predictors
top3_predictors <- final_results$importance %>%
  arrange(desc(median_drop)) %>% head(3) %>% pull(predictor)

cat("\n=== Creating association heatmaps for top 3 predictors ===\n")

for (pred_var in top3_predictors) {
  cont_table <- table(analysis_data[[pred_var]], analysis_data$quantification)
  row_props  <- prop.table(cont_table, margin = 1) * 100
  heatmap_df <- as.data.frame(row_props)
  colnames(heatmap_df) <- c("Category","Metric","Percentage")
  
  p_heatmap <- ggplot(heatmap_df, aes(x = Metric, y = Category, fill = Percentage)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = sprintf("%.1f%%", Percentage)),
              color = "white", size = 3, fontface = "bold") +
    scale_fill_gradient2(
      low = "#440154", mid = "#21908C", high = "#FDE724",
      midpoint = 25, name = "Percentage") +
    labs(title   = sprintf("Association: %s -> Resilience Metric", pretty_names[pred_var]),
         x = "Resilience Metric",
         y = gsub("_", " ", tools::toTitleCase(pred_var))) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank())
  
  filename <- sprintf("figure/cif.explore/Fig_CIF_association_%s.png", pred_var)
  ggsave(filename, p_heatmap, width = 7, height = 5, dpi = 300)
  cat(sprintf("Saved: %s\n", filename))
}

cat("\nAssociation heatmaps created for top predictors\n")

# ===== 12. Manuscript summary =====
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("MANUSCRIPT SUMMARY\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

cat(sprintf(
  "Sample size: %d observations across %d outcome classes (%s)\n",
  nrow(analysis_data),
  nlevels(analysis_data$quantification),
  paste(table(analysis_data$quantification), collapse = ", ")
))
cat(sprintf(
  "Predictors: %d (%d categorical, %d numeric)\n",
  length(predictor_names), length(predictors_cat), length(predictors_num)
))
cat(sprintf(
  "\nPerformance: Mean macro-F1 = %.3f (5%%-95%% CI: %.3f-%.3f)\n",
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

cat("\n", paste(rep("=", 60), collapse = ""), "\n\n")
