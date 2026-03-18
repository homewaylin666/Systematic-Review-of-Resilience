# Module 4S Conditional Inference Forest: sensitivity analysis (9 runs)
# Each run checks if a specific data-processing decision affects results.
# Results are cached as .Rdata files; already-completed runs are skipped.

rm(list=ls())
library(here)
setwd(here())

# ---- Load packages ----
library(party)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(caret)
library(patchwork)

# ---- Create output directories if needed ----
dir.create("table/cif.explore", recursive = TRUE, showWarnings = FALSE)
dir.create("figure",            recursive = TRUE, showWarnings = FALSE)
dir.create("data",              recursive = TRUE, showWarnings = FALSE)

# =====================================================================
# SECTION 1 — SHARED HELPER FUNCTIONS
# =====================================================================

table_split <- function(data, column, sep = ";", useNA = "ifany") {
  col_data   <- data[[column]]
  split_data <- unlist(strsplit(col_data, sep))
  split_data <- trimws(split_data)
  table(split_data, useNA = useNA)
}

merge_categories <- function(data, column, merge_list, sep = ";") {
  data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      if (is.na(x)) return(NA)
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      for (new_cat in names(merge_list)) {
        old_cats <- merge_list[[new_cat]]
        values[values %in% old_cats] <- new_cat
      }
      unique_values <- unique(values)
      if (length(unique_values) == 1) unique_values
      else paste(unique_values, collapse = paste0(sep, " "))
    }))
}

rename_conditional <- function(data, check_column, check_value, target_column,
                               old_value, new_value, sep = ";") {
  data %>%
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
        if (length(unique_values) == 1) unique_values
        else paste(unique_values, collapse = paste0(" ", sep, " "))
      } else {
        target_val
      }
    }))
}

remove_from_multivalue <- function(data, column, category_to_remove, sep = ";") {
  data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      if (is.na(x)) return(NA)
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      if (length(values) == 1) return(x)
      values <- values[values != category_to_remove]
      unique_values <- unique(values)
      if (length(unique_values) == 1) unique_values
      else paste(unique_values, collapse = paste0(sep, " "))
    }))
}

keep_priority <- function(data, column, priority_list, sep = ";") {
  data %>%
    mutate(!!column := sapply(.data[[column]], function(x) {
      if (is.na(x)) return(NA)
      values <- trimws(unlist(strsplit(as.character(x), sep)))
      if (length(values) == 1) return(x)
      matching_values <- values[values %in% priority_list]
      if (length(matching_values) == 0) return(x)
      priorities <- match(matching_values, priority_list)
      matching_values[which.min(priorities)]
    }))
}

compare_category_distribution <- function(data, predictor, cat_a, cat_b,
                                          response = "quantification", sep = "; ") {
  expanded <- data %>%
    mutate(.pred_original = .data[[predictor]]) %>%
    separate_rows(!!sym(predictor), sep = sep) %>%
    mutate(!!sym(predictor) := trimws(.data[[predictor]]))
  data_a <- expanded[expanded[[predictor]] == cat_a, ]
  data_b <- expanded[expanded[[predictor]] == cat_b, ]
  freq_a <- table(data_a[[response]])
  freq_b <- table(data_b[[response]])
  all_levels <- sort(unique(c(names(freq_a), names(freq_b))))
  freq_a <- freq_a[all_levels]; freq_a[is.na(freq_a)] <- 0; names(freq_a) <- all_levels
  freq_b <- freq_b[all_levels]; freq_b[is.na(freq_b)] <- 0; names(freq_b) <- all_levels
  cont <- rbind(freq_a, freq_b)
  rownames(cont) <- c(cat_a, cat_b)
  fisher.test(cont, simulate.p.value = TRUE, B = 5000)$p.value
}

# =====================================================================
# SECTION 2 — CIF MODEL FUNCTIONS (fixed: mincriterion=0, nperm=50)
# =====================================================================

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

perm_drop_predictor <- function(fit, valid_df, ycol, pred_col, metric_fun,
                                nperm = 50, seed = 207) {
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

# Full CV analysis: returns importance + performance metrics
cv_full_analysis <- function(formula, data,
                             cv_folds = 5, cv_repeats = 30, nperm = 50,
                             mtry_fixed = 3, mincriterion = 0, ntree = 500,
                             seed = 207) {
  all_vars        <- all.vars(formula)
  response_var    <- all_vars[1]
  predictor_names <- all_vars[-1]
  p <- length(predictor_names)
  mtry_fixed <- min(mtry_fixed, p)
  
  cat(sprintf("  [CIF] mtry=%d | folds=%d | repeats=%d | mincriterion=%.2f | ntree=%d\n",
              mtry_fixed, cv_folds, cv_repeats, mincriterion, ntree))
  
  ctrl <- cforest_control(teststat = "quad", testtype = "Univ",
                          mincriterion = mincriterion, ntree = ntree, mtry = mtry_fixed)
  set.seed(seed)
  y_all <- data[[response_var]]
  lev   <- levels(y_all)
  
  importance_list <- list()
  oof_preds       <- list()
  cv_scores       <- c()
  run_id          <- 0L
  rep_seeds       <- seed + seq_len(cv_repeats) * 100L
  
  total_runs <- cv_folds * cv_repeats
  pb <- txtProgressBar(min = 0, max = total_runs, style = 3)
  
  for (r in seq_len(cv_repeats)) {
    set.seed(rep_seeds[r])
    folds <- createFolds(y_all, k = cv_folds, list = TRUE, returnTrain = FALSE)
    
    for (k in seq_along(folds)) {
      run_id   <- run_id + 1L
      test_idx <- folds[[k]]
      train_df <- data[-test_idx, , drop = FALSE]
      valid_df <- data[ test_idx, , drop = FALSE]
      
      fit  <- cforest(formula, data = train_df, controls = ctrl)
      pred <- predict(fit, newdata = valid_df, type = "response")
      obs  <- factor(valid_df[[response_var]], levels = lev)
      pred <- factor(pred, levels = lev)
      
      cv_scores <- c(cv_scores, macroF1_strict(obs, pred))
      oof_preds[[length(oof_preds) + 1L]] <- data.frame(
        obs = obs, pred = pred, repeat_id = r, fold = k, stringsAsFactors = FALSE)
      
      for (pv in predictor_names) {
        drop_val <- perm_drop_predictor(fit, valid_df, response_var, pv,
                                        metric_fun = macroF1_strict, nperm = nperm,
                                        seed = rep_seeds[r] + k)
        importance_list[[length(importance_list) + 1L]] <- data.frame(
          run = paste0("r", r, "_k", k), predictor = pv, drop = drop_val,
          stringsAsFactors = FALSE)
      }
      setTxtProgressBar(pb, run_id)
    }
  }
  close(pb)
  
  # Aggregate importance
  imp_df <- bind_rows(importance_list)
  imp_summary <- imp_df %>%
    group_by(predictor) %>%
    summarise(median_drop = median(drop, na.rm = TRUE),
              mean_drop   = mean(drop,   na.rm = TRUE),
              p05         = quantile(drop, 0.05, na.rm = TRUE),
              p95         = quantile(drop, 0.95, na.rm = TRUE),
              pos_frac    = mean(drop > 0), .groups = "drop") %>%
    arrange(desc(median_drop))
  
  rank_df <- imp_df %>%
    group_by(run) %>%
    mutate(rank = rank(-drop, ties.method = "average")) %>%
    ungroup() %>%
    group_by(predictor) %>%
    summarise(top3_freq = mean(rank <= 3), .groups = "drop")
  
  imp_final <- left_join(imp_summary, rank_df, by = "predictor")
  
  # Aggregate performance
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
  
  cv_summary <- c(mean = mean(cv_scores), sd = sd(cv_scores),
                  p05  = quantile(cv_scores, 0.05),
                  p95  = quantile(cv_scores, 0.95))
  
  cat(sprintf("  Mean macro-F1 = %.4f (SD = %.4f) | Pooled = %.4f | Acc = %.4f\n",
              cv_summary["mean"], cv_summary["sd"], pooled_macroF1, pooled_accuracy))
  
  list(importance       = imp_final,
       importance_raw   = imp_df,
       cv_scores        = cv_summary,
       cv_scores_vec    = cv_scores,
       pooled_metrics   = c(macroF1 = pooled_macroF1, accuracy = pooled_accuracy),
       confusion_matrix = pooled_cm,
       per_class        = per_class,
       oof_predictions  = oof_df)
}

# =====================================================================
# SECTION 3 — BASE DATA LOADING & PROCESSING
# Build a clean base data frame; individual runs will fork from here
# =====================================================================

build_base_df <- function() {
  df <- read.csv("data/Gz.full.data.sheet_594_edited.csv")
  df <- df %>%
    dplyr::select(id, measurement, quantification, target_variable_type,
                  target_variable_group, level, approach, institution_country,
                  habitat_type, taxon, disturbance_type, disturbance_pattern,
                  observation_duration, journal, disturbance_both)
  df <- df %>% filter(measurement == "observed")
  
  # --- Quantification ---
  df <- df %>%
    mutate(quantification = ifelse(str_detect(quantification, ";"),
                                   "multidimensional quantification", quantification)) %>%
    mutate(quantification = case_when(
      quantification %in% c("recovery rate", "recovery degree", "recovery time",
                            "multidimensional quantification", "latitude") ~ quantification,
      TRUE ~ "others"))
  
  # --- Disturbance pattern (base: multi-value → "pulse") ---
  df <- df %>%
    mutate(disturbance_pattern = case_when(
      id == 543 ~ "press", id == 551 ~ "pulse",
      id == 1016 ~ "pulse", id == 1488 ~ "pulse",
      TRUE ~ disturbance_pattern)) %>%
    mutate(disturbance_pattern = case_when(
      str_detect(disturbance_pattern, ";") ~ "pulse",   # base behaviour
      TRUE ~ disturbance_pattern))
  
  # --- Observation duration ---
  df <- df %>%
    mutate(observation_duration = case_when(
      id == 551 ~ 10585, id == 1426 ~ 36500,
      id == 1488 ~ 365,  id == 1662 ~ 14600,
      TRUE ~ observation_duration)) %>%
    group_by(quantification) %>%
    mutate(observation_duration = ifelse(
      is.na(observation_duration),
      median(observation_duration, na.rm = TRUE), observation_duration)) %>%
    ungroup() %>%
    mutate(observation_duration = case_when(
      observation_duration > 0 ~ log10(observation_duration), TRUE ~ 0))
  
  # --- Target variable group ---
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
        TRUE ~ target_variable_type)) %>%
    rename_conditional("target_variable_type", "abiotic parameter",
                       "target_variable_group", "environmental context", "functional response") %>%
    rename_conditional("target_variable_type", "regional network parameter",
                       "target_variable_group", "environmental context", "process based indicator") %>%
    rename_conditional("target_variable_type", "land use",
                       "target_variable_group", "environmental context", "structure")
  
  # --- Approach ---
  df <- merge_categories(df, "approach", merge_list = list(
    "model/indoor" = c("modeling and simulation", "indoor experiment")))
  
  # --- Level ---
  df <- df %>%
    mutate(level = case_when(id == 586 ~ "ecosystem", TRUE ~ level))
  df <- merge_categories(df, "level", merge_list = list(
    "ecosystem/landscape" = c("landscape", "ecosystem")))
  
  # --- Disturbance type ---
  df <- df %>%
    mutate(disturbance_type = case_when(
      id == 543 ~ "biotic", id == 551 ~ "climatic", TRUE ~ disturbance_type))
  df <- merge_categories(df, "disturbance_type", merge_list = list(
    "physical template/land use" = c("geophysical", "hydrological", "landuse and infrastructure development"),
    "biotic pressure"            = c("biotic", "biological resource use"),
    "chemical/resource"          = c("chemical", "resource")))
  df <- remove_from_multivalue(df, "disturbance_type", "structural")
  
  # --- Taxon ---
  df <- df %>%
    mutate(taxon = case_when(
      is.na(taxon) ~ "large scale no specific taxon",
      str_detect(taxon, "bacteria|fungi|archaea|chromista|protozoa|phytoplankton|zooplankton") ~ "microbe included",
      str_detect(taxon, "plantae") & str_detect(taxon, "animalia") ~ "plantae and animalia",
      str_detect(taxon, "algae") ~ "plantae",
      taxon %in% c("plantae", "animalia") ~ taxon,
      TRUE ~ "others/virtual")) %>%
    merge_categories("taxon", merge_list = list(
      "no specific"    = c("others/virtual", "large scale no specific taxon"),
      "animalia/both"  = c("plantae and animalia", "animalia")))
  
  # --- Habitat type ---
  df <- df %>%
    mutate(habitat_type = case_when(is.na(habitat_type) ~ "no specific", TRUE ~ habitat_type))
  df <- merge_categories(df, "habitat_type", merge_list = list(
    "marine/coastal"  = c("oceanic", "marine.neritic", "coastal"),
    "open barren"     = c("desert", "shrubland", "other"),
    "open herbaceous" = c("savanna", "grassland", "agricultural"),
    "no specific/major" = c("multi", "no specific")))
  
  # --- Institution country ---
  df <- df %>% mutate(institution_country = toupper(institution_country))
  df <- merge_categories(df, "institution_country", merge_list = list(
    "north america" = c("US", "CA", "MX"),
    "central & south america" = c("GT","BZ","SV","HN","NI","CR","PA","CU","JM","HT","DO","TT","BB","GD","VC","LC","DM","AG","KN","BS","GL","BR","AR","CL","PE","CO","VE","EC","BO","PY","UY","GY","SR","GF"),
    "asia"    = c("CN","IN","JP","KR","ID","MY","TH","VN","PH","SG","TW","MM","KH","LA","BN","TL","MN","KZ","UZ","TM","TJ","KG","AF","PK","BD","LK","MV","NP","BT","IR","IQ","SY","LB","JO","IL","PS","SA","YE","OM","AE","QA","BH","KW","AM","AZ","GE"),
    "europe"  = c("AL","AD","AT","BY","BE","BA","BG","HR","CZ","DK","EE","FI","FR","DE","GR","HU","IS","IE","IT","LV","LI","LT","LU","MT","MD","MC","ME","NL","MK","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","CY","TR","AX","FO","GI","GG","JE","IM"),
    "africa"  = c("NG","ET","EG","ZA","KE","UG","DZ","SD","MA","AO","GH","MZ","MG","CM","CI","NE","BF","ML","MW","ZM","SO","SN","TD","ZW","GN","RW","BJ","TN","BI","ER","SL","TG","CF","LR","LY","MR","NA","GM","BW","GA","LS","GW","GQ","MU","SZ","DJ","RE","KM","CV","ST","SC","YT"),
    "oceania" = c("AU","NZ","PG","FJ","SB","VU","NC","PF","WS","GU","TO","KI","PW","MH","FM","TV","NR","CK","NU","TK","WF","AS","MP","UM","HM","CC","CX","NF")))
  df <- merge_categories(df, "institution_country", merge_list = list(
    "america"         = c("north america", "central & south america"),
    "europe & africa" = c("europe", "africa")))
  
  # --- Journal ---
  df <- merge_categories(df, "journal", merge_list = list(
    "general journal" = c("science", "nature",
                          "proceedings of the national academy of sciences of the united states of america")))
  df
}

# =====================================================================
# SECTION 4 — APPLY PRIORITY RULES (parameterised)
# =====================================================================

apply_priority_rules <- function(df,
                                 target_variable_priority,
                                 approach_priority,
                                 level_priority,
                                 pulse_priority,
                                 press_priority) {
  # Target variable group
  df <- df %>% keep_priority("target_variable_group", target_variable_priority)
  
  # Approach
  df <- df %>% keep_priority("approach", approach_priority)
  
  # Level
  df <- df %>% keep_priority("level", level_priority)
  
  # Disturbance type (pattern-dependent priority)
  df <- df %>%
    mutate(disturbance_type = case_when(
      disturbance_pattern == "pulse" ~
        keep_priority(df, "disturbance_type", pulse_priority)$disturbance_type,
      disturbance_pattern == "press" ~
        keep_priority(df, "disturbance_type", press_priority)$disturbance_type,
      TRUE ~ disturbance_type))
  
  df
}

# Default priority lists (from original code)
default_target_variable_priority <- c("process based indicator", "structure", "functional response", "quantify")
default_approach_priority         <- c("model/indoor", "field experiment", "field observation")
default_level_priority            <- c("ecosystem/landscape", "community", "population", "individual")
default_pulse_priority            <- c("fire", "chemical/resource", "physical template/land use", "biotic pressure", "climatic")
default_press_priority            <- c("physical template/land use", "biotic pressure", "climatic", "chemical/resource", "fire")

# =====================================================================
# SECTION 5 — PREPARE ANALYSIS DATA (factors, formula)
# =====================================================================

predictors_cat <- c("disturbance_pattern", "target_variable_group", "approach",
                    "level", "disturbance_type", "taxon", "habitat_type")
predictors_num <- "observation_duration"

prepare_analysis_data <- function(df) {
  ad <- df %>% dplyr::select(quantification, all_of(c(predictors_cat, predictors_num)))
  for (v in predictors_cat) ad[[v]] <- as.factor(ad[[v]])
  ad$quantification <- as.factor(ad$quantification)
  ad
}

build_formula <- function(analysis_data) {
  pnames <- setdiff(names(analysis_data), "quantification")
  as.formula(paste("quantification ~", paste(pnames, collapse = " + ")))
}

# =====================================================================
# SECTION 6 — EXPORT HELPERS
# =====================================================================

export_run_tables <- function(results, run_id, analysis_data) {
  prefix <- file.path("table/cif.explore", run_id)
  
  write.csv(results$importance,
            paste0(prefix, "_importance.csv"), row.names = FALSE)
  write.csv(results$per_class,
            paste0(prefix, "_per_class.csv"), row.names = FALSE)
  write.csv(as.data.frame.matrix(results$confusion_matrix),
            paste0(prefix, "_confusion_matrix.csv"))
  write.csv(data.frame(cv_macroF1 = results$cv_scores_vec),
            paste0(prefix, "_cv_scores.csv"), row.names = FALSE)
  
  # Overall metrics
  metrics_df <- data.frame(
    run          = run_id,
    mean_macroF1 = results$cv_scores["mean"],
    sd_macroF1   = results$cv_scores["sd"],
    p05          = results$cv_scores["p05"],
    p95          = results$cv_scores["p95"],
    pooled_macroF1 = results$pooled_metrics["macroF1"],
    accuracy       = results$pooled_metrics["accuracy"])
  write.csv(metrics_df, paste0(prefix, "_overall_metrics.csv"), row.names = FALSE)
  
  cat(sprintf("  [Export] Tables saved to table/cif.explore/%s_*.csv\n", run_id))
}

# =====================================================================
# SECTION 7 — RUN DEFINITIONS
# 9 sensitivity runs, each with a short label and a setup function
# =====================================================================

run_configs <- list(
  
  # Run 1: Reverse target_variable_priority
  list(
    id = "rev.variable",
    desc = "Reversed target_variable_priority",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = rev(default_target_variable_priority),
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 2: Reverse approach_priority
  list(
    id = "rev.approach",
    desc = "Reversed approach_priority",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = rev(default_approach_priority),
                                 level_priority           = default_level_priority,
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 3: Reverse level_priority
  list(
    id = "rev.level",
    desc = "Reversed level_priority",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = rev(default_level_priority),
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 4: Reverse pulse_priority
  list(
    id = "rev.pulse",
    desc = "Reversed pulse_priority",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = rev(default_pulse_priority),
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 5: Reverse press_priority
  list(
    id = "rev.press",
    desc = "Reversed press_priority",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = rev(default_press_priority))
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 6: Reverse both pulse and press priority
  list(
    id = "rev.all.disturbance",
    desc = "Reversed both pulse_priority and press_priority",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = rev(default_pulse_priority),
                                 press_priority           = rev(default_press_priority))
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 7: Multi-value disturbance_pattern → "press" instead of "pulse"
  list(
    id = "rev.pattern",
    desc = "Multi-value disturbance_pattern recoded as 'press' (not 'pulse')",
    setup = function() {
      df <- read.csv("data/Gz.full.data.sheet_594_edited.csv")
      df <- df %>%
        dplyr::select(id, measurement, quantification, target_variable_type,
                      target_variable_group, level, approach, institution_country,
                      habitat_type, taxon, disturbance_type, disturbance_pattern,
                      observation_duration, journal, disturbance_both) %>%
        filter(measurement == "observed")
      
      # Quantification (same as base)
      df <- df %>%
        mutate(quantification = ifelse(str_detect(quantification, ";"),
                                       "multidimensional quantification", quantification)) %>%
        mutate(quantification = case_when(
          quantification %in% c("recovery rate","recovery degree","recovery time",
                                "multidimensional quantification","latitude") ~ quantification,
          TRUE ~ "others"))
      
      # Disturbance pattern: multi-value → "press" (deviation from base)
      df <- df %>%
        mutate(disturbance_pattern = case_when(
          id == 543 ~ "press", id == 551 ~ "pulse",
          id == 1016 ~ "pulse", id == 1488 ~ "pulse",
          TRUE ~ disturbance_pattern)) %>%
        mutate(disturbance_pattern = case_when(
          str_detect(disturbance_pattern, ";") ~ "press",  # KEY CHANGE
          TRUE ~ disturbance_pattern))
      
      # Observation duration
      df <- df %>%
        mutate(observation_duration = case_when(
          id == 551 ~ 10585, id == 1426 ~ 36500,
          id == 1488 ~ 365,  id == 1662 ~ 14600,
          TRUE ~ observation_duration)) %>%
        group_by(quantification) %>%
        mutate(observation_duration = ifelse(
          is.na(observation_duration),
          median(observation_duration, na.rm = TRUE), observation_duration)) %>%
        ungroup() %>%
        mutate(observation_duration = case_when(
          observation_duration > 0 ~ log10(observation_duration), TRUE ~ 0))
      
      # All other processing same as build_base_df()
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
            TRUE ~ target_variable_type)) %>%
        rename_conditional("target_variable_type","abiotic parameter",
                           "target_variable_group","environmental context","functional response") %>%
        rename_conditional("target_variable_type","regional network parameter",
                           "target_variable_group","environmental context","process based indicator") %>%
        rename_conditional("target_variable_type","land use",
                           "target_variable_group","environmental context","structure")
      df <- merge_categories(df,"approach",merge_list=list("model/indoor"=c("modeling and simulation","indoor experiment")))
      df <- df %>% mutate(level=case_when(id==586~"ecosystem",TRUE~level))
      df <- merge_categories(df,"level",merge_list=list("ecosystem/landscape"=c("landscape","ecosystem")))
      df <- df %>% mutate(disturbance_type=case_when(id==543~"biotic",id==551~"climatic",TRUE~disturbance_type))
      df <- merge_categories(df,"disturbance_type",merge_list=list(
        "physical template/land use"=c("geophysical","hydrological","landuse and infrastructure development"),
        "biotic pressure"=c("biotic","biological resource use"),
        "chemical/resource"=c("chemical","resource")))
      df <- remove_from_multivalue(df,"disturbance_type","structural")
      df <- df %>%
        mutate(taxon=case_when(
          is.na(taxon)~"large scale no specific taxon",
          str_detect(taxon,"bacteria|fungi|archaea|chromista|protozoa|phytoplankton|zooplankton")~"microbe included",
          str_detect(taxon,"plantae")&str_detect(taxon,"animalia")~"plantae and animalia",
          str_detect(taxon,"algae")~"plantae",
          taxon%in%c("plantae","animalia")~taxon,
          TRUE~"others/virtual")) %>%
        merge_categories("taxon",merge_list=list("no specific"=c("others/virtual","large scale no specific taxon"),
                                                 "animalia/both"=c("plantae and animalia","animalia")))
      df <- df %>% mutate(habitat_type=case_when(is.na(habitat_type)~"no specific",TRUE~habitat_type))
      df <- merge_categories(df,"habitat_type",merge_list=list(
        "marine/coastal"=c("oceanic","marine.neritic","coastal"),
        "open barren"=c("desert","shrubland","other"),
        "open herbaceous"=c("savanna","grassland","agricultural"),
        "no specific/major"=c("multi","no specific")))
      df <- df %>% mutate(institution_country=toupper(institution_country))
      df <- merge_categories(df,"institution_country",merge_list=list(
        "north america"=c("US","CA","MX"),
        "central & south america"=c("GT","BZ","SV","HN","NI","CR","PA","CU","JM","HT","DO","TT","BB","GD","VC","LC","DM","AG","KN","BS","GL","BR","AR","CL","PE","CO","VE","EC","BO","PY","UY","GY","SR","GF"),
        "asia"=c("CN","IN","JP","KR","ID","MY","TH","VN","PH","SG","TW","MM","KH","LA","BN","TL","MN","KZ","UZ","TM","TJ","KG","AF","PK","BD","LK","MV","NP","BT","IR","IQ","SY","LB","JO","IL","PS","SA","YE","OM","AE","QA","BH","KW","AM","AZ","GE"),
        "europe"=c("AL","AD","AT","BY","BE","BA","BG","HR","CZ","DK","EE","FI","FR","DE","GR","HU","IS","IE","IT","LV","LI","LT","LU","MT","MD","MC","ME","NL","MK","NO","PL","PT","RO","RU","SM","RS","SK","SI","ES","SE","CH","UA","GB","VA","CY","TR","AX","FO","GI","GG","JE","IM"),
        "africa"=c("NG","ET","EG","ZA","KE","UG","DZ","SD","MA","AO","GH","MZ","MG","CM","CI","NE","BF","ML","MW","ZM","SO","SN","TD","ZW","GN","RW","BJ","TN","BI","ER","SL","TG","CF","LR","LY","MR","NA","GM","BW","GA","LS","GW","GQ","MU","SZ","DJ","RE","KM","CV","ST","SC","YT"),
        "oceania"=c("AU","NZ","PG","FJ","SB","VU","NC","PF","WS","GU","TO","KI","PW","MH","FM","TV","NR","CK","NU","TK","WF","AS","MP","UM","HM","CC","CX","NF")))
      df <- merge_categories(df,"institution_country",merge_list=list(
        "america"=c("north america","central & south america"),
        "europe & africa"=c("europe","africa")))
      df <- merge_categories(df,"journal",merge_list=list(
        "general journal"=c("science","nature","proceedings of the national academy of sciences of the united states of america")))
      
      # Apply default priority rules
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 8: Remove multidimensional quantification samples
  list(
    id = "remove.multi",
    desc = "Remove samples with quantification = multidimensional quantification",
    setup = function() {
      df <- build_base_df()
      # Remove multidimensional quantification BEFORE priority rules
      df <- df %>% filter(quantification != "multidimensional quantification")
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0
  ),
  
  # Run 9: mincriterion = 0.9 (significant splits only)
  list(
    id = "mincri.significant",
    desc = "mincriterion = 0.9 (only significant splits)",
    setup = function() {
      df <- build_base_df()
      df <- apply_priority_rules(df,
                                 target_variable_priority = default_target_variable_priority,
                                 approach_priority        = default_approach_priority,
                                 level_priority           = default_level_priority,
                                 pulse_priority           = default_pulse_priority,
                                 press_priority           = default_press_priority)
      prepare_analysis_data(df)
    },
    mincriterion = 0.9
  )
)

# =====================================================================
# SECTION 8 — MAIN LOOP: run each config, skip if cached
# =====================================================================

# Fixed hyperparameters (same as baseline)
MTRY_FIXED    <- 3
NTREE         <- 500
NPERM         <- 50
CV_FOLDS      <- 5
CV_REPEATS    <- 30
SEED          <- 207

all_plot_data <- list()  # collect results for combined figure

cat("\n")
cat(strrep("=", 60), "\n")
cat(sprintf("SENSITIVITY ANALYSIS: %d runs\n", length(run_configs)))
cat(strrep("=", 60), "\n\n")

for (cfg in run_configs) {
  run_id  <- cfg$id
  rdata_path <- file.path("data", paste0("H.CIF.", run_id, ".Rdata"))
  
  cat(strrep("-", 50), "\n")
  cat(sprintf("[%s] %s\n", run_id, cfg$desc))
  
  # --- Check cache ---
  if (file.exists(rdata_path)) {
    cat(sprintf("  [SKIP] Cache found: %s — loading...\n", rdata_path))
    env <- new.env()
    load(rdata_path, envir = env)
    all_plot_data[[run_id]] <- env$plot_data
    cat(sprintf("  Pooled macro-F1 = %.4f\n",
                env$plot_data$final_results$pooled_metrics["macroF1"]))
    next
  }
  
  # --- Build analysis data ---
  cat("  Building analysis data...\n")
  analysis_data <- cfg$setup()
  fml           <- build_formula(analysis_data)
  mc            <- cfg$mincriterion
  
  cat(sprintf("  n = %d | classes = %s\n",
              nrow(analysis_data),
              paste(table(analysis_data$quantification), collapse = ":")))
  
  # --- Run CIF ---
  cat(sprintf("  Running CIF (mincriterion = %.2f)...\n", mc))
  results <- cv_full_analysis(fml, analysis_data,
                              cv_folds     = CV_FOLDS,
                              cv_repeats   = CV_REPEATS,
                              nperm        = NPERM,
                              mtry_fixed   = MTRY_FIXED,
                              mincriterion = mc,
                              ntree        = NTREE,
                              seed         = SEED)
  
  # --- Export tables ---
  export_run_tables(results, run_id, analysis_data)
  
  # --- Build and save plot_data (mirroring original structure) ---
  pretty_names <- c(
    disturbance_pattern   = "Disturbance pattern",
    target_variable_group = "Target variable",
    approach              = "Approach",
    level                 = "Ecological Level",
    disturbance_type      = "Disturbance type",
    taxon                 = "Taxon",
    habitat_type          = "Habitat type",
    observation_duration  = "Observation duration (log)"
  )
  
  top3_predictors <- results$importance %>%
    arrange(desc(median_drop)) %>%
    head(3) %>%
    pull(predictor)
  
  plot_data <- list(
    run_id           = run_id,
    desc             = cfg$desc,
    final_results    = results,
    analysis_data    = analysis_data,
    class_dist       = table(analysis_data$quantification),
    mtry_final       = MTRY_FIXED,
    mincriterion_final = mc,
    predictor_names  = setdiff(names(analysis_data), "quantification"),
    top3_predictors  = top3_predictors,
    pretty_names     = pretty_names,
    imp_plot_df      = results$importance %>%
      arrange(median_drop) %>%
      mutate(predictor = factor(predictor, levels = predictor))
  )
  
  all_plot_data[[run_id]] <- plot_data
  
  save(plot_data, file = rdata_path)
  cat(sprintf("  [Saved] %s\n", rdata_path))
}

cat("\n")
cat(strrep("=", 60), "\n")
cat("All runs complete. Building combined figure...\n")
cat(strrep("=", 60), "\n\n")

# =====================================================================
# SECTION 9 — COMBINED 3×3 FIGURE
# =====================================================================

pretty_names_global <- c(
  disturbance_pattern   = "Disturbance pattern",
  target_variable_group = "Target variable",
  approach              = "Approach",
  level                 = "Ecological Level",
  disturbance_type      = "Disturbance type",
  taxon                 = "Taxon",
  habitat_type          = "Habitat type",
  observation_duration  = "Observation duration (log)"
)

cap_words      <- function(x) gsub("(^|\\s)(\\w)", "\\1\\U\\2", x, perl = TRUE)
space_to_nl    <- function(x) gsub(" ", "\n", x)

# Build one importance panel per run
build_importance_panel <- function(pd, panel_label) {
  imp_df <- pd$imp_plot_df
  imp_df$label <- dplyr::recode(as.character(imp_df$predictor), !!!pretty_names_global)
  imp_df$label <- gsub("Target variable", "Measured\nvariable", imp_df$label)
  imp_df$label <- gsub(" ", "\n", imp_df$label)
  imp_df$label <- factor(imp_df$label, levels = imp_df$label)
  
  mean_f1  <- pd$final_results$cv_scores["mean"]
  pooled   <- pd$final_results$pooled_metrics["macroF1"]
  subtitle <- sprintf("Mean F1=%.3f | Pooled F1=%.3f", mean_f1, pooled)
  
  ggplot(imp_df, aes(x = label, y = median_drop)) +
    geom_col(fill = "#3F4788", alpha = 0.9) +
    geom_errorbar(aes(ymin = p05, ymax = p95), width = 0.2, linewidth = 0.5) +
    coord_flip(ylim = c(min(imp_df$p05, 0) * 1.05,
                        max(imp_df$p95) * 1.25)) +
    labs(title  = panel_label,
         subtitle = subtitle,
         x = NULL,
         y = "Δ Macro-F1") +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title    = element_text(face = "bold", size = 9, hjust = 0.5),
      plot.subtitle = element_text(size = 7.5, hjust = 0.5, color = "gray40"),
      axis.text.y   = element_text(size = 7.5, lineheight = 0.85),
      axis.text.x   = element_text(size = 7),
      axis.title.x  = element_text(size = 8)
    )
}

# Panel labels (short descriptions)
panel_labels <- c(
  "rev.variable"        = "1. Rev. target-variable priority",
  "rev.approach"        = "2. Rev. approach priority",
  "rev.level"           = "3. Rev. level priority",
  "rev.pulse"           = "4. Rev. pulse-disturbance priority",
  "rev.press"           = "5. Rev. press-disturbance priority",
  "rev.all.disturbance" = "6. Rev. both disturbance priorities",
  "rev.pattern"         = "7. Multi-value pattern → press",
  "remove.multi"        = "8. Remove multidimensional metric",
  "mincri.significant"  = "9. mincriterion = 0.9"
)

run_order <- names(panel_labels)

panels <- lapply(run_order, function(rid) {
  pd    <- all_plot_data[[rid]]
  label <- panel_labels[[rid]]
  build_importance_panel(pd, label)
})

# Arrange 3×3
combined_fig <- wrap_plots(panels, ncol = 3, nrow = 3) +
  plot_annotation(
    title = "Sensitivity Analysis: Conditional Inference Forest (9 Runs)",
    subtitle = paste0("Each panel shows variable importance (permutation Δ Macro-F1, median ± 5%–95% CI).\n",
                      sprintf("ntree = %d | mtry = %d | nperm = %d | %d-fold CV × %d repeats",
                              NTREE, MTRY_FIXED, NPERM, CV_FOLDS, CV_REPEATS)),
    theme = theme(
      plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 9,  hjust = 0.5, color = "gray40")
    )
  )

fig_path <- "figure/Fig_CIF_sensitivity_9panels.png"
ggsave(fig_path, combined_fig, width = 18, height = 12, dpi = 300)
cat(sprintf("✓ Combined figure saved: %s\n", fig_path))

# =====================================================================
# SECTION 10 — SUMMARY TABLE across all runs
# =====================================================================

summary_rows <- lapply(run_order, function(rid) {
  pd <- all_plot_data[[rid]]
  fr <- pd$final_results
  top3 <- paste(head(fr$importance$predictor, 3), collapse = " > ")
  data.frame(
    run_id       = rid,
    description  = panel_labels[[rid]],
    n_samples    = nrow(pd$analysis_data),
    mean_macroF1 = round(fr$cv_scores["mean"], 4),
    sd_macroF1   = round(fr$cv_scores["sd"],   4),
    p05          = round(fr$cv_scores["p05"],  4),
    p95          = round(fr$cv_scores["p95"],  4),
    pooled_F1    = round(fr$pooled_metrics["macroF1"], 4),
    accuracy     = round(fr$pooled_metrics["accuracy"], 4),
    top3_predictors = top3,
    stringsAsFactors = FALSE
  )
})

summary_table <- bind_rows(summary_rows)
write.csv(summary_table, "table/cif.explore/00_sensitivity_summary.csv", row.names = FALSE)
cat("✓ Summary table saved: table/cif.explore/00_sensitivity_summary.csv\n")

cat("\n")
cat(strrep("=", 60), "\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
print(summary_table[, c("run_id", "mean_macroF1", "sd_macroF1", "pooled_F1", "top3_predictors")])
cat(strrep("=", 60), "\n")
