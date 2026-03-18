#### Module 3.3: Figure 4
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# ---- Import the data ----
df <- read.csv("data/Gz.full.data.sheet_594.csv")

# ---- Load required packages ----
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(RColorBrewer)
library(viridis)
library(multcomp)  # for Tukey test
library(ggsignif)  # for significance visualization
library(broom)     # for tidy statistical outputs

# ---- Data preprocessing function ----
expand_categorical <- function(data, col_name) {
  data %>%
    separate_rows(!!sym(col_name), sep = ";") %>%
    mutate(!!sym(col_name) := trimws(!!sym(col_name))) %>%
    filter(!!sym(col_name) != "" & !is.na(!!sym(col_name)))
}

# ---- Function to capitalize first letter of each word ----
capitalize_words <- function(x) {
  str_to_title(gsub("_", " ", x))
}

# ---- Function to format variable names for labels ----
format_var_name <- function(x) {
  gsub("_", " ", x) %>% str_to_title()
}

# ---- Define factor level orders ----
manipulation_order <- c("Model", "Manip.", "Unmanip.")
type_order <- c("Fire", "Climatic", "Hydrological", "Geophysical", "Chemical", 
                "Resource", "Biotic", "LID", "BRU", "Structural")

# ---- Preprocess data ----
df_processed <- df %>%
  # Remove rows with invalid observation_duration before log transformation
  filter(!is.na(observation_duration) & observation_duration > 0) %>%
  # Log transform observation_duration
  mutate(log_observation_duration = log(observation_duration)) %>%
  # Remove rows with NA or empty values in categorical variables
  filter(!is.na(disturbance_manipulation) & !is.na(disturbance_type) & !is.na(disturbance_pattern) &
           disturbance_manipulation != "" & disturbance_type != "" & disturbance_pattern != "")

# Define custom colors for disturbance pattern
pattern_colors <- c("Press" = "#3A2C78BB", "Pulse" = "#3CBC75BB")

# ---- Function to perform chi-square test and calculate standardized residuals ----
chi_square_analysis <- function(data, var1, var2) {
  # Create contingency table
  ct <- table(data[[var1]], data[[var2]])
  
  # Perform chi-square test
  chi_test <- chisq.test(ct)
  
  # Calculate standardized residuals
  std_residuals <- chi_test$stdres
  
  # Convert to data frame for plotting
  result_df <- expand.grid(
    var1 = rownames(std_residuals),
    var2 = colnames(std_residuals)
  ) %>%
    mutate(
      count = as.vector(ct),
      std_residual = as.vector(std_residuals),
      significant = abs(std_residual) >= 1.96,
      excess = std_residual > 1.96,
      deficit = std_residual < -1.96,
      # Create simplified labels
      residual_label = case_when(
        excess ~ "*+",
        deficit ~ "*-",
        TRUE ~ ""
      )
    )
  
  names(result_df)[1:2] <- c(var1, var2)
  
  return(list(
    data = result_df,
    chi_test = chi_test,
    p_value = chi_test$p.value
  ))
}

# ---- Function to perform Tukey HSD test for boxplots ----
perform_tukey_test <- function(data, categorical_var, continuous_var) {
  # Fit ANOVA model
  formula_str <- paste(continuous_var, "~", categorical_var)
  aov_model <- aov(as.formula(formula_str), data = data)
  
  # Perform Tukey HSD test
  tukey_result <- TukeyHSD(aov_model)
  
  # Extract significant pairs
  tukey_df <- as.data.frame(tukey_result[[1]]) %>%
    rownames_to_column("comparison") %>%
    filter(`p adj` < 0.05) %>%
    separate(comparison, into = c("group1", "group2"), sep = "-")
  
  return(list(
    tukey_result = tukey_result,
    significant_pairs = tukey_df,
    aov_model = aov_model
  ))
}


# ---- Prepare p1 ----
df_1 <- df_processed %>%
  # Expand categorical variables
  mutate(disturbance_both = as.character(disturbance_both)) %>%
  tidyr::separate_rows(disturbance_both, sep = ";\\s*") %>%
  mutate(disturbance_both = stringr::str_squish(disturbance_both)) %>%
  filter(!is.na(disturbance_both) & disturbance_both != "") %>%
  # Split into manipulation and type columns
  tidyr::separate(
    disturbance_both,
    into = c("disturbance_manipulation", "disturbance_type_raw"),
    sep = "_",
    extra = "merge", 
    fill  = "right"
  ) %>%
  # Clean and standardize text
  mutate(
    disturbance_manipulation = stringr::str_squish(tolower(disturbance_manipulation)),
    disturbance_type_raw     = stringr::str_squish(tolower(disturbance_type_raw)),
    disturbance_type_raw     = stringr::str_replace_all(disturbance_type_raw, "_", " ")
  ) %>%
  mutate(
    # Capitalize and clean category names
    disturbance_manipulation = case_when(
      stringr::str_detect(disturbance_manipulation, "^manip")    ~ "Manip.",
      stringr::str_detect(disturbance_manipulation, "^unmanip")  ~ "Unmanip.",
      stringr::str_detect(disturbance_manipulation, "^model")    ~ "Model",
      TRUE ~ str_to_title(disturbance_manipulation)
    ),
    disturbance_type_raw = dplyr::case_when(
      disturbance_type_raw == "landuse and infrastructure development" ~ "LID",
      disturbance_type_raw == "biological resource use"                ~ "BRU",
      TRUE ~ disturbance_type_raw
    ),
    disturbance_type = dplyr::case_when(
      disturbance_type_raw %in% c("LID","BRU") ~ disturbance_type_raw,
      TRUE ~ stringr::str_to_title(disturbance_type_raw)
    )
  ) %>%
  # Set factor levels to control order
  mutate(
    disturbance_manipulation = factor(disturbance_manipulation, levels = manipulation_order),
    disturbance_type = factor(disturbance_type, levels = type_order)) %>%
  filter(!is.na(disturbance_manipulation) & !is.na(disturbance_type))

chi_result1 <- chi_square_analysis(df_1, "disturbance_manipulation", "disturbance_type")
# ---- 1. disturbance_manipulation vs disturbance_type (Cat vs Cat) ----
plot1 <- chi_result1$data %>%
  ggplot(aes(x = disturbance_manipulation, y = disturbance_type, fill = count)) +
  geom_tile() +
  geom_text(aes(label = paste0(count, " ", residual_label)), 
            color = "white", size = 3) +
  scale_fill_viridis_c(name = "Count") +
  labs(tag = "(a)", title = paste0("Disturbance Manipulation vs Disturbance Type\n",
                      "χ² = ", round(chi_result1$chi_test$statistic, 3), 
                      ", p = ", round(chi_result1$p_value, 4)),
       x = "Disturbance Manipulation",
       y = "Disturbance Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---- Prepare p2 ----
df_2 <- df_processed %>%
  # Expand categorical variables
  expand_categorical("disturbance_manipulation") %>%
  expand_categorical("disturbance_pattern") %>%
  # Capitalize and clean category names
  mutate(
    disturbance_manipulation = case_when(
      str_to_lower(disturbance_manipulation) == "manipulated" ~ "Manip.",
      str_to_lower(disturbance_manipulation) == "unmanipulated" ~ "Unmanip.",
      TRUE ~ capitalize_words(disturbance_manipulation)
    ),
    disturbance_pattern = capitalize_words(disturbance_pattern)
  ) %>%
  # Set factor levels to control order
  mutate(
    disturbance_manipulation = factor(disturbance_manipulation, levels = manipulation_order),
    disturbance_pattern = factor(disturbance_pattern, levels = c("Press", "Pulse")))

chi_result2 <- chi_square_analysis(df_2, "disturbance_manipulation", "disturbance_pattern")
# ---- 2. disturbance_manipulation vs disturbance_pattern (Cat vs Cat) ----
plot2 <- chi_result2$data %>%
  ggplot(aes(x = disturbance_manipulation, y = count, fill = disturbance_pattern)) +
  geom_col(position = position_dodge(width = 0.9),
           colour = "black", linewidth = 0.25) +
  geom_text(aes(label = paste0(count, "\n", residual_label)), 
            position = position_dodge(width = 0.9), size = 3, color = "black") +
  scale_fill_manual(values = pattern_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(tag = "(b)", title = paste0("Disturbance Manipulation vs Disturbance Pattern\n",
                      "χ² = ", round(chi_result2$chi_test$statistic, 3), 
                      ", p = ", round(chi_result2$p_value, 4)),
       x = "Disturbance Manipulation",
       y = "Count",
       fill = "Disturbance Pattern") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Prepare p3 ----
df_3 <- df_processed %>%
  # Expand categorical variables
  expand_categorical("disturbance_type") %>%
  expand_categorical("disturbance_pattern") %>%
  # Capitalize and clean category names
  mutate(
    disturbance_type = case_when(
      str_to_lower(disturbance_type) == "landuse and infrastructure development" ~ "LID",
      str_to_lower(disturbance_type) == "biological resource use" ~ "BRU",
      TRUE ~ capitalize_words(disturbance_type)
    ),
    disturbance_pattern = capitalize_words(disturbance_pattern)
  ) %>%
  # Set factor levels to control order
  mutate(
    disturbance_type = factor(disturbance_type, levels = type_order),
    disturbance_pattern = factor(disturbance_pattern, levels = c("Press", "Pulse")))


chi_result3 <- chi_square_analysis(df_3, "disturbance_type", "disturbance_pattern")
# ---- 3. disturbance_type vs disturbance_pattern (Cat vs Cat) ----
plot3 <- chi_result3$data %>%
  ggplot(aes(x = disturbance_type, y = disturbance_pattern, fill = count)) +
  geom_tile() +
  geom_text(aes(label = paste0(count, "\n", residual_label)), 
            color = "white", size = 3) +
  scale_fill_viridis_c(name = "Count") +
  labs(tag = "(c)", title = paste0("Disturbance Type vs Disturbance Pattern\n",
                      "χ² = ", round(chi_result3$chi_test$statistic, 3), 
                      ", p = ", round(chi_result3$p_value, 4)),
       x = "Disturbance Type",
       y = "Disturbance Pattern") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ---- Prepare p4 ----
df_4 <- df_processed %>%
  # Expand categorical variables
  expand_categorical("disturbance_manipulation") %>%
  # Capitalize and clean category names
  mutate(
    disturbance_manipulation = case_when(
      str_to_lower(disturbance_manipulation) == "manipulated" ~ "Manip.",
      str_to_lower(disturbance_manipulation) == "unmanipulated" ~ "Unmanip.",
      TRUE ~ capitalize_words(disturbance_manipulation)
    )) %>%
  # Set factor levels to control order
  mutate(
    disturbance_manipulation = factor(disturbance_manipulation, levels = manipulation_order))

tukey_result1 <- perform_tukey_test(df_4, "disturbance_manipulation", "log_observation_duration")
# ---- 4. disturbance_manipulation vs log_observation_duration (Cat vs Con) ----
# Calculate y-axis range for significance brackets
y_range1 <- range(df_4$log_observation_duration, na.rm = TRUE)
y_max1 <- y_range1[2]
y_min1 <- y_range1[1]
y_expand1 <- (y_max1 - y_min1) * 0.15

plot4 <- df_4 %>%
  ggplot(aes(x = disturbance_manipulation, y = log_observation_duration, 
             fill = disturbance_manipulation)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(tag = "(d)", title = paste0("Disturbance Manipulation vs Observation Duration (ln)\n",
                      "ANOVA p = ", round(summary(tukey_result1$aov_model)[[1]]["disturbance_manipulation", "Pr(>F)"], 4)),
       x = "Disturbance Manipulation",
       y = "Observation Duration (ln days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Add significance brackets if there are significant differences
if(nrow(tukey_result1$significant_pairs) > 0) {
  comparisons <- list()
  for(i in 1:nrow(tukey_result1$significant_pairs)) {
    comparisons[[i]] <- c(tukey_result1$significant_pairs$group1[i], 
                          tukey_result1$significant_pairs$group2[i])
  }
  plot4 <- plot4 + stat_signif(comparisons = comparisons, 
                               map_signif_level = TRUE,
                               test = "t.test",
                               step_increase = 0.08,
                               tip_length = 0.02)
}

# ---- Prepare p5 ----
df_5 <- df_processed %>%
  # Expand categorical variables
  expand_categorical("disturbance_type") %>%
  # Capitalize and clean category names
  mutate(
    disturbance_type = case_when(
      str_to_lower(disturbance_type) == "landuse and infrastructure development" ~ "LID",
      str_to_lower(disturbance_type) == "biological resource use" ~ "BRU",
      TRUE ~ capitalize_words(disturbance_type)
    )) %>%
  # Set factor levels to control order
  mutate(
    disturbance_type = factor(disturbance_type, levels = type_order)) %>%
  # Remove NA values
  filter(!is.na(disturbance_type))

tukey_result2 <- perform_tukey_test(df_5, "disturbance_type", "log_observation_duration")
# ---- 5. disturbance_type vs log_observation_duration (Cat vs Con) ----
plot5 <- df_5 %>%
  ggplot(aes(x = disturbance_type, y = log_observation_duration, 
             fill = disturbance_type)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, alpha = 0.8) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(tag = "(e)", title = paste0("Disturbance Type vs Observation Duration(ln)\n",
                      "ANOVA p = ", round(summary(tukey_result2$aov_model)[[1]]["disturbance_type", "Pr(>F)"], 4)),
       x = "Disturbance Type",
       y = "Observation Duration (ln days)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Add significance brackets with better positioning for many comparisons
if(nrow(tukey_result2$significant_pairs) > 0) {
  # Create a more readable display by showing only the most important comparisons
  # or adding manual annotations
  significant_pairs_limited <- tukey_result2$significant_pairs
  
  # If there are too many pairs (>4), show only the first 4 most significant
  if(nrow(significant_pairs_limited) > 4) {
    significant_pairs_limited <- significant_pairs_limited[1:4, ]
    cat("Note: Only showing top 4 most significant pairwise comparisons for Disturbance Type plot due to space constraints.\n")
  }
  
  comparisons <- list()
  for(i in 1:nrow(significant_pairs_limited)) {
    comparisons[[i]] <- c(significant_pairs_limited$group1[i], 
                          significant_pairs_limited$group2[i])
  }
  
  plot5 <- plot5 + 
    stat_signif(comparisons = comparisons, 
                map_signif_level = TRUE,
                test = "t.test",
                step_increase = 0.08,
                tip_length = 0.01,
                textsize = 2.5) +
    annotate("text", x = Inf, y = Inf, 
             label = paste("Showing", nrow(significant_pairs_limited), "of", nrow(tukey_result2$significant_pairs), "significant pairs"),
             hjust = 1, vjust = 1, size = 2.5, color = "gray50")
}

# ---- Prepare p6 ----
df_6 <- df_processed %>%
  # Expand categorical variables
  expand_categorical("disturbance_pattern") %>%
  # Capitalize and clean category names
  mutate(
    disturbance_pattern = capitalize_words(disturbance_pattern)
  ) %>%
  # Set factor levels to control order
  mutate(
    disturbance_pattern = factor(disturbance_pattern, levels = c("Press", "Pulse")))

tukey_result3 <- perform_tukey_test(df_6, "disturbance_pattern", "log_observation_duration")

# ---- 6. disturbance_pattern vs log_observation_duration (Cat vs Con) ----
plot6 <- df_6 %>%
  ggplot(aes(x = disturbance_pattern, y = log_observation_duration, 
             fill = disturbance_pattern)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = pattern_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(tag = "(f)", title = paste0("Disturbance Pattern vs Observation Duration (ln)\n",
                      "ANOVA p = ", round(summary(tukey_result3$aov_model)[[1]]["disturbance_pattern", "Pr(>F)"], 4)),
       x = "Disturbance Pattern",
       y = "Observation Duration (ln days)") +
  theme_minimal() +
  theme(legend.position = "none")

# Add significance brackets if there are significant differences
if(nrow(tukey_result3$significant_pairs) > 0) {
  comparisons <- list()
  for(i in 1:nrow(tukey_result3$significant_pairs)) {
    comparisons[[i]] <- c(tukey_result3$significant_pairs$group1[i], 
                          tukey_result3$significant_pairs$group2[i])
  }
  plot6 <- plot6 + stat_signif(comparisons = comparisons, 
                               map_signif_level = TRUE,
                               test = "t.test",
                               step_increase = 0.08,
                               tip_length = 0.02)
}

# ---- Combine all plots ----
combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, 
                              ncol = 2, nrow = 3)
# ---- Save the plot ----
ggsave("figure/figure4.png", combined_plot, 
       width = 14, height = 12, dpi = 300)