# M5 Figure 7: The relation between quantification and its improtant factors
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")

# ---- import the data and package ----
df <- read.csv("Gb.Final_datasheet_406.csv")
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(scales)

# ---- deal with quantification ----
# Step 1: Clean underscore (keep your existing function)
clean_underscore <- function(value) {
  # Split by semicolon first
  parts <- unlist(strsplit(value, ";", fixed = TRUE))
  
  # For each part, if it contains underscore, keep only the part before underscore
  cleaned_parts <- sapply(parts, function(part) {
    part <- trimws(part)  # Remove whitespace
    if (grepl("_", part)) {
      return(sub("_.*", "", part))  # Remove everything from _ onwards
    } else {
      return(part)  # Keep as is if no underscore
    }
  })
  
  # Remove empty parts and return as semicolon-separated string
  cleaned_parts <- cleaned_parts[cleaned_parts != ""]
  return(paste(cleaned_parts, collapse = ";"))
}

# Apply the cleaning function
df <- df %>%
  mutate(cleaned_quantification = sapply(quantification, clean_underscore))

# Step 2: Process according to new rules
process_quantification <- function(value) {
  # Convert to lowercase for case-insensitive matching
  value_lower <- tolower(value)
  
  # Check if both "recovery" and "resistance" are present
  if (grepl("recovery", value_lower) && grepl("resistance", value_lower)) {
    return("recovery+resistance")
  }
  
  # Check if there's a semicolon (multiple dimensions)
  if (grepl(";", value)) {
    return("other multidimensional")
  }
  
  # Otherwise, return the cleaned single value
  return(trimws(value))
}

# Apply the new processing rules
df <- df %>%
  mutate(processed_quantification = sapply(cleaned_quantification, process_quantification))

# Step 3: Find top 6 most frequent values after processing
quantification_counts <- df %>%
  count(processed_quantification, sort = TRUE) %>%
  filter(processed_quantification != "")

# Get top 8 terms
top_8_terms <- quantification_counts %>%
  slice_head(n = 8) %>%
  pull(processed_quantification)

print("Top 8 most frequent terms after processing:")
print(top_8_terms)
print("Their frequencies:")
print(quantification_counts %>% slice_head(n = 8))

# Step 4: Replace non-top-6 terms with "Others"
df <- df %>%
  mutate(final_quantification = ifelse(
    processed_quantification %in% top_8_terms,
    processed_quantification,
    "others"
  ))

# Step 5: Set the order
quantification_order <- c(
  "inferred",
  "others",
  "tolerance",
  "invariability",
  "recovery time",
  "recovery speed",
  "recovery degree",
  "recovery+resistance",
  "other multidimensional"
)

# Step 6: Set the color
category_colors <- c(
  "inferred" = "#440154CC",
  "others" = "#E8E419CC",
  "tolerance" = "#3F4788CC",
  "invariability" = "#3A2C78CC",
  "recovery time" = "#94D840CC",
  "recovery speed" = "#74D055CC",
  "recovery degree" = "#3CBC75CC",
  "recovery+resistance" = "#228C8DCC",
  "other multidimensional" = "#2DB6F1CC"
)

# ---- split those multi-value ----
df_unique.level <- df %>%
  # split those multi-value: level
  separate_rows(level, sep = ";\\s*") %>%
  mutate(level = str_trim(level)) 

df_unique.pattern <- df %>%
  filter(quantification != "inferred") %>% 
  # split those multi-value: disturbance_pattern
  separate_rows(disturbance_pattern, sep = ";\\s*") %>%
  mutate(disturbance_pattern = str_trim(disturbance_pattern)) 

df_unique.approach <- df %>%
  # split those multi-value: approach
  separate_rows(approach, sep = ";\\s*") %>%
  mutate(approach = str_trim(approach)) 

df_unique.disturbance_type <- df %>%
  # There are only two geophysical, so I merge it with hydrological
  mutate(disturbance_type = str_replace_all(disturbance_type, "geophysical|hydrological", "hydro/geophysical")) %>%
  # Fix an error
  mutate(disturbance_type = if_else(
      disturbance_type == "biotic; abiotic","biotic; chemical; climatic",disturbance_type)) %>% 
  # split those multi-value: disturbance_type
  separate_rows(disturbance_type, sep = ";\\s*") %>%
  mutate(disturbance_type = str_trim(disturbance_type))

df_unique.target.group <- df %>%
  # split those multi-value: target variable group
  separate_rows(target_variable_group, sep = ";\\s*") %>%
  mutate(target_variable_group = str_trim(target_variable_group))

df_unique.target.type <- df %>%
  # split those multi-value: target variable type
  separate_rows(target_variable_type, sep = ";\\s*") %>%
    mutate(target_variable_type = str_trim(target_variable_type))

# ---- deal with level ----
level_order <- c("individual","population","community","ecosystem","landscape")
counts1 <- df_unique.level %>%
  filter(!is.na(level)) %>%
  mutate(
    level = tolower(level),
    level = factor(level, levels = level_order)
  ) %>%

  group_by(level, final_quantification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(level) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()

# ---- P1: with level ----
# set the order
counts1$final_quantification <- factor(counts1$final_quantification, 
levels = quantification_order)
# create the picture
p1 <- ggplot(counts1, aes(x = level, y = prop, fill = final_quantification)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # set the color
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different level
  geom_line(
    data = counts1 %>% distinct(level, total),
    aes(x = level, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts1 %>% distinct(level, total),
    aes(x = level, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two y axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts1$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by Study Level")

# print
print(p1)

# ---- P1b: with level (less quantification categories) ----
counts1_less <- counts1 %>%
  mutate(final_quantification_less = case_when(
    final_quantification %in% c("tolerance", "invariability","others") ~ "others",
    final_quantification %in% c("recovery+resistance", "other multidimensional") ~ "multidimensional",
    TRUE ~ as.character(final_quantification))) %>% 
  group_by(level, final_quantification_less) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(level) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()

quantification_order_less <- c(
  "inferred",
  "others",
  "recovery time",
  "recovery speed",
  "recovery degree",
  "multidimensional")

category_colors_less <- c(
  "inferred" = "#440154CC",
  "others" = "#E8E419CC",
  "recovery time" = "#94D840CC",
  "recovery speed" = "#74D055CC",
  "recovery degree" = "#3CBC75CC",
  "multidimensional" = "#228C8DCC")

counts1_less$final_quantification_less <- factor(counts1_less$final_quantification_less, 
                                                     levels = quantification_order_less)
p1b <- ggplot(counts1_less, aes(x = level, y = prop, fill = final_quantification_less)) +
  geom_bar(stat = "identity") +
  coord_flip() + scale_fill_manual(values = category_colors_less, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different level
  geom_line(
    data = counts1_less %>% distinct(level, total),
    aes(x = level, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts1_less %>% distinct(level, total),
    aes(x = level, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two x axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts1_less$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Resilience Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by Study Level (Less Quantification Categories)")

# print
print(p1b)
  
  
# ---- P2: with disturbance pattern ----
counts2 <- df_unique.pattern %>%
  filter(!is.na(disturbance_pattern)) %>%
  group_by(disturbance_pattern, final_quantification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(disturbance_pattern) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()

counts2$final_quantification <- factor(counts2$final_quantification, 
                                       levels = quantification_order)

p2 <- ggplot(counts2, aes(x = disturbance_pattern, y = prop, fill = final_quantification)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # set the color
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different level
  geom_line(
    data = counts2 %>% distinct(disturbance_pattern, total),
    aes(x = disturbance_pattern, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts2 %>% distinct(disturbance_pattern, total),
    aes(x = disturbance_pattern, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two y axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts2$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by disturbance pattern")

# print
print(p2)

# ---- deal with approach ----
approach_order <- c("modeling and simulation","indoor experiment","field experiment","field observation")
counts3 <- df_unique.approach %>%
  filter(!is.na(approach)) %>%
  mutate(
    approach = tolower(approach),
    approach = factor(approach, levels = approach_order)
  ) %>%
  
  group_by(approach, final_quantification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(approach) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()

# ---- P3: with approach ----
# set the order
counts3$final_quantification <- factor(counts3$final_quantification, 
                                       levels = quantification_order)
# create the picture
p3 <- ggplot(counts3, aes(x = approach, y = prop, fill = final_quantification)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # set the color
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different approach
  geom_line(
    data = counts3 %>% distinct(approach, total),
    aes(x = approach, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts3 %>% distinct(approach, total),
    aes(x = approach, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two y axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts1$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by Approach")

# print
print(p3)

# ---- P4: with observe duration ----
library(ggpubr)
# 1) Data prep: keep finite positives, drop "inferred", then log-transform.
counts4 <- df %>%
  filter(
    !is.na(final_quantification),
    final_quantification != "inferred",
    !is.na(observation_duration),
    observation_duration > 0
  ) %>%
  mutate(
    observation_duration = log(observation_duration),  # natural log
    # reverse order so the current bottom goes to the top
    final_quantification = factor(final_quantification, levels = rev(quantification_order))
  )

# 2) Pre-compute range for tidy annotation and tight axis limits
y_min  <- min(counts4$observation_duration, na.rm = TRUE)
y_max  <- max(counts4$observation_duration, na.rm = TRUE)
y_rng  <- y_max - y_min

# Smaller top room (enough for the global p-value but no large blank space)
top_expand    <- 0.10   # ~10% of data range above the top
bottom_expand <- 0.08   # small room for 'n=' labels

# Sample sizes for per-group labels
n_df <- counts4 %>% count(final_quantification, name = "n")

# Define pairwise comparisons (shrink later if you have too many groups)
my_comparisons <- if (length(levels(counts4$final_quantification)) >= 2) {
  combn(levels(counts4$final_quantification), 2, simplify = FALSE)
} else {
  list()
}

# 3) Build p4
p4 <- ggplot(
  counts4,
  aes(x = final_quantification, y = observation_duration, fill = final_quantification)
) +
  geom_boxplot(width = 0.65, alpha = 0.8, outlier.shape = 16, outlier.size = 0.9) +
  
  # Optional jitter to show distribution; comment out if too dense
  geom_jitter(
    aes(color = final_quantification),
    width = 0.12, height = 0, alpha = 0.18, size = 0.7, show.legend = FALSE
  ) +
  
  # Keep your palette; drop = FALSE only if you want to keep missing levels in the legend
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  scale_color_manual(values = category_colors, guide = "none") +
  
  labs(
    x = "Quantification",
    y = "ln(Observation Duration)",
    fill = "Quantification",
    title = "Observation duration by quantification method"
  ) +
  
  # Tighter expansion (no secondary axis)
  scale_y_continuous(
    expand = expansion(mult = c(bottom_expand, top_expand))
  ) +
  
  # Match p2's horizontal look
  coord_flip() +
  
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 12),
    legend.position = "bottom",
    legend.title    = element_text(size = 11),
    legend.text     = element_text(size = 10)
  ) +
  
  # Global ANOVA p-value placed just above the top whiskers (no big extra margin)
  stat_compare_means(
    method   = "anova",
    label    = "p.format",
    label.y  = y_max + top_expand * y_rng * 0.85,
    size     = 3
  )

# Add pairwise comparisons (adjusted) only if there are >= 2 groups
if (length(my_comparisons) > 0) {
  p4 <- p4 +
    stat_compare_means(
      comparisons     = my_comparisons,
      method          = "t.test",
      p.adjust.method = "BH",
      label           = "p.signif",
      hide.ns         = TRUE,
      step.increase   = 0.06,  # slightly tighter spacing
      tip.length      = 0.01
    )
}

# Add per-group sample size near the left margin (below boxes)
p4 <- p4 +
  geom_text(
    data = n_df,
    aes(x = final_quantification,
        y = y_min - bottom_expand * y_rng * 0.9,
        label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  )

# Print
print(p4)

# ---- P4b: with observe duration ----
library(rstatix)
# Compute upper whisker per group (boxplot.stats()[5] = upper whisker)
upper_df <- counts4 %>%
  group_by(final_quantification) %>%
  summarise(upper = boxplot.stats(observation_duration)$stats[5], .groups = "drop")

y_min <- min(counts4$observation_duration, na.rm = TRUE)
y_max <- max(counts4$observation_duration, na.rm = TRUE)
y_rng <- y_max - y_min

# Pairwise t-tests (BH corrected), keep only significant, and place brackets just above whiskers
stat_sig <- counts4 %>%
  pairwise_t_test(observation_duration ~ final_quantification,
                  p.adjust.method = "BH") %>%
  add_significance(p.col = "p.adj") %>%
  filter(p.adj <= 0.05) %>%
  arrange(p.adj) %>%
  add_xy_position(x = "final_quantification") %>%   # adds xmin/xmax
  left_join(upper_df, by = c("group1" = "final_quantification")) %>%
  rename(upper1 = upper) %>%
  left_join(upper_df, by = c("group2" = "final_quantification")) %>%
  rename(upper2 = upper) %>%
  mutate(
    base_y     = pmax(upper1, upper2),
    # put the first bracket just above the taller whisker, then stack by small steps
    y.position = base_y + (row_number()) * 0.03 * y_rng
  )

# Recompute top expansion just enough to fit the highest bracket (if needed)
top_need <- ifelse(nrow(stat_sig) > 0,
                   (max(stat_sig$y.position) - y_max) / y_rng + 0.04, 0.06)

p4 <- p4 +
  scale_y_continuous(expand = expansion(mult = c(0.08, max(0.06, top_need)))) +
  stat_pvalue_manual(
    stat_sig,
    label        = "p.adj.signif",   # or "p.adj.formatted" for exact p
    tip.length   = 0.01,
    bracket.size = 0.4,
    size         = 5
  )

print(p4)



# ---- deal with target variable group ----
target.group_order <- c("quantity","structure","process based indicator","functional response","environmental context")
counts5 <- df_unique.target.group %>%
  filter(!is.na(target_variable_group)) %>%
  mutate(
    target_variable_group = tolower(target_variable_group),
    target_variable_group = factor(target_variable_group, levels = target.group_order)
  ) %>%
  
  group_by(target_variable_group, final_quantification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(target_variable_group) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()

# ---- P5: with target variable group ----
# set the order
counts5$final_quantification <- factor(counts5$final_quantification, 
                                       levels = quantification_order)
# create the picture
p5 <- ggplot(counts5, aes(x = target_variable_group, y = prop, fill = final_quantification)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # set the color
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different target_variable_group
  geom_line(
    data = counts5 %>% distinct(target_variable_group, total),
    aes(x = target_variable_group, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts5 %>% distinct(target_variable_group, total),
    aes(x = target_variable_group, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two y axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts1$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by target variable")

# print
print(p5)

# ---- deal with target_variable_type ----
top10 <- df_unique.target.type %>%
  count(target_variable_type, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(target_variable_type)
df_unique.target.type <- df_unique.target.type %>%
  mutate(
    target_variable_type = if_else(
      target_variable_type %in% top10,
      target_variable_type,
      "others"
    ))

target.type_order <- c("others", "abiotic parameter", "growth", "physiological indicator", "ecosystem function", "demography parameter", "diversity","composition","biomass","abundance", "cover")
counts5b <- df_unique.target.type %>%
  filter(!is.na(target_variable_type)) %>%
  mutate(
    target_variable_type = tolower(target_variable_type),
    target_variable_type = factor(target_variable_type, levels = target.type_order)
  ) %>%
  
  group_by(target_variable_type, final_quantification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(target_variable_type) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()


# ---- P5b: with target variable type ----
# set the order
counts5b$final_quantification <- factor(counts5b$final_quantification, 
                                       levels = quantification_order)
# create the picture
p5b <- ggplot(counts5b, aes(x = target_variable_type, y = prop, fill = final_quantification)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # set the color
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different target_variable_group
  geom_line(
    data = counts5b %>% distinct(target_variable_type, total),
    aes(x = target_variable_type, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts5b %>% distinct(target_variable_type, total),
    aes(x = target_variable_type, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two y axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts1$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by target variable type")

# print
print(p5b)
# ---- deal with habitat type ----
top5 <- df %>%
  count(habitat_type, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(habitat_type)
df_habitat_type <- df %>%
  mutate(
    habitat_type = if_else(
      habitat_type %in% top5,
      habitat_type,
      "others"
    ))

habitat_type_order <- c("others", "marine.neritic", "coastal", "wetland", "grassland", "forest")
counts6 <- df_habitat_type %>%
  filter(!is.na(habitat_type)) %>%
  mutate(
    habitat_type = tolower(habitat_type),
    habitat_type = factor(habitat_type, levels = habitat_type_order)
  ) %>%
  
  group_by(habitat_type, final_quantification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(habitat_type) %>%
  mutate(
    total = sum(n),
    prop  = n / total * 100
  ) %>%
  ungroup()

# ---- P6: with habitat type ----
counts6$final_quantification <- factor(counts6$final_quantification, 
                                       levels = quantification_order)
# create the picture
p6 <- ggplot(counts6, aes(x = habitat_type, y = prop, fill = final_quantification)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  # set the color
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # proportion in the bar
  geom_text(aes(label = paste0(round(prop,1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  
  # Red line to show the counts of different habitat_type
  geom_line(
    data = counts6 %>% distinct(habitat_type, total),
    aes(x = habitat_type, y = total / max(total) * 100, group = 1),
    inherit.aes = FALSE,
    color = "red", size = 1
  ) +
  geom_point(
    data = counts6 %>% distinct(habitat_type, total),
    aes(x = habitat_type, y = total / max(total) * 100),
    inherit.aes = FALSE,
    color = "red", size = 2
  ) +
  
  # So we got two y axis
  scale_y_continuous(
    name    = "Proportion (%)",
    sec.axis = sec_axis(
      ~ . * max(counts1$total) / 100,
      name = "Total Count"
    )
  ) +
  
  labs(x = NULL, fill = "Quantification") +
  theme_bw() +
  theme(
    axis.text.y        = element_text(size = 12),
    axis.title.y.right = element_text(color = "red"),
    axis.text.y.right  = element_text(color = "red")
  ) +
  ggtitle("Quantification by habitat type")

# print
print(p6)
# ---- integrate all figures ----
library(patchwork)
# remove the legend
p.level <- p1 + theme(legend.position = "none")
p.pattern <- p2 + theme(legend.position = "none")
p.approach <- p3 + theme(legend.position = "none")
p.duration <- p4 + theme(legend.position = "none")
p.target.type <- p5b + theme(legend.position = "none")
p.target <- p5 + theme(legend.position = "none")
p.habitat <- p6 + theme(legend.position = "none")

combined_plot <- (p.pattern + p.duration) / 
  (p.target) # + p.approach

print(combined_plot)
# 1600 * 1200
