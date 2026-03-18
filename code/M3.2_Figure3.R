#### Module 3.2: Figure 3
rm(list = ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# ---- Import the data ----
df <- read.csv("data/Gz.full.data.sheet_594.csv")

# ---- Load required packages ----
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(tidyr)
library(stringr)

# ---- Step 1: Extract three columns ----
selected_data <- df %>%
  dplyr::select(level, approach, target_variable_group)

# ---- Step 2: Handle multi-value fields (separated by ;) ----
expand_data <- selected_data %>%
  # Create unique ID for each row
  mutate(id = row_number()) %>%
  # Separate level
  separate_rows(level, sep = ";") %>%
  # Separate approach
  separate_rows(approach, sep = ";") %>%
  # Separate target_variable_group
  separate_rows(target_variable_group, sep = ";") %>%
  # Remove white space
  mutate(
    level = str_trim(level),
    approach = str_trim(approach),
    target_variable_group = str_trim(target_variable_group)
  ) %>%
  # Remove empty values
  filter(
    !is.na(level) & level != "",
    !is.na(approach) & approach != "",
    !is.na(target_variable_group) & target_variable_group != ""
  )

# ---- Step 3: Rename approach values ----
processed_data <- expand_data %>%
  mutate(
    approach = case_when(
      approach == "field observation" ~ "FO",
      approach == "modeling and simulation" ~ "MD",
      approach == "indoor experiment" ~ "LM",
      approach == "field experiment" ~ "FM",
      TRUE ~ approach  # Keep other values unchanged
    )
  )

# ---- Step 4: Process target_variable_group and filter ----
final_data <- processed_data %>%
  # Replace dots with spaces
  mutate(target_variable_group = str_replace_all(target_variable_group, "\\.", " ")) %>%
  # Keep only specified categories
  filter(target_variable_group %in% c(
    "quantity", "structure", "process based indicator",
    "functional response", "environmental context"
  )) %>%
  # Rename columns
  rename(
    Level = level,
    Target_variable = target_variable_group,
    Approach = approach
  ) %>%
  dplyr::select(Level, Approach, Target_variable)

# ---- Step 5: Convert to title case ----
final_data[] <- lapply(final_data, function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  } else {
    x
  }
})

# ---- Step 6: Define variable order ----
level_order <- c("Landscape", "Ecosystem", "Community", "Population", "Individual")
approach_order <- c("FO", "FM", "LM", "MD")
target_order <- c(
  "Environmental context",
  "Functional response",
  "Structure",
  "Process based indicator",
  "Quantity"
)

# ---- Step 7: Prepare data for Sankey plot ----
alluvial_data <- final_data %>%
  mutate(
    Level = factor(Level, levels = level_order),
    Approach = factor(Approach, levels = approach_order),
    Target_variable = factor(Target_variable, levels = target_order)
  ) %>%
  count(Level, Approach, Target_variable, name = "freq") %>%
  filter(freq > 0)

# ---- Step 8: Define colour scheme (by Level) ----
level_colors <- c(
  "Landscape" = "#458AFCFF",
  "Ecosystem" = "#34F395FF",
  "Community" = "#96FE44FF",
  "Population" = "#F7C13AFF",
  "Individual" = "#E7490CFF"
)

# Check unique Level values
print("Unique Level values:")
print(unique(alluvial_data$Level))

# ---- Step 9: Create Sankey plot ----
p <- ggplot(alluvial_data,
            aes(y = freq, axis1 = Level, axis2 = Approach, axis3 = Target_variable)) +
  geom_alluvium(aes(fill = Level),
                width = 1/12, alpha = 0.7, curve_type = "cubic") +
  geom_stratum(width = 1/12, fill = "white", color = "black", size = 0.5) +
  # Add labels only to Approach column
  geom_text(
    stat = "stratum",
    aes(label = ifelse(x == 2, as.character(after_stat(stratum)), "")),
    size = 3, color = "black"
  ) +
  scale_x_discrete(
    limits = c("Level", "Approach", "Measured Variable"),
    labels = c("Level", "Approach", "Measured Variable"),
    expand = c(0.15, 0.05)
  ) +
  scale_fill_manual(values = level_colors, name = "Level") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold"),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.margin = margin(20, 40, 20, 40)
  ) +
  labs(title = "Data Flow: Level → Approach → Measured Variable", x = "")

# ---- Step 10: Add side labels ----
level_positions <- alluvial_data %>%
  group_by(Level) %>%
  summarise(total_freq = sum(freq), .groups = "drop") %>%
  arrange(desc(Level)) %>%
  mutate(
    cumsum_freq = cumsum(total_freq),
    y_pos = cumsum_freq - total_freq / 2
  )

target_positions <- alluvial_data %>%
  group_by(Target_variable) %>%
  summarise(total_freq = sum(freq), .groups = "drop") %>%
  arrange(desc(Target_variable)) %>%
  mutate(
    cumsum_freq = cumsum(total_freq),
    y_pos = cumsum_freq - total_freq / 2
  ) %>%
  mutate(
    Target_variable_label = case_when(
      Target_variable == "Environmental context" ~ "Environmental\ncontext",
      Target_variable == "Functional response"   ~ "Functional\nresponse",
      Target_variable == "Process based indicator" ~ "Process-based\nindicator",
      TRUE ~ as.character(Target_variable)
    )
  )

p_final <- p +
  annotate("text", x = 0.95, y = level_positions$y_pos,
           label = level_positions$Level, hjust = 1,
           size = 3.5, color = "black") +
  annotate("text", x = 3.05, y = target_positions$y_pos,
           label = target_positions$Target_variable_label,
           hjust = 0, size = 3.5, color = "black")


# ---- Export plots to figure/ folder ----
print(p_final)
# Ensure output folder exists
out_dir <- file.path(getwd(), "figure")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1) Standard figure (800 x 600 px) at 300 dpi
png(filename = file.path(out_dir, "figure3.png"),
    width = 3000/300, height = 1800/300, res = 300, units = "in")
print(p_final)
dev.off()
