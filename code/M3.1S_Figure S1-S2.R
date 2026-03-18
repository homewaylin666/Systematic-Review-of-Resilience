#### Module 3.1S: Figure S1-S2
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# Import the data
df <- read.csv("data/Gz.full.data.sheet_594.csv")

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)
library(forcats)

# Create figure directory if it doesn't exist
if (!dir.exists("figure")) {
  dir.create("figure")
}

# Step 1: Create new dataframe with selected columns and renamed columns
df_now <- df %>%
  dplyr::select(paper_id = id, framework = framework, level = level, Year = year)

# Step 2: Create 5-year periods with special handling for pre-2001
df_now <- df_now %>%
  mutate(
    # Create 5-year periods, with pre-2001 as one group
    Year_Period = case_when(
      Year <= 2000 ~ 2000,  # Group all pre-2001 (including 2000) as 2000
      TRUE ~ floor((Year - 2001) / 5) * 5 + 2001
    ),
    # Create period labels for better display
    Period_Label = case_when(
      Year_Period == 2000 ~ "Before 2001",
      TRUE ~ paste0(Year_Period, "-", Year_Period + 4)
    )
  )

########################################
# Step 3A: Process framework categories
########################################
df_now <- df_now %>%
  mutate(framework_norm = str_squish(str_to_lower(framework))) %>%
  mutate(
    Framework_Final = case_when(
      framework_norm %in% c("none") ~ "No framework",
      framework_norm %in% c("large scale framework", "large-scale framework") ~ "Large-scale framework",
      framework_norm %in% c("others/null = resilience+resistance", "others/null=resilience+resistance",
                            "others/null - resilience+resistance", "others/null") ~ "Unintegrated framework",
      framework_norm %in% c("resilience framework", "resilience") ~ "Resilience",
      framework_norm %in% c("stability=resilience+resistance(+others)",
                            "stability = resilience+resistance(+others)",
                            "stability") ~ "Stability",
      framework_norm %in% c("other") ~ "Other framework",
      TRUE ~ "Other framework"
    )
  )

# Define factor order for stacking (REVERSE order for proper stacking)
framework_levels <- c("Other framework", "Large-scale framework", "Resilience",
                      "Stability", "Unintegrated framework", "No framework")

# Manual colors for the six frameworks
framework_colors <- c(
  "No framework"           = "#30123BFF",
  "Unintegrated framework" = "#4686FBFF",
  "Stability"              = "#1AE4B6FF",
  "Resilience"             = "#A2FC3CFF",
  "Large-scale framework"  = "#FABA39FF",
  "Other framework"        = "#7A0403FF"
)

##############################################################
# Step 4A: Prepare framework data for visualization
##############################################################
viz_data_framework <- df_now %>%
  filter(!is.na(Year_Period), !is.na(Framework_Final)) %>%
  group_by(Year_Period, Period_Label, Framework_Final) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Ensure all factor levels are present
  mutate(Framework_Final = factor(Framework_Final, levels = framework_levels)) %>%
  complete(Year_Period, Framework_Final, fill = list(count = 0)) %>%
  # Add Period_Label back after complete
  mutate(
    Period_Label = case_when(
      Year_Period == 2000 ~ "Before 2001",
      TRUE ~ paste0(Year_Period, "-", Year_Period + 4)
    )
  ) %>%
  # Remove any rows with invalid data
  filter(!is.na(Year_Period), !is.na(Framework_Final)) %>%
  # Create a sequential position variable to fix spacing issue
  arrange(Year_Period) %>%
  mutate(Position = as.numeric(factor(Year_Period)))

############################### Figure S1: Framework 100% Stacked ###############################
# Calculate percentages and total counts for each period
viz_data_framework_percentage <- viz_data_framework %>%
  group_by(Position, Period_Label) %>%
  mutate(
    total = sum(count),
    percentage = ifelse(total > 0, count / total * 100, 0)
  ) %>%
  ungroup()

# Get total counts by period for the white trend line
total_counts_framework <- viz_data_framework_percentage %>%
  group_by(Position, Period_Label) %>%
  summarise(total = first(total), .groups = "drop")

# Calculate proper scaling factor for white line
max_total_framework <- max(total_counts_framework$total)

# Create 100% stacked area chart with white trend line
p_framework <- ggplot() +
  # Add stacked area chart using percentages
  geom_area(data = viz_data_framework_percentage, 
            aes(x = Position, y = percentage, fill = Framework_Final), 
            position = "stack", alpha = 0.8) +
  
  # Add white trend line for total sample size (scaled to fit 0-100%)
  geom_line(data = total_counts_framework, 
            aes(x = Position, y = total * 100 / max_total_framework), 
            color = "white", linewidth = 1.2, alpha = 0.9) +
  
  # Add points for the trend line
  geom_point(data = total_counts_framework, 
             aes(x = Position, y = total * 100 / max_total_framework), 
             color = "white", size = 2, alpha = 0.9) +
  
  # Customize colors
  scale_fill_manual(values = framework_colors, name = "Framework") +
  
  # Set up dual y-axes with proper scaling
  scale_y_continuous(
    name = "Percentage (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    sec.axis = sec_axis(
      transform = ~ . * max_total_framework / 100,  # Convert percentage back to actual sample size
      name = "Paper Number",
      breaks = function(x) pretty(x, n = 5)
    )
  ) +
  
  # Customize x-axis with equal spacing
  scale_x_continuous(
    name = "Year Period", 
    breaks = unique(viz_data_framework_percentage$Position),
    labels = function(x) {
      labels <- c()
      for(pos in x) {
        period_data <- viz_data_framework_percentage[viz_data_framework_percentage$Position == pos, ]
        if(nrow(period_data) > 0) {
          labels <- c(labels, unique(period_data$Period_Label)[1])
        }
      }
      return(labels)
    }
  ) +
  
  # Apply theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  
  # Update title
  labs(
    title = "100% Stacked Area Chart of Framework by Time Periods",
    subtitle = "White line represents total paper number per period"
  )

# Display and save the percentage plot
print(p_framework)
ggsave("figure/figureS2.framework.trend.png", plot = p_framework, 
       width = 4000/300, height = 3000/300, dpi = 300, units = "in")

############################################
# Step 3B: Create df.level by splitting ';'
############################################
df.level <- df_now %>%
  separate_rows(level, sep="\\s*;\\s*") %>%
  mutate(
    level = str_squish(level),
    level = case_when(
      str_to_lower(level) == "landscape" ~ "Landscape",
      str_to_lower(level) == "ecosystem" ~ "Ecosystem",
      str_to_lower(level) == "community" ~ "Community",
      str_to_lower(level) == "population" ~ "Population",
      str_to_lower(level) == "individual" ~ "Individual",
      TRUE ~ level
    )
  )

# Define factor order for stacking (REVERSED - now starts from Landscape)
level_levels <- c("Landscape", "Ecosystem", "Community", "Population", "Individual")

level_colors <- c(
  "Landscape"  = "#458AFCFF",
  "Ecosystem"  = "#34F395FF",
  "Community"  = "#96FE44FF",
  "Population" = "#F7C13AFF",
  "Individual" = "#E7490CFF"
)

##############################################################
# Step 4B: Prepare level data for visualization
##############################################################
viz_data_level <- df.level %>%
  filter(!is.na(Year_Period), !is.na(level)) %>%
  group_by(Year_Period, Period_Label, level) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Ensure all factor levels are present
  mutate(level = factor(level, levels = level_levels)) %>%
  complete(Year_Period, level, fill = list(count = 0)) %>%
  # Add Period_Label back after complete
  mutate(
    Period_Label = case_when(
      Year_Period == 2000 ~ "Before 2001",
      TRUE ~ paste0(Year_Period, "-", Year_Period + 4)
    )
  ) %>%
  # Remove any rows with invalid data
  filter(!is.na(Year_Period), !is.na(level)) %>%
  # Create a sequential position variable to fix spacing issue
  arrange(Year_Period) %>%
  mutate(Position = as.numeric(factor(Year_Period)))

############################### Figure S2: Level 100% Stacked ###############################
# Calculate percentages and total counts for each period
viz_data_level_percentage <- viz_data_level %>%
  group_by(Position, Period_Label) %>%
  mutate(
    total = sum(count),
    percentage = ifelse(total > 0, count / total * 100, 0)
  ) %>%
  ungroup()

# Get total counts by period for the white trend line
total_counts_level <- viz_data_level_percentage %>%
  group_by(Position, Period_Label) %>%
  summarise(total = first(total), .groups = "drop")

# Calculate proper scaling factor for white line
max_total_level <- max(total_counts_level$total)

# Create 100% stacked area chart with white trend line
p_level <- ggplot() +
  # Add stacked area chart using percentages
  geom_area(data = viz_data_level_percentage, 
            aes(x = Position, y = percentage, fill = level), 
            position = "stack", alpha = 0.8) +
  
  # Add white trend line for total sample size (scaled to fit 0-100%)
  geom_line(data = total_counts_level, 
            aes(x = Position, y = total * 100 / max_total_level), 
            color = "white", linewidth = 1.2, alpha = 0.9) +
  
  # Add points for the trend line
  geom_point(data = total_counts_level, 
             aes(x = Position, y = total * 100 / max_total_level), 
             color = "white", size = 2, alpha = 0.9) +
  
  # Customize colors
  scale_fill_manual(values = level_colors, name = "Level") +
  
  # Set up dual y-axes with proper scaling
  scale_y_continuous(
    name = "Percentage (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    sec.axis = sec_axis(
      transform = ~ . * max_total_level / 100,  # Convert percentage back to actual sample size
      name = "Paper Number",
      breaks = function(x) pretty(x, n = 5)
    )
  ) +
  
  # Customize x-axis with equal spacing
  scale_x_continuous(
    name = "Year Period", 
    breaks = unique(viz_data_level_percentage$Position),
    labels = function(x) {
      labels <- c()
      for(pos in x) {
        period_data <- viz_data_level_percentage[viz_data_level_percentage$Position == pos, ]
        if(nrow(period_data) > 0) {
          labels <- c(labels, unique(period_data$Period_Label)[1])
        }
      }
      return(labels)
    }
  ) +
  
  # Apply theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  
  # Update title
  labs(
    title = "100% Stacked Area Chart of Level by Time Periods",
    subtitle = "White line represents total paper number per period"
  )

# Display and save the percentage plot
print(p_level)
ggsave("figure/figureS1.level.trend.png", plot = p_level, 
       width = 4000/300, height = 3000/300, dpi = 300, units = "in")