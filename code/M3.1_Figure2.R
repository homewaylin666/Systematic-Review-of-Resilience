####  Module 3.1: Figure 2 - Real Count Stacked Area Chart
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# Import the data
df <- read.csv("data/Gz.full.data.sheet_594_edited.csv")

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

# Step 1: Create new dataframe with selected columns and renamed columns
df_now <- df %>%
  dplyr::select(paper_id = id, Quantification = quantification, Year = year)

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

# Step 3: Process Quantification column
df_now <- df_now %>%
  mutate(
    Quantification = case_when(
      # First: Change all elements containing ";" to "multidimensional quantification"
      grepl(";", Quantification) ~ "multidimensional quantification",
      
      # Second: For elements containing "_", keep only characters to the left of "_"
      grepl("_", Quantification) ~ sub("_.*", "", Quantification),
      
      # Otherwise keep original value
      TRUE ~ Quantification
    )
  ) %>%
  mutate(
    Quantification = case_when(
      # Third: Rename specific categories
      Quantification == "inferred" ~ "inference",
      # Fourth: Keep the five main categories, change others to "others"
      Quantification %in% c("recovery degree", "recovery rate", "recovery time", "multidimensional quantification", "inference") ~ Quantification,
      
      # All other categories become "others"
      TRUE ~ "others"
    )
  )

# Convert to title case
df_now[] <- lapply(df_now, function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  } else {
    x
  }
})

# Step 4: Prepare data for visualization
# Set factor levels in REVERSE order for proper stacking
quantification_levels <- c("Inference", "Others", "Recovery time", "Recovery rate", 
                           "Recovery degree", "Multidimensional quantification")

# Calculate actual counts by period
viz_data_periods <- df_now %>%
  group_by(Year_Period, Period_Label, Quantification) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Ensure all factor levels are present
  mutate(Quantification = factor(Quantification, levels = quantification_levels)) %>%
  complete(Year_Period, Quantification, fill = list(count = 0)) %>%
  # Add Period_Label back after complete
  mutate(
    Period_Label = case_when(
      Year_Period == 2000 ~ "Before 2001",
      TRUE ~ paste0(Year_Period, "-", Year_Period + 4)
    )
  ) %>%
  # Remove any rows with invalid data
  filter(!is.na(Year_Period), !is.na(Quantification)) %>%
  # Create a sequential position variable to fix spacing issue
  arrange(Year_Period) %>%
  mutate(Position = as.numeric(factor(Year_Period)))

# Step 5: Define colors for the six categories
category_colors <- c(
  "Inference" = "#440154FF",
  "Others" = "#E8E419FF",
  "Recovery time" = "#94D840FF",
  "Recovery rate" = "#74D055FF",
  "Recovery degree" = "#3CBC75FF",
  "Multidimensional quantification" = "#228C8DFF"
)

# Create figure directory if it doesn't exist
if (!dir.exists("figure")) {
  dir.create("figure")
}

############################ Step 6: Create Simple Stacked Area Chart (using Position for equal spacing) ############################### 
p1 <- ggplot() +
  # Add stacked area chart using actual counts and Position for x-axis
  geom_area(data = viz_data_periods, 
            aes(x = Position, y = count, fill = Quantification), 
            position = "stack", alpha = 0.8) +
  
  # Customize colors
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # Set up y-axis for actual counts
  scale_y_continuous(
    name = "Paper Number",
    breaks = function(x) pretty(x, n = 6),
    expand = expansion(mult = c(0, 0.02))  # Start from 0 and add small margin at top
  ) +
  
  # Customize x-axis with equal spacing
  scale_x_continuous(
    name = "Year Period", 
    breaks = unique(viz_data_periods$Position),
    labels = function(x) {
      # Map position back to period labels
      labels <- c()
      for(pos in x) {
        period_data <- viz_data_periods[viz_data_periods$Position == pos, ]
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
    title = "Stacked Area Chart of Resilience Quantification by Time Periods",
    subtitle = "Pre-2001 data grouped together, then 5-year periods from 2001 onwards"
  )

# Display and save the plot
print(p1)
ggsave("figure/figure2.num.png", plot = p1, width = 2000/300, height = 1500/300, 
       dpi = 300, units = "in")

############################### Step 7: 100% Stacked with White Trend Line ###############################
# Calculate percentages and total counts for each period
viz_data_percentage <- viz_data_periods %>%
  group_by(Position, Period_Label) %>%
  mutate(
    total = sum(count),
    percentage = ifelse(total > 0, count / total * 100, 0)
  ) %>%
  ungroup()

# Get total counts by period for the white trend line
total_counts_by_position <- viz_data_percentage %>%
  group_by(Position, Period_Label) %>%
  summarise(total = first(total), .groups = "drop")

# Calculate proper scaling factor for white line
max_total <- max(total_counts_by_position$total)

# Create 100% stacked area chart with white trend line
p2 <- ggplot() +
  # Add stacked area chart using percentages
  geom_area(data = viz_data_percentage, 
            aes(x = Position, y = percentage, fill = Quantification), 
            position = "stack", alpha = 0.8) +
  
  # Add white trend line for total sample size (scaled to fit 0-100%)
  geom_line(data = total_counts_by_position, 
            aes(x = Position, y = total * 100 / max_total), 
            color = "white", linewidth = 1.2, alpha = 0.9) +
  
  # Add points for the trend line
  geom_point(data = total_counts_by_position, 
             aes(x = Position, y = total * 100 / max_total), 
             color = "white", size = 2, alpha = 0.9) +
  
  # Customize colors
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # Set up dual y-axes with proper scaling
  scale_y_continuous(
    name = "Percentage (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    sec.axis = sec_axis(
      transform = ~ . * max_total / 100,  # Convert percentage back to actual sample size
      name = "Paper Number",
      breaks = function(x) pretty(x, n = 5)
    )
  ) +
  
  # Customize x-axis with equal spacing
  scale_x_continuous(
    name = "Year Period", 
    breaks = unique(viz_data_percentage$Position),
    labels = function(x) {
      labels <- c()
      for(pos in x) {
        period_data <- viz_data_percentage[viz_data_percentage$Position == pos, ]
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
    title = "100% Stacked Area Chart of Resilience Quantification by Time Periods",
    subtitle = "White line represents total paper number per period (5-year periods)"
  )

# Display and save the percentage plot
print(p2)
ggsave("figure/figure2.pro.png", plot = p2, width = 2000/300, height = 1500/300, 
       dpi = 300, units = "in")

############################### Step 8: 3-Year Periods Version (Post-1998) ###############################
# Create 3-year periods with special handling for pre-1999
df_now_3yr <- df %>%
  dplyr::select(paper_id = id, Quantification = quantification, Year = year) %>%
  mutate(
    # Create 3-year periods, with pre-1999 as one group
    Year_Period = case_when(
      Year <= 1998 ~ 1998,  # Group all pre-1999 (including 1998) as 1998
      TRUE ~ floor((Year - 1999) / 3) * 3 + 1999
    ),
    # Create period labels for better display
    Period_Label = case_when(
      Year_Period == 1998 ~ "Before 1999",
      TRUE ~ paste0(Year_Period, "-", Year_Period + 2)
    )
  )

# Process Quantification column (same as before)
df_now_3yr <- df_now_3yr %>%
  mutate(
    Quantification = case_when(
      grepl(";", Quantification) ~ "multidimensional quantification",
      grepl("_", Quantification) ~ sub("_.*", "", Quantification),
      TRUE ~ Quantification
    )
  ) %>%
  mutate(
    Quantification = case_when(
      Quantification == "inferred" ~ "inference",
      Quantification %in% c("recovery degree", "recovery rate", "recovery time", "multidimensional quantification", "inference") ~ Quantification,
      TRUE ~ "others"
    )
  )

# Convert to title case
df_now_3yr[] <- lapply(df_now_3yr, function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  } else {
    x
  }
})

# Calculate actual counts by 3-year period
viz_data_3yr <- df_now_3yr %>%
  group_by(Year_Period, Period_Label, Quantification) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(Quantification = factor(Quantification, levels = quantification_levels)) %>%
  complete(Year_Period, Quantification, fill = list(count = 0)) %>%
  mutate(
    Period_Label = case_when(
      Year_Period == 1998 ~ "Before 1999",
      TRUE ~ paste0(Year_Period, "-", Year_Period + 2)
    )
  ) %>%
  filter(!is.na(Year_Period), !is.na(Quantification)) %>%
  arrange(Year_Period) %>%
  mutate(Position = as.numeric(factor(Year_Period)))

# Calculate percentages for 3-year periods
viz_data_3yr_percentage <- viz_data_3yr %>%
  group_by(Position, Period_Label) %>%
  mutate(
    total = sum(count),
    percentage = ifelse(total > 0, count / total * 100, 0)
  ) %>%
  ungroup()

# Get total counts by period for the white trend line
total_counts_3yr <- viz_data_3yr_percentage %>%
  group_by(Position, Period_Label) %>%
  summarise(total = first(total), .groups = "drop")

# Calculate proper scaling factor for white line
max_total_3yr <- max(total_counts_3yr$total)

# Create 100% stacked area chart with white trend line (3-year periods)
p3 <- ggplot() +
  # Add stacked area chart using percentages
  geom_area(data = viz_data_3yr_percentage, 
            aes(x = Position, y = percentage, fill = Quantification), 
            position = "stack", alpha = 0.8) +
  
  # Add white trend line for total sample size (scaled to fit 0-100%)
  geom_line(data = total_counts_3yr, 
            aes(x = Position, y = total * 100 / max_total_3yr), 
            color = "white", linewidth = 1.2, alpha = 0.9) +
  
  # Add points for the trend line
  geom_point(data = total_counts_3yr, 
             aes(x = Position, y = total * 100 / max_total_3yr), 
             color = "white", size = 2, alpha = 0.9) +
  
  # Customize colors
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # Set up dual y-axes with proper scaling
  scale_y_continuous(
    name = "Percentage (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    sec.axis = sec_axis(
      transform = ~ . * max_total_3yr / 100,
      name = "Paper Number",
      breaks = function(x) pretty(x, n = 5)
    )
  ) +
  
  # Customize x-axis with equal spacing
  scale_x_continuous(
    name = "Year Period", 
    breaks = unique(viz_data_3yr_percentage$Position),
    labels = function(x) {
      labels <- c()
      for(pos in x) {
        period_data <- viz_data_3yr_percentage[viz_data_3yr_percentage$Position == pos, ]
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
    title = "100% Stacked Area Chart of Resilience Quantification by Time Periods",
    subtitle = "White line represents total paper number per period (3-year periods)"
  )

# Display and save the 3-year percentage plot
# print(p3) #too ugly

ggsave("figure/figure2.pro3.png", plot = p3, width = 2000/300, height = 1500/300, 
       dpi = 300, units = "in")


############################### Step 9: 1-Year Periods Version (From 1996) ###############################
# Create 1-year periods with special handling for pre-1996
df_now_1yr <- df %>%
  dplyr::select(paper_id = id, Quantification = quantification, Year = year) %>%
  mutate(
    # Create 1-year periods, with pre-1996 as one group
    Year_Period = case_when(
      Year <= 1995 ~ 1995,  # Group all pre-1996 (including 1995) as 1995
      TRUE ~ Year  # Each year is its own period from 1996 onwards
    ),
    # Create period labels for better display
    Period_Label = case_when(
      Year_Period == 1995 ~ "Before 1996",
      TRUE ~ as.character(Year_Period)
    )
  )

# Process Quantification column (same as before)
df_now_1yr <- df_now_1yr %>%
  mutate(
    Quantification = case_when(
      grepl(";", Quantification) ~ "multidimensional quantification",
      grepl("_", Quantification) ~ sub("_.*", "", Quantification),
      TRUE ~ Quantification
    )
  ) %>%
  mutate(
    Quantification = case_when(
      Quantification == "inferred" ~ "inference",
      Quantification %in% c("recovery degree", "recovery speed", "recovery time", "multidimensional quantification", "inference") ~ Quantification,
      TRUE ~ "others"
    )
  )

# Convert to title case
df_now_1yr[] <- lapply(df_now_1yr, function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
  } else {
    x
  }
})

# Calculate actual counts by 1-year period
viz_data_1yr <- df_now_1yr %>%
  group_by(Year_Period, Period_Label, Quantification) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(Quantification = factor(Quantification, levels = quantification_levels)) %>%
  complete(Year_Period, Quantification, fill = list(count = 0)) %>%
  mutate(
    Period_Label = case_when(
      Year_Period == 1995 ~ "Before 1996",
      TRUE ~ as.character(Year_Period)
    )
  ) %>%
  filter(!is.na(Year_Period), !is.na(Quantification)) %>%
  arrange(Year_Period) %>%
  mutate(Position = as.numeric(factor(Year_Period)))

# Calculate percentages for 1-year periods
viz_data_1yr_percentage <- viz_data_1yr %>%
  group_by(Position, Period_Label) %>%
  mutate(
    total = sum(count),
    percentage = ifelse(total > 0, count / total * 100, 0)
  ) %>%
  ungroup()

# Get total counts by period for the white trend line
total_counts_1yr <- viz_data_1yr_percentage %>%
  group_by(Position, Period_Label) %>%
  summarise(total = first(total), .groups = "drop")

# Calculate proper scaling factor for white line
max_total_1yr <- max(total_counts_1yr$total)

# Create 100% stacked area chart with white trend line (1-year periods)
p4 <- ggplot() +
  # Add stacked area chart using percentages
  geom_area(data = viz_data_1yr_percentage, 
            aes(x = Position, y = percentage, fill = Quantification), 
            position = "stack", alpha = 0.8) +
  
  # Add white trend line for total sample size (scaled to fit 0-100%)
  geom_line(data = total_counts_1yr, 
            aes(x = Position, y = total * 100 / max_total_1yr), 
            color = "white", linewidth = 1.2, alpha = 0.9) +
  
  # Add points for the trend line
  geom_point(data = total_counts_1yr, 
             aes(x = Position, y = total * 100 / max_total_1yr), 
             color = "white", size = 2, alpha = 0.9) +
  
  # Customize colors
  scale_fill_manual(values = category_colors, name = "Resilience Quantification") +
  
  # Set up dual y-axes with proper scaling
  scale_y_continuous(
    name = "Percentage (%)",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    sec.axis = sec_axis(
      transform = ~ . * max_total_1yr / 100,
      name = "Paper Number",
      breaks = function(x) pretty(x, n = 5)
    )
  ) +
  
  # Customize x-axis with equal spacing
  scale_x_continuous(
    name = "Year", 
    breaks = unique(viz_data_1yr_percentage$Position),
    labels = function(x) {
      labels <- c()
      for(pos in x) {
        period_data <- viz_data_1yr_percentage[viz_data_1yr_percentage$Position == pos, ]
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
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # 90 degree rotation for yearly labels
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  
  # Update title
  labs(
    title = "100% Stacked Area Chart of Resilience Quantification by Year",
    subtitle = "White line represents total paper number per year (annual data from 1996)"
  )

# Display and save the 1-year percentage plot
print(p4)
ggsave("figure/figure2.pro1.png", plot = p4, width = 2000/300, height = 1500/300, 
       dpi = 300, units = "in")

