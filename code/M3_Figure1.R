#### Module 3: Figure 1
rm(list=ls())
library(here)

# ---- import the data and package----
df <- read.csv(here("data", "Gz.full.data.sheet_594_edited.csv"))
# import the packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

###### Part 1: UpSet Plot ######
# Step 1: get 'observed' data
filtered_data <- df %>%
  filter(measurement == "observed") %>%
  dplyr::select(id, quantification)

# Step 2: Process underscore values
clean_underscore <- function(value) {
  parts <- unlist(strsplit(value, ";", fixed = TRUE))
  cleaned_parts <- sapply(parts, function(part) {
    part <- trimws(part)
    if (grepl("_", part)) {
      return(sub("_.*", "", part))
    } else {
      return(part)
    }
  })
  cleaned_parts <- cleaned_parts[cleaned_parts != ""]
  return(paste(cleaned_parts, collapse = ";"))
}

processed_data <- filtered_data %>%
  mutate(cleaned_definition = sapply(quantification, clean_underscore))

# Step 3: Find all unique terms (should be 7)
all_terms <- processed_data %>%
  separate_rows(cleaned_definition, sep = ";") %>%
  mutate(cleaned_definition = trimws(cleaned_definition)) %>%
  filter(cleaned_definition != "") %>%
  count(cleaned_definition, sort = TRUE)

# Use all terms (all 7 terms)
all_7_terms <- all_terms$cleaned_definition

# Step 4: Convert to binary matrix format
all_categories <- all_7_terms

create_binary_matrix <- function(data, value_col, categories) {
  binary_matrix <- matrix(0, nrow = nrow(data), ncol = length(categories))
  colnames(binary_matrix) <- categories
  
  for (i in 1:nrow(data)) {
    values <- unlist(strsplit(data[[value_col]][i], ";", fixed = TRUE))
    values <- trimws(values)
    values <- values[values != ""]
    
    for (category in categories) {
      if (category %in% values) {
        binary_matrix[i, category] <- 1
      }
    }
  }
  return(as.data.frame(binary_matrix))
}

binary_data <- create_binary_matrix(processed_data, "cleaned_definition", all_categories)
colnames(binary_data) <- gsub("^(.)", "\\U\\1", colnames(binary_data), perl = TRUE)

# Step 5: plotting
#### Using ComplexUpset #####
library(ComplexUpset)

# Convert to logical for ComplexUpset
binary_data_logical <- binary_data %>%
  mutate(across(everything(), ~ as.logical(.x)))

# Define custom colors for all 7 categories
n_categories <- ncol(binary_data)
# These colors are from viridis
custom_colors <- c(
  "#74D055FF",  # Recovery speed
  "#3CBC75FF",  # Recovery degree
  "#94D840FF",  # Recovery time
  "#2D718EFF",  # Resistance
  "#3F4788FF",  # Latitude
  "#E8E419FF",  # Other
  "#481568FF"  # Invariability
)

side_bar_colors <- custom_colors[1:n_categories]
names(side_bar_colors) <- colnames(binary_data)

# Create color mapping for ComplexUpset
color_mapping <- side_bar_colors
names(color_mapping) <- names(side_bar_colors)

# ComplexUpset plot with custom colors
upset_plot_complex <- ComplexUpset::upset(
  binary_data_logical,
  intersect = colnames(binary_data_logical),
  name = "Resilience Quantification",
  width_ratio = 0.1,
  set_sizes = upset_set_size(
    geom = geom_bar(
      mapping = aes(fill = group),
      width = 0.8
    )
  ) +
    scale_fill_manual(values = color_mapping, guide = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

###### Display and save the plots ######
# Create figure directory if it doesn't exist
dir.create("figure", showWarnings = FALSE)

# For UpSetR plot - Save to PNG
png(here("figure", "figure1.chart.png", width = 2100, height = 1575, res = 300))
UpSetR::upset(
  binary_data,
  sets = colnames(binary_data),
  sets.bar.color = side_bar_colors,
  main.bar.color = "#3F4788FF",
  matrix.color = "#3F4788FF",
  order.by = "freq",
  decreasing = TRUE,
  mb.ratio = c(0.7, 0.3),
  text.scale = c(1.3, 1.3, 1.2, 1.2, 1.5, 1.2),
  mainbar.y.label = "Resilience Quantification",
  sets.x.label = "Component Frequency"
)
dev.off()
cat("Figure 1 chart saved to: figure/figure1.chart.png\n")

###### Part 2: Pie chart ######
df_pie <- read.csv("data/Gz.full.data.sheet_594.csv")

df_pie$measurement <- paste0(
  toupper(substring(df_pie$measurement, 1, 1)),
  substring(df_pie$measurement, 2))
df_pie$measurement <- recode(df_pie$measurement, 
                             "Observed" = "Direct-response",
                             "Inferred" = "Inference-based")
df_pie$measurement <- factor(df_pie$measurement, levels = c("Direct-response", "Inference-based"))
# remove NA
df_pie <- df_pie %>% filter(!is.na(measurement))

counts <- df_pie %>%
  count(measurement) %>%
  arrange(measurement) %>%
  mutate(
    percent = n / sum(n) * 100,
    label   = paste0(round(percent, 1), "%")
  )
print("Counts data:")
print(counts)

my_colors <- c(
  "Direct-response" = "#3F4788FF",   
  "Inference-based" = "#440154FF"    
)

counts <- counts %>%
  mutate(
    frac = n / sum(n),
    ymax = cumsum(frac),
    ymin = lag(ymax, default = 0),
    mid  = (ymin + ymax) / 2
  )

direct_mid <- counts$mid[counts$measurement == "Direct-response"]
if (length(direct_mid) > 0 && !is.na(direct_mid)) {
  start_angle <- -pi/6 - pi * direct_mid
} else {
  start_angle <- 0
}

# plotting
png(here("figure", "figure1.pie.png", width = 3000, height = 2000, res = 300))
print(
  ggplot(counts, aes(x = 1, y = frac, fill = measurement)) +
    geom_col(width = 1, color = "white") +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              color = "white", size = 16) +
    scale_fill_manual(values = my_colors) +
    coord_polar(theta = "y", start = start_angle) +
    theme_void() +
    theme(legend.text = element_text(size = 20), legend.key.size = unit(1.5, "cm")) +
    ggtitle("Measurement Pie")
)
dev.off()
cat("Figure 1 pie chart saved to: figure/figure1.pie.png\n")
