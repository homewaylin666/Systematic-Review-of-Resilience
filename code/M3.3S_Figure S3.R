#### Module 3.3S: Figure S3
rm(list = ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# ---- Import the data ----
df <- read.csv("data/Gz.full.data.sheet_594.csv")

# ---- Load required packages ----
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(gridExtra)
library(scales)

# ---- Define custom colour ----
bar_color <- "#3F4788FF"

# ---- Helper: label/title casing ----
capitalize_words <- function(x) {
  str_to_title(gsub("_", " ", x))
}

# ---- Count helper: expand ONE var and count unique studies ----
count_var_unique_ids <- function(data, var) {
  d <- data %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    separate_rows(!!sym(var), sep = ";") %>%
    mutate(!!sym(var) := str_trim(!!sym(var))) %>%
    filter(.data[[var]] != "")
  
  denom <- d %>% distinct(id) %>% nrow()
  
  if (var == "disturbance_manipulation") {
    d <- d %>%
      mutate(disturbance_manipulation = case_when(
        str_detect(tolower(disturbance_manipulation), "^manip")   ~ "Manip.",
        str_detect(tolower(disturbance_manipulation), "^unmanip") ~ "Unmanip.",
        str_detect(tolower(disturbance_manipulation), "^model")   ~ "Model",
        TRUE ~ capitalize_words(disturbance_manipulation)
      ))
  } else if (var == "disturbance_type") {
    d <- d %>%
      mutate(disturbance_type = case_when(
        tolower(disturbance_type) == "landuse and infrastructure development" ~ "LID",
        tolower(disturbance_type) == "biological resource use"               ~ "BRU",
        TRUE ~ capitalize_words(disturbance_type)
      ))
  } else if (var == "disturbance_pattern") {
    d <- d %>% mutate(disturbance_pattern = capitalize_words(disturbance_pattern))
  }
  
  out <- d %>%
    distinct(id, !!sym(var)) %>%
    count(!!sym(var), name = "n_studies") %>%
    mutate(pct = 100 * n_studies / denom)
  
  list(data = out, denom = denom)
}

# ---- Plot helpers ----
plot_bar_with_pct <- function(df_counts, xvar, xlab, title, thin = FALSE) {
  width_val <- if (thin) 0.6 else 0.9
  ggplot(df_counts, aes(x = !!sym(xvar), y = n_studies)) +
    geom_col(fill = bar_color, colour = "black", linewidth = 0.3, width = width_val) +
    geom_text(aes(label = paste0(sprintf("%.1f", pct), "%")),
              vjust = -0.4, size = 3.2) +
    labs(x = xlab, y = "Studies (count)", title = title) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    theme_minimal() +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      plot.title   = element_text(size = 12, face = "bold")
    )
}

# ——— Fixed: histogram uses log10 axis directly with simple label ———
plot_hist_log10_simple <- function(data, raw_var, title) {
  d <- data %>%
    filter(!is.na(.data[[raw_var]]), .data[[raw_var]] > 0) %>%
    mutate(log10_val = log10(.data[[raw_var]]))
  
  # simple pretty breaks on log10 values (e.g. 0, 1, 2, …)
  bks <- pretty(d$log10_val)
  
  ggplot(d, aes(x = log10_val)) +
    geom_histogram(fill = bar_color, colour = "black", bins = 20, linewidth = 0.3) +
    labs(x = "Observation Duration (log10 days)", y = "Studies (count)", title = title) +
    scale_x_continuous(breaks = bks, labels = bks) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

# ---- Prepare counts (each variable expanded independently) ----
res_manip   <- count_var_unique_ids(df, "disturbance_manipulation")
res_type    <- count_var_unique_ids(df, "disturbance_type")
res_pattern <- count_var_unique_ids(df, "disturbance_pattern")

manipulation_order <- c("Model", "Manip.", "Unmanip.")
type_order <- c("Fire", "Climatic", "Hydrological", "Geophysical", "Chemical", 
                "Resource", "Biotic", "LID", "BRU", "Structural")
pattern_order <- c("Press", "Pulse")

manip_counts <- res_manip$data %>%
  mutate(disturbance_manipulation = factor(disturbance_manipulation, levels = manipulation_order)) %>%
  arrange(disturbance_manipulation)

type_counts <- res_type$data %>%
  mutate(disturbance_type = factor(disturbance_type, levels = type_order)) %>%
  arrange(disturbance_type)

pattern_counts <- res_pattern$data %>%
  mutate(disturbance_pattern = factor(disturbance_pattern, levels = pattern_order)) %>%
  arrange(disturbance_pattern)

# ---- Panels with succinct titles ----
p1 <- plot_bar_with_pct(
  manip_counts, "disturbance_manipulation",
  xlab  = "Disturbance Manipulation",
  title = "(a) Disturbance Manipulation",
  thin  = TRUE
)

p2 <- plot_bar_with_pct(
  type_counts, "disturbance_type",
  xlab  = "Disturbance Type",
  title = "(b) Disturbance Type",
  thin  = FALSE
)

p3 <- plot_bar_with_pct(
  pattern_counts, "disturbance_pattern",
  xlab  = "Disturbance Pattern",
  title = "(c) Disturbance Pattern",
  thin  = TRUE
)

p4 <- plot_hist_log10_simple(
  data = df, raw_var = "observation_duration",
  title = "(d) Observation Duration"
)

# ---- Combine all plots ----
combined_plot_S3 <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# ---- Save the plot ----
ggsave("figure/figureS3.png", combined_plot_S3, width = 10, height = 7.5, dpi = 300)
