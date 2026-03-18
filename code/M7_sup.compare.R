#### Journal Comparison Supplementary Figures
#### Compares "first-half selected journals" vs "remaining journals"
rm(list = ls())
library(here)
setwd(here())

# ---- Load packages ----
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggalluvial)
library(UpSetR)
library(grid)
library(cowplot)
library(magick)   # for UpSet panel stacking

# ---- Output directory ----
out_dir <- "figure/journal.compare"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Group labels ----
LABEL_A <- "First-Half Selected Journals"
LABEL_B <- "Remaining Journals"

# ---- The 7 selected journals (lowercase to match raw data) ----
selected_journals <- c(
  "nature",
  "science",
  "proceedings of the national academy of sciences",
  "nature ecology & evolution",
  "ecological indicators",
  "ecology letters",
  "molecular ecology"
)

# ---- Load data ----
df <- read.csv("data/Gz.full.data.sheet_594_edited.csv")

# ---- Split (single source dataset) ----
df_A <- df %>% filter(trimws(tolower(journal)) %in%  selected_journals)
df_B <- df %>% filter(!trimws(tolower(journal)) %in% selected_journals)

# Convenience aliases used by each figure section
df_edited_A <- df_A;  df_edited_B <- df_B
df_raw_A    <- df_A;  df_raw_B    <- df_B

cat(sprintf("Group A (%s): %d rows\n", LABEL_A, nrow(df_A)))
cat(sprintf("Group B (%s): %d rows\n", LABEL_B, nrow(df_B)))
stopifnot(nrow(df_A) > 0, nrow(df_B) > 0)

# ==============================================================================
# COLOUR PALETTES
# ==============================================================================

upset_bar_colors_raw <- c(
  "#74D055FF", "#3CBC75FF", "#94D840FF",
  "#2D718EFF", "#3F4788FF", "#E8E419FF", "#481568FF"
)

# Categories listed bottom-to-top as they appear in the stacked area.
# Multidimensional quantification at bottom, Inference at top.
quantification_levels <- c(
  "Multidimensional quantification", "Recovery degree", "Recovery rate",
  "Recovery time", "Others", "Inference"
)

category_colors <- c(
  "Inference"                       = "#440154FF",
  "Others"                          = "#E8E419FF",
  "Recovery time"                   = "#94D840FF",
  "Recovery rate"                   = "#74D055FF",
  "Recovery degree"                 = "#3CBC75FF",
  "Multidimensional quantification" = "#228C8DFF"
)

level_colors <- c(
  "Landscape"  = "#458AFCFF",
  "Ecosystem"  = "#34F395FF",
  "Community"  = "#96FE44FF",
  "Population" = "#F7C13AFF",
  "Individual" = "#E7490CFF"
)

pie_colors <- c(
  "Direct-response" = "#3F4788FF",
  "Inference-based" = "#440154FF"
)

# ==============================================================================
# FIGURE 1A: UpSet plots
# Strategy: exactly mirror the original working code — png() -> upset() -> dev.off()
# Then use magick to add a label banner above each panel and stack vertically.
# Do NOT use readPNG / ggdraw (causes black image due to alpha channel issues).
# ==============================================================================

clean_underscore <- function(value) {
  parts <- unlist(strsplit(value, ";", fixed = TRUE))
  cleaned <- sapply(parts, function(p) {
    p <- trimws(p)
    if (grepl("_", p)) sub("_.*", "", p) else p
  })
  paste(cleaned[cleaned != ""], collapse = ";")
}

create_binary_matrix <- function(data, value_col, categories) {
  mat <- matrix(0, nrow = nrow(data), ncol = length(categories))
  colnames(mat) <- categories
  for (i in seq_len(nrow(data))) {
    vals <- trimws(unlist(strsplit(data[[value_col]][i], ";", fixed = TRUE)))
    vals <- vals[vals != ""]
    for (cat in categories) if (cat %in% vals) mat[i, cat] <- 1
  }
  as.data.frame(mat)
}

prep_upset_data <- function(df_sub) {
  filtered <- df_sub %>%
    filter(measurement == "observed") %>%
    dplyr::select(id, quantification) %>%
    mutate(cleaned = sapply(quantification, clean_underscore))
  
  all_terms <- filtered %>%
    separate_rows(cleaned, sep = ";") %>%
    mutate(cleaned = trimws(cleaned)) %>%
    filter(cleaned != "") %>%
    count(cleaned, sort = TRUE)
  
  bin <- create_binary_matrix(filtered, "cleaned", all_terms$cleaned)
  colnames(bin) <- gsub("^(.)", "\\U\\1", colnames(bin), perl = TRUE)
  bin
}

bin_A <- prep_upset_data(df_edited_A)
bin_B <- prep_upset_data(df_edited_B)

# Align columns
all_cols <- union(colnames(bin_A), colnames(bin_B))
for (col in setdiff(all_cols, colnames(bin_A))) bin_A[[col]] <- 0
for (col in setdiff(all_cols, colnames(bin_B))) bin_B[[col]] <- 0
bin_A <- bin_A[, all_cols]
bin_B <- bin_B[, all_cols]
set_colors <- setNames(upset_bar_colors_raw[seq_along(all_cols)], all_cols)

# Render each panel exactly as in the original working code
tmp_A <- file.path(out_dir, "_tmp_upset_A.png")
tmp_B <- file.path(out_dir, "_tmp_upset_B.png")

png(tmp_A, width = 2100, height = 2520, res = 300)
UpSetR::upset(
  bin_A,
  sets            = colnames(bin_A),
  sets.bar.color  = set_colors,
  main.bar.color  = "#3F4788FF",
  matrix.color    = "#3F4788FF",
  order.by        = "freq",
  decreasing      = TRUE,
  mb.ratio        = c(0.7, 0.3),
  text.scale      = c(1.3, 1.3, 1.2, 1.2, 1.5, 1.2),
  mainbar.y.label = "Resilience Quantification",
  sets.x.label    = "Component Frequency"
)
dev.off()

png(tmp_B, width = 2100, height = 2520, res = 300)
UpSetR::upset(
  bin_B,
  sets            = colnames(bin_B),
  sets.bar.color  = set_colors,
  main.bar.color  = "#3F4788FF",
  matrix.color    = "#3F4788FF",
  order.by        = "freq",
  decreasing      = TRUE,
  mb.ratio        = c(0.7, 0.3),
  text.scale      = c(1.3, 1.3, 1.2, 1.2, 1.5, 1.2),
  mainbar.y.label = "Resilience Quantification",
  sets.x.label    = "Component Frequency"
)
dev.off()

# Use magick to add panel label and stack — avoids all alpha/background issues
add_label_banner <- function(img_path, label_text) {
  img   <- image_read(img_path)
  info  <- image_info(img)
  w     <- info$width
  
  # Create a white label strip above the plot
  banner <- image_blank(w, 80, color = "white") %>%
    image_annotate(label_text, size = 54, color = "black",
                   font = "sans", weight = 700,
                   gravity = "West", location = "+20+0")
  image_append(c(banner, img), stack = TRUE)
}

img_A <- add_label_banner(tmp_A, paste0("(a) ", LABEL_A))
img_B <- add_label_banner(tmp_B, paste0("(b) ", LABEL_B))

combined <- image_append(c(img_A, img_B), stack = FALSE)  # side-by-side
image_write(combined,
            file.path(out_dir, "fig1_upset_compare.png"),
            format = "png", density = 500)

# Clean up temp files
file.remove(tmp_A, tmp_B)
cat("Saved: fig1_upset_compare.png\n")

# ==============================================================================
# FIGURE 1B: Pie charts
# ==============================================================================

prep_pie <- function(df_sub) {
  df_sub %>%
    mutate(
      measurement = paste0(toupper(substring(measurement, 1, 1)),
                           substring(measurement, 2)),
      measurement = recode(measurement,
                           "Observed" = "Direct-response",
                           "Inferred" = "Inference-based"),
      measurement = factor(measurement,
                           levels = c("Direct-response", "Inference-based"))
    ) %>%
    filter(!is.na(measurement)) %>%
    count(measurement) %>%
    arrange(measurement) %>%
    mutate(
      frac  = n / sum(n),
      label = paste0(round(frac * 100, 1), "%"),
      ymax  = cumsum(frac),
      ymin  = lag(ymax, default = 0),
      mid   = (ymin + ymax) / 2
    )
}

make_pie <- function(counts, title_str) {
  dm <- counts$mid[counts$measurement == "Direct-response"]
  sa <- if (length(dm) > 0 && !is.na(dm[1])) -pi/6 - pi * dm[1] else 0
  ggplot(counts, aes(x = 1, y = frac, fill = measurement)) +
    geom_col(width = 1, color = "white") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5),
              color = "white", size = 5) +
    scale_fill_manual(values = pie_colors, name = "Measurement") +
    coord_polar(theta = "y", start = sa) +
    theme_void() +
    theme(legend.text     = element_text(size = 12),
          legend.key.size = unit(1.2, "cm"),
          plot.title      = element_text(size = 13, hjust = 0.5)) +
    ggtitle(title_str)
}

ggsave(file.path(out_dir, "fig1_pie_A.png"),
       make_pie(prep_pie(df_raw_A), LABEL_A),
       width = 5, height = 4, dpi = 500, units = "in")
ggsave(file.path(out_dir, "fig1_pie_B.png"),
       make_pie(prep_pie(df_raw_B), LABEL_B),
       width = 5, height = 4, dpi = 500, units = "in")
cat("Saved: fig1_pie_A.png, fig1_pie_B.png\n")

# ==============================================================================
# FIGURE 2: Smooth stacked area charts — num and pro, each 2-panel
#
# geom_area(position='stack') triggers a stat_align bug in ggplot2 >= 3.5
# causing unit() to fail. Fix: use geom_ribbon with pre-computed ymin/ymax.
# geom_ribbon is visually identical (smooth filled bands) but uses stat_identity.
# ==============================================================================

prep_trend_data <- function(df_sub) {
  d <- df_sub %>%
    dplyr::select(Quantification = quantification, Year = year) %>%
    filter(!is.na(Year), !is.na(Quantification), Quantification != "") %>%
    mutate(
      Year_Period = case_when(
        Year <= 2000 ~ 2000L,
        TRUE ~ as.integer(floor((Year - 2001) / 5) * 5 + 2001)
      ),
      Quantification = case_when(
        grepl(";", Quantification) ~ "Multidimensional quantification",
        grepl("_", Quantification) ~ {
          b <- sub("_.*", "", Quantification)
          paste0(toupper(substring(b, 1, 1)), substring(b, 2))
        },
        TRUE ~ paste0(toupper(substring(Quantification, 1, 1)),
                      substring(Quantification, 2))
      ),
      Quantification = case_when(
        Quantification == "Inferred" ~ "Inference",
        Quantification %in% quantification_levels ~ Quantification,
        TRUE ~ "Others"
      )
    )
  
  counts <- d %>% count(Year_Period, Quantification, name = "count")
  present_periods <- sort(unique(counts$Year_Period))
  present_cats    <- quantification_levels[
    quantification_levels %in% unique(counts$Quantification)
  ]
  stopifnot(length(present_cats) > 0, length(present_periods) > 0)
  
  expand.grid(
    Year_Period    = present_periods,
    Quantification = present_cats,
    stringsAsFactors = FALSE
  ) %>%
    left_join(counts, by = c("Year_Period", "Quantification")) %>%
    mutate(
      count          = ifelse(is.na(count), 0L, as.integer(count)),
      Period_Label   = ifelse(Year_Period == 2000L, "Before 2001",
                              paste0(Year_Period, "-", Year_Period + 4L)),
      Quantification = factor(Quantification, levels = present_cats),
      Position       = as.integer(factor(as.character(Year_Period),
                                         levels = as.character(present_periods)))
    ) %>%
    arrange(Position, Quantification)
}

viz_A <- prep_trend_data(df_edited_A)
viz_B <- prep_trend_data(df_edited_B)

cat("viz_A:", nrow(viz_A), "rows | NAs:", sum(is.na(viz_A)),
    "| cats:", paste(levels(viz_A$Quantification), collapse = ", "), "\n")
cat("viz_B:", nrow(viz_B), "rows | NAs:", sum(is.na(viz_B)),
    "| cats:", paste(levels(viz_B$Quantification), collapse = ", "), "\n")

# Pre-compute stacked ribbon coords (bottom-to-top in factor level order)
add_ribbon_coords <- function(viz_data) {
  viz_data %>%
    arrange(Position, Quantification) %>%
    group_by(Position) %>%
    mutate(ymax = cumsum(count), ymin = ymax - count) %>%
    ungroup()
}

add_ribbon_coords_pct <- function(viz_data) {
  viz_data %>%
    group_by(Position, Period_Label) %>%
    mutate(total      = sum(count),
           percentage = ifelse(total > 0, count / total * 100, 0)) %>%
    ungroup() %>%
    arrange(Position, Quantification) %>%
    group_by(Position) %>%
    mutate(ymax = cumsum(percentage), ymin = ymax - percentage) %>%
    ungroup()
}

safe_colors <- function(viz_data) {
  category_colors[levels(viz_data$Quantification)]
}

make_x_scale <- function(viz_data, show_labels = TRUE) {
  pl <- viz_data %>%
    dplyr::select(Position, Period_Label) %>%
    distinct() %>%
    arrange(Position)
  if (show_labels)
    scale_x_continuous(name = "Year Period",
                       breaks = pl$Position, labels = pl$Period_Label)
  else
    scale_x_continuous(name = NULL,
                       breaks = pl$Position, labels = rep("", nrow(pl)))
}

panel_theme <- theme_minimal() +
  theme(
    legend.position  = "none",
    axis.title       = element_text(size = 10, face = "bold"),
    axis.text        = element_text(size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(4, 6, 2, 6)
  )

# Shared legend — reverse=TRUE so top-of-stack category appears first in legend
make_shared_legend <- function(viz_data) {
  rd <- add_ribbon_coords(viz_data)
  cowplot::get_legend(
    ggplot(rd, aes(x = Position, ymin = ymin, ymax = ymax,
                   fill = Quantification)) +
      geom_ribbon(alpha = 0.8) +
      scale_fill_manual(values = safe_colors(viz_data),
                        name = "Resilience Quantification",
                        guide = guide_legend(reverse = TRUE)) +
      theme(legend.position = "bottom",
            legend.title    = element_text(size = 10, face = "bold"),
            legend.text     = element_text(size = 9))
  )
}

shared_legend <- make_shared_legend(viz_A)

# ---- NUM version ----
make_num_panel <- function(viz_data, panel_id, group_label, show_x = TRUE) {
  rd <- add_ribbon_coords(viz_data)
  ggplot(rd, aes(x = Position, ymin = ymin, ymax = ymax,
                 fill = Quantification)) +
    geom_ribbon(alpha = 0.8) +
    scale_fill_manual(values = safe_colors(viz_data),
                      name = "Resilience Quantification",
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(
      name   = paste0("Paper Number\n(", panel_id, ") ", group_label),
      breaks = function(x) pretty(x, n = 5),
      expand = expansion(mult = c(0, 0.02))
    ) +
    make_x_scale(viz_data, show_labels = show_x) +
    panel_theme
}

num_A <- make_num_panel(viz_A, "a", LABEL_A, show_x = FALSE)
num_B <- make_num_panel(viz_B, "b", LABEL_B, show_x = TRUE)

fig2_num_final <- plot_grid(
  plot_grid(num_A, num_B, ncol = 1, align = "v", rel_heights = c(1, 1.15)),
  shared_legend,
  ncol = 1, rel_heights = c(1, 0.12)
)
ggsave(file.path(out_dir, "fig2_num_compare.png"),
       fig2_num_final, width = 7, height = 7, dpi = 500, units = "in")
cat("Saved: fig2_num_compare.png\n")

# ---- PRO version ----
make_pro_panel <- function(viz_data, panel_id, group_label, show_x = TRUE) {
  rd <- add_ribbon_coords_pct(viz_data)
  
  totals  <- rd %>%
    group_by(Position) %>%
    summarise(total = first(total), .groups = "drop")
  max_tot <- max(totals$total, na.rm = TRUE)
  if (max_tot == 0) max_tot <- 1
  
  ggplot() +
    geom_ribbon(data = rd,
                aes(x = Position, ymin = ymin, ymax = ymax,
                    fill = Quantification),
                alpha = 0.8) +
    geom_line(data  = totals,
              aes(x = Position, y = total * 100 / max_tot),
              color = "white", linewidth = 1.1, alpha = 0.9) +
    geom_point(data = totals,
               aes(x = Position, y = total * 100 / max_tot),
               color = "white", size = 1.8, alpha = 0.9) +
    scale_fill_manual(values = safe_colors(viz_data),
                      name = "Resilience Quantification",
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(
      name   = paste0("Percentage (%)\n(", panel_id, ") ", group_label),
      limits = c(0, 100),
      breaks = seq(0, 100, 25),
      sec.axis = sec_axis(~ . * max_tot / 100,
                          name   = "Paper Number",
                          breaks = function(x) pretty(x, n = 4))
    ) +
    make_x_scale(viz_data, show_labels = show_x) +
    panel_theme
}

pro_A <- make_pro_panel(viz_A, "a", LABEL_A, show_x = FALSE)
pro_B <- make_pro_panel(viz_B, "b", LABEL_B, show_x = TRUE)

fig2_pro_final <- plot_grid(
  plot_grid(pro_A, pro_B, ncol = 1, align = "v", rel_heights = c(1, 1.15)),
  shared_legend,
  ncol = 1, rel_heights = c(1, 0.12)
)
ggsave(file.path(out_dir, "fig2_pro_compare.png"),
       fig2_pro_final, width = 7, height = 7, dpi = 500, units = "in")
cat("Saved: fig2_pro_compare.png\n")

# ==============================================================================
# FIGURE 3: Sankey / alluvial diagram — 2-panel
# ==============================================================================

prep_sankey <- function(df_sub) {
  level_order    <- c("Landscape", "Ecosystem", "Community", "Population", "Individual")
  approach_order <- c("FO", "FM", "LM", "MD")
  target_order   <- c("Environmental context", "Functional response",
                      "Structure", "Process based indicator", "Quantity")
  
  df_sub %>%
    dplyr::select(level, approach, target_variable_group) %>%
    mutate(id = row_number()) %>%
    separate_rows(level,                 sep = ";") %>%
    separate_rows(approach,              sep = ";") %>%
    separate_rows(target_variable_group, sep = ";") %>%
    mutate(across(c(level, approach, target_variable_group), str_trim)) %>%
    filter(level != "", approach != "", target_variable_group != "") %>%
    mutate(
      approach = case_when(
        approach == "field observation"        ~ "FO",
        approach == "modeling and simulation"  ~ "MD",
        approach == "indoor experiment"        ~ "LM",
        approach == "field experiment"         ~ "FM",
        TRUE ~ approach
      ),
      target_variable_group = str_replace_all(target_variable_group, "\\.", " ")
    ) %>%
    filter(target_variable_group %in% c(
      "quantity", "structure", "process based indicator",
      "functional response", "environmental context"
    )) %>%
    mutate(
      Level           = paste0(toupper(substring(level, 1, 1)),
                               substring(level, 2)),
      Approach        = approach,
      Target_variable = paste0(toupper(substring(target_variable_group, 1, 1)),
                               substring(target_variable_group, 2))
    ) %>%
    dplyr::select(Level, Approach, Target_variable) %>%
    mutate(
      Level           = factor(Level,           levels = level_order),
      Approach        = factor(Approach,        levels = approach_order),
      Target_variable = factor(Target_variable, levels = target_order)
    ) %>%
    count(Level, Approach, Target_variable, name = "freq") %>%
    filter(freq > 0)
}

alluvial_A <- prep_sankey(df_raw_A)
alluvial_B <- prep_sankey(df_raw_B)

compute_label_positions <- function(alluvial_data) {
  lp <- alluvial_data %>%
    group_by(Level) %>% summarise(tf = sum(freq), .groups = "drop") %>%
    arrange(desc(Level)) %>%
    mutate(cs = cumsum(tf), y = cs - tf / 2)
  
  tp <- alluvial_data %>%
    group_by(Target_variable) %>% summarise(tf = sum(freq), .groups = "drop") %>%
    arrange(desc(Target_variable)) %>%
    mutate(
      cs  = cumsum(tf),
      y   = cs - tf / 2,
      lbl = case_when(
        Target_variable == "Environmental context"   ~ "Environmental\ncontext",
        Target_variable == "Functional response"     ~ "Functional\nresponse",
        Target_variable == "Process based indicator" ~ "Process-based\nindicator",
        TRUE ~ as.character(Target_variable)
      )
    )
  list(level = lp, target = tp)
}

make_sankey_panel <- function(alluvial_data, panel_id, group_label) {
  pos <- compute_label_positions(alluvial_data)
  
  ggplot(alluvial_data,
         aes(y = freq, axis1 = Level, axis2 = Approach,
             axis3 = Target_variable)) +
    geom_alluvium(aes(fill = Level), width = 1/12, alpha = 0.7,
                  curve_type = "cubic") +
    geom_stratum(width = 1/12, fill = "white", color = "black", size = 0.4) +
    geom_text(stat = "stratum",
              aes(label = ifelse(x == 2,
                                 as.character(after_stat(stratum)), "")),
              size = 2.5, color = "black") +
    scale_x_discrete(limits = c("Level", "Approach", "Measured Variable"),
                     expand = c(0.15, 0.05)) +
    scale_fill_manual(values = level_colors, name = "Level") +
    theme_minimal() +
    theme(
      panel.grid      = element_blank(),
      axis.text.y     = element_blank(),
      axis.ticks      = element_blank(),
      axis.title.y    = element_blank(),
      axis.text.x     = element_text(size = 11, face = "bold"),
      legend.position = "none",
      plot.title      = element_text(size = 10, face = "bold", hjust = 0),
      plot.margin     = margin(4, 5, 4, 5)
    ) +
    labs(title = paste0("(", panel_id, ") ", group_label), x = "") +
    annotate("text", x = 0.95, y = pos$level$y,
             label = as.character(pos$level$Level), hjust = 1, size = 2.5) +
    annotate("text", x = 3.05, y = pos$target$y,
             label = pos$target$lbl, hjust = 0, size = 2.5)
}

sankey_A <- make_sankey_panel(alluvial_A, "a", LABEL_A)
sankey_B <- make_sankey_panel(alluvial_B, "b", LABEL_B)

shared_sankey_legend <- cowplot::get_legend(
  ggplot(alluvial_A,
         aes(y = freq, axis1 = Level, axis2 = Approach,
             axis3 = Target_variable)) +
    geom_alluvium(aes(fill = Level), width = 1/12, alpha = 0.7) +
    scale_fill_manual(values = level_colors, name = "Level") +
    theme(legend.position = "bottom",
          legend.title    = element_text(size = 10, face = "bold"),
          legend.text     = element_text(size = 9))
)

fig3_final <- plot_grid(
  plot_grid(sankey_A, sankey_B, ncol = 1),
  shared_sankey_legend,
  ncol = 1, rel_heights = c(1, 0.07)
)
ggsave(file.path(out_dir, "fig3_sankey_compare.png"),
       fig3_final, width = 7, height = 7, dpi = 500, units = "in")
cat("Saved: fig3_sankey_compare.png\n")

cat("\nAll figures saved to:", out_dir, "\n")
