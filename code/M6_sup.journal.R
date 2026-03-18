# M6_a heat map showing journal scope
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1")

# ---- import the data and package ----
df <- read.csv("data/Gz.full.data.sheet_594_edited.csv")
library(tidyverse)
library(ggplot2)
library(patchwork)

# ── 1. Rename journal categories ──────────────────────────────────────────────
title_case_no_of <- function(x) {
  words <- strsplit(x, " ")[[1]]
  # Keep lowercase: "of" and "and"; capitalise everything else
  words <- ifelse(words %in% c("of", "and"), words, str_to_title(words))
  paste(words, collapse = " ")
}

df <- df %>%
  mutate(journal = case_when(
    journal == "proceedings of the national academy of sciences of the united states of america" ~ "PNAS",
    TRUE ~ sapply(journal, title_case_no_of)
  ))

# ── 2. Rename column ──────────────────────────────────────────────────────────
df <- df %>% rename(`measured variable type` = target_variable_group)

# ── 3. Define the three columns and their expected categories ─────────────────
col_level    <- "level"
col_approach <- "approach"
col_mvt      <- "measured variable type"

level_cats    <- c("landscape", "ecosystem", "community", "population", "individual")
approach_cats <- c("field observation", "field manipulation", "lab manipulation",
                   "modeling-based simulation")
mvt_cats      <- c("environmental context", "functional response",
                   "process-based indicator",
                   "structure", "quantity")

# ── 4. Helper: compute proportion of each category per journal for one column ─
compute_prop <- function(df, col, cats, rename_map = NULL) {
  # Expand multi-value entries (split by "; ")
  expanded <- df %>%
    dplyr::select(journal, value = dplyr::all_of(col)) %>%
    tidyr::separate_rows(value, sep = ";\\s*") %>%
    dplyr::mutate(value = str_trim(value))
  
  # Apply any renaming inside the column values
  if (!is.null(rename_map)) {
    expanded <- expanded %>%
      dplyr::mutate(value = dplyr::recode(value, !!!rename_map))
  }
  
  # Count per journal x category, then compute proportion within journal
  prop_df <- expanded %>%
    dplyr::filter(value %in% cats) %>%
    dplyr::count(journal, value) %>%
    dplyr::group_by(journal) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::select(journal, value, prop)
  
  # Fill missing journal x category combos with 0
  all_combos <- expand.grid(
    journal = unique(df$journal),
    value   = cats,
    stringsAsFactors = FALSE
  )
  prop_df <- all_combos %>%
    left_join(prop_df, by = c("journal", "value")) %>%
    mutate(prop = replace_na(prop, 0))
  
  prop_df
}

# Rename maps: correct original names in df -> display names in figure
approach_rename <- c(
  "modeling and simulation" = "modeling-based simulation",
  "field experiment"        = "field manipulation",   # corrected original name
  "indoor experiment"       = "lab manipulation"       # corrected original name
)
mvt_rename <- c("process based indicator" = "process-based indicator")

prop_level    <- compute_prop(df, col_level,    level_cats)
prop_approach <- compute_prop(df, col_approach, approach_cats, approach_rename)
prop_mvt      <- compute_prop(df, col_mvt,      mvt_cats,      mvt_rename)

# ── 5. Combine all proportions ────────────────────────────────────────────────
all_props <- bind_rows(prop_level, prop_approach, prop_mvt) %>%
  rename(category = value)

all_cats_ordered <- c(level_cats, approach_cats, mvt_cats)
all_props <- all_props %>%
  mutate(category = factor(category, levels = all_cats_ordered))

# ── 6. Journal order: custom rank (top-to-bottom as specified) ─────────────
# Priority: Nature > Science > PNAS, then follow the provided image ranking.
# Journals not listed are appended at the end.
custom_rank <- c(
  "Nature", "Science", "PNAS",
  "Nature Ecology & Evolution",
  "Ecological Indicators",
  "Ecology Letters",
  "Molecular Ecology",
  "Journal of Applied Ecology",
  "Molecular Ecology Resources",
  "Global Ecology and Biogeography",
  "Journal of Ecology",
  "Ecology",
  "Ecography",
  "Functional Ecology",
  "Ecological Applications"
)

all_journals  <- unique(df$journal)
in_order      <- custom_rank[custom_rank %in% all_journals]
extras        <- setdiff(all_journals, in_order)
journal_order  <- c(in_order, extras)   # top-to-bottom display order
journal_levels <- rev(journal_order)    # factor levels: bottom of plot = index 1

all_props <- all_props %>%
  mutate(journal = factor(journal, levels = journal_levels))

# ── 7. Compute per-journal sample counts (raw rows, no multi-value expansion) ─
journal_n <- df %>%
  dplyr::count(journal, name = "n") %>%
  mutate(journal = factor(journal, levels = journal_levels))

# ── 8. Heatmap ────────────────────────────────────────────────────────────────
p_heat <- ggplot(all_props, aes(x = category, y = journal, fill = prop)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(prop > 0, sprintf("%.2f", prop), "")),
            size = 2.2, color = "black") +
  scale_fill_gradient(low = "#f7fbff", high = "#3F4788",
                      name = "Proportion", limits = c(0, 1)) +
  # Vertical separators between column groups
  geom_vline(xintercept = c(5.5, 9.5), color = "grey40", linewidth = 0.7) +
  # Column group annotation brackets and labels (drawn above plot area)
  annotate("segment",
           x    = c(0.6, 5.6, 9.6), xend = c(5.4, 9.4, 14.4),
           y    = 15.0,              yend = 15.0,
           linewidth = 0.7, color = "grey30") +
  annotate("text",
           x     = c(3.0, 7.5, 12.0),
           y     = 15.5,
           label = c("Level", "Approach", "Measured Variable Type"),
           size  = 3.2, fontface = "bold", color = "grey20") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off", ylim = c(0.5, 14.5)) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x        = element_text(angle = 40, hjust = 1, size = 8.5),
    axis.text.y        = element_text(size = 9),
    legend.position    = "bottom",
    legend.key.height  = unit(0.4, "cm"),
    legend.key.width   = unit(1.2, "cm"),
    panel.grid         = element_blank(),
    plot.margin        = margin(t = 35, r = 4, b = 5, l = 5)
  )

# ── 9. Bar chart: sample count per journal ────────────────────────────────────
p_bar <- ggplot(journal_n, aes(x = n, y = journal)) +
  geom_col(fill = "#3F4788", alpha = 0.75, width = 0.65) +
  geom_text(aes(label = paste0("n=", n)),
            hjust = -0.1, size = 2.8, color = "grey20") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Sample size", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y        = element_blank(),    # journal labels already on heatmap
    axis.ticks.y       = element_blank(),
    axis.text.x        = element_text(size = 8),
    axis.title.x       = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(t = 35, r = 12, b = 5, l = 2)
  )

# ── 10. Combine with patchwork and export ─────────────────────────────────────
combined <- p_heat + p_bar +
  plot_layout(widths = c(3.5, 1))   # heatmap ~3.5x wider than bar chart

ggsave("figure/figure_sup_journal.png", plot = combined,
       width = 12, height = 9, dpi = 300, bg = "white")

message("Done! Saved to figure/figure_sup_journal.png")