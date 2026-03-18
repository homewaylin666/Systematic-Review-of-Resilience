rm(list = ls())

# ---- Libraries ----
library(dplyr)
library(ggplot2)
library(maps)
library(stringr)
library(tidyr)
library(scales)
library(forcats)
library(patchwork)
library(cowplot)
library(here)

setwd(here())
# ============================================================
# DATA LOADING & PREPROCESSING
# ============================================================

df <- read.csv("data/Gz.full.data.sheet_594.csv")

df.selected <- df %>%
  dplyr::select(id, habitat_type, location, location_range,
                taxon, field_country, disturbance_type)

# ---- Coordinate parser ----
parse_coordinates <- function(location_string) {
  if (is.na(location_string) || location_string == "") return(c(NA, NA))
  s <- tolower(as.character(location_string))
  if (!str_detect(s, ",")) return(c(NA, NA))
  coords <- str_trim(str_split(s, ",")[[1]])
  if (length(coords) != 2) return(c(NA, NA))
  lat <- if (str_detect(coords[1], "^n")) suppressWarnings(as.numeric(str_replace(coords[1], "^n", "")))
  else if (str_detect(coords[1], "^s")) suppressWarnings(-as.numeric(str_replace(coords[1], "^s", "")))
  else NA
  lon <- if (str_detect(coords[2], "^e")) suppressWarnings(as.numeric(str_replace(coords[2], "^e", "")))
  else if (str_detect(coords[2], "^w")) suppressWarnings(-as.numeric(str_replace(coords[2], "^w", "")))
  else NA
  return(c(lat, lon))
}

# ---- Shared colour palettes ----
biome_color_list <- list(
  "Coastal"        = "#31AFF5FF", "Grassland"      = "#CDEC34FF",
  "Marine"         = "#476DE5FF", "Marine Neritic"  = "#476DE5FF",
  "Desert"         = "#FE992CFF", "Shrubland"       = "#E9D539FF",
  "Shrub Land"     = "#E9D539FF", "Wetland"         = "#1AE4B6FF",
  "Forest"         = "#3CB43CFF", "Other"           = "#975634FF",
  "Agricultural"   = "#7A0403FF", "Urban"           = "#3C4090FF",
  "Savanna"        = "#9AFE42FF", "Multi"           = "#000000"
)

pa_colors <- c(
  "LID"          = "#7A0403FF", "Fire"          = "#C62703FF",
  "Climatic"     = "#FE942AFF", "Resource"      = "#EBD239FF",
  "Biotic"       = "#AFFA37FF", "Chemical"      = "#1FE9AFFF",
  "Structural"   = "#2DB6F1FF", "Hydrological"  = "#455DD1FF",
  "Geophysical"  = "#550B1FFF", "BRU"           = "#30123BFF"
)

shape_mapping <- c("Plantae" = 24, "Animalia" = 25,
                   "Both" = 23, "NA" = 22, "Other" = 21)

# ---- Base coordinate data ----
coords_raw <- t(sapply(tolower(as.character(df.selected$location)),
                       parse_coordinates))
df.selected$Latitude  <- coords_raw[, 1]
df.selected$Longitude <- coords_raw[, 2]

df.coords <- df.selected %>%
  filter(!is.na(habitat_type),
         !is.na(Latitude), !is.na(Longitude)) %>%
  mutate(
    location_range = suppressWarnings(as.numeric(location_range)),
    location_range = if_else(!is.na(location_range) & location_range <= 2,
                             2, location_range),
    Range          = ifelse(!is.na(location_range) & location_range > 0,
                            log(location_range), NA_real_),
    Range_Category = cut(Range, breaks = 3,
                         labels = c("Small", "Medium", "Large"),
                         include.lowest = TRUE),
    Biome          = str_to_title(habitat_type),
    Biome          = str_replace(Biome, "Marine\\.neritic|Marine\\.Neritic",
                                 "Marine Neritic"),
    TaxonShape     = sapply(taxon, function(x) {
      if (is.na(x) || x == "") return("NA")
      tl <- tolower(x)
      has_multi <- str_detect(tl, "[;,]") | str_detect(tl, "\\band\\b")
      has_p <- str_detect(tl, "plantae")
      has_a <- str_detect(tl, "animalia")
      if (has_multi && has_p && has_a) "Both"
      else if (has_p) "Plantae"
      else if (has_a) "Animalia"
      else "Other"
    }),
    TaxonShape = factor(TaxonShape,
                        levels = c("Plantae", "Animalia", "Both", "Other", "NA"))
  ) %>%
  filter(!is.na(Range_Category))  # remove points with no valid study range

# Biome colour mapping (dynamic, covers any biome in data)
unique_biomes <- unique(df.coords$Biome)
color_mapping <- sapply(unique_biomes, function(b)
  if (b %in% names(biome_color_list)) biome_color_list[[b]] else "#999999")
names(color_mapping) <- unique_biomes

# ---- World map (no Antarctica) ----
world_map <- map_data("world") %>% filter(region != "Antarctica")

# ---- Manual coordinate corrections (df.coords) ----
df.coords <- df.coords %>% filter(id != 44) # id 44: lab study incorrectly assigned coordinates, remove from map
df.coords[df.coords$id == 186, c("Latitude", "Longitude")] <- list(-23.4, 151.9) # id 186: sites at 4 global locations; manually selected centroid-nearest site
df.coords[df.coords$id == 363,  c("Latitude", "Longitude")] <- list(-43.20, 171.6) # id 363: S latitude extracted as N during auto-parsing
df.coords[df.coords$id == 731,  c("Latitude", "Longitude")] <- list(-37.3, -64.6) # id 731: sites span USA and Argentina; Argentina has more sites so Argentina coords used
df.coords[df.coords$id == 894,  c("Latitude", "Longitude")] <- list(-34, 132) # id 894: auto-extracted coords imprecise; relocated based on study area description
df.coords[df.coords$id == 1319, c("Latitude", "Longitude")] <- list(11.4461, 27.8938) # id 1319: sites at 4 global locations; manually selected centroid-nearest site
df.coords[df.coords$id == 1479, c("Latitude", "Longitude")] <- list(59.6, -161.6) # id 1479: sites on both US east and west coasts; west coast used as coords explicitly given in paper
df.coords[df.coords$id == 1567, c("Latitude", "Longitude")] <- list(-36.7, 175.8) # id 1567: S latitude extracted as N during auto-parsing, same issue as id 363

# ============================================================
# PANEL (a): MAP — Global Distribution By Disturbance Type
# ============================================================

df.dist <- df.selected %>%
  filter(!is.na(Latitude), !is.na(Longitude)) %>%
  mutate(
    location_range = suppressWarnings(as.numeric(location_range)),
    location_range = if_else(!is.na(location_range) & location_range <= 2,
                             2, location_range),
    Range_Category = cut(
      ifelse(!is.na(location_range) & location_range > 0,
             log(location_range), NA_real_),
      breaks = 3, labels = c("Small", "Medium", "Large"),
      include.lowest = TRUE),
    disturbance_type = ifelse(id == 543,
                              str_replace(disturbance_type, "abiotic", "climatic;chemical"),
                              disturbance_type),
    disturbance_type = tolower(as.character(disturbance_type))
  ) %>%
  filter(!is.na(Range_Category)) %>%  # remove points with no valid study range
  separate_rows(disturbance_type, sep = ";") %>%
  mutate(disturbance_type = str_squish(disturbance_type)) %>%
  filter(!is.na(disturbance_type), disturbance_type != "") %>%
  mutate(disturbance_type = case_when(
    disturbance_type == "landuse and infrastructure development" ~ "LID",
    disturbance_type == "biological resource use"               ~ "BRU",
    disturbance_type == "hydrological"                          ~ "Hydrological",
    disturbance_type == "geophysical"                           ~ "Geophysical",
    TRUE ~ str_to_title(disturbance_type)
  ))

# ---- Manual coordinate corrections (df.dist) ----
df.dist <- df.dist %>% filter(id != 44)
df.coords[df.coords$id == 186, c("Latitude", "Longitude")] <- list(-23.4, 151.9)
df.dist[df.dist$id == 363,  c("Latitude", "Longitude")] <- list(-43.20, 171.6)
df.dist[df.dist$id == 731,  c("Latitude", "Longitude")] <- list(-37.3, -64.6)
df.dist[df.dist$id == 894,  c("Latitude", "Longitude")] <- list(-34, 132)
df.dist[df.dist$id == 1319, c("Latitude", "Longitude")] <- list(11.4461, 27.8938)
df.dist[df.dist$id == 1479, c("Latitude", "Longitude")] <- list(59.6, -161.6)
df.dist[df.dist$id == 1567, c("Latitude", "Longitude")] <- list(-36.7, 175.8)

pa <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "white", size = 0.1) +
  geom_point(data = df.dist,
             aes(x = Longitude, y = Latitude,
                 fill = disturbance_type, size = Range_Category),
             shape = 21, alpha = 0.85, color = "black", stroke = 0.2,
             position = position_jitter(width = 1.2, height = 1.2)) +
  scale_fill_manual(values = pa_colors, name = "Disturbance Type") +
  scale_size_manual(name = "Study Range",
                    values = c("Small" = 2.0, "Medium" = 3.2, "Large" = 4.4)) +
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85)) +
  scale_y_continuous(breaks = seq(-60, 80, 20),
                     labels = paste0(abs(seq(-60, 80, 20)),
                                     ifelse(seq(-60, 80, 20) >= 0, "°N", "°S"))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(size = 0.3),
    axis.title = element_blank(), panel.grid = element_blank(),
    legend.position = "bottom", legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  ggtitle("Global Distribution By Disturbance Type")

# ============================================================
# PANEL (b): PIE — Disturbance Type Proportions (original style)
# ============================================================

df.dt_pie <- df.dist %>%
  distinct(id, disturbance_type) %>%
  count(disturbance_type, name = "n") %>%
  mutate(
    prop         = n / sum(n),
    pct_txt      = scales::percent(prop, accuracy = 0.1),
    disturbance_type = fct_reorder(disturbance_type, prop, .desc = TRUE),
    label_inside = paste0(disturbance_type, "\n", pct_txt)
  )

fill_vec <- pa_colors[levels(df.dt_pie$disturbance_type)]

pb <- ggplot(df.dt_pie, aes(x = 1, y = prop, fill = disturbance_type)) +
  geom_col(width = 1, color = "white", size = 0.8) +
  geom_text(aes(x = 1.7, label = label_inside),
            position = position_stack(vjust = 0.5),
            size = 3.75, color = "black", fontface = "bold") +
  coord_polar(theta = "y", start = 19 * pi / 24, clip = "off") +
  scale_fill_manual(values = fill_vec, name = "Disturbance Type", drop = FALSE) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  ) +
  ggtitle("Disturbance Type Proportions")

# ============================================================
# PANEL (c): PIE — Biome Proportions (original style)
# ============================================================

df.biome <- df.coords %>%
  count(Biome, name = "n") %>%
  mutate(
    prop         = n / sum(n),
    pct_txt      = scales::percent(prop, accuracy = 0.1),
    Biome        = fct_reorder(Biome, prop, .desc = TRUE),
    label_inside = paste0(Biome, "\n", pct_txt)
  )

biome_fill_vec <- color_mapping[levels(df.biome$Biome)]

pc <- ggplot(df.biome, aes(x = 1, y = prop, fill = Biome)) +
  geom_col(width = 1, color = "white", size = 0.8) +
  geom_text(aes(x = 1.7, label = label_inside),
            position = position_stack(vjust = 0.5),
            size = 3.75, color = "black", fontface = "bold") +
  coord_polar(theta = "y", start = 5 * pi / 12, clip = "off") +
  scale_fill_manual(values = biome_fill_vec, name = "Biome") +
  theme_void(base_size = 11) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12)
  ) +
  ggtitle("Biome Proportions")

# ============================================================
# PANEL (d): MAP — Global Distribution Of Ecological Resilience Studies
# ============================================================

pd <- ggplot() +
  geom_polygon(data = world_map,
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white", size = 0.1) +
  geom_point(data = df.coords,
             aes(x = Longitude, y = Latitude,
                 fill = Biome, size = Range_Category, shape = TaxonShape),
             position = position_jitter(width = 1.5, height = 1.5),
             alpha = 0.8, color = "black", stroke = 0.3) +
  scale_fill_manual(
    values = color_mapping, name = "Biome",
    guide = guide_legend(override.aes = list(shape = 21, size = 3,
                                             stroke = 0.3, color = "black"))) +
  scale_shape_manual(
    values = shape_mapping, name = "Taxon",
    breaks = c("Plantae", "Animalia", "Both", "Other", "NA"),
    labels = c("Plantae"  = "Plantae",
               "Animalia" = "Animalia",
               "Both"     = "Plantae & Animalia",
               "Other"    = "Other Taxa",
               "NA"       = "Landscape Study"),
    guide = guide_legend(nrow = 3)) +
  scale_size_manual(
    name = "Study range",
    values = c("Small" = 2.5, "Medium" = 3.5, "Large" = 4.5),
    guide = guide_legend(nrow = 1)) +
  coord_fixed(1.3, xlim = c(-180, 180), ylim = c(-60, 85)) +
  scale_y_continuous(
    breaks = seq(-60, 80, by = 20),
    labels = paste0(abs(seq(-60, 80, 20)),
                    ifelse(seq(-60, 80, 20) >= 0, "°N", "°S")),
    position = "right") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_line(size = 0.3),
    axis.title = element_blank(), panel.grid = element_blank(),
    legend.position = "bottom", legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  ggtitle("Global Distribution Of Ecological Resilience Studies")

# ============================================================
# INDIVIDUAL PANEL EXPORTS
# ============================================================

ggsave("figure/figure5a.png", pa, width = 12, height = 7, dpi = 600, bg = "white")
ggsave("figure/figure5b.png", pb, width = 7,  height = 7, dpi = 600, bg = "white")
ggsave("figure/figure5c.png", pc, width = 7,  height = 7, dpi = 600, bg = "white")
ggsave("figure/figure5d.png", pd, width = 12, height = 7, dpi = 600, bg = "white")

# Supplementary taxa bar chart
df.taxon_bar <- df.selected %>%
  mutate(taxon = tolower(as.character(taxon))) %>%
  separate_rows(taxon, sep = ";|,|\\band\\b") %>%
  mutate(taxon = str_to_title(str_squish(taxon))) %>%
  filter(!is.na(taxon), taxon != "")

top5 <- df.taxon_bar %>%
  count(taxon) %>% arrange(desc(n)) %>% slice(1:5) %>% pull(taxon)

df.taxon_bar <- df.taxon_bar %>%
  mutate(taxon6 = ifelse(taxon %in% top5, taxon, "Other Taxa")) %>%
  distinct(id, taxon6) %>%
  count(taxon6, name = "n_studies") %>%
  mutate(
    prop      = n_studies / n_distinct(df.selected$id),
    label_txt = paste0(n_studies, " (", scales::percent(prop, accuracy = 0.1), ")"),
    taxon6    = fct_reorder(taxon6, n_studies, .desc = TRUE)
  )

ps4 <- ggplot(df.taxon_bar, aes(x = taxon6, y = n_studies)) +
  geom_col(fill = "#3F4788FF", width = 0.75) +
  geom_text(aes(label = label_txt), vjust = -0.25, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
  labs(x = NULL, y = "Number of studies", title = "Studies Including Each Taxa") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        plot.title  = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave("figure/figureS4_taxa.png", ps4, width = 6.5, height = 5.0,
       dpi = 600, bg = "white")

# ============================================================
# COMBINED LEGEND EXPORT
# ============================================================

# pa legend: Disturbance Type (unified dot size) + Study Range
leg_pa <- cowplot::get_legend(
  pa + theme(legend.position = "bottom",
             legend.box = "vertical",
             legend.key.size = unit(0.4, "cm")) +
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE,
                          override.aes = list(size = 3)),
      size = guide_legend(nrow = 1)
    )
)

# pd legend: Taxon + Biome only (Study Range removed — shared with pa)
leg_pd <- cowplot::get_legend(
  pd + theme(legend.position = "bottom",
             legend.box = "horizontal",
             legend.key.size = unit(0.4, "cm")) +
    guides(
      fill  = guide_legend(override.aes = list(size = 3,
                                               shape = 21,
                                               color = "black")),
      shape = guide_legend(nrow = 3,
                           override.aes = list(size = 3, fill = "grey50")),
      size  = "none"
    )
)

figure5_legend <- cowplot::plot_grid(leg_pa, leg_pd, ncol = 1, align = "v")

ggsave("figure/figure5_legend.png", figure5_legend,
       width = 12, height = 3.5, dpi = 600, bg = "white")
