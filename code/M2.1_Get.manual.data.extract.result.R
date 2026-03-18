# Module 2.1 Get the manual data extraction result I made before that I can still use
rm(list=ls())
library(here)

#import data sheets
df.old <- read.csv(here("data", "D(old).extract.data manually.csv"))
df.post <- read.csv(here("data", "Fp(old).data.extraction.postcheck50.csv"))
df.ez <- read.csv(here("data", "Ez.main.text.screened.result_594.csv"))

# Load dplyr for data manipulation
library(dplyr)
library(stringr)

#unify id col name
df.ez <- df.ez %>% rename(id = paper_id)
df.old <- df.old %>% rename(id = Abstract_ID)

######################## Clean the df.post ########################
existing_removed <- intersect(c("define", "remote_sensing"), names(df.post))
df.post <- df.post %>% select(-all_of(existing_removed))
rows_with_a <- grepl("a_", df.post$id, fixed = TRUE)
if (any(rows_with_a, na.rm = TRUE)) {
  removed_count <- sum(rows_with_a, na.rm = TRUE)
  df.post <- df.post %>% filter(!rows_with_a)
  message("Removed ", removed_count, " rows from df.post with id containing 'a_'.")
} else {
  message("No df.post rows with 'a_' in id found.")
}
df.post$id <- as.numeric(df.post$id)

######################## Clean the df.old ########################
rename_map <- c(
  "Measure" = "measurement",
  "Definition_framework.group" = "framework",
  "quantify.definition" = "quantification",
  "Attribute.type" = "target_variable_type",
  "Attribute.group" = "target_variable_group",
  "Study.level" = "level",
  "Approach" = "approach",
  "Institution.country" = "institution_country",
  "Location_country" = "field_country",
  "Location" = "location",
  "Location.range" = "location_range",
  "IUCN.habitat" = "habitat_type",
  "Taxon" = "taxon",
  "Time.duration" = "observation_duration"
)
existing_old_cols <- intersect(names(rename_map), names(df.old))
setdiff(names(rename_map), names(df.old))
df.old <- df.old %>% rename(!!!setNames(existing_old_cols, rename_map[existing_old_cols]))
# Process Disturbance_type in df.old:
#    - values are like "A_B" and possibly multiple values separated by ";" e.g. "A_B; A_C"
#    - create two new columns:
#         disturbance_manipulation  <- unique left-parts (A) across all entries, joined by ";"
#         disturbance_type          <- unique right-parts (B, C) across all entries, joined by ";"
#    - remove duplicates in each aggregated cell
process_disturbance_type <- function(x) {
  # x: character vector of disturbance-type entries
  # returns a data.frame with two character vectors: manipulation and type
  manipulation_out <- vector("character", length(x))
  type_out <- vector("character", length(x))
  
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || str_trim(val) == "") {
      manipulation_out[i] <- NA_character_
      type_out[i] <- NA_character_
      next
    }
    # split multiple entries by ';'
    entries <- unlist(str_split(val, ";\\s*"))
    lefts <- c()
    rights <- c()
    for (e in entries) {
      e_trim <- str_trim(e)
      if (str_detect(e_trim, "_")) {
        parts <- unlist(str_split(e_trim, "_", n = 2))
        lefts <- c(lefts, parts[1])
        rights <- c(rights, parts[2])
      } else {
        # if no underscore, ignore or treat left part only? we'll ignore such malformed entries but warn
        warning("Malformed Disturbance_type entry (no '_') encountered: '", e_trim, "'. Ignored for splitting.")
      }
    }
    # unique and join with ;
    lefts_u <- unique(lefts)
    rights_u <- unique(rights)
    manipulation_out[i] <- if (length(lefts_u) == 0) NA_character_ else paste(lefts_u, collapse = "; ")
    type_out[i] <- if (length(rights_u) == 0) NA_character_ else paste(rights_u, collapse = "; ")
  }
  data.frame(disturbance_manipulation = manipulation_out, disturbance_type = type_out, stringsAsFactors = FALSE)
}

if ("Disturbance_type" %in% names(df.old)) {
  dt_parsed <- process_disturbance_type(as.character(df.old$Disturbance_type))
  df.old <- bind_cols(df.old, dt_parsed)
  message("Processed Disturbance_type into disturbance_manipulation and disturbance_type.")
} else {
  message("No Disturbance_type column found in df.old; skipping disturbance parsing.")
}

# Process Disturbance_pattern in df.old:
#    - values are like "A_B" possibly multi-value separated by ';'
#    - we only keep the left-part (before '_') for each, unique, join with ';'
process_disturbance_pattern <- function(x) {
  out <- vector("character", length(x))
  for (i in seq_along(x)) {
    val <- x[i]
    if (is.na(val) || str_trim(val) == "") {
      out[i] <- NA_character_
      next
    }
    entries <- unlist(str_split(val, ";\\s*"))
    lefts <- c()
    for (e in entries) {
      e_trim <- str_trim(e)
      if (str_detect(e_trim, "_")) {
        parts <- unlist(str_split(e_trim, "_", n = 2))
        lefts <- c(lefts, parts[1])
      } else {
        # ignore malformed entries without '_'
        warning("Malformed Disturbance_pattern entry (no '_') encountered: '", e_trim, "'. Ignored.")
      }
    }
    lefts_u <- unique(lefts)
    out[i] <- if (length(lefts_u) == 0) NA_character_ else paste(lefts_u, collapse = ";")
  }
  out
}

if ("Disturbance_pattern" %in% names(df.old)) {
  df.old <- df.old %>%
    mutate(disturbance_pattern = process_disturbance_pattern(as.character(Disturbance_pattern)))
  message("Processed Disturbance_pattern into disturbance_pattern (left-part only).")
} else {
  message("No Disturbance_pattern column found in df.old; skipping.")
}

######################## Integrate the old manual data ########################
# 1) Check whether df.post's column names are a subset of df.old's column names
cols_post <- names(df.post)
cols_old  <- names(df.old)

if (!all(cols_post %in% cols_old)) {
  missing_cols <- setdiff(cols_post, cols_old)
  stop(
    "Column mismatch: the following columns are present in df.post but not in df.old:\n",
    paste(missing_cols, collapse = ", "),
    "\nAborting merge."
  )
} else {
  message("Column check passed: all df.post columns are present in df.old.")
}

# 2) Merge df.post and df.old into df.manual, keeping only the columns that appear in df.post
df.post$observation_duration <- as.numeric(df.post$observation_duration)
df.old$observation_duration <- as.numeric(df.old$observation_duration)
df.manual <- bind_rows(
  df.old  %>% select(all_of(cols_post)),
  df.post %>% select(all_of(cols_post))
)

# 3) Check for duplicate ids within each source (df.old and df.post) and report
dup_manual_ids <- unique(df.manual$id[duplicated(df.manual$id)])
if (length(dup_manual_ids) > 0) {
  message("Note: df.manual contains duplicated id(s) after combining (showing unique values): ",
          paste(dup_manual_ids, collapse = ", "))
} else {
  message("df.manual contains no duplicated ids after combining.")
}

######################## find the overlap papers ########################
# 4) Select rows from df.manual whose id appears in df.ez$id and copy them into df.op
#    Use character comparison to be robust to numeric vs character types
ez_ids <- as.character(df.ez$id)
df.manual <- df.manual %>% mutate(id = as.character(id))

df.op <- df.manual %>%
  filter(id %in% ez_ids)

# 5) Report the number of rows in df.op
n_op <- nrow(df.op)
message("Number of overlapping rows copied to df.op: ", n_op)

# 115 rows.Ok, but I need 150 rows, so let's draw 35 more from df.ez and let me manually collect them to have 150 samples for training and validation.

######################## draw 35 more and output with the overlap ########################
# 6) Find candidate ids that are in df.ez but not in df.op
candidate_ids <- setdiff(df.ez$id, df.op$id)

# 7) Create an empty data.frame with random 35 rows and same columns, filled with "wait for manual check"
set.seed(128)
sampled_ids <- sample(candidate_ids, size = 35)
op_cols <- names(df.op)
new_rows <- as.data.frame(matrix("wait for manual check", nrow = length(sampled_ids), ncol = length(op_cols)),
                          stringsAsFactors = FALSE)
names(new_rows) <- op_cols
new_rows$id <- sampled_ids

# 8) Combine the original df.op with the newly created rows to produce df.op.wfmc
df.op$id <- as.numeric(df.op$id)
new_rows$id <- as.numeric(new_rows$id)
df.op <- df.op %>%
  mutate(across(-id, as.character))
df.op.wfmc <- bind_rows(df.op, new_rows)

# 9) Export df.op.wfmc to CSV in working directory
df.op.wfmc <- df.op.wfmc[order(df.op.wfmc$id), ]
out_file <- "F0.manual.collect.sample_150.csv"
write.csv(df.op.wfmc, here("data", out_file), row.names = FALSE)

