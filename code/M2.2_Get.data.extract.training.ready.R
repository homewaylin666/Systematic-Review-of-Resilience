# Module 2.2 Separate the manual data extraction result to training, validation and apply sets.
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")

#import data sheet
df.op <- read.csv("F0.manual.collect.sample_150_edited.csv", stringsAsFactors = FALSE)
df.ez <- read.csv("Ez.main.text.screened.result_594.csv", stringsAsFactors = FALSE)

# Source folder that contains PDFs
pdf_source <- "/Users/homeway/Desktop/Resilience/Chapter1/papers/A_1227.paper.passed.abstract.filtering"

# Load dplyr for data manipulation
library(dplyr)
library(stringr)

# --- Helper: find files in source_dir that start with "<id>_" and return full paths ---
find_files_starting_with_id <- function(ids, source_dir) {
  all_files <- list.files(path = source_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
  basenames <- basename(all_files)
  matched <- character(0)
  ids_chr <- as.character(ids)
  for (id in ids_chr) {
    pattern <- paste0("^", id, "_")
    sel <- grepl(pattern, basenames, perl = TRUE)
    if (any(sel)) {
      matched <- c(matched, all_files[sel])
    }
  }
  unique(matched)
}

df.op$id <- as.character(df.op$id)

# 1) Sample 75 random rows from df.op -> df.op.train (seed = 201)
set.seed(201)
df.op.train <- df.op %>% sample_n(75)

# Export the training list CSV
write.csv(df.op.train, "F1.75.paper.sample.for.training.csv", row.names = FALSE)

# Create training folder and copy PDFs whose filename starts with "<id>_"
train_folder <- file.path(getwd(), "F1.75.paper.for.training")
if (!dir.exists(train_folder)) dir.create(train_folder, recursive = TRUE)

train_files <- find_files_starting_with_id(df.op.train$id, pdf_source)
file.copy(train_files, train_folder, overwrite = TRUE)

# 2) The remaining 75 rows -> df.op.validate
df.op.validate <- df.op %>% filter(!(id %in% df.op.train$id))

# Export validation CSV
write.csv(df.op.validate, "F2.75.paper.sample.for.validation.csv", row.names = FALSE)

# Create validation folder and copy PDFs for df.op.validate
val_folder <- file.path(getwd(), "F2.75.paper.for.validation")
if (!dir.exists(val_folder)) dir.create(val_folder, recursive = TRUE)

val_files <- find_files_starting_with_id(df.op.validate$id, pdf_source)
file.copy(val_files, val_folder, overwrite = TRUE)

# 3) Prepare df.left: rename df.ez$paper_id -> id and keep only id, then keep ids NOT in df.op
if (!"paper_id" %in% names(df.ez)) stop("df.ez must contain column 'paper_id'.")
df.ez <- df.ez %>% rename(id = paper_id)
df.ez$id <- as.character(df.ez$id)

# Keep only ids not in df.op
df.left <- df.ez %>% filter(!(id %in% df.op$id)) %>% select(id)

# Create folder for leftover PDFs and copy all matching files
left_folder <- file.path(getwd(), "F3.444.paper.left")
if (!dir.exists(left_folder)) dir.create(left_folder, recursive = TRUE)

left_files <- find_files_starting_with_id(df.left$id, pdf_source)
file.copy(left_files, left_folder, overwrite = TRUE)

# 4) From df.left sample 50 (seed = 201) -> df.post
set.seed(201)
df.post <- df.left %>% sample_n(50)

# 5) Add columns from df.op (except id) to df.post, but set all non-id values to "wait.for.check"
op_nonid_cols <- setdiff(names(df.op), "id")

# Create a template data.frame with the same non-id columns filled with "wait.for.check"
template <- as.data.frame(matrix("wait.for.check", nrow = nrow(df.post), ncol = length(op_nonid_cols)),
                          stringsAsFactors = FALSE)
names(template) <- op_nonid_cols

# Combine id column with template to form the enriched df.post
df.post <- bind_cols(df.post %>% mutate(id = as.character(id)), template)

# 6) Export df.post as "Fp.50.paper.for.post.check.csv"
write.csv(df.post, "Fp.50.paper.for.post.check.csv", row.names = FALSE)
