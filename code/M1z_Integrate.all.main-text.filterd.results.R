# Module 0.8c Integrate all the abstract filtered results
rm(list=ls())
library(here)

# ------------------------
# 1) Locate the three CSV files that contain "5.2e" in their filenames
#    in three locations:
#      - "E1.llm.output" subfolder (-> train)
#      - "E2.llm.output" subfolder (-> validate)
#      - current working directory (-> apply)
#    Only proceed if exactly one matching file is found in each location.
# ------------------------

# helper to find files containing "5.2e" (case sensitive as you requested)
find_one_5.2e <- function(dir_path) {
  files <- list.files(path = dir_path, pattern = "5\\.2e", full.names = TRUE)
  csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
  if (length(csv_files) != 1) {
    stop(paste0("Expected exactly 1 '5.2e' CSV in: ", dir_path,
                " — found ", length(csv_files), ". Aborting."))
  }
  csv_files[1]
}

# paths
dir_e1 <- file.path(here("data", "E1.llm.output"))
dir_e2 <- file.path(here("data", "E2.llm.output"))
dir_wd <- here("data")

# find files (these will error via stop() if the condition is not met)
train_file    <- find_one_5.2e(dir_e1)
validate_file <- find_one_5.2e(dir_e2)
apply_file    <- find_one_5.2e(dir_wd)

# ------------------------
# 2) Import the three CSVs and name them train, validate, apply
# ------------------------
train    <- read.csv(train_file,    stringsAsFactors = FALSE, check.names = FALSE)
validate <- read.csv(validate_file, stringsAsFactors = FALSE, check.names = FALSE)
apply    <- read.csv(apply_file,    stringsAsFactors = FALSE, check.names = FALSE)

# ------------------------
# 3) Combine (row-bind) the three dataframes into full.result
#    They have identical columns, so rbind is appropriate.
# ------------------------
full.result <- rbind(train, validate, apply)

# ------------------------
# 4) Keep only rows with Criteria.met == "IN" (drop the duplicate first)
# ------------------------
# Remove the paper_id column
full.result <- full.result[full.result$paper_id != "1189", ]

# Filter rows (exact match to "IN")
df <- subset(full.result, `Criteria met` == "IN")

# ------------------------
# 5) Export df to working directory with file name including row count
#    Filename pattern: Ez.main.text.screened.result_xxx.csv
# ------------------------
row_count <- nrow(df)
out_name <- paste0("Ez.main.text.screened.result_", row_count, ".csv")
write.csv(df, here("data", out_name), row.names = FALSE)

