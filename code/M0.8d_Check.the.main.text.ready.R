# Module 0.8d Check if the folder get all the main-text pdf needed.
rm(list=ls())
library(here)

#import data sheets
df <- read.csv(here("data", "Cc.abstract.screened.result_1227.csv"))

# Path to the folder containing PDFs
f <- here("papers", "A_1227.paper.passed.abstract.filtering")

# --- 1) Read list of pdf filenames in the folder ---
# Only keep files that end with .pdf (case-insensitive)
pdf_files <- list.files(path = f, pattern = "\\.pdf$", ignore.case = TRUE, full.names = FALSE)

# --- 2) Extract numeric Abstract_ID from each filename ---
# Filenames are assumed to be in the format: "<number>_<rest-of-filename>.pdf"
# We'll extract the leading number before the first underscore.
extract_id_from_name <- function(fname) {
  # remove extension first
  name_no_ext <- sub("\\.[Pp][Dd][Ff]$", "", fname)
  # extract leading digits before underscore
  id_str <- sub("^([0-9]+)_.*$", "\\1", name_no_ext)
  # if the pattern did not match, return NA (we will handle NA later)
  id_num <- as.numeric(id_str)
  if (is.na(id_num)) return(NA_real_) else return(id_num)
}

folder_ids <- vapply(pdf_files, extract_id_from_name, numeric(1))

# Optionally warn about file names that did not match the expected pattern
bad_names <- pdf_files[is.na(folder_ids)]
if (length(bad_names) > 0) {
  warning("The following files do not match the expected '<number>_<title>.pdf' pattern and were ignored:\n",
          paste(bad_names, collapse = "\n"))
  # drop NA ones from lists
  pdf_files <- pdf_files[!is.na(folder_ids)]
  folder_ids <- folder_ids[!is.na(folder_ids)]
}

# Unique IDs found in folder
folder_ids_unique <- sort(unique(folder_ids))

# --- 3) Get Abstract_IDs from df (assume df exists in environment and has Abstract_ID column) ---
# We assume Abstract_ID column contains only numbers (as you stated)
df_ids <- sort(unique(as.numeric(df$Abstract_ID)))

# --- 4) Compare sets to find mismatches ---
# IDs that are present in df but missing in folder -> need.to.download
need_to_download_ids <- setdiff(df_ids, folder_ids_unique)

# IDs that are present in folder but missing in df -> need.to.delete
need_to_delete_ids <- setdiff(folder_ids_unique, df_ids)

# --- 5) If there are no mismatches, print All match! and remove D0.download.list.csv if exists ---
out_csv_path <- file.path(here("data", "D0.download.list.csv"))

if (length(need_to_download_ids) == 0 && length(need_to_delete_ids) == 0) {
  cat("All match!\n")
  # remove the CSV file in working directory if it exists
  if (file.exists(out_csv_path)) {
    file.remove(out_csv_path)
    cat("Existing D0.download.list.csv removed.\n")
  }
} else {
  # --- 6) Build the report data frame with three columns: Abstract_ID, Title, task ---
  # For need.to.download rows: Title should come from df$Title
  if (length(need_to_download_ids) > 0) {
    # subset df for those IDs and keep unique rows (in case of duplicates)
    download_rows <- unique(df[df$Abstract_ID %in% need_to_download_ids, c("Abstract_ID", "Title")])
    # Ensure Abstract_ID is numeric for consistency
    download_rows$Abstract_ID <- as.numeric(download_rows$Abstract_ID)
    download_rows$task <- "need.to.download"
  } else {
    download_rows <- data.frame(Abstract_ID = numeric(0), Title = character(0), task = character(0), stringsAsFactors = FALSE)
  }
  
  # For need.to.delete rows: Title = "find in folder"
  if (length(need_to_delete_ids) > 0) {
    delete_rows <- data.frame(
      Abstract_ID = need_to_delete_ids,
      Title = rep("find in folder", length(need_to_delete_ids)),
      task = rep("need.to.delete", length(need_to_delete_ids)),
      stringsAsFactors = FALSE
    )
  } else {
    delete_rows <- data.frame(Abstract_ID = numeric(0), Title = character(0), task = character(0), stringsAsFactors = FALSE)
  }
  
  # Combine both sets; for neatness, sort by task then Abstract_ID
  report_df <- rbind(download_rows, delete_rows)
  report_df <- report_df[order(report_df$task, as.numeric(report_df$Abstract_ID)), ]
  
  # Ensure column types and names
  report_df$Abstract_ID <- as.numeric(report_df$Abstract_ID)
  report_df$Title <- as.character(report_df$Title)
  report_df$task <- as.character(report_df$task)
  
  # --- 7) Write CSV to working directory ---
  write.csv(report_df, out_csv_path, row.names = FALSE)
  cat("Mismatch report written to:", out_csv_path, "\n")
}
