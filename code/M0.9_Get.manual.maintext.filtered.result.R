# Module 0.9 Get the manual main-text filtered result I made before that I can still use
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")

#import data sheets
dfo <- read.csv("Cc(old).screened.result_896.csv")
dfp <- read.csv("Dp(old).maintext.filtering.postcheck50.csv")
df.cc <- read.csv("Cc.abstract.screened.result_1227.csv")

# Load dplyr for data manipulation
library(dplyr)

# 1) Rename Abstract_ID -> paper_id in df and dfo (if the column exists)
if ("Abstract_ID" %in% names(df.cc)) {
  df.cc <- df.cc %>% rename(paper_id = Abstract_ID)
} else {
  stop("df does not contain a column named 'Abstract_ID'")
}

if ("Abstract_ID" %in% names(dfo)) {
  dfo <- dfo %>% rename(paper_id = Abstract_ID)
} else {
  stop("dfo does not contain a column named 'Abstract_ID'")
}

# 2) Remove rows in dfo where quantify_main.text.filter == "uncheck"
#    (keep rows where it's not "uncheck" or is NA)
if (!"quantify_main.text.filter" %in% names(dfo)) {
  stop("dfo does not contain a column named 'quantify_main.text.filter'")
}
dfo <- dfo %>%
  filter(!(quantify_main.text.filter == "uncheck"))

# 3) Rename dfo$quantify_main.text.filter -> human.coder_HL
dfo <- dfo %>% rename(human.coder_HL = quantify_main.text.filter)

# 4) Rename dfp$HL.judgement -> human.coder_HL
if (!"HL.judgement" %in% names(dfp)) {
  stop("dfp does not contain a column named 'HL.judgement'")
}
dfp <- dfp %>% rename(human.coder_HL = HL.judgement)

# 5) Extract paper_id, Title, human.coder_HL from dfo and dfp and combine into df.old
#    Ensure the required columns exist before selecting
req_cols_op <- c("paper_id", "Title", "human.coder_HL")

if (!all(req_cols_op %in% names(dfo))) {
  stop("dfo is missing one of the required columns: paper_id, Title, human.coder_HL")
}
if (!all(req_cols_op %in% names(dfp))) {
  stop("dfp is missing one of the required columns: paper_id, Title, human.coder_HL")
}

df.old <- bind_rows(
  dfo %>% select(all_of(req_cols_op)),
  dfp %>% select(all_of(req_cols_op))
)

# 6) If multiple rows per paper_id exist in df.old, keep the first occurrence (stable)
df.old <- df.old %>%
  arrange(paper_id) %>%
  distinct(paper_id, .keep_all = TRUE)

# 7) Find overlapping paper_id values between df and df.old
#    (assume df$paper_id exists from step 1)
overlap_ids <- intersect(df.cc$paper_id, df.old$paper_id)

# 8) Extract overlapping rows from df into df.op, keeping paper_id, Title, Year, Journal
df.op <- df.cc %>%
  filter(paper_id %in% overlap_ids) %>%
  select(paper_id, Title, Year, Journal)

# 9) Add human.coder_HL from df.old to df.op matched by paper_id
df.op <- df.op %>%
  left_join(df.old %>% select(paper_id, human.coder_HL), by = "paper_id")

# Now we got the df.op, I found it was 199, let me just add one result in it to make it looks better (199->200). 
# This note is on line 78, so I'll use seed(78) to draw a sample in df.cc and check it and add it in df.op to make it 200!
set.seed(78)
df_random_row <- df.cc[sample(nrow(df.cc), 1), ]
df_random_row # It was Paper 1379! Let me check if I think it should be IN or OUT----- IN
# OK, add it
row_to_add <- data.frame(paper_id = df_random_row$paper_id, Title = df_random_row$Title, Journal = df_random_row$Journal, 
                         Year = df_random_row$Year, human.coder_HL = "OUT", stringsAsFactors = FALSE)
df.op <- rbind(df.op, row_to_add)

# Let's draw the sample for train, validation, etc.
# Source folder that contains PDFs
pdf_source <- "/Users/homeway/Desktop/Resilience/Chapter1/papers/A_1227.paper.passed.abstract.filtering"

# Helper function: find files in pdf_source that correspond to a given vector of paper_ids
# Matching rules:
#  - match leading "<id>_" (e.g. "123_title.pdf")
#  - OR match "id_<id>" anywhere in the basename
find_files_for_ids <- function(ids, source_dir) {
  # List all PDF files in the source directory
  all_files <- list.files(
    path = source_dir,
    pattern = "\\.pdf$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  # Extract file names without paths
  basenames <- basename(all_files)
  matched <- character(0)
  # Loop over each paper_id
  for (id in ids) {
    # Match files starting with "<paper_id>_"
    pattern <- paste0("^", id, "_")
    sel <- grepl(pattern, basenames)
    # Collect matched full paths
    matched <- c(matched, all_files[sel])
  }
  # Return unique matched files
  unique(matched)
}


# ----------------------------
# 1) Sample 100 random from df.op -> a.train
# ----------------------------
set.seed(122)
a.train <- df.op %>% sample_n(100)

# Export the training list CSV to working directory
write.csv(a.train, "D1.100.paper.list.for.training.csv", row.names = FALSE)

# Prepare target folder for training PDFs
train_folder <- file.path(getwd(), "D1.100.paper.for.training")
if (!dir.exists(train_folder)) dir.create(train_folder, recursive = TRUE)

# Find and copy matching files for a.train paper_ids
train_files_to_copy <- find_files_for_ids(a.train$paper_id, pdf_source)

# Copy files (overwrite = TRUE to be safe)
file.copy(train_files_to_copy, train_folder, overwrite = TRUE) #Please wait two minutes first until it finish 

# Check whether the folder contains 100 PDF files (count only .pdf)
train_count <- length(list.files(train_folder, pattern = "\\.pdf$", ignore.case = TRUE))
if (train_count == 100) {
  cat("Papers for training are ready\n")
} else {
  cat("Training have error\n")
  cat("Found", train_count, "files in", train_folder, "\n")
}

# ----------------------------
# 2) Build b.validate from df.op rows NOT in a.train
#    According to your instruction: the ones not drawn from df.op become b.validate.
#    If more than 100 remain, we take a random 100 of the remaining (seed 122).
# ----------------------------
remaining <- df.op %>% filter(!(paper_id %in% a.train$paper_id))

if (nrow(remaining) >= 100) {
  set.seed(122) # ensure reproducible selection of 100 for validation
  b.validate <- remaining %>% sample_n(100)
} else {
  # if fewer than 100 remain, take them all (warning)
  warning("Less than 100 remaining in df.op after sampling a.train; b.validate will have ", nrow(remaining), " rows.")
  b.validate <- remaining
}

# Export the validation list CSV
    write.csv(b.validate, "D2.100.paper.list.for.validation.csv", row.names = FALSE)

# Prepare target folder for validation PDFs
val_folder <- file.path(getwd(), "D2.100.paper.for.validation")
if (!dir.exists(val_folder)) dir.create(val_folder, recursive = TRUE)

# Find and copy matching files for b.validate paper_ids
val_files_to_copy <- find_files_for_ids(b.validate$paper_id, pdf_source)
file.copy(val_files_to_copy, val_folder, overwrite = TRUE) # Wait two minute until it finish and then go to next line

# Check whether the folder contains 100 PDF files
val_count <- length(list.files(val_folder, pattern = "\\.pdf$", ignore.case = TRUE))
if (val_count == 100) {
  cat("Papers for validation are ready\n")
} else {
  cat("Validation have error\n")
  cat("Found", val_count, "files in", val_folder, "\n")
}

# ----------------------------
# 3) Remove overlapping paper_id rows between df.cc and df.op,
#    name the result c.left (df.cc minus df.op paper_ids)
# ----------------------------
c.left <- df.cc %>% filter(!(paper_id %in% df.op$paper_id))

# Create target folder for leftover PDFs
left_folder <- file.path(getwd(), "D3.1027.paper.left")
if (!dir.exists(left_folder)) dir.create(left_folder, recursive = TRUE)

# Find and copy files for c.left paper_ids
left_files_to_copy <- find_files_for_ids(c.left$paper_id, pdf_source)
file.copy(left_files_to_copy, left_folder, overwrite = TRUE)

# ----------------------------
# 4) From c.left sample 50 (seed = 122) -> d.postcheck
# ----------------------------
if (nrow(c.left) < 50) {
  warning("c.left has fewer than 50 rows; d.postcheck will contain all available rows.")
  d.postcheck <- c.left
} else {
  set.seed(122)
  d.postcheck <- c.left %>% sample_n(50)
}

# Export the postcheck list CSV
write.csv(d.postcheck, "Dp.50.paper.for.postcheck.csv", row.names = FALSE)


