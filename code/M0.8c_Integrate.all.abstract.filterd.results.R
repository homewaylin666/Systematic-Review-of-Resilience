# Module 0.8c Integrate all the abstract filtered results
rm(list=ls())
library(here)
library(dplyr)

#import data sheets
df.train <- read.csv(here("data", "C1a.50.for.abstract.filtering_edited.csv"))
df.validate <- read.csv(here("data", "C1b.100.for.abstract.filtering.validation_edited.csv"))
df.apply <- read.csv(here("data", "Cb.left.abstract.filtered.result.csv"))
df.post <- read.csv(here("data", "Cb1.50.for.abstract.filtering.postcheck_edited.csv"))

################## merge the first three and keep useful cols ##################
# Filter df.train and df.validate to human.coder_HL == "yes"
df_train_yes <- df.train[df.train$llm.gpt.5.2_new.prompt3 == "yes", c(
  "Abstract_ID", "Title", "Abstract", "Year",
  "Keywords.author", "Journal"
)]

df_validate_yes <- df.validate[df.validate$llm.gpt.5.2_new.prompt3 == "yes", c(
  "Abstract_ID", "Title", "Abstract", "Year",
  "Keywords.author", "Journal"
)]

# Extract required columns from df.apply (no filtering)
df_apply_all <- df.apply[, c(
  "Abstract_ID", "Title", "Abstract", "Year",
  "Keywords.author", "Journal"
)]

# Combine all into df
df <- rbind(
  df_train_yes,
  df_validate_yes,
  df_apply_all
)

# Sort by Abstract_ID (ascending)
df <- df[order(df$Abstract_ID), ]

################## merge the human checked sample as a backup ##################
df.hc <- rbind(
  df.train[, c(
    "Abstract_ID", "Title", "Abstract", "Year", "Journal",
    "Keywords.author", "human.coder_HL", "note"
  )],
  df.validate[, c(
    "Abstract_ID", "Title", "Abstract", "Year", "Journal",
    "Keywords.author", "human.coder_HL", "note"
  )],
  df.post[, c(
    "Abstract_ID", "Title", "Abstract", "Year", "Journal",
    "Keywords.author", "human.coder_HL", "note"
  )]
)

################## I've found problematic papers, delete them! ##################
df <- df[df$Abstract_ID != 1525, ] # retracted
df <- df[!df$Abstract_ID %in% c(2108, 2119, 2123, 2131, 2140, 2146, 2147), ] # duplicate

################## export this file ##################
num <- nrow(df)
file_name <- paste0("Cc.abstract.screened.result_", num, ".csv")
write.csv(df, here("data",file_name), row.names = FALSE)
write.csv(df.hc, here("data", 'Cb3.all.human.checked.result.csv'))
