# Module 0.5b exclude papers that only mention resilience in index keyword
rm(list=ls())
library(here)

# import the integrated search results
sr<- read.csv(here("data", "B.serach.result_2334.csv"))

# recognize the papers that only mention resilience in index keyword
sr$trait <- ifelse(
  apply(sr[c("Title", "Abstract", "Keywords.author")], 1, function(row) {
    any(grepl("resilien", row, ignore.case = TRUE))
  }),
  0,  # if any of these 3 cols content "resilient" or "resilience".
  1   # if all no
)

# exclude them
sr <- sr[sr$trait != 1,]
sr$trait <- NULL

#export this file
sr.num <- nrow(sr)
file_name <- paste0("Bb.serach.result.exclude.indexkeyword_", sr.num, ".csv")
write.csv(sr, file_name, row.names = FALSE)
