# Module 0.6 Find the new-added papers
rm(list=ls())
library(here)

# import the search results
old.sr <- read.csv(here("data", "Bb.old.serach.result.exclude.indexkeyword_1058.csv"))
new.sr <- read.csv(here("data", "Bb.serach.result.exclude.indexkeyword_1959.csv"))

### create a df that only include the new-added ones
new.add <- setdiff(new.sr$Title, old.sr$Title)
new.add.df <- dplyr::filter(new.sr, Title %in% new.add)

# an insurance here
if (nrow(new.add.df) != nrow(new.sr)-nrow(old.sr)) { # it means the different search results 
  # now have different but similar name for the same papers
  # let's merge them first
  library(dplyr)
  merged_sr <- bind_rows(old.sr, new.sr)
  # mark the duplicate papers in a typo-recognizing way
  library(stringdist)
  titles <- merged_sr$Title
  dist_mat <- stringdistmatrix(titles, titles, method = "lv")
  hc <- hclust(as.dist(dist_mat), method = "single")
  clusters <- cutree(hc, h = 2)
  merged_sr$ClusterID <- clusters
  # only keep the unique one
  new.add.df <- merged_sr %>%
    group_by(ClusterID) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    select(-ClusterID)
}

#If the nrow of new.add.df is still over nrow(new.sr)-nrow(old.sr), 
# just correct it manually, must be only few mistakes left.

# change its Abstract ID
add.num <- max(old.sr$Abstract_ID) + 1
add.len <- nrow(new.add.df)
new.add.df <- new.add.df %>%
  mutate(Abstract_ID = add.num:(add.num + add.len-1))

# merge the old one and new one
new.df <- bind_rows(old.sr, new.add.df)

# export this file
file_name <- paste0("Bn.serach.result_", nrow(new.df), ".csv")
write.csv(new.df, file_name, row.names = FALSE)
