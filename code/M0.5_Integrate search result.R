# Module 0.5 Integrate Scopus and Wos search results
rm(list=ls())
library(here)
# import the search results
Wos<- read.csv(here("data", "A1.search.result_WoS_2181.csv"))
Sco<- read.csv(here("data", "A2.search.result_Scopus_1586.csv"))

#extract information for each df
Wos_clear <- Wos[,c("Article.Title", "Abstract", "Publication.Year", "Source.Title", "Author.Keywords", "Keywords.Plus")]
Sco_clear <- Sco[,c("Title","Abstract","Year","Source.title", "Author.Keywords", "Index.Keywords" )] 
clear_col <- c("Title","Abstract","Year", "Journal", "Keywords.author", "Keywords.index")
colnames(Wos_clear) <- c(clear_col)
colnames(Sco_clear) <- c(clear_col)

#integrate them
library(dplyr)
merged_search.result <- bind_rows(Wos_clear, Sco_clear) %>% 
  distinct(Title, .keep_all = T)

# then, I want to delete those "Correction article"
merged_search.result <- merged_search.result %>%
  filter(!grepl("Correction to", Title))

# however, due to some typos from Scopus, finding the exact same title is not enough.
# I also need to check similar Titles
library(stringdist)
titles <- merged_search.result$Title
dist_mat <- stringdistmatrix(titles, titles, method = "lv")
hc <- hclust(as.dist(dist_mat), method = "single")
clusters <- cutree(hc, h = 2)
merged_search.result$ClusterID <- clusters

#only keep the duplicate one and manually check, check abstract would be easy.
duplicates <- merged_search.result %>%
  group_by(ClusterID) %>%
  filter(n() > 1) %>%      # cluster >1
  ungroup() %>%
  arrange(ClusterID)

#no problem, then only keep the first one of each cluster
search.result <- merged_search.result %>%
  group_by(ClusterID) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-ClusterID) %>%
  mutate(Abstract_ID = row_number())

#export this file
sr.num <- nrow(search.result)
file_name <- paste0("B.serach.result_", sr.num, ".csv")
write.csv(search.result, here("data", file_name), row.names = FALSE)
