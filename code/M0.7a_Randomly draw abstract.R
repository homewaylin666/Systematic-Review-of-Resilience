# Module 0.7a Randomly draw abstracts
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")

# import the abstract filtering table
abstract <- read.csv("Bn.serach.result_1959.csv")
################## for LLM main-text filtering ##################
# Step 1: Exclude the data from 2026
library(dplyr)
filtered_abstract <- abstract %>%
  filter(!Year %in% c(2026))
# Step 2: Randomly sample 150 rows from the filtered dataset
set.seed(117)
sampled_150 <- filtered_abstract %>%
  sample_n(150)
# Step 3: Randomly chose 50 of them to create training set
C1a_50 <- sampled_150 %>%
  sample_n(50)
write.csv(C1a_50,"C1a.50.for.abstract.filtering.csv",row.names = FALSE)
# Step 4: Chose the left 150 to create validate set
C1b_100 <- sampled_150 %>%
  anti_join(C1a_50, by = colnames(sampled_150))
write.csv(C1b_100,"C1b.100.for.abstract.filtering.validation.csv",row.names = FALSE)
# Step 5: Use all remaining abstracts
C1c_others <- filtered_abstract %>%
  anti_join(sampled_150, by = colnames(filtered_abstract))
write.csv(C1c_others,"C1c.others.for.abstract.filtering.csv",row.names = FALSE)
# Step 6: Randomly sample 50 abstracts from C1c for post-check (Cb1)
Cb1_50 <- C1c_others %>%
  sample_n(50)
write.csv(Cb1_50,"Cb1.50.for.abstract.filtering.postcheck.csv",row.names = FALSE)
