# Module 0.8 Filter the screened result
rm(list=ls())
setwd("/Users/homeway/Desktop/Resilience/Chapter1/data")

# import the screened result
sr <- read.csv("C3.left.abstract.filtering_5.2_Temp_0_edited.csv")

# removed the rows that didn't pass the protocol
s.sr <- subset(
  sr,
  Review == "NO" &
    Eco.Quantify == "YES" &
    Abiotic == "NO" &
    Human == "NO" &
    Year != 2026
)

# removed the columns that no longer needed
s.sr <- s.sr[, !names(s.sr) %in% c("Review","Eco.Quantify","Abiotic","Human")]

# export this file
write.csv(s.sr, "Cb.left.abstract.filtered.result.csv", row.names = FALSE)
