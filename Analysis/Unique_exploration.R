# This is to look at the unique species

# created on 19 Feb 18

#Load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/RawData")

#naming datafile seedrain from excel file and pulling data from sheet 4
unique_seeds <-read_excel("Unique_species.xlsx", sheet=1, col_names=TRUE, na= "NA")

# Bring in other file 
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

ecology <- read.csv("seedtraits_tidy_sub_notrt_nw.csv", header = TRUE)

# join the two files

unique_seeds2 <- left_join(unique_seeds, ecology, by = "species")

str(unique_seeds2)
unique_seeds2$unique <- as.factor(unique_seeds2$unique)

write.csv(unique_seeds2, "unique_seeds_traits.csv", row.names = FALSE)
#########################################################################
# Figures

ggplot(unique_seeds2, aes(unique, lifeform))+
  geom_bar(stat = "identity", aes(abundance))




