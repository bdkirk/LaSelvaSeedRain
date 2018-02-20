# This file was started on 20 Feb 18 to look at whether adults of the seeds found in the traps were also found in the experimental plots as adults. Potential seed source.


# Load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/RawData")

# bring in data
psource <- read_excel("ECOS_SeedRain_9Sept17_ar.xlsx", sheet = 2, col_names=TRUE, na= "NA")

#remove column of raw, uncorrected data
psource2 <- subset(psource, select = c(1, 2, 4))
colnames(psource2) <- c("family", "species", "adult")
str(psource2)
psource2$species <- as.factor(psource2$species)
psource2$species <- tolower(psource2$species)

# bring in data on which species 
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

seeds_sub <- read.csv("abund_summary_year_notrtsp_nw.csv", header = TRUE)

#remove irrelevant rows
seeds_sub2 <- subset(seeds_sub, select = c(1:5))
str(seeds_sub2)

#join two datasets
pss <- left_join(seeds_sub2, psource2, by= "species")

#remove NA row
pss <- pss[complete.cases(pss),]
pss$adult <- as.factor(pss$adult)

#bring in ecology
ecology <- read.csv("seedtraits_tidy_sub_notrt_nw.csv", header = TRUE)

# join two datasets
pss_ecology <- left_join(pss, ecology, by = "species")
str(pss_ecology)
pss_ecology$species <- as.factor(pss_ecology$species)
pss_ecology$family <- as.factor(pss_ecology$family)

#########################################################################

# 1) How many adults were found in the experimental plot out of the 118 species?  

summary(pss_ecology)
# 61 yes and 57 no

# create subset to look at species that came in 

pss_no <- filter(pss_ecology, adult =="n")
pss_yes <- filter(pss_ecology, adult =="y")

summary(pss_no)
summary(pss_yes)


# 2) Are they unique to a particular treatment?

# bring in data
unique <- read.csv("unique_seeds_traits.csv", header= TRUE)

#remove unnecessary columns
unique2 <- subset(unique, select = c(1, 2))
# combine data sets 
pss_no_unique <- left_join(pss_no, unique2, by = "species")

summary(pss_no_unique)
