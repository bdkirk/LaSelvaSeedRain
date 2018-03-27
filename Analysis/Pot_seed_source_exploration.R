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

ecology_sub <- read.csv("ecology_sub_notrt_nw.csv")

#join two datasets
pss <- left_join(ecology_sub, psource2, by= "species")

#remove NA row
str(pss)
pss$adult <- as.factor(pss$adult)
pss$family <- as.factor(pss$family)
pss$lifeform <- as.factor(pss$lifeform)


#########################################################################

# 1) How many adults were found in the experimental plot out of the 118 species?  

summary(pss)
# 64 yes and 57 no

# find the number of families
count(pss$family) #40

# create subset to look at species that came in 
pss_no <- filter(pss, adult =="n")
pss_yes <- filter(pss, adult =="y")

summary(pss_no)
summary(pss_yes) # palms were already found within plot. no new palm species arrived...

# Create figure with stacked bars for the life form and total number of species

# a1) pss_no evaluation of number of species by life form

# melt data so treatment is a variable
pss_no1 <- subset(pss_no, select= c(1:7))
pss_no2 <- melt(pss_no1, id.vars = c("species", "dispersal", "lifeform"), value=seednum)
pss_no3 <- filter(pss_no2, value >= 1)
pss_no_lf <- ddply(pss_no3, .(variable, lifeform), summarise, sum=(length(species)))

# a2) pss_no evaluation of abundance of seeds by life form
pss_no_lf_abund <- ddply(pss_no3, .(variable, lifeform), summarise, sum=(sum(value)))

# a3) pss_no evaluation of density of seeds by life form
   # calculated in excel by dividing by total treatment plots

# Create data for figure with life form for seeds with conspecifics in plots

# b1) pss_yes evaluation of number of species by life form

# melt data so treatment is a variable
pss_yes1 <- subset(pss_yes, select= c(1:7))
pss_yes2 <- melt(pss_yes1, id.vars = c("species", "dispersal", "lifeform"), value=seednum)
pss_yes3 <- filter(pss_yes2, value>= 1)
pss_yes_lf <- ddply(pss_yes3, .(variable, lifeform), summarise, sum=(length(species)))

# b2) pss_yes evaluation of abundance of seeds by life form
pss_yes_lf_abund <- ddply(pss_yes3, .(variable, lifeform), summarise, sum=(sum(value)))

# b3) pss_no evaluation of density of seeds by life form




# What is the abundance of animal seeds that are being brought in relative to wind dispersed seeds. Does it vary based on treatment?
pss_no_animal <- filter(pss_no, dispersal =="animal")
#pss_no_animal_abund <- ddply(pss_no_animal, .())

# What family has the greatest abundance of seed rain?
pss_ecology2 <- subset(pss, select= c(1:5))
pss_ecology3 <- melt(pss_ecology2, id.vars= c("species"), value=seednum)

pss_ecology3$family <- pss_ecology$family[match(pss_ecology$species, pss_ecology3$species)]

pss_ecology4 <- ddply(pss_ecology3, .(family), summarise, seednum=sum(value))

# 2) Are they unique to a particular treatment?

# bring in data
unique <- read.csv("unique_seeds_traits.csv", header= TRUE)

#remove unnecessary columns
unique2 <- subset(unique, select = c(1, 2))
# combine data sets 
pss_no_unique <- left_join(pss_no, unique2, by = "species")

summary(pss_no_unique)
