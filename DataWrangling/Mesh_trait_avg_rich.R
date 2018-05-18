# This file was started on 17 May 18

# This file explores average number of species per trap by (1) dispersal mode, (2) life form, (3) successional affinity

# load libraries
library(tidyverse); library(readxl)
# note tidyverse blocks: stats::filter()


# bring in data files
# first bring in ecology file that is more updated from succession data
setwd("~/M.S. Thesis/Data/GitHubProjects/Succession/Data/Raw")

ecology <- read_excel("Seedsize_sa2.xlsx", sheet=2, col_names=TRUE, na= "NA")

#clean up file to have just information required for further analysis
ecology2 <- subset(ecology, select= c(3, 4, 5, 6, 13))
ecology2[is.na(ecology2)] <- "NA"

#check to see what columns are and amend
str(ecology2)
ecology2$species <- as.factor(ecology2$species)
ecology2$lifeform <- as.factor(ecology2$lifeform)
ecology2$dispersal <- as.factor(ecology2$dispersal)
ecology2$size_cat <- as.factor(ecology2$size_cat)
ecology2$suc_affinity <- as.factor(ecology2$suc_affinity)

#change working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

seed_data <- read.csv("yearsub_no_trtsp_nw.csv")

#clean up file so only have pertinent columns
seed_data2 <- subset(seed_data, select= c(1, 2, 3, 4, 5, 8, 10))

# check to see if columns have same assignment
str(seed_data2) # they do now

# Merge files so you have one to work with
avg_seedeco <- left_join(seed_data2, ecology2, by = "species")

### Dispersal Mode ####
# create full dispersal file to export?


# Create two separate files for dispersal mode
library(stats)
animal <- filter(avg_seedeco, dispersal == "animal")
wind <- filter(avg_seedeco, dispersal == "wind")
mech <- filter(avg_seedeco, dispersal == "mechanical")

# summarise files to get average number per trap
library(plyr)

# a) animal! 
#First summarise by species within traps
animal_rich <- ddply(animal, .(trap, species, meshtype), summarise, sum= sum(total_seednum))
str(animal_rich)
#summarise number of species found in each trap # missing from trap 63
animal_rich2 <- ddply(animal_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 63 # Shout out to Sara Schwarz for helping me figure this out!
animal_rich2[(nrow(animal_rich2)+1),] <- c("63", "meshreg", "0")
str(animal_rich2)

#convert to numeric for calculations to follow
animal_rich2$richness <- as.numeric(animal_rich2$richness)

#find average number of seeds per trap and se 
animal_rich3 <- ddply(animal_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# b) wind!
#First summarise by species within traps
wind_rich <- ddply(wind, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
wind_rich2 <- ddply(wind_rich, .(trap, meshtype), summarise, richness= length(species))

#find average number of seeds per trap and se
wind_rich3 <- ddply(wind_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# c) mechanical! # this doesn't appear to be accurate because we did not find mechanical in each trap.
#First summarise by species within traps
mech_rich <- ddply(mech, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
mech_rich2 <- ddply(mech_rich, .(trap, meshtype), summarise, richness= length(species))

#find average number of seeds per trap and se
#mech_rich3 <- ddply(mech_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))
#NOTE: For Mechanical, did not add in 72 additional traps so this information is not useful.

### Life Form ####

# Create two separate files for life form
tree <- filter(avg_seedeco, lifeform == "tree")
shrub <- filter(avg_seedeco, lifeform == "shrub")
liana <- filter(avg_seedeco, lifeform == "liana")
palm <- filter(avg_seedeco, lifeform == "palm")
unk <- filter(avg_seedeco, lifeform == "NA")


# a) tree! #### Missing a tree species from trap number 14...not sure if can add a zero in somehow...
#First summarise by species within traps
tree_rich <- ddply(tree, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
tree_rich2 <- ddply(tree_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
tree_rich2[(nrow(tree_rich2)+1),] <- c("14", "meshreg", "0")
str(tree_rich2)

#convert to numeric for calculations to follow
tree_rich2$richness <- as.numeric(tree_rich2$richness)

#find average number of seeds per trap and se
tree_rich3 <- ddply(tree_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# b) shrub!
#First summarise by species within traps
shrub_rich <- ddply(shrub, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
shrub_rich2 <- ddply(shrub_rich, .(trap, meshtype), summarise, richness= length(species))

#find average number of seeds per trap and se
shrub_rich3 <- ddply(shrub_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# c) liana!
#First summarise by species within traps
liana_rich <- ddply(liana, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
liana_rich2 <- ddply(liana_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 63 # Shout out to Sara Schwarz for helping me figure this out!
liana_rich2[(nrow(liana_rich2)+1),] <- c("6", "meshreg", "0")
liana_rich2[(nrow(liana_rich2)+1),] <- c("14", "meshreg", "0")
liana_rich2[(nrow(liana_rich2)+1),] <- c("22", "meshreg", "0")
liana_rich2[(nrow(liana_rich2)+1),] <- c("39", "meshreg", "0")
liana_rich2[(nrow(liana_rich2)+1),] <- c("48", "meshreg", "0")
liana_rich2[(nrow(liana_rich2)+1),] <- c("71", "meshreg", "0")

str(liana_rich2)

#convert to numeric for calculations to follow
liana_rich2$richness <- as.numeric(liana_rich2$richness)

#find average number of seeds per trap and se
liana_rich3 <- ddply(liana_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# d) palm!
#First summarise by species within traps
palm_rich <- ddply(palm, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
palm_rich2 <- ddply(palm_rich, .(trap, meshtype), summarise, richness= length(species))

# 69 missing traps, too small

#find average number of seeds per trap and se
#palm_rich3 <- ddply(palm_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# e) Unknowns!
#First summarise by species within traps
unk_rich <- ddply(unk, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
unk_rich2 <- ddply(unk_rich, .(trap, meshtype), summarise, richness= length(species))

#34 traps did not have unknowns. did not add these into rows.

#find average number of seeds per trap and se
#unk_rich3 <- ddply(unk_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))



#### Successional Affinity #####

# Create separate files for successional affinity
lgen <- filter(avg_seedeco, suc_affinity == "generalist")
lyf <- filter(avg_seedeco, suc_affinity == "yf")
lof <- filter(avg_seedeco, suc_affinity == "of")
tgen <- filter(avg_seedeco, suc_affinity == "gen")
tsec <- filter(avg_seedeco, suc_affinity == "sec_sp")
tog <- filter(avg_seedeco, suc_affinity == "og_sp")
unknown <- filter(avg_seedeco, suc_affinity == "NA")

# a) liana generalist! #### Missing a tree species from trap number 14
#First summarise by species within traps
lgen_rich <- ddply(lgen, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
lgen_rich2 <- ddply(lgen_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("6", "meshreg", "0")
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("14", "meshreg", "0")
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("22", "meshreg", "0")
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("39", "meshreg", "0")
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("48", "meshreg", "0")
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("71", "meshreg", "0")
lgen_rich2[(nrow(lgen_rich2)+1),] <- c("74", "meshreg", "0")

str(lgen_rich2)

#convert to numeric for calculations to follow
lgen_rich2$richness <- as.numeric(lgen_rich2$richness)

#find average number of seeds per trap and se
lgen_rich3 <- ddply(lgen_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))


# b) liana yf! 
#First summarise by species within traps
lyf_rich <- ddply(lyf, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
lyf_rich2 <- ddply(lyf_rich, .(trap, meshtype), summarise, richness= length(species))

# 21 missing!!!

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
lyf_rich2[(nrow(lyf_rich2)+1),] <- c("14", "meshreg", "0")
str(lyf_rich2)

#convert to numeric for calculations to follow
lyf_rich2$richness <- as.numeric(lyf_rich2$richness)

#find average number of seeds per trap and se
lyf_rich3 <- ddply(lyf_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))


# c) liana of! #### Not doing this one right now, found in 2 traps
#First summarise by species within traps
lof_rich <- ddply(lof, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
lof_rich2 <- ddply(lof_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
lof_rich2[(nrow(lof_rich2)+1),] <- c("14", "meshreg", "0")
str(lof_rich2)

#convert to numeric for calculations to follow
lof_rich2$richness <- as.numeric(lof_rich2$richness)

#find average number of seeds per trap and se
lof_rich3 <- ddply(lof_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# d) generalist tree! 
#First summarise by species within traps
tgen_rich <- ddply(tgen, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
tgen_rich2 <- ddply(tgen_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for 9 traps
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("13", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("14", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("17", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("48", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("61", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("63", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("71", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("45", "meshreg", "0")
tgen_rich2[(nrow(tgen_rich2)+1),] <- c("50", "meshreg", "0")

str(tgen_rich2)

#convert to numeric for calculations to follow
tgen_rich2$richness <- as.numeric(tgen_rich2$richness)

#find average number of seeds per trap and se
tgen_rich3 <- ddply(tgen_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))


# e) secondary specialist tree! 
#First summarise by species within traps
tsec_rich <- ddply(tsec, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
tsec_rich2 <- ddply(tsec_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
tsec_rich2[(nrow(tsec_rich2)+1),] <- c("14", "meshreg", "0")
tsec_rich2[(nrow(tsec_rich2)+1),] <- c("10", "meshreg", "0")
str(tsec_rich2)

#convert to numeric for calculations to follow
tsec_rich2$richness <- as.numeric(tsec_rich2$richness)

#find average number of seeds per trap and se
tsec_rich3 <- ddply(tsec_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))


# a) old growth tree! # not running tonight
#First summarise by species within traps
tog_rich <- ddply(tog, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
tog_rich2 <- ddply(tog_rich, .(trap, meshtype), summarise, richness= length(species))

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
tog_rich2[(nrow(tog_rich2)+1),] <- c("14", "meshreg", "0")
str(tog_rich2)

#convert to numeric for calculations to follow
tog_rich2$richness <- as.numeric(tog_rich2$richness)

#find average number of seeds per trap and se
tog_rich3 <- ddply(tog_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))

# a) unkowns! 
#First summarise by species within traps
unknown_rich <- ddply(unknown, .(trap, species, meshtype), summarise, sum= sum(total_seednum))

#summarise number of species found in each trap
unknown_rich2 <- ddply(unkown_rich, .(trap, meshtype), summarise, richness= length(species))

# missing 34 traps. will not run.

# add zero for trap 14 # Shout out to Sara Schwarz for helping me figure this out!
unknown_rich2[(nrow(unknown_rich2)+1),] <- c("14", "meshreg", "0")
str(unknown_rich2)

#convert to numeric for calculations to follow
unknown_rich2$richness <- as.numeric(unknown_rich2$richness)

#find average number of seeds per trap and se
unknown_rich3 <- ddply(unknown_rich2, .(meshtype), summarise, mean= round(mean(richness), 2), se=round(sd(richness)/sqrt(length(richness)), 4))


