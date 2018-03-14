# Started on 7 March 18

# abandoned this file on 14 March 18 and chose to do all of the wrangling for Table 1 in more updated ecology file in succession project.






# create file to find the species richness, percent of total abundance and seed density

# load libraries
library(tidyverse); library(dplyr); library(scales)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

# bring in dataset
dispersal <- read.csv("seedtrait_dis_abund.csv")

#get the abundance of seeds per treatment and then the percentage
d_abund <- ddply(dispersal, .(treatment, dispersal), summarise, seednum=sum(seednum))

#code is not working as of 9 March 18
d_abund2 <- d_abund %>% group_by(treatment) %>% summarise(trt_tot = sum(seednum)) %>% right_join(d_abund) %>% mutate(percent = percent(seednum/trt_tot))

d_abund2 <- 

# get number of species for each dispersal mode
species_dispersal <- filter(dispersal, seednum >= 1)
species_dispersal2 <- ddply(species_dispersal, .(treatment, dispersal), summarise, richness= length(species))

# get the density of seeds per treatment per category
seed_d <- d_abund




############## Life form #################
lifeform <- read.csv("seedtrait_life_abund.csv")

#get the abundance of seeds per treatment and then the percentage
l_abund <- ddply(lifeform, .(treatment, lifeform), summarise, seednum=sum(seednum))

l_abund2 <- l_abund %>% group_by(treatment) %>% summarise(trt_tot = sum(seednum)) %>% right_join(l_abund) %>% mutate(percent = percent(seednum/trt_tot))

# get number of species for each dispersal mode
species_lf <- filter(lifeform, seednum >= 1)
species_lf2 <- ddply(species_lf, .(treatment, lifeform), summarise, richness= length(species))        

# Calculate density in excel by dividing abundance to the treatment by the area of traps per treatment
