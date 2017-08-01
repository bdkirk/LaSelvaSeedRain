#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species abundance can be found
# Abundance is defined as the total number of seeds found in each plot.  Will have 15 observations
#Experimental design: RCBD, C. Total df = 14


#Load libraries
library(readr); library(plyr); library(ggplot2); library(reshape2)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#naming datafile seedrain from excel file
abundance <- read.csv("yearsub_no_trtsp.csv")

#This sums up the plots according to species for abundance calculations
seedrain_plot <- ddply(abundance, .(plot, species), summarise, seednum=sum(total_seednum))

str(abundance)
abund <- ddply(abundance, .(plot), summarise, total_seednum=sum(total_seednum))

write.csv (abund, "abund_sub_notrtsp.csv", row.names = FALSE)

#Need to do the following:
#added in columns in excel for predictor variables of treatment and block

# finished on 15 May 2017
