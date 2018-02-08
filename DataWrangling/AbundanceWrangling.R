#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species abundance can be found
# Abundance is defined as the total number of seeds found in each plot.  Will have 15 observations
#Experimental design: RCBD, C. Total df = 14


#Load libraries
library(readr); library(plyr); library(ggplot2); library(reshape2); library(tidyr)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#naming datafile seedrain from excel file
abundance <- read.csv("yearsub_no_trtsp.csv")

#This sums up the plots according to species for abundance calculations
str(abundance)
abund <- ddply(abundance, .(plot), summarise, total_seednum=sum(total_seednum))

#add in columns for block and treatment
abund2 <- separate(abund, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv (abund2, "abund_sub_notrtsp.csv", row.names = FALSE)
# finished on 15 May 2017

#get data for summary table

##check to see how many species should actually be in summary table 
seedrain_plot <- ddply(abundance, .(plot, species), summarise, seednum=sum(total_seednum))
seedrain_treatment <- ddply(abundance, .(treatment, species), summarise, seednum=sum(total_seednum))

#spread the data so that 
abund_summary <- spread(seedrain_treatment, treatment, seednum)

#add in zeros for NA
abund_summary[is.na(abund_summary)] <- 0
#this works but will not change the column/unkowns to zero


#add seeds per m2/year to table
#a) first add in other columns
abund_summary$Hial_R <- NA
abund_summary$Pema_R <- NA
abund_summary$Viko_R <- NA
abund_summary$Vogu_R <- NA

#b) add values in.
abund_summary$Hial_R <- (abund_summary$Hial)/(10.4)
abund_summary$Pema_R <- (abund_summary$Pema)/(10.4)
abund_summary$Viko_R <- (abund_summary$Viko)/(10.4)
abund_summary$Vogu_R <- (abund_summary$Vogu)/(7.8)
abund_summary$spp_total <- abund_summary$Hial+abund_summary$Pema + abund_summary$Viko + abund_summary$Vogu
write.csv(abund_summary, "abund_summary_year_notrtsp.csv", row.names=FALSE)

#####Notes
# Redone on 22 Jan 18 because names needed to be changed.

# redone on 2 Feb 18 to add in columns for block and treatment
