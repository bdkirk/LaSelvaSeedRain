#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species abundance can be found
# Abundance is defined as the total number of seeds found in each plot.  Will have 15 observations
#Experimental design: RCBD, C. Total df = 14


#Load libraries
library(readr); library(plyr); library(ggplot2); library(reshape2); library(tidyr)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#naming datafile seedrain from excel file
abundance <- read.csv("yearsub_no_trtsp_nw.csv")

#sums up by treatment, block and species to look at what's going on within each block and treatment combo
block_abund <- ddply(abundance, .(treatment, block, species), summarise, seednum=sum(total_seednum))
write.csv(block_abund, "trt_blk_spp_abund.csv", row.names= FALSE)

#sums up by treatment, block, trap, species
trap_abund <- ddply(abundance, .(treatment, block, trap, species), summarise, seednum=sum(total_seednum))
write.csv(trap_abund, "trt_blk_trap_spp_abund.csv", row.names = FALSE)

#This sums up the plots according to species for abundance calculations
str(abundance)
abund <- ddply(abundance, .(plot), summarise, total_seednum=sum(total_seednum))

#add in columns for block and treatment
abund2 <- separate(abund, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv (abund2, "abund_sub_notrtsp_nw.csv", row.names = FALSE)
# finished on 15 May 2017

#get data for summary table

##check to see how many species should actually be in summary table 
seedrain_plot <- ddply(abundance, .(plot, species), summarise, seednum=sum(total_seednum))
seedrain_treatment <- ddply(abundance, .(treatment, species), summarise, seednum=sum(total_seednum))

#spread the data so that 
abund_summary <- spread(seedrain_treatment, treatment, seednum)

#add in zeros for NA
abund_summary[is.na(abund_summary)] <- 0
#this works but will not change the column/unknowns to zero


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
write.csv(abund_summary, "abund_summary_year_notrtsp_nw.csv", row.names=FALSE)

#####Notes
# Redone on 22 Jan 18 because names needed to be changed.

# redone on 2 Feb 18 to add in columns for block and treatment

# redone on 12 Feb 18 to correct species names that were not corrected and removed three woody species.

