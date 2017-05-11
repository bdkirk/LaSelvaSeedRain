#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species diversity can be found
# Seed rain diversity: Richness, Shannon-Weiner diversity index, Evenness (Pielou's)
# There will be 15 total observations with traps summarised up to the plot level.
#Experimental design: RCBD, C. Total df = 14

#load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base)

#bring in original data file with overstory species removed that has been cut to the specific dates
setwd("Data/TidyData")

seeddiv <- read.csv("yearsub_no_cpysp.csv")

#summarise the data by plot
plotsum <- ddply(seeddiv, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING NMDS DATA##########################################################
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
species_data <- dcast(plotsum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
species_data <- species_data[,c(names(species_data)[1],sort(names(species_data)[2:ncol(species_data)]))]
#identifies all seed rain species that are NA
species_data <- species_data[, -which(names(species_data)=="NA")]
#Replace NA's with zeros
species_data[is.na(species_data)] <- 0

#create csv file that can be used to do NMDS calculations
write.csv(species_data, "div_sub_nocpy.csv", row.names = FALSE)

##This data file has no overstory species and is subsetted for a year long period from 2-24-14 to 2-23-15

