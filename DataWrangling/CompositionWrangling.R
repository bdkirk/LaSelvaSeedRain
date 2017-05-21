# Started on 13 May 2017
# This involves wrangling of data so that tree effects on seed species composition can be determined.  This data needs to be in a wide format.
# Traps will be summarised to the plot level
# Note when analyzing that the model needs to include blocks

#load libraries
library(readr); library(plyr); library(ggplot2); library(reshape2); library(base)

#Get tidy file of all original data and reformat
setwd("Data/TidyData")

#This is file with the years worth of data that has been subsetted and no canopy species were included in seedrain
seedcomp<- read.csv("yearsub_no_cpysp.csv")

#summarise the data by plot
plotsum <- ddply(seedcomp, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA##########################################################
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
species_comp <- dcast(plotsum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
species_comp <- species_comp[,c(names(species_comp)[1],sort(names(species_comp)[2:ncol(species_comp)]))]
#identifies all seed rain species that are NA
species_comp <- species_comp[, -which(names(species_comp)=="NA")]
#Replace NA's with zeros
species_comp[is.na(species_comp)] <- 0

#create csv file that can be used to do NMDS calculations
write.csv(species_comp, "comp_sub_nocpy.csv", row.names = FALSE)

# need to manually add in block and canopysp columns in excel

#Finished on 12 May 17