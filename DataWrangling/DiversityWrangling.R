#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species diversity can be found
# Seed rain diversity: Richness, Shannon-Weiner diversity index, Evenness (Pielou's)
# There will be 15 total observations with traps summarised up to the plot level.
#Experimental design: RCBD, C. Total df = 14

#load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(vegan)

#bring in original data file with overstory species removed that has been cut to the specific dates
setwd("Data/TidyData")

seeddiv <- read.csv("yearsub_no_trtsp.csv")

#summarise the data by plot
plotsum <- ddply(seeddiv, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA##########################################################
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
div_data <- dcast(plotsum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
div_data <- div_data[,c(names(div_data)[1],sort(names(div_data)[2:ncol(div_data)]))]
#identifies all seed rain species that are NA
div_data <- div_data[, -which(names(div_data)=="NA")]
#Replace NA's with zeros
div_data[is.na(div_data)] <- 0

#####add in columns for richness, evenness and shannon-wiener diversity
x <- div_data[,1]
y <- div_data[,2:124]


#calculating diversity indices
y$richness <- specnumber(y)

y$diversity <- diversity(y, index = "shannon")

y$evenness <- (y$diversity/(log(y$richness)))

#change diversity to something more recognizable
y$divnorm <- exp(diversity(y, index = "shannon"))

#bind x and y back together
div_data2 <- cbind(x, y[,124:127])
tail(div_data2)
#create csv file that can be used to do NMDS calculations
write.csv(div_data2, "div_sub_notrtsp.csv", row.names = FALSE)

# NEED to manually add in block and treatment

#finished on 21 May 2017
#After meeting with Dr. Dixon on 30 June decided to not use the normalized data for the analysis because through the analysis it will be normalized anyway.

##This data file has no treatment species and is subsetted for a year long period from 2-24-14 to 2-23-15

#data reran on 24 Jan 18 because names were updated.