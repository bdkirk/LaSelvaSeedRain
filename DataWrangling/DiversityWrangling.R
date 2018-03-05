#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species diversity can be found
# Seed rain diversity: Richness, Shannon-Weiner diversity index, Evenness (Pielou's)
# There will be 15 total observations with traps summarised up to the plot level.
#Experimental design: RCBD, C. Total df = 14

#load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(vegan); library(tidyr)

#bring in original data file with overstory species removed that has been cut to the specific dates
setwd("Data/TidyData")

seeddiv <- read.csv("yearsub_no_trtsp_nw.csv")

#summarise the data by plot
plotsum <- ddply(seeddiv, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA##########################################################
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
div_data <- dcast(plotsum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
div_data <- div_data[,c(names(div_data)[1],sort(names(div_data)[2:ncol(div_data)]))]
#identifies all seed rain species that are NA
#div_data <- div_data[, -which(names(div_data)=="NA")]
#Replace NA's with zeros
div_data[is.na(div_data)] <- 0

#####add in columns for richness, evenness and shannon-wiener diversity
x <- div_data[,1]
y <- div_data[,2:122]

str(div_data)
#calculating diversity indices
y$richness <- specnumber(y)

y$diversity <- diversity(y, index = "shannon")

y$evenness <- (y$diversity/(log(y$richness)))

#change diversity to something more recognizable
y$divnorm <- exp(diversity(y, index = "shannon"))

#bind x and y back together
div_data2 <- cbind(x, y[,122:125])
tail(div_data2)

#change name for X column
names(div_data2)[names(div_data2) == "x"] <- "plot"

#add in columns for treatment and block
div_data3 <- separate(div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(div_data3, "div_sub_notrtsp_nw.csv", row.names = FALSE)


#finished on 21 May 2017
#After meeting with Dr. Dixon on 30 June decided to not use the normalized data for the analysis because through the analysis it will be normalized anyway.

##This data file has no treatment species and is subsetted for a year long period from 2-24-14 to 2-23-15

#data reran on 24 Jan 18 because names were updated.

# Rewrote data on 2 Feb 18 so I could add in treatment and block

# Rewrote data on 12 Feb 18 to remove non-woody species


#### Create file for mean and se for sigmaplot ####
#a) melt- this organizes data so that all variables fall in the same column as a descriptor and then the value is in the next column.

# drop plot and block data.
div_data4 <- div_data3
div_data4$plot <- NULL
div_data4$block <- NULL

melt_div <- melt(div_data4, id.vars = c("treatment"))

div_data5 <- ddply(melt_div, c("treatment", "variable"), summarise, mean=mean(value), sd= sd(value), SE = (sd(value)/sqrt(length(value))))

write.csv(div_data5, "div_se_mean_nw.csv", row.names = FALSE)

# Rewrote on 3 March to include treatment species not of that same plot