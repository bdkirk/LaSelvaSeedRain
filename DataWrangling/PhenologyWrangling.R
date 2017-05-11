#Looking into Phenology
#started on 9 May 2017
#First goal is to see if there are differences between the two repeated months
#Second goal is to create a rank abundance curve and choose which species based on total abundance over whole study to show monthly changes in abundance over time.
#Third goal is to make analysis file for the species.

#Traps were checked between 20 Jan 14 (2nd half- 21 Jan 14) and 9 March 15

#load libraries
library(plyr); library(ggplot2); library(readr); library(reshape2); library(BiodiversityR); library(dplyr); library(scales)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#######  1) Repeated Months comparison  #######

#Read in data; this includes overstory species as well.  Should not matter because you will be sorting according to most abundant species.
phenology <- read.csv("seedrain_all_tidy.csv")
# This is the file with species removed "seedrain_nocanopysp_tidy.csv"

######THIS IS WHERE YOU NEED TO START FROM####

#Summarise by date, plot and species. This step adds up seeds of the same species and data, plot combo so that there are several rows with a sum of that species seednum
dp_seed <- ddply(twomonths, .(Date.out, plot, species), summarise, total_seednum=sum(total_seednum))

#convert date from a factor to a date
dp_seed$Date.out <- as.Date(dp_seed$Date.out, format = "%m/%d/%Y")
summary(dp_seed)

#1) Try to do a rank abundance plot:
#have to convert to an NMDS format first to make the community data frame with sites as rows
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var"column you want to be the variable")
species_data <- dcast(dp_seed, Date.out + plot ~ species, value.var="total_seednum")
#This sorts the data after it is created
species_data <- species_data[,c(names(species_data)[1:2],sort(names(species_data)[3:ncol(species_data)]))]
#identifies all seed rain species that are NA
species_data <- species_data[, -which(names(species_data)=="NA")]
#Replace NA's with zeros
species_data[is.na(species_data)] <- 0


#do a rank abundance plot to see which seeds have the most

rankdata <- rankabundance(species_data, y="dp_seed",factor="total_seednum", "species" ,t=qt(0.975,df=n-1))

rankdata <- rankabundance(species_data)
species_data$Date.out <- as.factor(species_data$Date.out)
rankabunplot(rankdata,addit=F,labels="species",scale="abundance",scaledx=F,type="o",
             xlim=c(min(xpos),max(xpos)),ylim=c(0,max(species_data[,scale])),specnames=c(3:74))

rankabuncomp(x,y="",factor,scale="abundance",scaledx=F,type="o",rainbow=T,
             legend=T,xlim=c(1,max1), ylim=c(0,max2), ...)


#2)Create plots using ggplot
##Summarise the data by month.
#Create a month column

# create variables of the week and month of each observation:
dp_seed$month <- as.Date(cut(dp_seed$Date.out,breaks = "month"))
#If ever wanted to look at weeks use code below
#log$Week <- as.Date(cut(log$Date, breaks = "week", start.on.monday = FALSE)) # changes weekly break point to Sunday

#create a monthly seed species, plot total
month_seed <- ddply(dp_seed, .(plot, species, month), summarise, seednum=sum(total_seednum))

#Code on website: plots month totals across the three months that were compared.
ggplot(data = month_seed, aes(month, seednum, color = plot)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar")+ # or "line"
  scale_x_date(date_breaks = "1 month", labels=date_format ("%b-%Y")) # custom x-axis labels

#plots the totals for each plot compared side by side
ggplot(data = month_seed, aes(month, seednum)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "point")+ # or "line"
    facet_wrap(~plot)


#ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
 # geom_point(mapping = aes(color = class)) + 
 # geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
