# started on 9 May 2017
# This wrangling will result in a tidy file that can be used to look at rank abundance plots of the 30 seed species with the highest seednum.  All dates will be used.  Will need to average seeds on a monthly basis.
# Will look at individual abundances of 30 most prolific species
# will also look at how plot abundances vary over the 14 months.
# This will require two tidy files: (1) wide format that is not summarized but that can but used to look at species abundances to determine the most abundant species. (2) wide format but summed across months for each plot.

# Traps first set out 13 Jan 2014 and traps picked up on 9 March 2015.  420 total trap days.

#load libraries
library(plyr); library(ggplot2); library(readr); library(reshape2); library(BiodiversityR); library(dplyr); library(scales); library(base)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

# (1) Determining individually most abundant seed species

#import tidy file of seedrain species for whole 14 WITH canopy species so you can see how they compare with other species in terms of total abundance.
pheno <- read.csv("seedrain_all_tidy_nw.csv")

#summarise the data by plot
phenosum <- ddply(pheno, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA##########################################################
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
pheno_data <- dcast(phenosum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
pheno_data <- pheno_data[,c(names(pheno_data)[1],sort(names(pheno_data)[2:ncol(pheno_data)]))]
#identifies all seed rain species that are NA
pheno_data <- pheno_data[, -which(names(pheno_data)=="NA")]
#Replace NA's with zeros
pheno_data[is.na(pheno_data)] <- 0

#create a tidy csv file for rank abundance of individual species
write.csv(pheno_data, "pheno_ovsty_tidy_nw.csv", row.names = FALSE)

head(pheno_data)
#do a rank abundance plot to see which seeds have the most
x <- pheno_data[,2:133]
y <- pheno_data[,1]

rankdata <- rankabundance(x, y="y",factor="species", "plot" ,t=qt(0.975,df=14))
write.csv(rankdata, "pheno_rankdata.csv")
rankplot <- rankabunplot(rankdata, addit=FALSE, labels="species",scale="logabun")

########Create monthly summaries for the plots######
pheno$date <- as.Date(pheno$date, format = ("%Y-%m-%d"))
pheno$month <- as.Date(cut(pheno$date, breaks = "month"))

#create a monthly seed species, plot total
month_seed <- ddply(pheno, .(plot, species, month), summarise, seednum=sum(total_seednum))

#Code on website: plots month totals across the three months that were compared.
ggplot(data = month_seed, aes(month, seednum, color = plot)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar")+ # or "line"
  scale_x_date(date_breaks = "1 month", labels=date_format ("%b-%Y")) # custom x-axis labels

#plots the totals for each plot compared side by side
ggplot(data = month_seed, aes(month, seednum)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line")+ # or "line"
    facet_wrap(~plot, scales = "free_y")


#ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
 # geom_point(mapping = aes(color = class)) + 
 # geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)


#######Testing to see what it might look like averaged across treatments#########
#summarise the data by treatment
trtsum <- ddply(pheno, .(treatment, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA##########################################################
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
pheno_data <- dcast(phenosum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
pheno_data <- pheno_data[,c(names(pheno_data)[1],sort(names(pheno_data)[2:ncol(pheno_data)]))]
#identifies all seed rain species that are NA
pheno_data <- pheno_data[, -which(names(pheno_data)=="NA")]
#Replace NA's with zeros
pheno_data[is.na(pheno_data)] <- 0

#create a tidy csv file for rank abundance of individual species
write.csv(pheno_data, "pheno_trt_tidy.csv", row.names = FALSE)

## Testing to see if data is in the correct format for analysis...thus far it is not.
#do a rank abundance plot to see which seeds have the most

rankdata <- rankabundance(pheno_data, y="pheno",factor="species", "plot" ,t=qt(0.975,df=n-1))
str(pheno_data)
rankdata <- rankabundance(species_data)

rankabunplot(rankdata,addit=F,labels="species",scale="abundance",scaledx=F,type="o",
             xlim=c(min(xpos),max(xpos)),ylim=c(0,max(species_data[,scale])),specnames=c(3:74))

rankabuncomp(x,y="",factor,scale="abundance",scaledx=F,type="o",rainbow=T,
             legend=T,xlim=c(1,max1), ylim=c(0,max2), ...)


########Create monthly summaries for the treatments######
#already has months added as a column in pheno
trtgrp <- pheno

#create a monthly seed species, plot total
trt_month_seed <- ddply(pheno, .(treatment, species, month), summarise, seednum=sum(total_seednum))

#Code on website: plots month totals across the three months that were compared.
ggplot(data = trt_month_seed, aes(month, seednum, color = treatment)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar")+ # or "line"
  scale_x_date(date_breaks = "1 month", labels=date_format ("%b-%Y")) # custom x-axis labels

#plots the totals for each plot compared side by side
ggplot(data = trt_month_seed, aes(month, seednum)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line")+ # or "line"
  facet_wrap(~treatment, scales = "free_y")
