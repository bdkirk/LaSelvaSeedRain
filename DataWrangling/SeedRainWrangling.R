# This is data munging for seed rain from ECOS project
#started on 10 feb 17

#Load libraries
library(readxl)
library(plyr)
library(ggplot2)
library(reshape2)

#set working directory to folder
setwd("Data/RawData")

#naming datafile seedrain from excel file and pulling data from sheet 4
seedrain<-read_excel("Lluvia de semillas_RBA_20Apr15_bk_10Mar17.xlsx", sheet=4, col_names=TRUE, na= "NA")

#Change col names to english and abbreviate
colnames(seedrain) <- c("week", "date", "trap", "sample", "canopysp", "block", "quad", "type", "species", "fruitnum", "seednum", "poop", "damaged", "obs")

#convert all seed species names to lowercase to prevent capitalization errors
seedrain$species <- tolower(seedrain$species)
#check to see what species names are now
levels(seedrain$species)

#change incorrect species names
levels(seedrain$species)<-gsub("kochnyi", "koschnyi", levels(seedrain$species))
levels(seedrain$species)<-gsub("seemanii", "seemannii", levels(seedrain$species))
levels(seedrain$species)<-gsub("tecaphora", "thecaphora", levels(seedrain$species))
levels(seedrain$species)<-gsub("dominguensis", "domingensis", levels(seedrain$species))
levels(seedrain$species)<-gsub("Werahuia gladioflora", "Werauhia gladioliflora", levels(seedrain$species))
levels(seedrain$species)<-gsub("guatemalnsis", "guatemalensis", levels(seedrain$species))
#check to see what species names are now
levels(seedrain$species)

#Add in small seed final
seedrain_new <-read_excel("Lluvia de semillas_RBA_20Apr15_bk_10Mar17.xlsx", sheet=6, col_names=TRUE, na= "NA")

#Change col names to english and abbreviate
colnames(seedrain_new) <- c("trap", "date", "dategerminated", "seednum", "species", "startgerminate", "roottrainer", "comments")
#Look for species name inconsistencies in new data set
levels(seedrain_new)
#Change incorrect species names




#shows what type of variable is in which column
str(seedrain)
str(seedrain_new)

#provides summary values for all columns in seed rain
summary(seedrain)
summary(seedrain_new)

#Select just trap, date, seednum and species
##This process pulls out only the trap, date, seednum and species which are found in both datasheets so they can be merged
seedrain_all <- seedrain[, which(names(seedrain)%in%c("trap", "date", "seednum", "species"))]
seedrain_new <- seedrain_new[, which(names(seedrain_new)%in%c("trap", "date", "seednum", "species"))] #This replaces the old seedrain_new file

#Adding in columns for the dataset to know which came from the orignal dataset and which came from the small seed final dataset
#seedrain_all$Data_set <- "Original"
#seedrain_new$Data_Set <- "New"

##Check to see if the two data sets match before binding new data to old data
identical(sort(names(seedrain_all)), sort(names(seedrain_new)))
#Stacks the two data sets with the small seed final set at the bottom.
seedrain_all <- rbind(seedrain_all, seedrain_new)


#Summarise by date, trap and species
seedrain_all <- ddply(seedrain_all, .(date, trap, species), summarise, total_seednum=sum(seednum))

#assigning canopy species, block and quad to a particular trap number
##want to add in overstory treatment to new dataset by matching it to trap

trap_trt <- seedrain[, c("trap", "canopysp", "block", "quad")]
trap_trt <- ddply(trap_trt, .(trap, canopysp, block, quad), summarise, n=length(quad))
trap_trt <- trap_trt[,-5]

#check and make sure merged data has same number of rows as seedrain_all originally had
seedrain_all <- merge(seedrain_all, trap_trt, by="trap", all.x=TRUE)


#merges data together
seedrain_all <- merge(seedrain_all, trap_trt, by="trap", all.x=TRUE)
#function to calculate the total seed number across the four treatments
ddply(seedrain_all, .(canopysp), summarise, total=sum(seednum))


#################Add in Mesh type
seedrain_all$meshtype <- NA

#Create list of trap numbers
trap_numbers <- seq(1:75)
#Create list of numbers that are meshsmall
meshsmall <- c(1, 2, 4, 7, 8, 9, 11, 12, 15, 16, 18, 19, 21, 23, 24, 27, 29, 30, 32, 34, 35, 36, 37, 38, 41, 42, 43, 46, 47, 49, 51, 52, 53, 57, 58, 59, 62, 64, 65, 66, 67, 69, 72, 73, 75)

#assigns meshreg to all other traps
meshreg <- trap_numbers[-which(trap_numbers%in%meshsmall)]
meshreg

#Assign Meshsmall to traps with fine mesh liner
seedrain_all$meshtype[seedrain_all$trap %in% meshsmall] <- "meshsmall"
seedrain_all$meshtype[seedrain_all$trap %in% meshreg] <- "meshreg"
#brackets tell which row should be pulled out


###Next section involves finding the number of days a trap was set out for
#Make date a character
seedrain_all$date <- as.character(seedrain_all$date)

#Get unique rows of trap and date
seed_rain_unique <- ddply(seedrain_all, .(date, trap), summarise, n=length(trap))
#Get minimum date for each trap
seed_rain_unique_min_date <- ddply(seed_rain_unique, .(trap), summarise, min=min(date))

#Merge minimum date into seed_rain_unique
seed_rain_unique <- merge(seed_rain_unique, seed_rain_unique_min_date, by=c("trap"), all.x=TRUE)
#Sort by trap and date
seed_rain_unique <- seed_rain_unique[order(seed_rain_unique$trap, seed_rain_unique$date),]
#Create days variable
seed_rain_unique$days <- NA

#If it is the first day for this trap,
#subtract 14-01-13, else subtract the previous date
for (i in 1:{nrow(seed_rain_unique)}){
  if (seed_rain_unique$date[{i}]==seed_rain_unique$min[{i}]){
    seed_rain_unique$days[{i}] <- as.Date(seed_rain_unique$date[{i}], format="%Y-%m-%d")-as.Date("2014-01-13", format="%Y-%m-%d")
  } else {
    seed_rain_unique$days[{i}] <- as.Date(seed_rain_unique$date[{i}], format="%Y-%m-%d")-as.Date(seed_rain_unique$date[{i-1}], format="%Y-%m-%d")
  }
}

#Take out the n variable
seed_rain_unique <- seed_rain_unique[,-which(names(seed_rain_unique)=="n")]

#Merge the days variable back into the main data set
seedrain_all <- merge(seedrain_all, seed_rain_unique, by=c("date", "trap"), all.x=TRUE)

#check to see which date-trap combos are missing. Check these to see if they were actually collected, but not in database because no seeds were found. 
date_trap<-as.data.frame(with(seedrain, table(date, trap)))
date_trap[date_trap$Freq==0 & date_trap$date != "2014-01-21" & date_trap$date!= "2014-01-20",]
#Fixed the above issue with dates on 11 Mar 17

###

#This removes the overstory species from the dataset 
removed_species <- seedrain_all[-which(seedrain_all$species%in%c("species1", "species2", "species3", "species4")),]

#add code to write this as a csv file and export it as a unique dataset

########
#Create data set with just meshsmall traps
seedrain_smallmesh <- seedrain[seedrain$meshtype=="meshsmall",]

#Create data set with just regmesh raps
seedrain_regmesh <- seedrain[seedrain$meshtype=="meshreg",]
#Remove overstory tree species or create two groups, one with and one without overstory tree species. 

####CREATING NMDS DATA##########################################################
#Create species columns for use in NMDS and ordination
species_data <- dcast(seedrain_all, date+trap ~ species, value.var="total_seednum")

#
species_data <- species_data[, -which(names(species_data)=="NA")]
#Replace NA's with zeros
species_data[is.na(species_data)] <- 0

species_data2 <- merge(species_data, trap_trt, by=c("trap"), all.x=TRUE)
species_data2 <- species_data2[,c(1,2,ncol(species_data2), (3:ncol(species_data2)-1))]
species_data2 <- species_data2[,-which(names(species_data2)=="date.1")]
#create csv file that can be used to do NMDS calculations
write.csv(species_data, "species_data.csv")

###Practice Creating plots#######################
#Species column is turned into a factor
seedrain$species<-as.factor(seedrain$species)
summary(seedrain$species)
#made canopysp a factor
seedrain$canopysp<-as.factor(seedrain$canopysp)
summary(seedrain$canopysp) #not sure this is needed

#plot(indepdentvar, dependentvar)
plot(seedrain$trap, seedrain$seednum, xlab="Trap Number", ylab= "Number of Seeds", main= "Total seeds across all traps")

#categorical x --This produces the total number of observations for each canopysp
ggplot(seedrain, aes(canopysp))+
  geom_bar(stat="count") #default stat for geom_bar is count. Count takes a count of the number of cases, need categorical x variable, and no y-variable. 

#plot x and y variables--This produces the total number of seeds found in each plot
ggplot(seedrain, aes(meshtype, seednum))+
  geom_boxplot()
ggplot(seedrain, aes(canopysp, seednum))+
  geom_violin()
ggplot(seedrain, aes(species, seednum))+
  geom_boxplot()

##categorical variables can have tables that use frequencies or proportions
#This will give the total number of observations in each group
table(seedrain$canopysp)
#This will give you all the observations for a particular variable
length(seedrain$canopysp)
#this will produce a two-way table
table(seedrain$canopysp, seedrain$meshtype)

###numerical values produce means, medians, variance var(), sd, range, min, max
canopysp<-(seedrain$canopysp)
species<-(seedrain$species)
boxplot(canopysp~species)

library(dplyr)
ddply(seedrain, "canopysp", summarise, 
      sppnum = length(unique(species)))
##below does not work
 ggplot("trap", aes("canopysp", "block"))+
  +     geom_violin()

speciessum <- mean(seedrain$species, seedrain$canopysp)

ggplot("canopysp")

ggplot("trap")
totseeds <- sum(seedrain$seednum)
ggplot(totseeds)
plot(canopysp)


###########Once finished wrangling save data in a tidy file###########
setwd("../")
setwd("TidyData")
write.csv(seedrain,"seedraintidy.csv")
#csv is now in tidy data






