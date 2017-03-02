# This is data munging for seed rain from ECOS project
#started on 10 feb 17


library(readxl)
library(plyr)
library(ggplot2)

#set working directory to folder
setwd("Data/RawData")

#naming datafile seedrain from excel file and pulling data from sheet 4
seedrain<-read_excel("Lluvia de semillas_RBA_20Apr15.xlsx", sheet=4, col_names=TRUE, na= "NA")

#shows what type of variable is in which column
str(seedrain)

#Change col names to english and abbreviate
colnames(seedrain) <- c("week", "date", "trap", "sample", "canopysp", "block", "quad", "type", "species", "fruitnum", "seednum", "poop", "damaged", "obs")

#Check to see that names were changed
str(seedrain)
#provides summary values for all columns in seed rain
summary(seedrain)

#Species column is turned into a factor
seedrain$species<-as.factor(seedrain$species)
summary(seedrain$species)

#change incorrect species names
levels(seedrain$species)<-gsub("kochnyi", "koschnyi", levels(seedrain$species))
levels(seedrain$species)<-gsub("seemanii", "seemannii", levels(seedrain$species))
levels(seedrain$species)<-gsub("tecaphora", "thecaphora", levels(seedrain$species))
levels(seedrain$species)<-gsub("dominguensis", "domingensis", levels(seedrain$species))
levels(seedrain$species)<-gsub("Werahuia gladioflora", "Werauhia gladioliflora", levels(seedrain$species))

#check to see what species names are now
levels(seedrain$species)

seedrain$meshtype <- NA

#Create list of trap numbers
trap_numbers <- seq(1:75)
#Create list of numbers that are meshsmall
meshsmall <- c(1, 2, 4, 7, 8, 9, 11, 12, 15, 16, 18, 19, 21, 23, 24, 27, 29, 30, 32, 34, 35, 36, 37, 38, 41, 42, 43, 46, 47, 49, 51, 52, 53, 57, 58, 59, 62, 64, 65, 66, 67, 69, 72, 73, 75)

meshreg <- trap_numbers[-which(trap_numbers%in%meshsmall)]
meshreg

#Assign Meshsmall to traps that are mesh small
seedrain$meshtype[seedrain$trap %in% meshsmall] <- "meshsmall"
seedrain$meshtype[seedrain$trap %in% meshreg] <- "meshreg"

#Create data set with just Meshsmall traps
seed_rain_unique_meshsmall <- seedrain[seedrain$meshtype=="meshsmall",]

###Next section involves finding the number of days a trap was set out for
#Make date a character
seedrain$date <- as.character(seedrain$date)

#Get unique rows of trap and date
seed_rain_unique <- ddply(seedrain, .(date, trap), summarise, n=length(trap))
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
seedrain <- merge(seedrain, seed_rain_unique, by=c("date", "trap"), all.x=TRUE)





#remove rows of data on unnamed species (no longer exist in new file from Ricardo)
seedrain2<-seedrain[seedrain$species!="Muestra 100",]  #this is not working yet.

#Once finished wrangling save data in a tidy file
setwd("../")
setwd("TidyData")
write.csv(seedrain,"seedraintidy.csv")
#csv is now in tidy data






