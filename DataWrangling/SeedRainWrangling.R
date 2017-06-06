# This is data munging for seed rain from ECOS project
#started on 10 feb 17
#tidy file completed on 2 April 2017

#Load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base)

#set working directory to folder
setwd("Data/RawData")

#naming datafile seedrain from excel file and pulling data from sheet 4
seedrain <-read_excel("Lluvia de semillas_RBA_20Apr15_bk_10Mar17.xlsx", sheet=4, col_names=TRUE, na= "NA")

#Change col names to english and abbreviate
colnames(seedrain) <- c("week", "date", "trap", "sample", "treatment", "block", "quad", "type", "species", "fruitnum", "seednum", "poop", "damaged", "obs")

#convert all seed species names to lowercase to prevent capitalization errors
seedrain$species <- tolower(seedrain$species)

#convert species to a factor from a character
seedrain$species<-as.factor(seedrain$species)

#check to see what species names are now
levels(seedrain$species)
summary(seedrain$species)
#change incorrect species names
levels(seedrain$species)<-gsub("kochnyi", "koschnyi", levels(seedrain$species))
levels(seedrain$species)<-gsub("seemanii", "seemannii", levels(seedrain$species))
levels(seedrain$species)<-gsub("tecaphora", "thecaphora", levels(seedrain$species))
levels(seedrain$species)<-gsub("dominguensis", "domingensis", levels(seedrain$species))
levels(seedrain$species)<-gsub("guatemalnsis", "guatemalensis", levels(seedrain$species))
levels(seedrain$species)<-gsub ("anona papilionella", "annona papilionella", levels(seedrain$species))
levels(seedrain$species)<-gsub("bursera simarouba", "bursera simaruba", levels(seedrain$species))
levels(seedrain$species)<-gsub("casearea", "casearia", levels(seedrain$species))
levels(seedrain$species)<-gsub("alchorneiodes", "alchorneoides", levels(seedrain$species))
levels(seedrain$species)<- gsub("sapindioides", "sapindoides", levels(seedrain$species))
levels(seedrain$species)<-gsub("papilosa", "papillosa", levels(seedrain$species))
levels(seedrain$species)<-gsub("conostegia crenula", "clidemia crenulata", levels(seedrain$species))
levels(seedrain$species) <- gsub("aerugynosa", "aeruginosa", levels(seedrain$species))
#check to see what species names are now
levels(seedrain$species)

#Add in small seed final
seedrain_new <-read_excel("Lluvia de semillas_RBA_20Apr15_bk_10Mar17.xlsx", sheet=6, col_names=TRUE, na= "NA")

#Change col names to english and abbreviate
colnames(seedrain_new) <- c("trap", "date", "dategerminated", "seednum", "species", "startgerminate", "roottrainer", "comments")

###Look for species name inconsistencies in new data set
#convert all seed species names to lowercase to prevent capitalization errors
seedrain_new$species <- tolower(seedrain_new$species)

#convert species to a factor from a character
seedrain_new$species<-as.factor(seedrain_new$species)

#check species names
levels(seedrain_new$species)

#Change incorrect species names
levels(seedrain_new$species)<-gsub("adelobotrys adcendens", "adelobotrys adscendens", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("clidemia crenula", "clidemia crenulata", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("foresteronia myriantha", "Forsteronia myriantha", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("hamelia xenocarpa", "hamelia xerocarpa", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("pyramidatha", "pyramidata", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("warczewiczia", "warszewiczia", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("witheringya asterotrycha", "witheringia asterotricha", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("conostegia densiflora", "miconia approximata", levels(seedrain_new$species))

#Check to make sure names look good
levels(seedrain_new$species)

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

#Summarise by date, trap and species. This step adds up seeds of the same species and data, trap combo so that it is one row with a sum of that species seednum
seedrain_all <- ddply(seedrain_all, .(date, trap, species), summarise, total_seednum=sum(seednum))


#assigning canopy species, block and quad to a particular trap number
##want to add in overstory treatment to new dataset by matching it to trap
trap_trt <- seedrain[, c("trap", "treatment", "block", "quad")]
trap_trt <- ddply(trap_trt, .(trap, treatment, block, quad), summarise, n=length(quad))
trap_trt <- trap_trt[,-5]

#check and make sure merged data has same number of rows as seedrain_all originally had
#Merges data together
seedrain_all <- merge(seedrain_all, trap_trt, by="trap", all.x=TRUE)
##VERIFIED THIS DOES ON 2 APRIL 17


####### Testing Functions #####
#function to calculate the total seed number across the four treatments and the four blocks
seed_b_c <- ddply(seedrain_all, .(treatment, block), summarise, total=sum(total_seednum))
seed_b_c

#funciton used to calculate the total number of seeds for each of the species in seed rain
species_seeds <- ddply(seedrain_all, .(species), summarise, total=sum(total_seednum))
species_seeds

seed_2 <- ddply(seedrain_all, .(treatment, species), summarise, total=sum(total_seednum))
seed_2
#function to calculate the total seed number across the four blocks
seed_b <- ddply(seedrain_all, .(block), summarise, total=sum(total_seednum))
seed_b
#function to calculate the total seed number across the four overstory treatments
seed_c <- ddply(seedrain_all, .(treatment), summarise, total=sum(total_seednum))
seed_c



#################Add in Mesh type#####
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


###Next section involves finding the number of days a trap was set out for####
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

#Calculate the total number of days each trap was out
trapdays <- ddply(seed_rain_unique, .(trap), summarise, total=sum(days))
trapdays

##################################################
#Merge the days variable back into the main data set
seedrain_all <- merge(seedrain_all, seed_rain_unique, by=c("date", "trap"), all.x=TRUE)

#check to see which date-trap combos are missing. Check these to see if they were actually collected, but not in database because no seeds were found. 
date_trap<-as.data.frame(with(seedrain, table(date, trap)))
date_trap[date_trap$Freq==0 & date_trap$date != "2014-01-21" & date_trap$date!= "2014-01-20",]
#Fixed the above issue with dates on 11 Mar 17

##########################################
#add in plot column to identify the 15 different plots
seedrain_all$plot <- NA

plot_numbers <- seq(1:75)
#Create list of plots with treatment_block combo
hial1 <- c(1, 2, 3, 4, 5)
viko1 <- c(6, 7, 8, 9, 10)
pema1 <- c(11, 12, 13, 14, 15)
hial2 <- c(16, 17, 18, 19, 20)
viko2 <- c(21, 22, 23, 24, 25)
pema2 <- c(26, 27, 28, 29, 30)
vogu2 <- c(31, 32, 33, 34, 35)
hial3 <- c(36, 37, 38, 39, 40)
viko3 <- c(41, 42, 43, 44, 45)
pema3 <- c(46, 47, 48, 49, 50)
vogu3 <- c(51, 52, 53, 54, 55)
hial4 <- c(56, 57, 58, 59, 60)
viko4 <- c(61, 62, 63, 64, 65)
pema4 <- c(66, 67, 68, 69, 70)
vogu4 <- c(71, 72, 73, 74, 75)

#Assign plots to traps (15 plots)
seedrain_all$plot[seedrain_all$trap %in% hial1] <- "hial1"
seedrain_all$plot[seedrain_all$trap %in% hial2] <- "hial2"
seedrain_all$plot[seedrain_all$trap %in% hial3] <- "hial3"
seedrain_all$plot[seedrain_all$trap %in% hial4] <- "hial4"
seedrain_all$plot[seedrain_all$trap %in% pema1] <- "pema1"
seedrain_all$plot[seedrain_all$trap %in% pema2] <- "pema2"
seedrain_all$plot[seedrain_all$trap %in% pema3] <- "pema3"
seedrain_all$plot[seedrain_all$trap %in% pema4] <- "pema4"
seedrain_all$plot[seedrain_all$trap %in% viko1] <- "viko1"
seedrain_all$plot[seedrain_all$trap %in% viko2] <- "viko2"
seedrain_all$plot[seedrain_all$trap %in% viko3] <- "viko3"
seedrain_all$plot[seedrain_all$trap %in% viko4] <- "viko4"
seedrain_all$plot[seedrain_all$trap %in% vogu2] <- "vogu2"
seedrain_all$plot[seedrain_all$trap %in% vogu3] <- "vogu3"
seedrain_all$plot[seedrain_all$trap %in% vogu4] <- "vogu4"

#remove unnecessary columns
seedrain_all$min <- NULL

#remove rows that have unknown species, called desconocido
#did this manually in excel

#make columns appropriate variable
seedrain_all$block <-as.factor(seedrain_all$block)
seedrain_all$meshtype <- as.factor(seedrain_all$meshtype)
seedrain_all$trap <- as.factor(seedrain_all$trap)
seedrain_all$plot <- as.factor(seedrain_all$plot)
seedrain_all$treatment <- as.factor(seedrain_all$treatment)
str(seedrain_all)

###csv file for all of the data
setwd("../")
setwd("TidyData")

#This file contains all the original data including overstory species
write.csv(seedrain_all, "seedrain_all_tidy.csv", row.names = FALSE)


##create csv file for data without overstory species included####
#This removes the overstory species from the dataset 
removed_species <- seedrain_all[-which(seedrain_all$species%in%c("hieronyma alchorneoides", "pentaclethra macroloba", "virola koschnyi", "vochysia guatemalensis")),]

#write this as a csv file and export it as a unique dataset
write.csv(removed_species, "seedrain_notrtsp_tidy.csv", row.names = FALSE)

#######creating a subset of data for one years worth of data
# Decided to clip off the first month and the last month because were getting used to data collection the first month and were rushing in the last month
# Will create a subset of removed_species to use
removed_species$date <- as.character(removed_species$date)
removed_sub <- subset(removed_species, date > "2014-02-23" & date < "2015-02-24")

removed_sub$date <- as.factor(removed_sub$date)

summary(removed_sub$date)

###HAHAH it worked. And what R!!!

#Write as a new csv for the subset with removed canopy
write.csv(removed_sub, "yearsub_no_trtsp.csv", row.names = FALSE)


#all files were written 5 April 2017
#Then they were rewritten 6 April 2017
# (hopefully) Finals were rewritten 11 May 2017
# Files were rewritten again after confirmation from Ricardo that two species with no seeds recorded had seeds that were not read to germinate.

