# This is data munging for seed rain from ECOS project
#started on 10 feb 17
#tidy file completed on 2 April 2017

#Load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/RawData")

#naming datafile seedrain from excel file and pulling data from sheet 4
seedrain <-read_excel("Lluvia de semillas_RBA_20Apr15_bk_15Jan18.xlsx", sheet=4, col_names=TRUE, na= "NA")

#May get a warning message that states expecting numeric but got 'Ave' this is for the damage/processing of seeds by birds and for the defecation aspect.  About 1000 seeds were presumed to be dispersed by birds and found as defecation or possible defecation in traps.

#Change col names to english and abbreviate
colnames(seedrain) <- c("week", "date", "trap", "sample", "treatment", "block", "quad", "type", "species", "fruitnum", "seednum", "poop", "damaged", "obs")

#convert all seed species names to lowercase to prevent capitalization errors
seedrain$species <- tolower(seedrain$species)

#convert species to a factor from a character
seedrain$species<-as.factor(seedrain$species)

#check to see what species names are now
levels(seedrain$species)
summary(seedrain$species)

#change incorrect species names gsub("current incorrect", "correction", levels(data$column))
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
levels(seedrain$species) <- gsub("brosimun", "brosimum", levels(seedrain$species))
levels(seedrain$species)<-gsub("senna papilosa", "senna papillosa", levels(seedrain$species))
levels(seedrain$species)<-gsub("rollinia pittierii", "annona papilionella", levels(seedrain$species))
levels(seedrain$species)<-gsub("paullinia cf. fasciculata", "paullinia fasciculata", levels(seedrain$species)) #c.f. means that it resembles this species.  I will assume this will be enough for the analyses because it is a distinct species.
levels(seedrain$species)<-gsub("siparuna paucifolia", "siparuna pauciflora", levels(seedrain$species))

#check to see what species names are now
levels(seedrain$species)

## Export part of seed rain data to allow for look at differences between fruit and seed.
## pound out when not needing to export
#corrections have been made since this was last looked at
#write.csv(seedrain, "fos_sr.csv")

#Add in small seed final
seedrain_new <-read_excel("Lluvia de semillas_RBA_20Apr15_bk_15Jan18.xlsx", sheet=6, col_names=TRUE, na= "NA")

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
levels(seedrain_new$species)<-gsub("foresteronia myriantha", "forsteronia myriantha", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("hamelia xenocarpa", "hamelia xerocarpa", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("pyramidatha", "pyramidata", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("warczewiczia", "warszewiczia", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("witheringya asterotrycha", "witheringia asterotricha", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("conostegia densiflora", "miconia approximata", levels(seedrain_new$species))
levels(seedrain_new$species)<-gsub("donell-smithi", "donnell-smithii", levels(seedrain_new$species))

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

#function used to calculate the total number of seeds for each of the species in seed rain
species_seeds <- ddply(seedrain_all, .(species), summarise, total=sum(total_seednum))
species_seeds

seed_2 <- ddply(seedrain_all, .(treatment, species), summarise, total=sum(total_seednum))
seed_2
#function to calculate the total seed number across the four blocks
seed_b <- ddply(seedrain_all, .(species, block, treatment), summarise, total=sum(total_seednum))
seed_b
write.csv(seed_b, "blockcheck.csv")

#function to calculate the total seed number across the four overstory treatments
seed_c <- ddply(seedrain_all, .(treatment, block), summarise, total=sum(total_seednum))
write.csv(seed_c, "blockabund.csv")

seed_d <- ddply(seedrain_all, .(species, treatment), summarise, total=sum(total_seednum))
seed_e <- ddply(seedrain_all, .(treatment, species), summarise, total=sum(total_seednum))
write.csv(seed_e, "summarycheck.csv")


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
#Fixed the above issue with dates on 11 Mar 17. It will show 0 rows.

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

#Remove non-woody species
seedrain_all2 <- seedrain_all[-which(seedrain_all$species%in%c("heterocondylus vitalbae", "witheringia asterotricha", "solanum volubile", "piper arcteacuminatum")),]

#This file contains all the original data including overstory species
write.csv(seedrain_all2, "seedrain_all_tidy_nw.csv", row.names = FALSE)

####### extra verification ########
### Verified with code below that there are 2551 rows with no data just added to get dates correct.
#test3 <- seedrain_all2
#test4 <- test3[complete.cases(test3),]
##create csv file for data without overstory species included####
#This removes the overstory species from the dataset # Removes all treatment seeds not just the ones within the plots of that treatment
#removed_species <- seedrain_all[-which(seedrain_all$species%in%c("hieronyma alchorneoides", "pentaclethra macroloba", "virola koschnyi", "vochysia guatemalensis")),]

#See how many rows should be removed in piping or filtering (total = 369 removed)
#no_hial_sp <- filter(seedrain_all2, species== "hieronyma alchorneoides" & treatment =="Hial") # 319

#no_viko_sp <- filter(seedrain_all2, species== "virola koschnyi" & treatment =="Viko") # 6

#no_pema_sp <- filter(seedrain_all2, species== "pentaclethra macroloba" & treatment =="Pema") #17, only found in Pema

#no_vogu_sp <- filter(seedrain_all2, species== "vochysia guatemalensis" & treatment =="Vogu") # 27

#code removes treatment species seeds of that particular treatment
#currently not working, code works but it doesn't look pretty because the 3k columns with essentially no data but a date value turn into weird NAs. This will be resolved with ddply calculations in data wrangling for specific analyses 5 March 18.
#str(seedrain_all2)
#seedrain_all2$species <- as.factor(seedrain_all2$species)
#seedrain_all2$species[seedrain_all2$species == 'NA'] <- 'none'


### below may not be in the correct order as it was cut from just below to make code look cleaner
#no_trt_sp2 <- seedrain_all2[!(seedrain_all2$species== "hieronyma alchorneoides" & seedrain_all2$treatment =="Hial"), ]
#no_trt_sp2[is.na(no_trt_sp2)] <- 0

#test <- seedrain_all2 %>% filter(!(species == "hieronyma alchorneoides" & treatment == "Hial"),!(species == "vochysia guatemalensis" & treatment == "Vogu"), !(species == "pentaclethra macroloba" & treatment == "Pema"), !(species == "virola koschnyi" & treatment == "Viko"))
#test1 <- seedrain_all2 %>% filter(!(species == "hieronyma alchorneoides" & treatment == "Hial"))

# make a new test with complete cases
#test <- no_trt_sp
#test2 <- test[complete.cases(test),]

###### remove conspecific treatment species #####
no_trt_sp <- seedrain_all2[!((seedrain_all2$species== "vochysia guatemalensis" & seedrain_all2$treatment =="Vogu")| (seedrain_all2$species== "pentaclethra macroloba" & seedrain_all2$treatment =="Pema")| (seedrain_all2$species== "virola koschnyi" & seedrain_all2$treatment =="Viko")| (seedrain_all2$species== "hieronyma alchorneoides" & seedrain_all2$treatment =="Hial")), ]

#remove NA rows to make dataset look cleaner
no_trt_sp2 <- no_trt_sp[complete.cases(no_trt_sp),]

#write this as a csv file and export it as a unique dataset
write.csv(no_trt_sp2, "seedrain_notrtsp_tidy_nw.csv", row.names = FALSE)

#######creating a subset of data for one years worth of data####
# Decided to clip off the first month and the last month because were getting used to data collection the first month and were rushing in the last month
# Will create a subset of removed_species to use
no_trt_sp2$date <- as.character(no_trt_sp2$date)
removed_sub2 <- subset(no_trt_sp2, date > "2014-02-23" & date < "2015-02-24")

removed_sub$date <- as.factor(removed_sub$date)

summary(removed_sub$date)

###HAHAH it worked. And what R!!!

#Write as a new csv for the subset with removed canopy
write.csv(removed_sub, "yearsub_no_trtsp_nw.csv", row.names = FALSE)

#all files were written 5 April 2017
#Then they were rewritten 6 April 2017
# (hopefully) Finals were rewritten 11 May 2017
# Files were rewritten again after confirmation from Ricardo that two species with no seeds recorded had seeds that were not ready to germinate.

#Files rewritten again and updated on 15 Jan 18.  Added one species name which needed to be changed. See github notes.  Rewritten again on 22 Jan 18.
# Brosimun needed to be changed to Brosimum. 2-11-18

#Files were rewritten twice on 2-12-18 because additional species had not been included

# Files were rewritten on 3 March to include treatment species seeds that were found in other plots.