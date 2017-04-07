# This is data munging for seed rain from ECOS project
#started on 10 feb 17
#tidy file completed on 2 April 2017

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
levels(seedrain$species)<-gsub("Werahuia gladioflora", "Werauhia gladioliflora", levels(seedrain$species))
levels(seedrain$species)<-gsub("guatemalnsis", "guatemalensis", levels(seedrain$species))
levels(seedrain$species)<-gsub ("anona papilionella", "annona papilionella", levels(seedrain$species))
levels(seedrain$species)<-gsub("bursera simarouba", "bursera simaruba", levels(seedrain$species))
levels(seedrain$species)<-gsub("casearea", "casearia", levels(seedrain$species))
levels(seedrain$species)<-gsub("alchorneiodes", "alchorneoides", levels(seedrain$species))
levels(seedrain$species)<- gsub("sapindioides", "sapindoides", levels(seedrain$species))
levels(seedrain$species)<-gsub("papilosa", "papillosa", levels(seedrain$species))
levels(seedrain$species)<-gsub("conostegia crenula", "clidemia crenulata", levels(seedrain$species))
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

trap_trt <- seedrain[, c("trap", "canopysp", "block", "quad")]
trap_trt <- ddply(trap_trt, .(trap, canopysp, block, quad), summarise, n=length(quad))
trap_trt <- trap_trt[,-5]

#check and make sure merged data has same number of rows as seedrain_all originally had
#Merges data together
seedrain_all <- merge(seedrain_all, trap_trt, by="trap", all.x=TRUE)
##VERIFIED THIS DOES ON 2 APRIL 17

#function to calculate the total seed number across the four treatments and the four blocks
seed_b_c <- ddply(seedrain_all, .(canopysp, block), summarise, total=sum(total_seednum))
seed_b_c
#function to calculate the total seed number across the four blocks
seed_b <- ddply(seedrain_all, .(block), summarise, total=sum(total_seednum))
seed_b
#function to calculate the total seed number across the four overstory treatments
seed_c <- ddply(seedrain_all, .(canopysp), summarise, total=sum(total_seednum))

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

#Merge the days variable back into the main data set
seedrain_all <- merge(seedrain_all, seed_rain_unique, by=c("date", "trap"), all.x=TRUE)

#check to see which date-trap combos are missing. Check these to see if they were actually collected, but not in database because no seeds were found. 
date_trap<-as.data.frame(with(seedrain, table(date, trap)))
date_trap[date_trap$Freq==0 & date_trap$date != "2014-01-21" & date_trap$date!= "2014-01-20",]
#Fixed the above issue with dates on 11 Mar 17

#This sums up the traps according to species
seedrain_final <- ddply(seedrain_all, .(trap, species), summarise, seednum=sum(total_seednum))

#merge this into seedrain_all
#This adds in canopy species by matching them with the trap from the other data
#original$column <- new$column[match(original$column, new$column)]
seedrain_final$canopysp <- seedrain_all$canopysp[match(seedrain_final$trap, seedrain_all$trap)]
seedrain_final$block <- seedrain_all$block[match(seedrain_final$trap, seedrain_all$trap)]
seedrain_final$quad <- seedrain_all$quad[match(seedrain_final$trap, seedrain_all$trap)]
seedrain_final$meshtype <- seedrain_all$meshtype[match(seedrain_final$trap, seedrain_all$trap)]
seedrain_final$days <- seedrain_all$days[match(seedrain_final$trap, seedrain_all$trap)]


###csv file for all of the data
setwd("../")
setwd("TidyData")
write.csv(seedrain_final,"seedrain_alltidy.csv", row.names = FALSE)

##create csv file for data without overstory species included####
#This removes the overstory species from the dataset 
removed_species <- seedrain_final[-which(seedrain_final$species%in%c("hieronyma alchorneoides", "pentaclethra macroloba", "virola koschnyi", "vochysia guatemalensis")),]

#write this as a csv file and export it as a unique dataset
write.csv(removed_species, "seedrain_nocanopysp_tidy.csv", row.names = FALSE)



#####Small mesh, two data sets, one with overstory sp. included and one without. ########
#Create data set with just meshsmall traps
seedrain_smallmesh <- seedrain_final[seedrain_final$meshtype=="meshsmall",]
#write csv file for all data with small mesh
write.csv(seedrain_smallmesh, "smallmesh_tidy.csv", row.names = FALSE)

#create data set with smallmesh and no overstory species (NOS) included
seedrain_smallmesh_NOS <- removed_species[removed_species$meshtype=="meshsmall",]
#write csv file for all data with small mesh and no overstory species included
write.csv(seedrain_smallmesh_NOS, "smallmesh_NOS_tidy.csv", row.names = FALSE)

###regular mesh, two data sets, one with overstory sp. included and one without.#########
#Create data set with just regmesh raps
seedrain_regmesh <- seedrain_final[seedrain_final$meshtype=="meshreg",]
#write csv file for all data with reg mesh
write.csv(seedrain_regmesh, "regmesh_tidy.csv", row.names = FALSE)

#create data set with reg mesh and no overstory species (NOS) included
seedrain_regmesh_NOS <- removed_species[removed_species$meshtype=="meshreg",]
#write csv file for data with reg mesh and no overstory species included
write.csv(seedrain_regmesh_NOS, "regmesh_NOS_tidy.csv", row.names = FALSE)


####CREATING NMDS DATA##########################################################
#Create species columns for use in NMDS and ordination

# DO NOT NEED THIS ORDER BELOW- seedrain_final <- seedrain_final[order(seedrain_final$species),]
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var"column you want to be the variable")
species_data <- dcast(seedrain_final, trap + canopysp + block + quad + meshtype + days ~ species, value.var="seednum")
#This sorts the data after it is created
species_data <- species_data[,c(names(species_data)[1:6],sort(names(species_data)[7:ncol(species_data)]))]
#identifies all seed rain species that are NA
species_data <- species_data[, -which(names(species_data)=="NA")]
#Replace NA's with zeros
species_data[is.na(species_data)] <- 0

#create csv file that can be used to do NMDS calculations
write.csv(species_data, "species_data.csv", row.names = FALSE)


#####Create a file with no overstory species#####
ovsty_rem<- dcast(removed_species, trap + canopysp + block + quad + meshtype + days ~ species, value.var="seednum")
#identifies all seed rain species that are NA
ovsty_rem <- ovsty_rem[, -which(names(ovsty_rem)=="NA")]
#Replace NA's with zeros
ovsty_rem[is.na(ovsty_rem)] <- 0
#create csv file that can be used to do NMDS calculations
write.csv(ovsty_rem, "NMDS_ovsty_rem.csv", row.names = FALSE)


####create a file with small mesh, overstory removed######
small_rem<- dcast(seedrain_smallmesh_NOS, trap + canopysp + block + quad + meshtype + days ~ species, value.var="seednum")
#identifies all seed rain species that are NA
small_rem <- small_rem[, -which(names(small_rem)=="NA")]
#Replace NA's with zeros
small_rem[is.na(small_rem)] <- 0

#create csv file that can be used to do NMDS calculations
write.csv(small_rem, "NMDS_small_rem.csv", row.names = FALSE)


###Create a file with reg mesh, overstory removed#####
reg_rem<- dcast(seedrain_regmesh_NOS, trap + canopysp + block + quad + meshtype + days ~ species, value.var="seednum")
#identifies all seed rain species that are NA
reg_rem <- reg_rem[, -which(names(reg_rem)=="NA")]
#Replace NA's with zeros
reg_rem[is.na(reg_rem)] <- 0
#create csv file that can be used to do NMDS calculations
write.csv(reg_rem, "NMDS_reg_rem.csv", row.names = FALSE)


###Create a file with small mesh, overstory included#####
small_all<- dcast(seedrain_smallmesh, trap + canopysp + block + quad + meshtype + days ~ species, value.var="seednum")
#identifies all seed rain species that are NA
small_all <- small_all[, -which(names(small_all)=="NA")]
#Replace NA's with zeros
small_all[is.na(small_all)] <- 0
#create csv file that can be used to do NMDS calculations
write.csv(small_all, "NMDS_small_all.csv", row.names = FALSE)


####Create a file with reg mesh, overstory included####
reg_all<- dcast(seedrain_regmesh, trap + canopysp + block + quad + meshtype + days ~ species, value.var="seednum")
#identifies all seed rain species that are NA
reg_all <- reg_all[, -which(names(reg_all)=="NA")]
#Replace NA's with zeros
reg_all[is.na(reg_all)] <- 0
#create csv file that can be used to do NMDS calculations
write.csv(reg_all, "NMDS_reg_all.csv", row.names = FALSE)


#all files were written 5 April 2017
#Then they were rewritten 6 April 2017