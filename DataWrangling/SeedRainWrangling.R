# This is data munging for seed rain from ECOS project
#started on 10 feb 17


library(readxl)
library(plyr)
library(ggplot2)

#set working directory to folder
setwd("Data/RawData")


seedrain<-read_excel("Seed rain_2014-2015_Lluvia de semillas_RBA_20_Abr_15.xlsx", sheet=3, col_names=TRUE, na= "NA")

str(seedrain)
colnames(seedrain) <- c("week", "date", "trap", "sample", "canopysp", "block", "quad", "type", "species", "fruitnum", "seednum", "poop", "damaged", "obs")


str(seedrain)
summary(seedrain)

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

setwd("../")
setwd("TidyData")
write.csv(seedrain,"seedraintidy.csv")
#csv is now in tidy data




#Clean up data to look at species across the four plots removing data that is irrelevant

#remove rows of data on unnamed species
seedrain2<-seedrain[seedrain$species!="Muestra 100",]  #this is not working yet.
