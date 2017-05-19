# Started on 12 May 2017
# This is the wrangling file for the second analysis looking at whether there are differences in seed rain abundance, diversity, composition and phenology across the two mesh sizes
# This data will be analyzed as a split plot design.  Whole- Block (4); whole factor- trt group/overstory spp (4); Subplot/splitplot- plot (15); subplot/splitplot factor- mesh(2)

# This will require a summarized long format tidy file for abundance; a wide format tidy file for diversity and composition; pheno?? #not sure of pheno yet, will find out after meeting with stat help

############ Abundance ############
#add library
library(dplyr); library(plyr)

#Set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")
#bring in datafile
mesh_abund <- read.csv("yearsub_no_cpysp.csv")

#summarise to the trap level
sum_trap <- ddply(mesh_abund, .(trap), summarise, seednum=sum(total_seednum))

#assigning canopy species, block and quad to a particular trap number
##want to add in overstory treatment to new dataset by matching it to trap
trap_trt <- mesh_abund[, c("trap", "canopysp", "block", "meshtype", "plot")]
trap_trt <- ddply(trap_trt, .(trap, canopysp, block, meshtype, plot), summarise, n=length(plot))
trap_trt <- trap_trt[,-6]

#check and make sure two data files have same number of rows to be merged.
#Merges data together
mesh_abund2 <- merge(sum_trap, trap_trt, by="trap", all.x=TRUE)

write.csv(mesh_abund2, "Mesh_abund_analysis")


#######Diversity#######
#load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base)

#bring in original data file with overstory species removed that has been cut to the specific dates
setwd("Data/TidyData")

#bring in data
meshdiv <- read.csv("yearsub_no_cpysp.csv")

#summarise by plot
trapsum <- ddply(meshdiv, .(trap, species), summarise, seednum=sum(total_seednum))

#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
meshdiv_data <- dcast(trapsum, trap ~ species, value.var="seednum")
#This sorts the data after it is created
meshdiv_data <- meshdiv_data[,c(names(meshdiv_data)[1],sort(names(meshdiv_data)[2:ncol(meshdiv_data)]))]
#identifies all seed rain species that are NA
meshdiv_data <- meshdiv_data[, -which(names(meshdiv_data)=="NA")]
#Replace NA's with zeros
meshdiv_data[is.na(meshdiv_data)] <- 0

#create csv file that can be used to do NMDS calculations
write.csv(meshdiv_data, "mesh_div_analysis.csv", row.names = FALSE)



######### Composition ########
#bring in data
meshcomp <- read.csv("yearsub_no_cpysp.csv")

#summarise by plot
trapsum <- ddply(meshcomp, .(trap, species), summarise, seednum=sum(total_seednum))

#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
meshcomp_data <- dcast(trapsum, trap ~ species, value.var="seednum")
#This sorts the data after it is created
meshcomp_data <- meshcomp_data[,c(names(meshcomp_data)[1],sort(names(meshcomp_data)[2:ncol(meshcomp_data)]))]
#identifies all seed rain species that are NA
meshcomp_data <- meshcomp_data[, -which(names(meshcomp_data)=="NA")]
#Replace NA's with zeros
meshcomp_data[is.na(meshcomp_data)] <- 0

#create csv file that can be used to do NMDS calculations
write.csv(meshcomp_data, "mesh_comp_analysis.csv", row.names = FALSE)
