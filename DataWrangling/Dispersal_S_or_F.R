#This file will look at differences in the seed that comes in as fruit
#Currently this file does not look at all seed rain data collected because some of it does not include material seed was sourced from whether in fruit or not in fruit.
# File created on 15 Jan 18

#Load libraries
library(readxl); library(plyr); library(ggplot2); library(reshape2); library(base); library(dplyr); library(vegan)

#set working directory to folder
setwd("Data/RawData")

#get file
disperse <- read.csv("fos_sr.csv", header = TRUE)

#Add in plot data
#add in plot column to identify the 15 different plots
disperse$plot <- NA

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
disperse$plot[disperse$trap %in% hial1] <- "hial1"
disperse$plot[disperse$trap %in% hial2] <- "hial2"
disperse$plot[disperse$trap %in% hial3] <- "hial3"
disperse$plot[disperse$trap %in% hial4] <- "hial4"
disperse$plot[disperse$trap %in% pema1] <- "pema1"
disperse$plot[disperse$trap %in% pema2] <- "pema2"
disperse$plot[disperse$trap %in% pema3] <- "pema3"
disperse$plot[disperse$trap %in% pema4] <- "pema4"
disperse$plot[disperse$trap %in% viko1] <- "viko1"
disperse$plot[disperse$trap %in% viko2] <- "viko2"
disperse$plot[disperse$trap %in% viko3] <- "viko3"
disperse$plot[disperse$trap %in% viko4] <- "viko4"
disperse$plot[disperse$trap %in% vogu2] <- "vogu2"
disperse$plot[disperse$trap %in% vogu3] <- "vogu3"
disperse$plot[disperse$trap %in% vogu4] <- "vogu4"

#Create two datasets to look at differences between seeds alone and seeds found in fruit
#1) Dataset for seeds alone
disperse$type <- as.factor(disperse$type)
seeds <- filter(disperse, type == "Semilla")

#2) Dataset for seeds in fruit
fruit <- filter(disperse, type == "Fruto")



#####   Abundance #####
###1) Seeds alone ####
### A) Calculate abundance of seeds found alone by treatment
seeds_treat <- ddply(seeds, .(treatment), summarise, total_seednum=sum(seednum))

### B) Calculate abundance of seeds found alone by plot
seeds_plot <- ddply(seeds, .(plot), summarise, total_seednum=sum(seednum))
write.csv(seeds_plot, "seeds_abund.csv")
#Manually added in treatment and block

### C) Calculate abundance of seeds found alone by species and treatment
seeds_ts <- ddply(seeds, .(species, treatment), summarise, total_seednum=sum(seednum))

### D) Calculate abundance of seeds found alone by species and plot
seeds_ps <- ddply(seeds, .(species, plot), summarise, total_seednum=sum(seednum))

### E) Graphs
seed_abund <- read.csv("seeds_abund.csv", header = TRUE)
boxplot(seed_abund$total_seednum~ seed_abund$treatment, xlab="treatment", ylab="seednum")


###2) Fruits ####
### A) Calculate abundance of fruits found alone by treatment
fruit_treat <- ddply(fruit, .(treatment), summarise, total_seednum=sum(fruitnum))

### B) Calculate abundance of fruits found alone by plot
fruit_plot <- ddply(fruit, .(plot), summarise, total_seednum=sum(fruitnum))
write.csv(fruit_plot, "fruit_abund.csv")
#manually add in treatment and block

### C) Calculate abundance of fruits found alone by species and treatment
fruit_ts <- ddply(fruit, .(species, treatment), summarise, total_seednum=sum(fruitnum))

### D) Calculate abundance of fruits found alone by species and plot
fruit_ps <- ddply(fruit, .(species, plot), summarise, total_seednum=sum(fruitnum))

### E) Graphs
fruit_abund <- read.csv("fruit_abund.csv", header = TRUE)
boxplot(fruit_abund$total_seednum~ fruit_abund$treatment, xlab="treatment", ylab="seednum")


###3) Seeds found in fruits####
### A) Calculate abundance of seeds found in fruit
fs_treat <- ddply(fruit, .(treatment), summarise, total_seednum=sum(seednum))

### B) Calculate abundance of seeds found alone by plot
fs_plot <- ddply(fruit, .(plot), summarise, total_seednum=sum(seednum))
write.csv(fs_plot, "fs_abund.csv")
#Manually add in treatment and species.

### C) Calculate abundance of seeds found alone by species and treatment
fs_ts <- ddply(fruit, .(species, treatment), summarise, total_seednum=sum(seednum))

### D) Calculate abundance of seeds found alone by species and plot
fs_ps <- ddply(fruit, .(species, plot), summarise, total_seednum=sum(seednum))

### E) Graphs
fs_abund <- read.csv("fs_abund.csv", header = TRUE)
boxplot(fs_abund$total_seednum~ fs_abund$treatment, xlab="treatment", ylab="seednum")




##### Diversity ######
###1) Seeds alone ####

#summarise the data by plot
seeds_sum <- ddply(seeds, .(plot, species), summarise, total_seednum=sum(seednum))

####CREATING WIDE DATA##
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
seeds_div <- dcast(seeds_sum, plot ~ species, value.var="total_seednum")
#This sorts the data after it is created
seeds_div <- seeds_div[,c(names(seeds_div)[1],sort(names(seeds_div)[2:ncol(seeds_div)]))]
#identifies all seed rain species that are NA
seeds_div <- seeds_div[, -which(names(seeds_div)=="NA")]
#Replace NA's with zeros
seeds_div[is.na(seeds_div)] <- 0

#####add in columns for richness, evenness and shannon-wiener diversity
x <- seeds_div[,1]
y <- seeds_div[,2:79]


#calculating diversity indices
y$richness <- specnumber(y)

y$diversity <- diversity(y, index = "shannon")

y$evenness <- (y$diversity/(log(y$richness)))

#change diversity to something more recognizable
y$divnorm <- exp(diversity(y, index = "shannon"))

#bind x and y back together
seeds_div2 <- cbind(x, y[,79:82])

#check to see this looks as expected
tail(seeds_div2)

##CHANGE WORKING DIRECTORY SO FILES WILL SAVE TO CORRECT LOCATION##
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#create csv file that can be used to do NMDS calculations
write.csv(seeds_div2, "seeds_alone_div.csv", row.names = FALSE)
write.csv(seeds_div, "seeds_alone_comp.csv", row.names = FALSE)

#graphs
 ## A) Richness
seeds_rich <- read.csv("seeds_alone_div.csv", header = TRUE)
boxplot(seeds_rich$richness~ seeds_rich$treatment, xlab= "treatment", ylab= "species richness")

## B) Diversity (Ignore that datafile is called seeds_rich)
boxplot(seeds_rich$diversity~ seeds_rich$treatment, xlab= "treatment", ylab= "species diversity")

###2) Fruit #####

#summarise the data by plot
fruit_sum <- ddply(fruit, .(plot, species), summarise, total_fruitnum=sum(fruitnum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
fruit_div <- dcast(fruit_sum, plot ~ species, value.var="total_fruitnum")
#This sorts the data after it is created
fruit_div <- fruit_div[,c(names(fruit_div)[1],sort(names(fruit_div)[2:ncol(fruit_div)]))]
#identifies all seed rain species that are NA
fruit_div <- fruit_div[, -which(names(fruit_div)=="NA")]
#Replace NA's with zeros
fruit_div[is.na(fruit_div)] <- 0

#####add in columns for richness, evenness and shannon-wiener diversity
a <- fruit_div[,1]
b <- fruit_div[,2:46]


#calculating diversity indices
b$richness <- specnumber(b)

b$diversity <- diversity(b, index = "shannon")

b$evenness <- (b$diversity/(log(b$richness)))

#change diversity to something more recognizable
b$divnorm <- exp(diversity(b, index = "shannon"))

#bind x and y back together
fruit_div2 <- cbind(a, b[,46:49])

#check to see this looks as expected
tail(fruit_div2)

#create csv file that can be used to do NMDS calculations
write.csv(fruit_div2, "fruit_div.csv", row.names = FALSE)
write.csv(fruit_div, "fruit_comp.csv", row.names = FALSE)

#graphs
## A) Richness
fruit_rich <- read.csv("fruit_div.csv", header = TRUE)
boxplot(fruit_rich$richness~ fruit_rich$treatment, xlab= "treatment", ylab= "species richness")

## B) Diversity (Ignore that datafile is called fruit_rich)
boxplot(fruit_rich$diversity~ fruit_rich$treatment, xlab= "treatment", ylab= "species diversity")



###3) Seeds in fruit ####

#summarise the data by plot
fs_sum <- ddply(fruit, .(plot, species), summarise, total_seednum=sum(seednum))

###CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
fs_div <- dcast(fs_sum, plot ~ species, value.var="total_seednum")
#This sorts the data after it is created
fs_div <- fs_div[,c(names(fs_div)[1],sort(names(fs_div)[2:ncol(fs_div)]))]
#identifies all seed rain species that are NA
fs_div <- fs_div[, -which(names(fs_div)=="NA")]
#Replace NA's with zeros
fs_div[is.na(fs_div)] <- 0

#####add in columns for richness, evenness and shannon-wiener diversity
c <- fs_div[,1]
d <- fs_div[,2:46]


#calculating diversity indices
d$richness <- specnumber(d)

d$diversity <- diversity(d, index = "shannon")

d$evenness <- (d$diversity/(log(d$richness)))

#change diversity to something more recognizable
d$divnorm <- exp(diversity(d, index = "shannon"))

#bind x and y back together
fs_div2 <- cbind(c, d[,46:49])

#check to see this looks as expected
tail(fs_div2)

#create csv file that can be used to do NMDS calculations
write.csv(fs_div2, "seeds_fruit_div.csv", row.names = FALSE)
write.csv(fs_div, "seeds_fruit_comp.csv", row.names = FALSE)

### GRAPHS
## A) Richness
fs_rich <- read.csv("seeds_fruit_div.csv", header = TRUE)
boxplot(fs_rich$richness~ fs_rich$treatment, xlab= "treatment", ylab= "species richness")

## B) Diversity (Ignore that datafile is called fs_rich)
boxplot(fs_rich$diversity~ fs_rich$treatment, xlab= "treatment", ylab= "species diversity")


####BEFORE MOVING ON ####
#Make sure to update comp files with appropriate variables for analysis to include treatment and block
library(geomorph)
### Composition ##########
##  ---------------------------------------------------
# Function for Plotting NMS results (Nick Lyon's code)
##  ---------------------------------------------------

# Function that plots NMS points with different colors for groups (only works for 4 groups)
## mod = object returned by metaMDS
## groupcol = group column (duh)
## g1 - 4 = grouping variables as written in dataframe
## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
## legcont = legend content, vector of labels for legend

nmsplot <- function(mod, groupcol, g1, g2, g3, g4, legpos, legcont) {
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none')
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 21, bg = "#006600")
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = "#FF6600")
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 23, bg = "#990066")
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 24, bg = "#0066CC")
  
  # Ordinate SD ellipses around the centroid
  ordiellipse(mod, groupcol, col = c("#006600", "#FF6600", "#990066", "#0066CC"), display = "sites", kind = "sd", label = F)
  
  # Add legend
  legend(legpos, legend = legcont, pch = c(21, 22, 23, 24), pt.bg = c("#006600", "#FF6600", "#990066", "#0066CC"), cex = 0.75)
  
}

###1) Seeds alone ####

#read in data
comp_seeds <- read.csv("seeds_alone_comp.csv")

# Using Bray-Curtis dissimilarity index
#Get data
ncol(comp_seeds) # 81 columns

str(comp_seeds[,1:10])
comp_seeds$block <- as.factor(comp_seeds$block)

#get only the species data from the comp_seeds file
comp_seeds1 <- comp_seeds[,4:81]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
compseeds.bc <- vegdist(comp_seeds1, method = "bray", binary = FALSE)
compseeds.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = comp_seeds) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(compseeds.bc ~ block+treatment, data = comp_seeds) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
advanced.procD.lm(compseeds.bc ~ treatment, ~ 1, ~ treatment, data = comp_seeds) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(compseeds.bc ~ block+ treatment , ~block, ~ treatment, data = comp_seeds) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
compseeds.mds <- metaMDS(comp_seeds1, autotransform = F, expand = F, k = 2, try = 100)
compseeds.mds$stress
#Ordination
nmsplot(compseeds.mds, comp_seeds$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.

#I am not sure what this does.
stressplot(compseeds.mds)



###2) Fruit ####

#read in data
comp_fruit <- read.csv("fruit_comp.csv")

# Using Bray-Curtis dissimilarity index
#Get data
ncol(comp_fruit) # 48 columns

str(comp_fruit[,1:10])
comp_fruit$block <- as.factor(comp_fruit$block)

#get only the species data from the comp_fruit file
comp_fruit1 <- comp_fruit[,4:48]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
compfruit.bc <- vegdist(comp_fruit1, method = "bray", binary = FALSE)
compfruit.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = comp_fruit) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(compfruit.bc ~ block+treatment, data = comp_fruit) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
advanced.procD.lm(compfruit.bc ~ treatment, ~ 1, ~ treatment, data = comp_fruit) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(compfruit.bc ~ block+ treatment , ~block, ~ treatment, data = comp_fruit) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
compfruit.mds <- metaMDS(comp_fruit1, autotransform = F, expand = F, k = 2, try = 100)
compfruit.mds$stress
#Ordination
nmsplot(compfruit.mds, comp_fruit$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.

#I am not sure what this does.
stressplot(compfruit.mds)


###3) Seeds in fruit ####
#read in data
comp_fs <- read.csv("seeds_fruit_comp.csv")

# Using Bray-Curtis dissimilarity index
#Get data
ncol(comp_fs) # 48 columns

str(comp_fs[,1:10])
comp_fs$block <- as.factor(comp_fs$block)

#get only the species data from the comp_fs file
comp_fs1 <- comp_fs[,4:48]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
compfs.bc <- vegdist(comp_fs1, method = "bray", binary = FALSE)
compfs.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = comp_fs) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(compfs.bc ~ block+treatment, data = comp_fs) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
advanced.procD.lm(compfs.bc ~ treatment, ~ 1, ~ treatment, data = comp_fs) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(compfs.bc ~ block+ treatment , ~block, ~ treatment, data = comp_fs) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
compfs.mds <- metaMDS(comp_fs1, autotransform = F, expand = F, k = 2, try = 100)
compfs.mds$stress
#Ordination
nmsplot(compfs.mds, comp_fs$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.

#I am not sure what this does.
stressplot(compfs.mds)
