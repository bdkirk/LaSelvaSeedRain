### This is the Ecology wrangling file
## File started on 8 February 2018

# load libraries
library(readxl); library(plyr); library(dplyr); library(tidyr);library(ggplot2); library(reshape2); library(base); library(vegan)

# set working directory
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/RawData")

# bring in data
ecology <-read_excel("Ecology.xlsx", sheet=1, col_names=TRUE, na= "NA")

# change column names
colnames(ecology) <- c("species", "lifeform", "dispersal", "sa", "unique", "twelvemonths", "ricardo", "latersurvey")

# change dispersal mode and life form to lowercase
ecology$lifeform <- tolower(ecology$lifeform)
ecology$dispersal <- tolower(ecology$dispersal)
ecology$species <- tolower(ecology$species)

# change class type
ecology$species <- as.factor(ecology$species)
ecology$dispersal <- as.factor(ecology$dispersal)
ecology$lifeform <- as.factor(ecology$lifeform)

# look to see class
str(ecology)
levels(ecology$species)
levels(ecology$species)<-gsub("paulliniafasciculata", "paullinia fasciculata", levels(ecology$species)) #not sure why this is like this.
levels(ecology$species)

# check
str(ecology)

# Removed non-woody species (vines) and used 12 months of data

#A) create separate files for dispersal mode and life form to work on analyses

#1) Dispersal Mode
dispersal_mode <- subset(ecology, select = c(species, dispersal))
str(dispersal_mode)

#2) Life form
life_form <- subset(ecology, select= c(1, 2))
str(life_form)

############################################################################
# B) bring in other files to combine to look at diversity, richness, and abundance

#set new wd to bring in file
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#abund_summary_year_notrtsp.csv has species abundances by treatment
abund <- read.csv("abund_summary_year_notrtsp_nw.csv")

#remove unneeded summary rows of seeds/m2/yr
abund2 <- subset(abund, select = c(1:5))
abund2$species <- tolower(abund2$species)
#remove species that will not match with dispersal dataset.
str(abund2)
abund2$species <- as.factor(abund2$species)

#### 1) Abundance
###### a) Dispersal Mode

# combine abund2 and dispersal files
dis_abund <- right_join(dispersal_mode, abund2, by="species")
# right join matches the two datasets by abund2 which has the restriction to 12 months and does not include treatment species.

# removed rubiaceae and unknown row.
dis_abund2 <- dis_abund[complete.cases(dis_abund),]
str(dis_abund2)

#change class
dis_abund2$species <- as.factor(dis_abund2$species)

dis_abund3 <- gather(dis_abund2, "treatment", "seednum", 3:6 )

write.csv(dis_abund3, "seedtrait_dis_abund.csv", row.names = FALSE)

d_abund <- ddply(dis_abund3, .(treatment, dispersal), summarise, seednum=sum(seednum))

###### b) Lifeform
life_abund <- right_join(life_form, abund2, by= "species")

#remove NA's life forms (ficus sp, ficus sp2, rubiaceae, lauraceae and NA)
life_abund2 <- life_abund[complete.cases(life_abund),]

life_abund3 <- gather(life_abund, "treatment", "seednum", 3:6)

write.csv(life_abund3, "seedtrait_life_abund.csv", row.names = FALSE)

l_abund <- ddply(life_abund3, .(treatment, lifeform), summarise, seednum=sum(seednum))

#### 2) Richness
# Need file that has species and treatment then variables (life form and dispersal mode)

## a) dispersal mode
 #Altered file above to just include names

#### Different approach based on talking with Haldre on 2/13/18
# subset data for dispersal mode and look at abundance, species richness, diversity and seed species composition across treatments.

#set new wd to bring in file
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#load libraries
library(tidyr); library(dplyr); library(plyr); library(readr)

#bring in dataset
species_abund <- read.csv("yearsub_no_trtsp_nw.csv", header = TRUE)

species_abund2 <- ddply(species_abund, .(plot, species), summarise, total_seednum=sum(total_seednum))

species_abund3 <- right_join(species_abund2, dispersal_mode, by= "species")
species_abund4 <- species_abund3[complete.cases(species_abund3),]

#create separate files for wind, animal and mechanical.
wind_abund <- filter (species_abund4, dispersal == "wind")
animal_abund <- filter(species_abund4, dispersal == "animal")
mech_abund <- filter(species_abund4, dispersal =="mechanical")

#summarise those files
wind_abund2 <- ddply(wind_abund, .(plot), summarise, seednum=sum(total_seednum))
animal_abund2 <- ddply(animal_abund, .(plot), summarise, seednum=sum(total_seednum))
mech_abund2 <- ddply(mech_abund, .(plot), summarise, seednum=sum(total_seednum))

#add block and treatment to those
wind_abund3 <- separate(wind_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)
animal_abund3 <- separate(animal_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)
mech_abund3 <- separate(mech_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#write files to do analysis for abundance
# change working directory if not in tidy file

write.csv(wind_abund3, "wind_abund_tidy.csv", row.names = FALSE)
write.csv(animal_abund3, "animal_abund_tidy.csv", row.names = FALSE)
write.csv(mech_abund3, "mech_abund_tidy.csv", row.names = FALSE)


### Now work on tidy files for diversity analyses
#1) Animal diversity first
animal_plotsum <- ddply(animal_abund, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
animal_div_data <- dcast(animal_plotsum, plot ~ species, value.var="seednum")

#Replace NA's with zeros
animal_div_data[is.na(animal_div_data)] <- 0

#writing composition analysis tidy file
animal_comp_data <- separate(animal_div_data, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv(animal_comp_data, "animal_comp_sub_notrtsp_nw.csv", row.names = FALSE)

#####add in columns for richness, evenness and shannon-wiener diversity
x <- animal_div_data[,1]
y <- animal_div_data[,2:101]

str(animal_div_data)
#calculating diversity indices
y$richness <- specnumber(y)
y$diversity <- diversity(y, index = "shannon")
y$evenness <- (y$diversity/(log(y$richness)))

#change diversity to something more recognizable
y$divnorm <- exp(diversity(y, index = "shannon"))

#bind x and y back together
animal_div_data2 <- cbind(x, y[,101:104])
tail(animal_div_data2)

#change name for X column
names(animal_div_data2)[names(animal_div_data2) == "x"] <- "plot"

#add in columns for treatment and block
animal_div_data3 <- separate(animal_div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(animal_div_data3, "animal_div_sub_notrtsp_nw.csv", row.names = FALSE)

#################################
#2) Wind Diversity
wind_plotsum <- ddply(wind_abund, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
wind_div_data <- dcast(wind_plotsum, plot ~ species, value.var="seednum")

#Replace NA's with zeros
wind_div_data[is.na(wind_div_data)] <- 0

# creating composition analysis data
wind_comp_data <- separate(wind_div_data, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv(wind_comp_data, "wind_comp_sub_notrtsp_nw.csv", row.names = FALSE)

#####add in columns for richness, evenness and shannon-wiener diversity
a <- wind_div_data[,1]
b <- wind_div_data[,2:17]

str(wind_div_data)

#calculating diversity indices
b$richness <- specnumber(b)
b$diversity <- diversity(b, index = "shannon")
b$evenness <- (b$diversity/(log(b$richness)))

#change diversity to something more recognizable
b$divnorm <- exp(diversity(b, index = "shannon"))

#bind x and y back together
wind_div_data2 <- cbind(a, b[,17:20])
tail(wind_div_data2)

#change name for X column
names(wind_div_data2)[names(wind_div_data2) == "a"] <- "plot"

#add in columns for treatment and block
wind_div_data3 <- separate(wind_div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(wind_div_data3, "wind_div_sub_notrtsp_nw.csv", row.names = FALSE)


#################
# Not worth doing the mechanical diversity because there are too few species 

###############################################################################
##############################################################################
###############################################################################

# Looking at life forms.
species_abund5 <- right_join(species_abund2, life_form, by= "species")

#create separate files for wind, animal and mechanical.
liana_abund <- filter (species_abund5, lifeform == "liana")
tree_abund <- filter(species_abund5, lifeform == "tree")
shrub_abund <- filter(species_abund5, lifeform =="shrub")
palm_abund <- filter(species_abund5, lifeform =="palm")

#summarise those files
liana_abund2 <- ddply(liana_abund, .(plot), summarise, seednum=sum(total_seednum))
tree_abund2 <- ddply(tree_abund, .(plot), summarise, seednum=sum(total_seednum))
shrub_abund2 <- ddply(shrub_abund, .(plot), summarise, seednum=sum(total_seednum))
palm_abund2 <- ddply(palm_abund, .(plot), summarise, seednum=sum(total_seednum))

#add block and treatment to those
liana_abund3 <- separate(liana_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)
tree_abund3 <- separate(tree_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)
shrub_abund3 <- separate(shrub_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)
palm_abund3 <- separate(palm_abund2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#write files to do analysis for abundance
write.csv(liana_abund3, "liana_abund_tidy.csv", row.names = FALSE)
write.csv(tree_abund3, "tree_abund_tidy.csv", row.names = FALSE)
write.csv(shrub_abund3, "shrub_abund_tidy.csv", row.names = FALSE)
write.csv(palm_abund3, "palm_abund_tidy.csv", row.names = FALSE)


### Now work on tidy files for diversity analyses
#1) Liana diversity first
liana_plotsum <- ddply(liana_abund, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
liana_div_data <- dcast(liana_plotsum, plot ~ species, value.var="seednum")

#Replace NA's with zeros
liana_div_data[is.na(liana_div_data)] <- 0

#writing comp file
liana_comp_data <- separate(liana_div_data, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv(liana_comp_data, "liana_comp_sub_notrtsp_nw.csv", row.names = FALSE)

#####add in columns for richness, evenness and shannon-wiener diversity
c <- liana_div_data[,1]
d <- liana_div_data[,2:25]

str(liana_div_data)
#calculating diversity indices
d$richness <- specnumber(d)
d$diversity <- diversity(d, index = "shannon")
d$evenness <- (d$diversity/(log(d$richness)))

#change diversity to something more recognizable
d$divnorm <- exp(diversity(d, index = "shannon"))

#bind x and y back together
liana_div_data2 <- cbind(c, d[,25:28])
tail(liana_div_data2)

#change name for X column
names(liana_div_data2)[names(liana_div_data2) == "c"] <- "plot"

#add in columns for treatment and block
liana_div_data3 <- separate(liana_div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(liana_div_data3, "liana_div_sub_notrtsp_nw.csv", row.names = FALSE)

#################################
#2) Tree Diversity
tree_plotsum <- ddply(tree_abund, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
tree_div_data <- dcast(tree_plotsum, plot ~ species, value.var="seednum")

#Replace NA's with zeros
tree_div_data[is.na(tree_div_data)] <- 0

#writing comp data
tree_comp_data <- separate(tree_div_data, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv(tree_comp_data, "tree_comp_sub_notrtsp_nw.csv", row.names = FALSE)

#####add in columns for richness, evenness and shannon-wiener diversity
e <- tree_div_data[,1]
f <- tree_div_data[,2:50]

str(tree_div_data)

#calculating diversity indices
f$richness <- specnumber(f)
f$diversity <- diversity(f, index = "shannon")
f$evenness <- (f$diversity/(log(f$richness)))

#change diversity to something more recognizable
f$divnorm <- exp(diversity(f, index = "shannon"))

#bind x and y back together
tree_div_data2 <- cbind(e, f[,50:53])
tail(tree_div_data2)

#change name for X column
names(tree_div_data2)[names(tree_div_data2) == "e"] <- "plot"

#add in columns for treatment and block
tree_div_data3 <- separate(tree_div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(tree_div_data3, "tree_div_sub_notrtsp_nw.csv", row.names = FALSE)


#################
# 3) Shrub Diversity
shrub_plotsum <- ddply(shrub_abund, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
shrub_div_data <- dcast(shrub_plotsum, plot ~ species, value.var="seednum")

#Replace NA's with zeros
shrub_div_data[is.na(shrub_div_data)] <- 0

#writing comp data
shrub_comp_data <- separate(shrub_div_data, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv(shrub_comp_data, "shrub_comp_sub_notrtsp_nw.csv", row.names = FALSE)

#####add in columns for richness, evenness and shannon-wiener diversity
g <- shrub_div_data[,1]
h <- shrub_div_data[,2:47]

str(shrub_div_data)

#calculating diversity indices
h$richness <- specnumber(h)
h$diversity <- diversity(h, index = "shannon")
h$evenness <- (h$diversity/(log(h$richness)))

#change diversity to something more recognizable
h$divnorm <- exp(diversity(h, index = "shannon"))

#bind x and y back together
shrub_div_data2 <- cbind(g, h[,47:50])
tail(shrub_div_data2)

#change name for X column
names(shrub_div_data2)[names(shrub_div_data2) == "g"] <- "plot"

#add in columns for treatment and block
shrub_div_data3 <- separate(shrub_div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(shrub_div_data3, "shrub_div_sub_notrtsp_nw.csv", row.names = FALSE)

#############
# 3) Palm Diversity
palm_plotsum <- ddply(palm_abund, .(plot, species), summarise, seednum=sum(total_seednum))

####CREATING WIDE DATA###
#Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
palm_div_data <- dcast(palm_plotsum, plot ~ species, value.var="seednum")

#Replace NA's with zeros
palm_div_data[is.na(palm_div_data)] <- 0

#writing comp data
palm_comp_data <- separate(palm_div_data, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

write.csv(palm_comp_data, "palm_comp_sub_notrtsp_nw.csv", row.names = FALSE)

#####add in columns for richness, evenness and shannon-wiener diversity
i <- palm_div_data[,1]
j <- palm_div_data[,2:6]

str(palm_div_data)

#calculating diversity indices
j$richness <- specnumber(j)
j$diversity <- diversity(j, index = "shannon")
j$evenness <- (j$diversity/(log(j$richness)))

#change diversity to something more recognizable
j$divnorm <- exp(diversity(j, index = "shannon"))

#bind x and y back together
palm_div_data2 <- cbind(i, j[,6:9])
tail(palm_div_data2)

#change name for X column
names(palm_div_data2)[names(palm_div_data2) == "i"] <- "plot"

#add in columns for treatment and block
palm_div_data3 <- separate(palm_div_data2, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

#create csv file that can be used to do NMDS calculations
write.csv(palm_div_data3, "palm_div_sub_notrtsp_nw.csv", row.names = FALSE)
