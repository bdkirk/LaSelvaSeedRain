# Seed composition

# load libraries
library(ggplot2); library(vegan); library(geomorph)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#### CHANGE THESE WITH NEW COMP FILES> JUST make comp instead of div.
# bring in data
animalcomp <- read.csv("animal_comp_sub_notrtsp_nw.csv", header = TRUE)
animalcomp$block <- as.factor(animalcomp$block)

windcomp <- read.csv("wind_comp_sub_notrtsp_nw.csv", header = TRUE)
windcomp$block <- as.factor(windcomp$block)

abioticcomp <- read.csv("abiotic_comp_sub_notrtsp_nw.csv", header = TRUE)
abioticcomp$block <- as.factor(abioticcomp$block)

lianacomp <- read.csv("liana_comp_sub_notrtsp_nw.csv", header = TRUE)
lianacomp$block <- as.factor(lianacomp$block)
lianacomp <- lianacomp[complete.cases(lianacomp),]

shrubcomp <- read.csv("shrub_comp_sub_notrtsp_nw.csv", header = TRUE)
shrubcomp$block <- as.factor(shrubcomp$block)
shrubcomp <- shrubcomp[complete.cases(shrubcomp),]

treecomp <- read.csv("tree_comp_sub_notrtsp_nw.csv", header = TRUE)
treecomp$block <- as.factor(treecomp$block)
treecomp <- treecomp[complete.cases(treecomp),]

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

###################################################################################
######################## Dispersal Mode ##########################################

###### Animal dispersal #########################################################

# Using Bray-Curtis dissimilarity index
ncol(animalcomp)
str(animalcomp[,1:10])

#get only the species data from the animalcomp file
seed_animal_comp <- animalcomp[,4:106]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
animal.seedcomp.bc <- vegdist(seed_animal_comp, method = "bray", binary = FALSE)
animal.seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(animal.seedcomp.bc ~ treatment+block, data = animalcomp) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(animal.seedcomp.bc ~ block+treatment, data = animalcomp) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
#advanced.procD.lm(animal.seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = animalcomp) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(animal.seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = animalcomp) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
animal.seedcomp.mds <- metaMDS(seed_animal_comp, autotransform = F, expand = F, k = 2, try = 100)
animal.seedcomp.mds$stress

#Ordination
nmsplot(animal.seedcomp.mds, animalcomp$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.


###### Wind dispersal #########################################################

# Using Bray-Curtis dissimilarity index
ncol(windcomp)
str(windcomp[,1:10])

#get only the species data from the windcomp file
seed_wind_comp <- windcomp[,4:20]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
wind.seedcomp.bc <- vegdist(seed_wind_comp, method = "bray", binary = FALSE)
wind.seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(wind.seedcomp.bc ~ treatment+block, data = windcomp) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(wind.seedcomp.bc ~ block+treatment, data = windcomp) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
#advanced.procD.lm(wind.seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = windcomp) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(wind.seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = windcomp) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
wind.seedcomp.mds <- metaMDS(seed_wind_comp, autotransform = F, expand = F, k = 2, try = 100)
wind.seedcomp.mds$stress

#Ordination
nmsplot(wind.seedcomp.mds, windcomp$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.



###### Abiotic dispersal #########################################################

# Using Bray-Curtis dissimilarity index
ncol(abioticcomp)
str(abioticcomp[,1:10])

#get only the species data from the abioticcomp file
seed_abiotic_comp <- abioticcomp[,4:21]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
abiotic.seedcomp.bc <- vegdist(seed_abiotic_comp, method = "bray", binary = FALSE)
abiotic.seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(abiotic.seedcomp.bc ~ treatment+block, data = abioticcomp) # this is not correct bc the type I and type III do not match correctly to compare with the treatment.
procD.lm(abiotic.seedcomp.bc ~ block+treatment, data = abioticcomp) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
#advanced.procD.lm(abiotic.seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = abioticcomp) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(abiotic.seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = abioticcomp) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
abiotic.seedcomp.mds <- metaMDS(seed_abiotic_comp, autotransform = F, expand = F, k = 2, try = 100)
abiotic.seedcomp.mds$stress

#Ordination
nmsplot(abiotic.seedcomp.mds, abioticcomp$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.



################################################################################################################# Life Form ############################################

###### Liana Composition ##########################################################

# Using Bray-Curtis dissimilarity index
ncol(lianacomp)
str(lianacomp[,1:10])

#get only the species data from the lianacomp file
seed_liana_comp <- lianacomp[,4:27]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
liana.seedcomp.bc <- vegdist(seed_liana_comp, method = "bray", binary = FALSE)
liana.seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(liana.seedcomp.bc ~ treatment+block, data = lianacomp) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(liana.seedcomp.bc ~ block+treatment, data = lianacomp) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
#advanced.procD.lm(liana.seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = lianacomp) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(liana.seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = lianacomp) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
liana.seedcomp.mds <- metaMDS(seed_liana_comp, autotransform = F, expand = F, k = 2, try = 100)
liana.seedcomp.mds$stress

#Ordination
nmsplot(liana.seedcomp.mds, lianacomp$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.

###### Shrub Composition ##########################################################

# Using Bray-Curtis dissimilarity index
ncol(shrubcomp)
str(shrubcomp[,1:10])

#get only the species data from the shrubcomp file
seed_shrub_comp <- shrubcomp[,4:46]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
shrub.seedcomp.bc <- vegdist(seed_shrub_comp, method = "bray", binary = FALSE)
shrub.seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(shrub.seedcomp.bc ~ treatment+block, data = shrubcomp) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(shrub.seedcomp.bc ~ block+treatment, data = shrubcomp) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
#advanced.procD.lm(shrub.seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = shrubcomp) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(shrub.seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = shrubcomp) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
shrub.seedcomp.mds <- metaMDS(seed_shrub_comp, autotransform = F, expand = F, k = 2, try = 100)
shrub.seedcomp.mds$stress

#Ordination
nmsplot(shrub.seedcomp.mds, shrubcomp$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.

###### Tree Composition ##########################################################

# Using Bray-Curtis dissimilarity index
ncol(treecomp)
str(treecomp[,1:10])

#get only the species data from the treecomp file
seed_tree_comp <- treecomp[,4:55]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
tree.seedcomp.bc <- vegdist(seed_tree_comp, method = "bray", binary = FALSE)
tree.seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(tree.seedcomp.bc ~ treatment+block, data = treecomp) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(tree.seedcomp.bc ~ block+treatment, data = treecomp) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
#advanced.procD.lm(tree.seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = treecomp) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(tree.seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = treecomp) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
tree.seedcomp.mds <- metaMDS(seed_tree_comp, autotransform = F, expand = F, k = 2, try = 100)
tree.seedcomp.mds$stress

#Ordination
nmsplot(tree.seedcomp.mds, treecomp$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.


#################################################################################
############# Jaccard ################################

# 1) Dispersal mode ##########

### a) Animal Dispersal ####
#Get data
animalcomp_j <- animalcomp

ncol(animalcomp_j)
str(animalcomp_j[,1:10])

#get only the species data from the compdata file
animal_comp_j <- animalcomp_j[,4:106]

#Turn this data into binary code to verify if vegdist does that
#Don't need to use binary, just select binary=true
animal_b <- decostand(animal_comp_j, method = "pa")
# Need this for mds


#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
animalcomp.jc <- vegdist(animal_comp_j, method = "jaccard", binary = TRUE)
animalcomp.jc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = compdata) # this is not correct bc the type I and type III do not match correctly to compare with the treatment.
procD.lm(animalcomp.jc ~ block+treatment, data = animalcomp_j) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction
### when running these results found p-value for treatment to be 

#This does a pair-wise comparison of the data

#advanced.procD.lm(animalcomp.jc ~ treatment, ~ 1, ~ treatment, data = animalcomp_j) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(animalcomp.jc ~ block+ treatment , ~block, ~ treatment, data = animalcomp_j) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
animalcompj.mds <- metaMDS(animal_b, autotransform = F, expand = F, k = 2, try = 100)
animalcompj.mds$stress
str(animalcompj.mds)

#Ordination
nmsplot(animalcompj.mds, animalcomp_j$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("Hial", "Pema", "Viko", "Vogu"))
## Use this one to check which block the plots were in.
nmsplot(animalcompj.mds, animalcomp_j$block, "1", "2", "3", "4",
        "topright", c("1", "2", "3", "4")) ##These need to be in the order they are in the file.


### b) Abiotic Dispersal ####
#Get data
abioticcomp_j <- abioticcomp

ncol(abioticcomp_j)
str(abioticcomp_j[,1:10])

#get only the species data from the compdata file
abiotic_comp_j <- abioticcomp_j[,4:21]

#Turn this data into binary code to verify if vegdist does that
#Don't need to use binary, just select binary=true
abiotic_b <- decostand(abiotic_comp_j, method = "pa")
# Need this for mds


#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
abioticcomp.jc <- vegdist(abiotic_comp_j, method = "jaccard", binary = TRUE)
abioticcomp.jc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = compdata) # this is not correct bc the type I and type III do not match correctly to compare with the treatment.
procD.lm(abioticcomp.jc ~ block+treatment, data = abioticcomp_j) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction
### when running these results found p-value for treatment to be 

#This does a pair-wise comparison of the data

#advanced.procD.lm(abioticcomp.jc ~ treatment, ~ 1, ~ treatment, data = abioticcomp_j) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(abioticcomp.jc ~ block+ treatment , ~block, ~ treatment, data = abioticcomp_j) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
abioticcompj.mds <- metaMDS(abiotic_b, autotransform = F, expand = F, k = 2, try = 100)
abioticcompj.mds$stress
str(abioticcompj.mds)

#Ordination
nmsplot(abioticcompj.mds, abioticcomp_j$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("Hial", "Pema", "Viko", "Vogu"))
## Use this one to check which block the plots were in.
nmsplot(abioticcompj.mds, abioticcomp_j$block, "1", "2", "3", "4",
        "topright", c("1", "2", "3", "4")) ##These need to be in the order they are in the file.


### Life form ####

### a) Liana ####
#Get data
lianacomp_j <- lianacomp

ncol(lianacomp_j)
str(lianacomp_j[,1:10])

#get only the species data from the compdata file
liana_comp_j <- lianacomp_j[,4:27]

#Turn this data into binary code to verify if vegdist does that
#Don't need to use binary, just select binary=true
liana_b <- decostand(liana_comp_j, method = "pa")
# Need this for mds


#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
lianacomp.jc <- vegdist(liana_comp_j, method = "jaccard", binary = TRUE)
lianacomp.jc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = compdata) # this is not correct bc the type I and type III do not match correctly to compare with the treatment.
procD.lm(lianacomp.jc ~ block+treatment, data = lianacomp_j) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction
### when running these results found p-value for treatment to be 

#This does a pair-wise comparison of the data

#advanced.procD.lm(lianacomp.jc ~ treatment, ~ 1, ~ treatment, data = lianacomp_j) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(lianacomp.jc ~ block+ treatment , ~block, ~ treatment, data = lianacomp_j) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
lianacompj.mds <- metaMDS(liana_b, autotransform = F, expand = F, k = 2, try = 100)
lianacompj.mds$stress
str(lianacompj.mds)

#Ordination
nmsplot(lianacompj.mds, lianacomp_j$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("Hial", "Pema", "Viko", "Vogu"))
## Use this one to check which block the plots were in.
nmsplot(lianacompj.mds, lianacomp_j$block, "1", "2", "3", "4",
        "topright", c("1", "2", "3", "4")) ##These need to be in the order they are in the file.

### b) shrub ####
#Get data
shrubcomp_j <- shrubcomp

ncol(shrubcomp_j)
str(shrubcomp_j[,1:10])

#get only the species data from the compdata file
shrub_comp_j <- shrubcomp_j[,4:46]

#Turn this data into binary code to verify if vegdist does that
#Don't need to use binary, just select binary=true
shrub_b <- decostand(shrub_comp_j, method = "pa")
# Need this for mds


#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
shrubcomp.jc <- vegdist(shrub_comp_j, method = "jaccard", binary = TRUE)
shrubcomp.jc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = compdata) # this is not correct bc the type I and type III do not match correctly to compare with the treatment.
procD.lm(shrubcomp.jc ~ block+treatment, data = shrubcomp_j) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction
### when running these results found p-value for treatment to be 

#This does a pair-wise comparison of the data

#advanced.procD.lm(shrubcomp.jc ~ treatment, ~ 1, ~ treatment, data = shrubcomp_j) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(shrubcomp.jc ~ block+ treatment , ~block, ~ treatment, data = shrubcomp_j) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
shrubcompj.mds <- metaMDS(shrub_b, autotransform = F, expand = F, k = 2, try = 100)
shrubcompj.mds$stress
str(shrubcompj.mds)

#Ordination
nmsplot(shrubcompj.mds, shrubcomp_j$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("Hial", "Pema", "Viko", "Vogu"))
## Use this one to check which block the plots were in.
nmsplot(shrubcompj.mds, shrubcomp_j$block, "1", "2", "3", "4",
        "topright", c("1", "2", "3", "4")) ##These need to be in the order they are in the file.

### c) tree #####
#Get data
treecomp_j <- treecomp

ncol(treecomp_j)
str(treecomp_j[,1:10])

#get only the species data from the compdata file
tree_comp_j <- treecomp_j[,4:55]

#Turn this data into binary code to verify if vegdist does that
#Don't need to use binary, just select binary=true
tree_b <- decostand(tree_comp_j, method = "pa")
# Need this for mds


#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
treecomp.jc <- vegdist(tree_comp_j, method = "jaccard", binary = TRUE)
treecomp.jc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = compdata) # this is not correct bc the type I and type III do not match correctly to compare with the treatment.
procD.lm(treecomp.jc ~ block+treatment, data = treecomp_j) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction
### when running these results found p-value for treatment to be 

#This does a pair-wise comparison of the data

#advanced.procD.lm(treecomp.jc ~ treatment, ~ 1, ~ treatment, data = treecomp_j) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(treecomp.jc ~ block+ treatment , ~block, ~ treatment, data = treecomp_j) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
treecompj.mds <- metaMDS(tree_b, autotransform = F, expand = F, k = 2, try = 100)
treecompj.mds$stress
str(treecompj.mds)

#Ordination
nmsplot(treecompj.mds, treecomp_j$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("Hial", "Pema", "Viko", "Vogu"))
## Use this one to check which block the plots were in.
nmsplot(treecompj.mds, treecomp_j$block, "1", "2", "3", "4",
        "topright", c("1", "2", "3", "4")) ##These need to be in the order they are in the file.
