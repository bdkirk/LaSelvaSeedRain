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
animalcomp$block <- as.factor(animalcomp$block)

#get only the species data from the animalcomp file
seed_animal_comp <- animalcomp[,4:103]

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
seed_wind_comp <- windcomp[,4:19]

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
seed_shrub_comp <- shrubcomp[,4:49]

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
seed_tree_comp <- treecomp[,4:52]

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



