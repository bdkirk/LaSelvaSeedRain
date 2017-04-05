#Started on 4 April 2017
##This is Nick Lyon's code

#Read in libraries
library(ggplot2); library(vegan); library(geomorph)
#set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#Get data
speciesdata <- read.csv("div_ab_rich.csv")
#This line of code restricts the data analyzed to having a species number greater than 5; Confirm the biology makes this a good number.
speciesdata2 <- subset(speciesdata, specnumber(speciesdata[,6:143]) >= 5)
#This eliminates all the factors so that they are not used in parts of the analysis
seedspecies <- speciesdata2[,6:143]


##  --------------------------------------------------------------------------------------------------------------------------------------  ##
# Function for Plotting NMS results (Nick Lyon's code)
##  --------------------------------------------------------------------------------------------------------------------------------------  ##

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
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 22, bg = "#006600")
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = "#FF6600")
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 22, bg = "#990066")
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 22, bg = "#0066CC")
  
  # Ordinate SD ellipses around the centroid
  ordiellipse(mod, groupcol, col = c("#006600", "#FF6600", "#990066", "#0066CC"), display = "sites", kind = "sd", label = F)
  
  # Add legend
  legend(legpos, legend = legcont, fill = c("#006600", "#FF6600", "#990066", "#0066CC"))
  
}

#############Seedrain Analysis for All data####
# Using Bray-Curtis dissimilarity index
#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
seedrain.bc <- vegdist(seedspecies)

# PERMANOVA (Anderson et al. 2001)
procD.lm(seedrain.bc ~ canopysp*block, data = speciesdata2) # significant diff btw overstory plots
#This does a pair-wise comparison of the data
advanced.procD.lm(seedrain.bc ~ canopysp, ~ 1, ~ canopysp, data = speciesdata2) # Four overstory treatments compared with one another

#NMDS
seedspecies.mds <- metaMDS(seedspecies, autotransform = F, expand = F, k = 2, try = 20)
seedspecies.mds$stress

#Ordination
nmsplot(seedspecies.mds, speciesdata2$canopysp, "Hial", "Vogu", "Pema", "Viko",
        "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))

#I am not sure what this does.
stressplot(seedspecies.mds)






###Now you will do the same thing analyzing effect of MESH TYPE###############################
#Bring data in
speciesdata_allMesh <- read.csv("div_ab_rich.csv")
seedrain<- read.csv("seedrain_alltidy.csv")

#original$column <- new$column[match(original$column, new$column)]
speciesdata_allMesh$meshtype <- seedrain$meshtype[match(speciesdata_allMesh$trap, seedrain$trap)]
#Check to see if column was added
str(speciesdata_allMesh[,140:149])
str(speciesdata_allMesh[,1:10])
#Creates a subset for species greater than 5
sd_allmesh_sub <- subset(speciesdata_allMesh, specnumber(speciesdata_allMesh[,6:142]) >=5)
#Creates this subset
seed_mesh_all <- sd_allmesh_sub[,6:142]

##You will now use speciesdata_allMesh for with the factors and seed_mesh_all for without the factors
##This begins the analysis
# Using Bray-Curtis dissimilarity index #name new object and do the vegdist (part of vegan) for the data with no factors, this computes dissimilarity indices to use in the PERMANOVA
seed_mesh_all.bc <- vegdist(seed_mesh_all)

# PERMANOVA (Anderson et al. 2001)
#Use data from dissimilarity and can compare treatments 
###THIS code will not work because meshtype does not have more than two factos so it cannot be contrasted.
#procD.lm(seed_mesh_all.bc ~ canopysp + meshtype, data = sd_allmesh_sub) # treatments- canopy is statistically significant, meshtype is not statistically signficant
procD.lm(seed_mesh_all.bc ~ canopysp + block, data = sd_allmesh_sub)

#This does a pair-wise comparison of the data
advanced.procD.lm(seed_mesh_all.bc ~ canopysp+meshtype, ~canopysp, ~meshtype, data = sd_allmesh_sub) # This shows that there isn't an interaction effect between canopy sp and meshtype

#NMDS
#new object <- metaMDS(nofactorfile, auto blah blah....)
meshtype.mds <- metaMDS((seed_mesh_all), autotransform = F, expand = F, k = 2, try = 20)
seedspecies.mds$stress

#Ordination ####this really doesn't do anything because code is written for 4 objects, not two

nmsplot(meshtype.mds, sd_allmesh_sub$meshtype, "meshreg", "meshsmall", 
        "bottomleft", c("Reg", "Small"))
stressplot(meshtype.mds)

#-#-#_#3_3_THIS IS WHERE YOU NEED TO START
######################Look at data with no overstory##########
#Get data
speciesdata <- read.csv("div_ab_rich.csv")
#This line of code restricts the data analyzed to having a species number greater than 5; Confirm the biology makes this a good number.
speciesdata2 <- subset(speciesdata, specnumber(speciesdata[,4:139]) >= 5)
#This eliminates all the factors so that they are not used in parts of the analysis
seedspecies <- speciesdata2[,4:139]