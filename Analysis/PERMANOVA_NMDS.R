#Started on 4 April 2017
##This is Nick Lyon's code

#Read in libraries
library(ggplot2); library(vegan); library(geomorph)
#set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

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
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 22, bg = "#006600")
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = "#FF6600")
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 22, bg = "#990066")
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 22, bg = "#0066CC")
  
  # Ordinate SD ellipses around the centroid
  ordiellipse(mod, groupcol, col = c("#006600", "#FF6600", "#990066", "#0066CC"), display = "sites", kind = "sd", label = F)
  
  # Add legend
  legend(legpos, legend = legcont, fill = c("#006600", "#FF6600", "#990066", "#0066CC"), cex = 0.75)
  
}

#############Seedrain Analysis for All data####
# Using Bray-Curtis dissimilarity index
#Get data
#speciesdata <- read.csv("species_data.csv")
#ncol(speciesdata)
#str(speciesdata[,1:10])
#This line of code restricts the data analyzed to having a species number greater than 5; Confirm the biology makes this a good number.
#speciesdata2 <- subset(speciesdata, specnumber(speciesdata[,7:144]) >= 1)

#speciesdata$delete <- 0
#for (i in 1:nrow(speciesdata)){
 # speciesdata$delete[i] <- sum(speciesdata[i,6:143]>0)
#}

#subsetspecies_data <- speciesdata[speciesdata$delete>=5,]
#This eliminates all the factors so that they are not used in parts of the analysis
#seedspecies <- speciesdata2[,6:143]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
#seedrain.bc <- vegdist(seedspecies)

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedrain.bc ~ canopysp*block, data = speciesdata2) # significant diff btw overstory plots

#This does a pair-wise comparison of the data
#advanced.procD.lm(seedrain.bc ~ canopysp, ~ 1, ~ canopysp, data = speciesdata2) # Four overstory treatments compared with one another

#NMDS
#seedspecies.mds <- metaMDS(seedspecies, autotransform = F, expand = F, k = 2, try = 20)
#seedspecies.mds$stress

#Ordination
#nmsplot(seedspecies.mds, speciesdata2$canopysp, "Hial", "Vogu", "Pema", "Viko",
       # "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))

#I am not sure what this does.
#stressplot(seedspecies.mds)


#####NMDS for all data####
# Using Bray-Curtis dissimilarity index
#Get data
speciesdata <- read.csv("species_data.csv")
ncol(speciesdata)
str(speciesdata[,1:10])
speciesdata$block <-as.factor(speciesdata$block)
speciesdata$trap <- as.factor(speciesdata$trap)
str(speciesdata)

seedspecies <- speciesdata[,7:143]
#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
seedrain.bc <- vegdist(seedspecies)
# PERMANOVA (Anderson et al. 2001)
procD.lm(seedrain.bc ~ canopysp*block, data = speciesdata) # significant diff btw overstory plots
#This does a pair-wise comparison of the data
advanced.procD.lm(seedrain.bc ~ canopysp, ~ 1, ~ canopysp, data = speciesdata) # Four overstory treatments compared with one another
#NMDS
seedspecies.mds <- metaMDS(seedspecies, autotransform = F, expand = F, k = 2, try = 20)
seedspecies.mds$stress
#Ordination
nmsplot(seedspecies.mds, speciesdata$canopysp, "Hial", "Vogu", "Pema", "Viko",
        "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))
#I am not sure what this does.
stressplot(seedspecies.mds)

#HERE#
#####All data with no overstory species included###############################
#Bring data in
ovsty_rem_nmds <- read.csv("NMDS_ovsty_rem.csv")
View(ovsty_rem_nmds)
ncol(ovsty_rem_nmds)
str(ovsty_rem_nmds[,1:10])
#Creates a subset for species greater than 5
#ovsty_rem_sub <- subset(ovsty_rem_nmds, specnumber(ovsty_rem_nmds[,5:137]) >=5)
#Creates this subset
ovsty_rem_sub <- ovsty_rem_nmds[,7:139]

# Using Bray-Curtis dissimilarity index #name new object and do the vegdist (part of vegan) for the data with no factors, this computes dissimilarity indices to use in the PERMANOVA
ovsty_rem.bc <- vegdist(ovsty_rem_sub)

# PERMANOVA (Anderson et al. 2001)
#Use data from dissimilarity and can compare treatments 
###THIS code will not work because meshtype does not have more than two factos so it cannot be contrasted.
#procD.lm(seed_mesh_all.bc ~ canopysp + meshtype, data = sd_allmesh_sub) # treatments- canopy is statistically significant, meshtype is not statistically signficant
procD.lm(ovsty_rem.bc ~ canopysp + block + meshtype + trap, data = ovsty_rem_nmds)

#This does a pair-wise comparison of the data
advanced.procD.lm(ovsty_rem.bc ~ canopysp, ~ 1, ~ canopysp, data = ovsty_rem_nmds) # This shows that there isn't an interaction effect between canopy sp and meshtype

#NMDS
#new object <- metaMDS(nofactorfile, auto blah blah....)
ovsty_rem.mds <- metaMDS((ovsty_rem_sub), autotransform = F, expand = F, k = 2, try = 20)
ovsty_rem.mds$stress

#Ordination 
nmsplot(ovsty_rem.mds, ovsty_rem_nmds$canopysp, "Hial", "Vogu", "Pema", "Viko",
        "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))
stressplot(ovsty_rem.mds)

######################Small mesh with no overstory##########
#Get data
small_rem_nmds <- read.csv("NMDS_small_rem.csv")
#This line of code restricts the data analyzed to having a species number greater than 5; Confirm the biology makes this a good number.
#check what the last columns were
ncol(small_rem_nmds)
str(small_rem_nmds[,1:10])

#Creates a subset for species greater than 5
#small_rem_sub <- subset(small_rem_nmds, specnumber(small_rem_nmds[,5:119]) >=5)
#Creates this subset
small_rem_sub <- small_rem_nmds[,7:121]

# Using Bray-Curtis dissimilarity index #name new object and do the vegdist (part of vegan) for the data with no factors, this computes dissimilarity indices to use in the PERMANOVA
small_rem.bc <- vegdist(small_rem_sub)

# PERMANOVA (Anderson et al. 2001)
#Use data from dissimilarity and can compare treatments 
###THIS code will not work because meshtype does not have more than two factos so it cannot be contrasted.
#procD.lm(seed_mesh_all.bc ~ canopysp + meshtype, data = sd_allmesh_sub) # treatments- canopy is statistically significant, meshtype is not statistically signficant
procD.lm(small_rem.bc ~ canopysp + block, data = small_rem_nmds)

#This does a pair-wise comparison of the data
advanced.procD.lm(small_rem.bc ~ canopysp, ~ 1, ~ canopysp, data = small_rem_nmds) # This shows that there isn't an interaction effect between canopy sp and meshtype

#NMDS
#new object <- metaMDS(nofactorfile, auto blah blah....)
small_rem.mds <- metaMDS((small_rem_sub), autotransform = F, expand = F, k = 2, try = 20)
small_rem.mds$stress

#Ordination 
nmsplot(small_rem.mds, small_rem_nmds$canopysp, "Hial", "Vogu", "Pema", "Viko",
        "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))
stressplot(small_rem.mds)

##############reg mesh with no overstory#########
#Get data
reg_rem_nmds <- read.csv("NMDS_reg_rem.csv")
View(reg_rem_nmds)
#This line of code restricts the data analyzed to having a species number greater than 5; Confirm the biology makes this a good number.
#check what the last columns were
ncol(reg_rem_nmds)
str(reg_rem_nmds[,1:10])
str(reg_rem_nmds[,70:74])

#Creates this subset
reg_rem_sub <- reg_rem_nmds[,7:74]

levels(reg_rem_sub$species)

###########issue!!!
# Using Bray-Curtis dissimilarity index #name new object and do the vegdist (part of vegan) for the data with no factors, this computes dissimilarity indices to use in the PERMANOVA
reg_rem.bc <- vegdist(reg_rem_sub)
#dim(reg_rem_ssub)
#length(dimnames(reg_rem_ssub))
# PERMANOVA (Anderson et al. 2001)
#Use data from dissimilarity and can compare treatments 
procD.lm(reg_rem.bc ~ canopysp + block, data = reg_rem_nmds)

#This does a pair-wise comparison of the data
advanced.procD.lm(reg_rem.bc ~ canopysp, ~ 1, ~ canopysp, data = reg_rem_nmds) # This shows that there isn't an interaction effect between canopy sp and meshtype

#NMDS
#new object <- metaMDS(nofactorfile, auto blah blah....)
reg_rem.mds <- metaMDS((reg_rem_sub), autotransform = F, expand = F, k = 2, try = 20)
reg_rem.mds$stress

#Ordination 
nmsplot(reg_rem.mds, reg_rem_nmds$canopysp, "Hial", "Vogu", "Pema", "Viko",
        "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))
stressplot(reg_rem.mds)

