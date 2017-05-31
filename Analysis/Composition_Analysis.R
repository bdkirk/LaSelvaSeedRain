# Started on 19 May 2017
# Used code created by Nick Lyon


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


# Using Bray-Curtis dissimilarity index
#Get data
compdata <- read.csv("comp_sub_nocpy.csv")
###note: this data file does not have block or canopysp.  will need to adjust when doing permanova below

ncol(compdata)
str(compdata[,1:10])
compdata$block <- as.factor(compdata$block)

#get only the species data from the compdata file
seed_comp <- compdata[,4:127]
#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
seedcomp.bc <- vegdist(seed_comp)
# PERMANOVA (Anderson et al. 2001)
procD.lm(seedcomp.bc ~ canopysp*block, data = compdata) # significant interaction found and significant difference between blocks

#This does a pair-wise comparison of the data
advanced.procD.lm(seedcomp.bc ~ canopysp, ~ 1, ~ canopysp, data = compdata) # Four overstory treatments compared with one another

#NMDS
seedcomp.mds <- metaMDS(seed_comp, autotransform = F, expand = F, k = 2, try = 50)
seedcomp.mds$stress
#Ordination
nmsplot(seedcomp.mds, compdata$canopysp, "Hial", "Vogu", "Pema", "Viko",
        "bottomleft", c("HIAL", "VOGU", "PEMA", "VIKO"))
#I am not sure what this does.
stressplot(seedcomp.mds)


### Ran on 21 May and did not find significant difference between the treatments ###