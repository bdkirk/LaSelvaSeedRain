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
compdata <- read.csv("comp_sub_notrtsp.csv")

ncol(compdata)
str(compdata[,1:10])
compdata$block <- as.factor(compdata$block)

#get only the species data from the compdata file
seed_comp <- compdata[,4:126]

#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
seedcomp.bc <- vegdist(seed_comp, method = "bray", binary = FALSE)
seedcomp.bc

# PERMANOVA (Anderson et al. 2001)
#procD.lm(seedcomp.bc ~ treatment+block, data = compdata) # this is not correct bc the type I and type III do not match correctly to compare with teh treatment.
procD.lm(seedcomp.bc ~ block+treatment, data = compdata) # correct one

#Dr. Dixon recommended using the + or additive to account for the interaction

#This does a pair-wise comparison of the data
advanced.procD.lm(seedcomp.bc ~ treatment, ~ 1, ~ treatment, data = compdata) # this is a null hypothesis but it doesn't account for random error variation from blocks.
advanced.procD.lm(seedcomp.bc ~ block+ treatment , ~block, ~ treatment, data = compdata) #correct one; this one correctly looks at differences amongst the treatments when comparing the blocks


#NMDS
seedcomp.mds <- metaMDS(seed_comp, autotransform = F, expand = F, k = 2, try = 100)
seedcomp.mds$stress
#Ordination
nmsplot(seedcomp.mds, compdata$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
##These need to be in the order they are in the file.

#I am not sure what this does.
stressplot(seedcomp.mds)

#EDIT TO DEMONSTRATE A SCREEPLOT
#Modified code from https://websites.pmc.ucsc.edu/~mclapham/Rtips/ordination.htm
#data_matrix: rows are sites, columns are species
#reps: number of random starts per number of factors
#max_factors: maximum number of factors to do ordination with

NMDS.scree<-function(data_matrix, reps=3, max_factors=2) { 
  
  n <- nrow(data_matrix)
  stopifnot(n>max_factors)
  
  results <- sapply(rep(1:max_factors,each=reps), function(k){
    metaMDS(data_matrix,autotransform=F,k=k)$stress
  })
  
  plot(rep(1:max_factors,each=reps),results,
       xlim=c(0,max_factors), ylim=c(0,results[1]),
       xlab="# of Dimensions",ylab="Stress",main="NMDS screeplot")
  
}

NMDS.scree(seed_comp, reps=3, max_factors=7)
#Based on this, you might use three dimensions rather than 2 dimensions
seedcomp.mds2 <- metaMDS(seed_comp, autotransform = F, expand = F, k = 3, try = 100)
stressplot(seedcomp.mds2)
nmsplot(seedcomp.mds2, compdata$treatment, "hial", "pema", "viko", "vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))

### Ran on 21 May and did not find significant difference between the treatments ###
#Reran on 19 Jul after talkin with Dr. Dixon and we found that 2 dimensions was sufficient and that there are significant differences between vogu and pema.