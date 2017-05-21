# Started on 17 May 2017
# This will include several different analyses for abundance, diversity, composition and phenology


#####Abundance Analysis#####

#Libraries
library(dplyr); library(plyr); library(stats); library(lme4)

#Bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

meshabund <- read.csv("Mesh_abund_analysis")

#plot residuals
mabundanalysis <- lmer(seednum~ canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = meshabund)
abund.res = resid(mabundanalysis) 

plot(meshabund$seednum, abund.res, ylab="Residuals", xlab="Seed Abundance", main="Mesh Abundance Residuals") 
abline(0, 0) 






#######Diversity Analysis######
#load library
library(vegan); library(base); library (stats); library(lme4)
# bring in data
mesh_diversity <- read.csv("mesh_div_analysis.csv")

#1) Richness
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$richness)
boxplot(mesh_diversity$richness~ mesh_diversity$canopysp, main= "Seed species richness per treatment", xlab="canopysp", ylab="species richness")

ggplot(mesh_diversity, aes(block, richness, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~canopysp)

##Residuals##

mrichanalysis <- lmer(richness~ canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity)
rich.res = resid(mrichanalysis) 

plot(mesh_diversity$richness, rich.res, ylab="Residuals", xlab="Seed Species Richness", main="Mesh Richness Residuals") 
abline(0, 0) 

#OR
plot(mrichanalysis)



#2) Diversity

#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$diversity)
boxplot(mesh_diversity$diversity~ mesh_diversity$canopysp, main= "Seed species diversity per treatment", xlab="canopysp", ylab="species diversity")

ggplot(mesh_diversity, aes(block, diversity, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~canopysp)

##Residuals##

mesh_divanalysis <- lmer(diversity+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity)
meshdiv.res = resid(mesh_divanalysis) 

plot(mesh_diversity$diversity, meshdiv.res, ylab="Residuals", xlab="Seed Species Diversity", main="Mesh Diversity Residuals") 
abline(0, 0) 

#OR
plot(mesh_divanalysis)




#3) Evenness

#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$evenness)
boxplot(mesh_diversity$evenness~ mesh_diversity$canopysp, main= "Seed species evenness per treatment", xlab="canopysp", ylab="species evenness")

ggplot(mesh_diversity, aes(block, evenness, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~canopysp)

##Residuals##

mesh_evenanalysis <- lmer(evenness~ canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity)
mesheven.res = resid(mesh_evenanalysis) 

plot(mesh_diversity$evenness, mesheven.res, ylab="Residuals", xlab="Seed Species Evenness", main="Mesh Evenness Residuals") 
abline(0, 0) 

#OR
plot(mesh_evenanalysis)






######## Composition ###############

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
  plot(mod, display = 'treatments', choice = c(1, 2), type = 'none')
  
  # Add points for each group with a different color per group
  points(mod$points[groupcol == g1, 1], mod$points[groupcol == g1, 2], pch = 22, bg = "#006600")
  points(mod$points[groupcol == g2, 1], mod$points[groupcol == g2, 2], pch = 22, bg = "#FF6600")
  points(mod$points[groupcol == g3, 1], mod$points[groupcol == g3, 2], pch = 22, bg = "#990066")
  points(mod$points[groupcol == g4, 1], mod$points[groupcol == g4, 2], pch = 22, bg = "#0066CC")
  
  # Ordinate SD ellipses around the centroid
  ordiellipse(mod, groupcol, col = c("#006600", "#FF6600", "#990066", "#0066CC"), display = "treatents", kind = "sd", label = F)
  
  # Add legend
  legend(legpos, legend = legcont, fill = c("#006600", "#FF6600", "#990066", "#0066CC"), cex = 0.75)
  
}


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
