# Started on 17 May 2017
# This will include several different analyses for abundance, diversity, composition and phenology


#####Abundance Analysis#####

#Libraries
library(dplyr); library(plyr); library(stats); library(lme4); library(readr); library(ggplot2); library(lsmeans)

#Bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

meshabund <- read.csv("Mesh_abund_analysis.csv")

str(meshabund)
meshabund$trap <- as.factor(meshabund$trap)
meshabund$block <- as.factor(meshabund$block)
str(meshabund)

ggplot(meshabund, aes(block, seednum, color =meshtype))+
  geom_boxplot()+
  facet_grid(.~canopysp)+
  ggtitle("Treatment abundance by mesh type")

hist(meshabund$seednum)
boxplot(meshabund$seednum~ meshabund$meshtype, xlab="Meshtype", ylab="seednum", main= "Seed abundance per treatment")

#plot residuals
mabundanalysis <- glmer(seednum~ canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block)+ (1|trap), family= "poisson", data = meshabund)
abund.res = resid(mabundanalysis) 

plot(meshabund$seednum, abund.res, ylab="Residuals", xlab="Seed Abundance", main="Mesh Abundance Residuals") 
abline(0, 0) 

mesh_abun.res <- resid(mabundanalysis)
mesh_abund.pred <- predict(mabundanalysis)

plot(mesh_abund.pred, mesh_abun.res)
abline(0,0)

qqnorm(meshabund$seednum)
qqline(meshabund$seednum, col = 'red')

hist(mesh_abun.res)

anova(mabundanalysis)
summary(mabundanalysis)

lsmeans(mabundanalysis, "canopysp", contr = "pairwise")

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for hial-pema (0.746)
ptukey((0.746*sqrt(2)), nmeans= 4, df=8, lower = F)
#=0.876

#contrast for hial-viko (-0.284)
ptukey(abs(-0.284)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.991

#contrast for hial-vogu (2.247)
ptukey(1.855*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.3174

#contrast for viko-pema (-1.031)
ptukey(abs(-1.031)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.737

#contrast for vogu-pema (1.179)
ptukey(1.179*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.655

#contrast for viko-vogu (2.113)
ptukey(2.113*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.228

###############################
#######Diversity Analysis######
###############################
#load library
library(vegan); library(base); library (stats); library(lme4)
# bring in data
mesh_diversity <- read.csv("mesh_div_analysis.csv")

str(mesh_diversity)
mesh_diversity$block <- as.factor(mesh_diversity$block)
mesh_diversity$trap <- as.factor(mesh_diversity$trap)
str(mesh_diversity)

#1) Richness
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$richness)
boxplot(mesh_diversity$richness~ mesh_diversity$canopysp, main= "Seed species richness per treatment", xlab="Planted Tree Species", ylab="Species Richness")

ggplot(mesh_diversity, aes(block, richness, color=meshtype))+
  geom_boxplot()+
  facet_grid(.~canopysp)+
  ggtitle("Richness for treatments by mesh type")

##Residuals##

mrichanalysis <- glmer(richness~ canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity, family=poisson) #model fails to converge when you add in the individual observation of trap as a random effect.

mesh_rich.res <-  resid(mrichanalysis) 
mesh_rich.pred <- predict(mrichanalysis)
plot(mesh_rich.pred, mesh_rich.res, ylab="Residuals", xlab="Seed Species Richness", main="Mesh Richness Residuals") 
abline(0, 0) 

hist(mesh_rich.pred)

qqnorm(mesh_diversity$richness)
qqline(mesh_diversity$richness, col = 'red')

summary(mrichanalysis)
anova(mrichanalysis)

#OR
plot(mrichanalysis)

#Not appropriate to do a tukey comparison for mesh type because only two.

lsmeans(mrichanalysis, "canopysp", contr= "pairwise")
#contrast to compare between treatmetns
ptukey(3.214*sqrt(2), nmeans= 4, df=3, lower = F)
#=0.17164

#2) Diversity

#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$diversity)
boxplot(mesh_diversity$diversity~ mesh_diversity$canopysp, main= "Seed species diversity per treatment", xlab="canopysp", ylab="species diversity")

ggplot(mesh_diversity, aes(block, diversity, color=meshtype))+
  geom_boxplot()+
  facet_grid(.~canopysp)+
  ggtitle("Diversity by Mesh Type")

##Residuals##

mesh_divanalysis <- lmer(diversity~canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity)
meshdiv.res <-  resid(mesh_divanalysis) 
meshdiv.pred <- predict(mesh_divanalysis)

plot(meshdiv.pred, meshdiv.res)
#plot(mesh_diversity$diversity, meshdiv.res, ylab="Residuals", xlab="Seed Species Diversity", main="Mesh Diversity Residuals") 
abline(0, 0) 


hist(meshdiv.pred)

qqnorm(mesh_diversity$diversity)
qqline(mesh_diversity$diversity, col = 'red')


#OR
plot(mesh_divanalysis)

anova(mesh_divanalysis, test= "F")
summary(mesh_divanalysis)


#try using lm and glmer instead of lmer; R is unhappy when anything besides lmer is used.
#mesh_divanalysis2 <- glmer(diversity~canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity)

lsmeans(mesh_divanalysis, "meshtype", contr= "pairwise")
#this is significant

#3) Evenness

#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$evenness)
boxplot(mesh_diversity$evenness~ mesh_diversity$canopysp, main= "Seed species evenness per treatment", xlab="canopysp", ylab="species evenness")

ggplot(mesh_diversity, aes(block, evenness, color=meshtype))+
  geom_boxplot()+
  facet_grid(.~canopysp)+
  ggtitle("Evenness by Mesh Type")

##Residuals##

mesh_evenanalysis <- lmer(evenness~ canopysp+block+meshtype+meshtype:canopysp+(1|canopysp:block), data = mesh_diversity)
mesheven.res <- resid(mesh_evenanalysis) 
mesheven.pred <- predict(mesh_evenanalysis)
plot(mesheven.pred, mesheven.res)

plot(mesh_diversity$evenness, mesheven.res, ylab="Residuals", xlab="Seed Species Evenness", main="Mesh Evenness Residuals") 
abline(0, 0) 

#OR
plot(mesh_evenanalysis)

anova(mesh_evenanalysis)
summary(mesh_evenanalysis)

lsmeans(mesh_evenanalysis, "meshtype", contr="pairwise")
#no significance found

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


# Using Bray-Curtis dissimilarity index
#Get data
mesh_compdata <- read.csv("mesh_comp_analysis.csv")
ncol(mesh_compdata)
str(mesh_compdata[,123:128])
mesh_compdata$block <-as.factor(mesh_compdata$block)
mesh_compdata$trap <- as.factor(mesh_compdata$trap)
str(mesh_compdata)

mesh_seedcomp <- mesh_compdata[,2:124]
#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
mesh_seedcomp.bc <- vegdist(mesh_seedcomp)
# PERMANOVA (Anderson et al. 2001)
#procD.lm(mesh_seedcomp.bc ~ canopysp+block+meshtype, data = mesh_compdata) #not the correct version
procD.lm(mesh_seedcomp.bc ~ plot+meshtype, data = mesh_compdata) #do they differ btw mesh types

procD.lm(mesh_seedcomp.bc ~ plot+meshtype+meshtype:block+meshtype:canopysp, data = mesh_compdata) #do they differ between the species/trt, accounting for block and canopysp with regards to meshtype

#procD.lm(mesh_seedcomp.bc ~ block*canopysp+meshtype+meshtype:block+meshtype:canopysp, data = mesh_compdata)

#This does a pair-wise comparison of the data
advanced.procD.lm(mesh_seedcomp.bc ~ block*canopysp +meshtype + meshtype:block +meshtype:canopysp, 
    ~ block*canopysp + meshtype +meshtype:block, 
    ~ meshtype:canopysp, data = mesh_compdata) # Four overstory treatments compared with one another to see if there are differences between regular and fine mesh at the treatment level.  Look in the p-values section and constrast meshsmall.TRT to meshreg.TRT.

#NMDS
mesh_seedcomp.mds <- metaMDS(mesh_seedcomp, autotransform = F, expand = F, k = 2, try = 100)
mesh_seedcomp.mds$stress
#Ordination
nmsplot(mesh_seedcomp.mds, mesh_compdata$canopysp, "Hial", "Pema", "Viko", "Vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
#I am not sure what this does.
stressplot(mesh_seedcomp.mds)
