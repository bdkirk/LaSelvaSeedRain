# Started on 17 May 2017
# This will include several different analyses for abundance, diversity, composition and phenology


#####Abundance Analysis#####

#Libraries
library(dplyr); library(plyr); library(stats); library(lme4); library(readr); library(ggplot2); library(lsmeans); library(ggResidpanel); library(multcomp); library(emmeans)

#Bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

meshabund <- read.csv("Mesh_abund_analysis.csv", header = TRUE)

str(meshabund)
meshabund$trap <- as.factor(meshabund$trap)
meshabund$block <- as.factor(meshabund$block)
str(meshabund)

ggplot(meshabund, aes(block, seednum, color =meshtype))+
  geom_boxplot()+
  facet_grid(.~treatment)+
  ggtitle("Treatment abundance by mesh type")

hist(meshabund$seednum)
boxplot(meshabund$seednum~ meshabund$meshtype, xlab="Meshtype", ylab="seednum", main= "Seed abundance per treatment")

#model
mabundanalysis <- glmer(seednum~ treatment+block+meshtype+meshtype:treatment+(1|treatment:block)+ (1|trap), family= "poisson", data = meshabund)

#residuals
resid_panel(resid(mabundanalysis), fitted(mabundanalysis), bins=25)

anova(mabundanalysis, test = "F")
summary(mabundanalysis)

# Does mesh type bias abundance of seed rain?
lsmeans(mabundanalysis, "meshtype", contr = "pairwise") # This does not give accurate estimates

#Get z-value from lsmeans and then convert with t-test to lsmeans. z-values are like t-values with inf df
2*pt(q= -3.436, df= 62, lower=TRUE)
# Yes there is a significant difference in abundance based on mesh type P= 0.001 (t-test)

#library(emmeans) #only un# if want to run emmeans. This will overwrite lme4
# use emmip to look at interactions

#emmip(mabundanalysis, treatment~meshtype)# this shows that there is an interaction. 

#emmeans(mabundanalysis, contr~meshtype) #gives the same results.

# Question- is there a mesh-treatment interaction?

#pf(q=f statistic, df1= treatment df, df2= error df, lower.tail = false if testing against a null hypothesis)
pf(1.7268, df1=3, df2=62, lower.tail= FALSE) # Use f-value for interaction from model anova and df for corresponding variable
# p = 0.171, No there is not a significant treatment by mesh interaction.


###############################
#######Diversity Analysis######
###############################

#load library
library(vegan); library(base); library (stats); library(lme4)
# bring in data
mesh_diversity <- read.csv("mesh_div_analysis.csv", header = TRUE)

str(mesh_diversity)
mesh_diversity$block <- as.factor(mesh_diversity$block)
mesh_diversity$trap <- as.factor(mesh_diversity$trap)
str(mesh_diversity)

                            #1) Richness
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$richness)
boxplot(mesh_diversity$richness~ mesh_diversity$treatment, main= "Seed species richness per treatment", xlab="Planted Tree Species", ylab="Species Richness")

ggplot(mesh_diversity, aes(block, richness, color=meshtype))+
  geom_boxplot()+
  facet_grid(.~treatment)+
  ggtitle("Richness for treatments by mesh type")


#model
#changed to a lmer model and removed poisson distribution because residuals are normally distributed
mrichanalysis <- lmer(richness~ treatment+block+meshtype+meshtype:treatment+(1|treatment:block), data = mesh_diversity) #model fails to converge when you add in the individual observation of trap as a random effect as it should because that row-wise observation level random effect is not needed.

##Residuals##
resid_panel(resid(mrichanalysis), fitted(mrichanalysis), bins = 25 )

summary(mrichanalysis)
anova(mrichanalysis)

#OR
#plot(mrichanalysis)

# Question- does species richness differ between mesh types?

lsmeans(mrichanalysis, "meshtype", contr= "pairwise") #YES!
# p< 0.0001

# Question- is there a mesh*treatment interaction? 
pf(q=7.7121, df1= 3, df2= 56, lower.tail = FALSE)
# p= 0.0002, yes there is

# Yes but what is it?? How do I determine pairwise differences?
lsmeans(mrichanalysis, "meshtype", contr= "pairwise", by = "treatment")
# all are statistically significant

# Apriori hypothesis testing??
# add a component in data frame for animal and abiotically dispersed then run test:
#lsmeans(mrichanalysis, "meshtype", contr= "pairwise", by = "dispersal")
# this didn't work because you need to add dispersal into the model..not sure where it would belong.


                                 #2) Diversity

#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$diversity)
boxplot(mesh_diversity$diversity~ mesh_diversity$treatment, main= "Seed species diversity per treatment", xlab="treatment", ylab="species diversity")

ggplot(mesh_diversity, aes(block, diversity, color=meshtype))+
  geom_boxplot()+
  facet_grid(.~treatment)+
  ggtitle("Diversity by Mesh Type")


# model
mesh_divanalysis <- lmer(diversity~treatment+block+meshtype+meshtype:treatment+(1|treatment:block), data = mesh_diversity)
#try using lm and glmer instead of lmer; R is unhappy when anything besides lmer is used.
#mesh_divanalysis2 <- glmer(diversity~treatment+block+meshtype+meshtype:treatment+(1|treatment:block), data = mesh_diversity)

##Residuals##
resid_panel(resid(mesh_divanalysis), fitted(mesh_divanalysis), bins=25)


anova(mesh_divanalysis, test= "F")
summary(mesh_divanalysis)

# Does diversity vary in each mesh type?
lsmeans(mesh_divanalysis, "meshtype", contr= "pairwise")
#this is significant, p< 0.0001

# Question- interaction between mesh type and treatment
pf(q=0.5829, df1= 3, df2= 56, lower.tail = FALSE) # p = 0.628
# no.

#3) Evenness

#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(mesh_diversity$evenness)
boxplot(mesh_diversity$evenness~ mesh_diversity$treatment, main= "Seed species evenness per treatment", xlab="treatment", ylab="species evenness")

ggplot(mesh_diversity, aes(block, evenness, color=meshtype))+
  geom_boxplot()+
  facet_grid(.~treatment)+
  ggtitle("Evenness by Mesh Type")


# model
mesh_evenanalysis <- lmer(evenness~ treatment+block+meshtype+meshtype:treatment+(1|treatment:block), data = mesh_diversity)


##Residuals##
resid_panel(resid(mesh_evenanalysis), fitted(mesh_evenanalysis), bins = 25)

anova(mesh_evenanalysis)
summary(mesh_evenanalysis)

lsmeans(mesh_evenanalysis, "meshtype", contr="pairwise")
#no significance found, p= 0.4538, 

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
str(mesh_compdata[,121:126])
mesh_compdata$block <-as.factor(mesh_compdata$block)
mesh_compdata$trap <- as.factor(mesh_compdata$trap)
str(mesh_compdata)

mesh_seedcomp <- mesh_compdata[,2:122]
#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
mesh_seedcomp.bc <- vegdist(mesh_seedcomp)
# PERMANOVA (Anderson et al. 2001)
#procD.lm(mesh_seedcomp.bc ~ treatment+block+meshtype, data = mesh_compdata) #not the correct version
procD.lm(mesh_seedcomp.bc ~ plot+meshtype, data = mesh_compdata) #do they differ btw mesh types

procD.lm(mesh_seedcomp.bc ~ plot+meshtype+meshtype:block+meshtype:treatment, data = mesh_compdata) #do they differ between the species/trt, accounting for block and treatment with regards to meshtype

#procD.lm(mesh_seedcomp.bc ~ block*treatment+meshtype+meshtype:block+meshtype:treatment, data = mesh_compdata)

#This does a pair-wise comparison of the data
advanced.procD.lm(f1= mesh_seedcomp.bc ~ block*treatment +meshtype + meshtype:block +meshtype:treatment, 
    f2= ~ block*treatment + meshtype +meshtype:block, 
    group= ~ meshtype:treatment, data = mesh_compdata) # Four overstory treatments compared with one another to see if there are differences between regular and fine mesh at the treatment level.  Look in the p-values section and constrast meshsmall.TRT to meshreg.TRT.

#NMDS
mesh_seedcomp.mds <- metaMDS(mesh_seedcomp, autotransform = F, expand = F, k = 2, try = 100)
mesh_seedcomp.mds$stress

#Ordination
nmsplot(mesh_seedcomp.mds, mesh_compdata$treatment, "Hial", "Pema", "Viko", "Vogu",
        "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
#I am not sure what this does.
stressplot(mesh_seedcomp.mds)

# The below code was added on 29 Aug after meeting with Katie Rey, stat help.  She helped modify the code but the small and regular mesh sizes do not show in the legend.  Need to work on this.
library(car)

#modifying composition code
##  ---------------------------------------------------
# Function for Plotting NMS results (Nick Lyon's code)
##  ---------------------------------------------------

# Function that plots NMS points with different colors for groups (only works for 4 groups)
## mod = object returned by metaMDS
## groupcol = group column (duh)
## g1 - 4 = grouping variables as written in dataframe
## legpos = legend position, either numeric vector of x/y coords or shorthand accepted by "legend" function
## legcont = legend content, vector of labels for legend

nmsplot_new <- function(mod, groupcol1= "mesh_compdata$treatment", groupcol2= "mesh_compdata$meshtype", g1, g2, g3, g4, g5, g6, legpos = "topright", legcont, ellipse_option, ellipse_legend, ellipse_labels) {
  # Create plot
  plot(mod, display = 'sites', choice = c(1, 2), type = 'none')
  
  #add colors
  col1 <- "#7b3294" # Hial
  col2 <- "#008837" # Pema
  col3 <- "#c2a5cf" # Viko
  col4 <- "#fdae61" # Vogu
  col5 <- "#d7191c" # fine mesh
  col6 <- "#2c7bb6" # regular mesh

  # Add points for each treatment with a different color per treatment
  points(mod$points[groupcol1 == g1, 1], mod$points[groupcol1 == g1, 2], bg = col1)
  points(mod$points[groupcol1 == g2, 1], mod$points[groupcol1 == g2, 2], bg = col2)
  points(mod$points[groupcol1 == g3, 1], mod$points[groupcol1 == g3, 2], bg = col3)
  points(mod$points[groupcol1 == g4, 1], mod$points[groupcol1 == g4, 2], bg = col4)
  
  # Add shapes for points for each meshtype for each treatment
  points(mod$points[groupcol2 == g5, 1], mod$points[groupcol2 == g5, 2], pch = 16)
  points(mod$points[groupcol2 == g6, 1], mod$points[groupcol2 == g6, 2], pch = 17)
  
    # Ordinate SD ellipses around the centroid
  ordiellipse(mod, groupcol2, col = c(col5, col6), display = "sites", kind = "sd", label = F)
  
  # Add legend
  legend(legpos, legend = legcont, pch = c(16, 17), pt.bg = c(col1, col2, col3, col4), cex = .75)
  
  legend(ellipse_legend, legend = ellipse_labels, col= c(col5, col6), cex = 1)
  
}


nmsplot_new(mesh_seedcomp.mds, mesh_compdata$treatment, g1= "Hial", g2 = "Pema", g3 = "Viko", g4= "Vogu", mesh_compdata$meshtype, g5="meshsmall", g6= "meshregular", c("Hieronyma", "Pentaclethra", "Virola", "Vochysia", "Fine Mesh", "Regular Mesh"))


#This tests whether the ellipses are working off the right data by showing the mesh types for each trap rather than the treatments.
nmsplot_new(mesh_seedcomp.mds, mesh_compdata$meshtype, "meshsmall", "meshreg", "topright", c("Small", "Regular"), mesh_compdata$meshtype, "bottomright", c("Small", "Regular"))

############# Try plotting this all in ggplot ######################
library(ggplot2); library(dplyr)
#extract points from NMDS
nmds_points <- as.data.frame(mesh_seedcomp.mds[["points"]])

#get attribute data in two columns
attrib <- select(mesh_compdata, treatment, meshtype)

#bind columns together
nmds_points2 <- cbind(nmds_points, attrib) # this is a base function
#nmds_points3 <- bind_cols(nmds_points, attrib) # this workds too

ggplot(nmds_points2, aes(MDS1, MDS2, color= treatment, shape =meshtype))+
  geom_point(size= 3)+
  #scale_fill_brewer(palette = "Dark2")+ # not working
  stat_ellipse(aes(MDS1, MDS2, color = meshtype, type ="norm"))+
  theme_bw()


##Need to continue working on making small and regular mesh appear in figure as well as moving to Jacard or Sorenson

############ Presence/Absense #############

# must have run previous code for this to work properly
mesh_seedcomp_j <- mesh_compdata[,2:122]

# need file with binary data:
binary <- decostand(mesh_seedcomp_j, method = "pa")


#name new object and do the vegdist (part of vegan), this computes dissimilarity indices to use in the PERMANOVA
mesh_seedcomp.jc <- vegdist(mesh_seedcomp_j, method = "jaccard", binary = TRUE)
# PERMANOVA (Anderson et al. 2001)
#procD.lm(mesh_seedcomp.bc ~ treatment+block+meshtype, data = mesh_compdata) #not the correct version
procD.lm(mesh_seedcomp.jc ~ plot+meshtype, data = mesh_compdata) #do they differ btw mesh types
# Yes. p= 0.001, f=18.8, Rsq=0.177

procD.lm(mesh_seedcomp.jc ~ plot+meshtype+meshtype:block+meshtype:treatment, data = mesh_compdata) #do they differ between the species/trt, accounting for block and treatment with regards to meshtype ; this is answering the question of whether or not there is an interaction. and yes there is p= 0.001, f=1.2, Rsq= 0.034

#procD.lm(mesh_seedcomp.jc ~ block*treatment+meshtype+meshtype:block+meshtype:treatment, data = mesh_compdata)

#This does a pair-wise comparison of the data
advanced.procD.lm(mesh_seedcomp.jc ~ block*treatment +meshtype + meshtype:block +meshtype:treatment, 
                  ~ block*treatment + meshtype +meshtype:block, 
                  ~ meshtype:treatment, data = mesh_compdata) 

#NMDS
mesh_seedcompj.mds <- metaMDS(binary, autotransform = F, expand = F, k = 2, try = 100)
mesh_seedcomp.mds$stress
#Ordination
nmsplot(mesh_seedcompj.mds, mesh_compdata$treatment, "Hial", "Pema", "Viko", "Vogu", "topright", c("HIAL", "PEMA", "VIKO", "VOGU"))
#I am not sure what this does.
stressplot(mesh_seedcomp.mds)
#nmsplot(mesh_seedcompj.mds, mesh_compdata$meshtype, "meshsmall", "meshregular", "topright", c("Fine Mesh", "Regular Mesh"))

### Using github technique to plot:


#extract points from NMDS
jc_points <- as.data.frame(mesh_seedcompj.mds[["points"]])

#get attribute data in two columns
attrib <- select(mesh_compdata, treatment, meshtype)

#bind columns together
jc_points2 <- cbind(jc_points, attrib) # this is a base function
#mds_points3 <- bind_cols(mds_points, attrib) # this workds too

ggplot(jc_points2, aes(MDS1, MDS2, color= treatment, shape =meshtype))+
  geom_point(size= 3)+
  #scale_fill_brewer(palette = "Dark2")+ # not working
  stat_ellipse(aes(MDS1, MDS2, color = meshtype, type ="norm"))+
  theme_bw()
# Holy smokes, those are some groups!