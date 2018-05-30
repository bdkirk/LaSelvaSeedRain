# Seed traits Diversity analyses for both richness and shannon-weiner diversity

# load libraries
library(ggplot2); library(car); library(lsmeans); library(stats);library(multcomp); library(lme4); library(ggResidpanel)

# set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

# bring in data
animaldiv <- read.csv("animal_div_sub_notrtsp_nw.csv", header = TRUE)
animaldiv$block <- as.factor(animaldiv$block)
animaldiv <- animaldiv[complete.cases(animaldiv),]

winddiv <- read.csv("wind_div_sub_notrtsp_nw.csv", header = TRUE)
winddiv$block <- as.factor(winddiv$block)
winddiv <- winddiv[complete.cases(winddiv),]

abioticdiv <- read.csv("abiotic_div_sub_notrtsp_nw.csv", header = TRUE)
abioticdiv$block <- as.factor(abioticdiv$block)

lianadiv <- read.csv("liana_div_sub_notrtsp_nw.csv", header = TRUE)
lianadiv$block <- as.factor(lianadiv$block)
lianadiv <- lianadiv[complete.cases(lianadiv),]

shrubdiv <- read.csv("shrub_div_sub_notrtsp_nw.csv", header = TRUE)
shrubdiv$block <- as.factor(shrubdiv$block)
shrubdiv <- shrubdiv[complete.cases(shrubdiv),]

treediv <- read.csv("tree_div_sub_notrtsp_nw.csv", header = TRUE)
treediv$block <- as.factor(treediv$block)
treediv <- treediv[complete.cases(treediv),]


# Note to self:
### currently, residuals rewrite over one another. Which should be fine as long as you run the code but you may one to change at some point.


########################################################################################################################################################################
########## Dispersal Mode richness analysis ####################
##### Data Exploration for animal dispersal #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(animaldiv$richness)
boxplot(animaldiv$richness~ animaldiv$treatment, main= "Animal dispsersed seed richness across treatments", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(animaldiv, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(animaldiv, table(treatment, richness))
boxplot(animaldiv$richness~animaldiv$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = animaldiv))
vif(glm(richness~ treatment*block, data = animaldiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(animaldiv, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(animaldiv, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(animaldiv, aes(block, richness, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(animaldiv, table(block, treatment))

##########  Analysis   ###############

#1) Does species richness of seeds differ between planted tree species treatments? 

##a) model development
animal_richness<-lm(richness~block+treatment, data=animaldiv)

##b) plot residuals to look for homogeneity

animal_rich_resid <- resid_panel(resid(animal_richness), fitted(animal_richness), bins = 20)

##d) summary of data
summary(animal_richness)
anova(animal_richness, test= "Chi") 

##e) getting p-values
#use for finding z scores for info below
summary(glht(animal_richness, mcp(treatment="Tukey")))

#Either method works, results are the same.
lsmeans(animal_richness, "treatment", contr= "pairwise")
#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)
#IMPORTANT: need to use abs(zscore) for negative values.

#contrast for hial-pema (-3.566)
ptukey((abs(-3.566)*sqrt(2)), nmeans= 4, df=8, lower = F)
#=0.0300

#contrast for hial-viko (-0.172) 
ptukey(abs(-0.172)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.998

#contrast for hial-vogu (-0.211) 
ptukey(abs(-0.211)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.996

#contrast for viko-pema (3.401) 
ptukey(abs(3.401)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.037

#contrast for vogu-pema (3.081)
ptukey(3.081*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.059

#contrast for viko-vogu (-0.056) with K.R. method 0.245
ptukey(abs(-0.056)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.999



##### Data Exploration for wind dispersal #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(winddiv$richness)
boxplot(winddiv$richness~ winddiv$treatment, main= "wind dispersed seed richness across treatments", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(winddiv, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(winddiv, table(treatment, richness))
boxplot(winddiv$richness~winddiv$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = winddiv))
vif(glm(richness~ treatment*block, data = winddiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(winddiv, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(winddiv, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(winddiv, aes(block, richness, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(winddiv, table(block, treatment))

########## Analysis   ###############

#1) Does species richness of seeds differ between planted tree species treatments? 

##a) model development
# data appears to be normally distributed so this is the correct model. Tried with glmer and lm
wind_richness <- lm(richness~block+treatment, data = winddiv)

##b) plot residuals to look for homogeneity
wind_rich_resid <- resid_panel(resid(wind_richness), fitted(wind_richness), bins = 20)
wind_rich_resid

##d) summary of data
summary(wind_richness)
anova(wind_richness, test= "F") 

##e) getting p-values
# found in anova table



##### Data Exploration for abiotic dispersal #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(abioticdiv$richness)
boxplot(abioticdiv$richness~ abioticdiv$treatment, main= "abiotic dispersed seed richness across treatments", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(abioticdiv, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(abioticdiv, table(treatment, richness))
boxplot(abioticdiv$richness~abioticdiv$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = abioticdiv))
vif(glm(richness~ treatment*block, data = abioticdiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(abioticdiv, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(abioticdiv, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(abioticdiv, aes(block, richness, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(abioticdiv, table(block, treatment))

########## Analysis   ###############

#1) Does species richness of seeds differ between overstory tree species? 

##a) model development
#abiotic_richness<-glm(richness~block+treatment, data=abioticdiv) #not really a poisson distribution
abiotic_richness2 <- lm(richness~block+treatment, data=abioticdiv)

##b) plot residuals to look for homogeneity
abiotic_rich_resid <- resid_panel(resid(abiotic_richness2), fitted(abiotic_richness2), bins = 20)
abiotic_rich_resid

##d) summary of data
summary(abiotic_richness2)
anova(abiotic_richness2, test = "F")

##e) getting p-values
#No need to get p-values because differences are small.

########## Life Form richness analysis ####################
##### Data Exploration for liana life form #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(lianadiv$richness)
boxplot(lianadiv$richness~ lianadiv$treatment, main= "liana seed richness across treatments", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(lianadiv, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest liana species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(lianadiv, table(treatment, richness))
boxplot(lianadiv$richness~lianadiv$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = lianadiv))
vif(glm(richness~ treatment*block, data = lianadiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(lianadiv, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(lianadiv, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(lianadiv, aes(block, richness, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(lianadiv, table(block, treatment))

######### Analysis   ###############

#1) Does species richness of seeds differ between planted tree species treatments? 

##a) model development
liana_richness<-lm(richness~block+treatment, data=lianadiv)

##b) plot residuals to look for homogeneity
liana_rich_resid <- resid_panel(resid(liana_richness), fitted(liana_richness), bins = 20)
liana_rich_resid

##c) summary of data
summary(liana_richness)
anova(liana_richness, test= "F") 

##d) getting p-values
# May need to do this later on.

##### Data Exploration for shrub life form #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(shrubdiv$richness)
boxplot(shrubdiv$richness~ shrubdiv$treatment, main= "shrub seed richness across treatments", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(shrubdiv, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest shrub species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(shrubdiv, table(treatment, richness))
boxplot(shrubdiv$richness~shrubdiv$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = shrubdiv))
vif(glm(richness~ treatment*block, data = shrubdiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(shrubdiv, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(shrubdiv, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(shrubdiv, aes(block, richness, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(shrubdiv, table(block, treatment))

######### Analysis   ###############

#1) Does species richness of seeds differ between planted tree species treatments? 

##a) model development
shrub_richness<-lm(richness~block+treatment, data=shrubdiv)

##b) plot residuals to look for homogeneity
shrub_rich_resid <- resid_panel(resid(shrub_richness), fitted(shrub_richness), bins = 20)
shrub_rich_resid

##c) summary of data
summary(shrub_richness)
anova(shrub_richness, test= "F") 

##d) getting p-values
# no reason to get these

##### Data Exploration for tree life form #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(treediv$richness)
boxplot(treediv$richness~ treediv$treatment, main= "tree seed richness across treatments", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(treediv, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest tree species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(treediv, table(treatment, richness))
boxplot(treediv$richness~treediv$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = treediv))
vif(glm(richness~ treatment*block, data = treediv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(treediv, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(treediv, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(treediv, aes(block, richness, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(treediv, table(block, treatment))


########## Analysis   ###############

#1) Does species richness of seeds differ between planted tree species treatments?

##a) model development
#tree_richness<-glmer(richness~block+treatment+(1|plot), family = poisson, data=treediv) # Not needed because histogram showed normal data.

tree_rich <- lm(richness~block+treatment, data = treediv)

##b) plot residuals to look for homogeneity
tree_rich_resid <- resid_panel(resid(tree_rich), fitted(tree_rich), bins = 20)
tree_rich_resid

##c) summary of data
summary(tree_rich)
anova(tree_rich, test= "F") 

##d) getting p-values
# found in other summaries because this is just an lm test.


########################################################################################################################################################################

####### Dispersal Mode Diversity Analysis ################

##### Animal dispersed Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(animaldiv$diversity)
boxplot(animaldiv$diversity~ animaldiv$treatment, main= "Species Diversity per treatment", xlab="treatment", ylab="species diversity")

# pema has lower diversity

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(animaldiv,aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(animaldiv, table(treatment, diversity))
boxplot(animaldiv$diversity~animaldiv$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = animaldiv))
#vif(glm(diversity~ treatment*block, data = animaldiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(animaldiv, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(animaldiv, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(animaldiv, aes(block, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

ggplot(animaldiv, aes(block, diversity, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)

with(animaldiv, ftable(treatment, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(animaldiv, table(block, treatment))
###########  Analysis   ###############

#1) Does diversity of animal dispersed seed species differ between planted tree species treatments? 

##a) model development
animal_diversity<-lm(diversity~block+treatment, data=animaldiv)

##b)plot residuals to look at homogeneity
animal_div_resid <- resid_panel(resid(animal_diversity), fitted(animal_diversity), bins = 20)
animal_div_resid

##d) summary of analysis
summary(animal_diversity)
anova(animal_diversity, test = "F") #should not use if unbalanced

##e) look for p-values
# no significant difference

##### Wind dispersed Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(winddiv$diversity)
boxplot(winddiv$diversity~ winddiv$treatment, main= "Wind dispersal seed diversity per treatment", xlab="treatment", ylab="species diversity")

#vogu and hial have greatest diversity 

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(winddiv, aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(winddiv, table(treatment, diversity))
boxplot(winddiv$diversity~winddiv$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = winddiv))
#vif(glm(diversity~ treatment*block, data = winddiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(winddiv, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(winddiv, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(winddiv, aes(block, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

ggplot(winddiv, aes(block, diversity, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

##########  Analysis   ###############
#1) Does diversity of wind dispersed seed species differ between planted treatments? 

##a) model development
wind_diversity<-lm(diversity~block+treatment, data=winddiv)

##b)plot residuals to look at homogeneity
wind_div_resid <- resid_panel(resid(wind_diversity), fitted(wind_diversity), bins = 20)
wind_div_resid

##d) summary of analysis
summary(wind_diversity)
anova(wind_diversity, test = "F") #should not use if unbalanced

##e) look for p-values
#not needed, found above


##### Abiotic Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(abioticdiv$diversity)
boxplot(abioticdiv$diversity~ abioticdiv$treatment, main= "abiotic dispersal seed diversity per treatment", xlab="treatment", ylab="species diversity")

#vogu and hial have greatest diversity 

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(abioticdiv, aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")

#Viko is very different than the rest

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(abioticdiv, table(treatment, diversity))
boxplot(abioticdiv$diversity~abioticdiv$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = abioticdiv))
#vif(glm(diversity~ treatment*block, data = abioticdiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(abioticdiv, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(abioticdiv, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(abioticdiv, aes(block, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

ggplot(abioticdiv, aes(block, diversity, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

##########  Analysis   ###############
#1) Does diversity of wind dispersed seed species differ between planted treatments? 

##a) model development
abiotic_diversity<-lm(diversity~block+treatment, data=abioticdiv)

##b)plot residuals to look at homogeneity
abiotic_div_resid <- resid_panel(resid(abiotic_diversity), fitted(abiotic_diversity), bins = 20)
abiotic_div_resid

##d) summary of analysis
summary(abiotic_diversity)
anova(abiotic_diversity, test = "F") 

##e) look for p-values
#not needed, found above



##### Life Form Diversity Analysis ##########

##### Liana Diversity Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(lianadiv$diversity)
boxplot(lianadiv$diversity~ lianadiv$treatment, main= "Liana species diversity per treatment", xlab="treatment", ylab="species diversity")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(lianadiv, aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")
#appears like pema has greater liana diversity

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(lianadiv, table(treatment, diversity))
boxplot(lianadiv$diversity~lianadiv$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = lianadiv))
#vif(glm(diversity~ treatment*block, data = lianadiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(lianadiv, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(lianadiv, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(lianadiv, aes(block, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

ggplot(lianadiv, aes(block, diversity, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)


#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

##########  Analysis   ###############

#1) Does diversity of liana species differ between planted tree species treatments? 

##a) model development
liana_diversity<-lm(diversity~block+treatment, data=lianadiv)

##b)plot residuals to look at homogeneity
liana_div_resid <- resid_panel(resid(liana_diversity), fitted(liana_diversity), bins = 20)
liana_div_resid

##c) summary of analysis
summary(liana_diversity)
anova(liana_diversity, test = "F") #should not use if unbalanced

##d) look for p-values
# no need.


##### Shrub diversity Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(shrubdiv$diversity)
boxplot(shrubdiv$diversity~ shrubdiv$treatment, main= "Species Diversity per treatment", xlab="treatment", ylab="species diversity")

#Hial has a very narrow range of diversity for shrubs, vogu has great differences across blocks.


#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(shrubdiv, aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(shrubdiv, table(treatment, diversity))
boxplot(shrubdiv$diversity~shrubdiv$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = shrubdiv))
#vif(glm(diversity~ treatment*block, data = shrubdiv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(shrubdiv, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(shrubdiv, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(shrubdiv, aes(block, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

ggplot(shrubdiv, aes(block, diversity, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

######### Analysis   ###############

#1) Does diversity of shrub seed species differ between planted tree species treatments? 

##a) model development
shrub_diversity<-lm(diversity~block+treatment, data=shrubdiv)

##b)plot residuals to look at homogeneity
shrub_div_resid <- resid_panel(resid(shrub_diversity), fitted(shrub_diversity), bins = 20)
shrub_div_resid


##d) summary of analysis
summary(shrub_diversity)
anova(shrub_diversity, test = "F") #should not use if unbalanced

##e) look for p-values
# no need

##### Tree Diversity Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(treediv$diversity)
boxplot(treediv$diversity~ treediv$treatment, main= "Species Diversity per treatment", xlab="treatment", ylab="species diversity")

#pema has greatest variance


#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(treediv, aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(treediv, table(treatment, diversity))
boxplot(treediv$diversity~treediv$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = treediv))
#vif(glm(diversity~ treatment*block, data = treediv))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(treediv, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(treediv, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(treediv, aes(block, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

ggplot(treediv, aes(block, diversity, color=treatment))+
  geom_bar(stat="identity")+
  facet_grid(.~treatment)

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

########## Analysis   ###############

#1) Does diversity of tree species seeds differ between planted tree species treatments? 

##a) model development
tree_diversity<-lm(diversity~block+treatment, data=treediv)

##b)plot residuals to look at homogeneity
tree_div_resid <- resid_panel(resid(tree_diversity), fitted(tree_diversity), bins = 20)
tree_div_resid

##d) summary of analysis
summary(tree_diversity)
anova(tree_diversity, test = "F") #should not use if unbalanced

##e) look for p-values

###Found no difference on 31 May between using glm or lm for shannon wiener diversity. lm is simpler so I will use this.  Talked with Dr. Dixon on 30 June and glm will revert to lm if the family is Guassian.

lsmeans(tree_diversity, "treatment", contr= "pairwise")
#These p-values are not significant but wanted to see how close PEMA was to significance

