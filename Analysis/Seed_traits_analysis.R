# Seed traits analysis started 2-11-18


# this will be a characterization of the dispersal mode and life form after talking with Ann on 2/12/18.
# The analysis will be percentages and graphical representations.
# this changed again after talk with Haldre on 2/13/18.  Will subset data and see how it differs across dispersal mode and life form across treatments
# because this file is so large, will only include analysis of abundance for dispersal mode and life form, other files will include richness, diversity and comp.

#set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#load libraries
library(ggplot2); library(car); library(lsmeans); library(stats); library(lme4); library(dplyr); library(readr); library(multcomp)

#read in files
wind_a_analysis <- read.csv("wind_abund_tidy.csv", header = TRUE)
str(wind_a_analysis)
#removes NA row
wind_a_analysis$block <- as.factor(wind_a_analysis$block)

wind_a_analysis <- wind_a_analysis[complete.cases(wind_a_analysis),]

animal_a_analysis <- read.csv("animal_abund_tidy.csv", header = TRUE)
animal_a_analysis$block <- as.factor(animal_a_analysis$block)
str(animal_a_analysis)
#removes NA row
animal_a_analysis <- animal_a_analysis[complete.cases(animal_a_analysis),]


mech_a_analysis <- read.csv("mech_abund_tidy.csv", header = TRUE)
mech_a_analysis$block <- as.factor(mech_a_analysis$block)
str(mech_a_analysis) #check to see if change was incorporated
#removes NA row
mech_a_analysis <- mech_a_analysis[complete.cases(mech_a_analysis),]

################## Wind Abundance Analysis #########################
########### Data Exploration for specific analysis#########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(wind_a_analysis$seednum)
boxplot(wind_a_analysis$seednum~ wind_a_analysis$treatment, xlab="treatment", ylab="seednum", main= "Seed abundance per treatment")

#Much more variance in viko
#one outlier in Y, hial 2 but will keep, X is categorical

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use plot? to make sure we have all combinations)
with(wind_a_analysis, plot(treatment, seednum))

##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(seednum~ treatment +block, data = wind_a_analysis))

# model shows interactions when include the treatment*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(wind_a_analysis, aes(block, seednum))+
  geom_bar(stat = "identity", aes(fill=treatment))
#more variance with Hial, minimal variance with vogu. pema and viko have a similar variance.

ggplot(wind_a_analysis, aes(treatment, seednum))+
  geom_bar(stat = "identity", aes(fill=block))

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(wind_a_analysis, aes(treatment, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(wind_a_analysis, aes(block, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(wind_a_analysis, table(block, treatment))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
#library(dplyr); library(ggplot2)
#Find outliers
ggplot(wind_a_analysis, aes(treatment, seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

wind_a_analysis %>% 
  filter(!is.na(seednum)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(wind_a_analysis, aes(block, seednum))+
  geom_boxplot(aes(fill=block))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")
#summary
# block 2 and Hial have more variability than other block and canopsysp treatments
# can go ahead looking at total seed abundance relative to block and treatment. 


#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)
#removed above
# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of wind dispersed seeds differ between planted tree species treatments? 

##a)changed model to glmer because this will account for overdispersion with the poisson distribution.
wind.abund.glm <- glmer(seednum ~ block+treatment+(1|plot), data=wind_a_analysis, family = poisson)

##b)plot residuals to look at homogeneity
wind.abund.res <- resid(wind.abund.glm) #pearson or deviance?
wind.abund.pred <- predict(wind.abund.glm)
# abund.pred_count <- exp(predict(abund.glm))
# r_test <- wind_a_analysis$total_seednum-abund.pred_count
# plot(r_test, abund.res)

plot(wind.abund.pred, wind.abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
qqnorm(wind.abund.res)
qqline(wind.abund.res, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(wind.abund.res)
#just odd, perhaps skewed right

##d) summary of data
anova(wind.abund.glm, test= "F")
summary(wind.abund.glm)

##e) getting p-values
#find Tukeys (HSD) and use for this data

#use for finding z scores for info below
summary(glht(wind.abund.glm, mcp(treatment="Tukey")))

#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for pema-hial (-0.748)
ptukey(abs(-0.748)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.875

#contrast for viko-hial (-2.472)
ptukey(abs(-2.472)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.139

#contrast for vogu-hial (0.568)
ptukey(0.568*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.9388

#contrast for viko-pema (1.705)
ptukey(1.705*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.381

#contrast for vogu-pema (0.127)
ptukey(0.127*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.999

#contrast for vogu-viko (-1.668)
ptukey(1.668*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.3976




################## Animal Abundance Analysis #########################
########### Data Exploration for specific analysis#########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(animal_a_analysis$seednum)
boxplot(animal_a_analysis$seednum~ animal_a_analysis$treatment, xlab="treatment", ylab="seednum", main= "Animal seed dispersed abundance per treatment")

#Much more variance in hial, relatively none in vogu.

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use plot? to make sure we have all combinations)
with(animal_a_analysis, plot(treatment, seednum))

##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(seednum~ treatment +block, data = animal_a_analysis))

# model shows interactions when include the treatment*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(animal_a_analysis, aes(block, seednum))+
  geom_bar(stat = "identity", aes(fill=treatment))
# higher abundance of seeds in animal dispersed block 2 for hial and viko. extremely low for pema in block 2 but higher in other blocks

ggplot(animal_a_analysis, aes(treatment, seednum))+
  geom_bar(stat = "identity", aes(fill=block))
# hial has as much seeds in block two that were animal dispersed as the total number of seeds found in other blocks

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(animal_a_analysis, aes(treatment, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(animal_a_analysis, aes(block, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(animal_a_analysis, table(block, treatment))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
#library(dplyr); library(ggplot2)
#Find outliers
ggplot(animal_a_analysis, aes(treatment, seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

animal_a_analysis %>% 
  filter(!is.na(seednum)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(animal_a_analysis, aes(block, seednum))+
  geom_boxplot(aes(fill=block))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")
#summary
# block 2 and Hial have more variability than other block and canopsysp treatments
# can go ahead looking at total seed abundance relative to block and treatment. 


#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)
#removed above
# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of animal dispersed seeds differ between planted tree species treatments? 

##a)changed model to glmer because this will account for overdispersion with the poisson distribution.
animal.abund.glm <- glmer(seednum ~ block+treatment+(1|plot), data=animal_a_analysis, family = poisson)

##b)plot residuals to look at homogeneity
animal.abund.res <- resid(animal.abund.glm) #pearson or deviance?
animal.abund.pred <- predict(animal.abund.glm)
# abund.pred_count <- exp(predict(abund.glm))
# r_test <- animal_a_analysis$total_seednum-abund.pred_count
# plot(r_test, abund.res)

plot(animal.abund.pred, animal.abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
qqnorm(animal.abund.res)
qqline(animal.abund.res, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(animal.abund.res)
#just odd, perhaps skewed right

##d) summary of data
anova(animal.abund.glm, test= "F")
summary(animal.abund.glm)

##e) getting p-values
#find Tukeys (HSD) and use for this data

#use for finding z scores for info below
summary(glht(animal.abund.glm, mcp(treatment="Tukey")))

#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for pema-hial (0.314)
ptukey(abs(0.314)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.988

#contrast for viko-hial (0.008)
ptukey(abs(0.008)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.999

#contrast for vogu-hial (-2.239)
ptukey(abs(-2.239)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.192

#contrast for viko-pema (-0.307)
ptukey(abs(-0.307)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.989

#contrast for vogu-pema (-2.523)
ptukey(abs(-2.523)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.130

#contrast for vogu-viko (-2.246)
ptukey(abs(-2.246)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.191


############ Mechanical- not enough for actual analysis #######


hist(mech_a_analysis$seednum)
boxplot(mech_a_analysis$seednum~ mech_a_analysis$treatment, xlab="treatment", ylab="seednum", main= "Mechanical seed dispersed abundance per treatment")

ggplot(mech_a_analysis, aes(block, seednum))+
  geom_bar(stat = "identity", aes(fill=treatment))
# higher abundance of seeds in animal dispersed block 2 for hial and viko. extremely low for pema in block 2 but higher in other blocks

ggplot(mech_a_analysis, aes(treatment, seednum))+
  geom_bar(stat = "identity", aes(fill=block))
# hial has as much seeds in block two that were animal dispersed as the total number of seeds found in other blocks

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(mech_a_analysis, aes(treatment, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(mech_a_analysis, aes(block, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)




############################################################################################################################################################################################################################################################

##### Life form abundance analysis ##############

# read in files
liana_a_analysis <- read.csv("liana_abund_tidy.csv", header= TRUE)
liana_a_analysis <- liana_a_analysis[complete.cases(liana_a_analysis),]
liana_a_analysis$block <- as.factor(liana_a_analysis$block)

tree_a_analysis <- read.csv("tree_abund_tidy.csv", header= TRUE)
tree_a_analysis <- tree_a_analysis[complete.cases(tree_a_analysis),]
tree_a_analysis$block <- as.factor(tree_a_analysis$block)

shrub_a_analysis <- read.csv("shrub_abund_tidy.csv", header= TRUE)
shrub_a_analysis <- shrub_a_analysis[complete.cases(shrub_a_analysis),]
shrub_a_analysis$block <- as.factor(shrub_a_analysis$block)

palm_a_analysis <- read.csv("palm_abund_tidy.csv", header= TRUE)
palm_a_analysis <- palm_a_analysis[complete.cases(palm_a_analysis),]
palm_a_analysis$block <- as.factor(palm_a_analysis$block)

### 1) Liana
################## Liana Analysis #########################
########### Data Exploration for specific analysis#########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(liana_a_analysis$seednum)
boxplot(liana_a_analysis$seednum~ liana_a_analysis$treatment, xlab="treatment", ylab="seednum", main= "liana life form abundance per treatment")

#mean of pema lower than hial

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use plot? to make sure we have all combinations)
with(liana_a_analysis, plot(treatment, seednum))

##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(seednum~ treatment +block, data = liana_a_analysis))

# model shows interactions when include the treatment*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(liana_a_analysis, aes(block, seednum))+
  geom_bar(stat = "identity", aes(fill=treatment))
# higher abundance of seeds in liana dispersed block 2 for hial and viko. extremely low for pema in block 2 but higher in other blocks

ggplot(liana_a_analysis, aes(treatment, seednum))+
  geom_bar(stat = "identity", aes(fill=block))
# hial has as much seeds in block two that were liana dispersed as the total number of seeds found in other blocks

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(liana_a_analysis, aes(treatment, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(liana_a_analysis, aes(block, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(liana_a_analysis, table(block, treatment))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
#library(dplyr); library(ggplot2)
#Find outliers
ggplot(liana_a_analysis, aes(treatment, seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

liana_a_analysis %>% 
  filter(!is.na(seednum)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(liana_a_analysis, aes(block, seednum))+
  geom_boxplot(aes(fill=block))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")
#summary
# block 2 and Hial have more variability than other block and canopsysp treatments
# can go ahead looking at total seed abundance relative to block and treatment. 


#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)
#removed above
# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of liana dispersed seeds differ between planted tree species treatments? 

##a)changed model to glmer because this will account for overdispersion with the poisson distribution.
liana.abund.glm <- glmer(seednum ~ block+treatment+(1|plot), data=liana_a_analysis, family = poisson)

##b)plot residuals to look at homogeneity
liana.abund.res <- resid(liana.abund.glm) #pearson or deviance?
liana.abund.pred <- predict(liana.abund.glm)
# abund.pred_count <- exp(predict(abund.glm))
# r_test <- liana_a_analysis$total_seednum-abund.pred_count
# plot(r_test, abund.res)

plot(liana.abund.pred, liana.abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
qqnorm(liana.abund.res)
qqline(liana.abund.res, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(liana.abund.res)
#skewed left

##d) summary of data
anova(liana.abund.glm, test= "F")
summary(liana.abund.glm)

##e) getting p-values
#find Tukeys (HSD) and use for this data

#use for finding z scores for info below
summary(glht(liana.abund.glm, mcp(treatment="Tukey")))

#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for pema-hial (-2.972)
ptukey(abs(-2.972)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.0691

#contrast for viko-hial (-1.285)
ptukey(abs(-1.285)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.596

#contrast for vogu-hial (-0.822)
ptukey(abs(-0.822)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.843

#contrast for viko-pema (1.695)
ptukey(abs(1.695)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.385

#contrast for vogu-pema (1.878)
ptukey(abs(1.878)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.308

#contrast for vogu-viko (0.341)
ptukey(abs(0.341)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.985


### 2) Shrub

################## Shrub Abundance Analysis #########################
########### Data Exploration for specific analysis#########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(shrub_a_analysis$seednum)
boxplot(shrub_a_analysis$seednum~ shrub_a_analysis$treatment, xlab="treatment", ylab="seednum", main= "shrub life form abundance per treatment")

#hial large

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use plot? to make sure we have all combinations)
with(shrub_a_analysis, plot(treatment, seednum))

##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(seednum~ treatment +block, data = shrub_a_analysis))

# model shows interactions when include the treatment*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(shrub_a_analysis, aes(block, seednum))+
  geom_bar(stat = "identity", aes(fill=treatment))
# higher abundance of seeds in shrub dispersed block 2 for hial and viko. extremely low for pema in block 2 but higher in other blocks

ggplot(shrub_a_analysis, aes(treatment, seednum))+
  geom_bar(stat = "identity", aes(fill=block))
# hial is the only plot to have such a large number of shrubs but in other plots, the nubmers are relatively low.

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(shrub_a_analysis, aes(treatment, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(shrub_a_analysis, aes(block, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(shrub_a_analysis, table(block, treatment))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
#library(dplyr); library(ggplot2)
#Find outliers
ggplot(shrub_a_analysis, aes(treatment, seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

shrub_a_analysis %>% 
  filter(!is.na(seednum)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(shrub_a_analysis, aes(block, seednum))+
  geom_boxplot(aes(fill=block))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")
#summary
# block 2 and Hial have more variability than other block and canopsysp treatments
# can go ahead looking at total seed abundance relative to block and treatment. 


#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)
#removed above
# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of shrub life form seeds differ between planted tree species treatments? 

##a)changed model to glmer because this will account for overdispersion with the poisson distribution.
shrub.abund.glm <- glmer(seednum ~ block+treatment+(1|plot), data=shrub_a_analysis, family = poisson)

##b)plot residuals to look at homogeneity
shrub.abund.res <- resid(shrub.abund.glm) #pearson or deviance?
shrub.abund.pred <- predict(shrub.abund.glm)
# abund.pred_count <- exp(predict(abund.glm))
# r_test <- shrub_a_analysis$total_seednum-abund.pred_count
# plot(r_test, abund.res)

plot(shrub.abund.pred, shrub.abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
qqnorm(shrub.abund.res)
qqline(shrub.abund.res, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(shrub.abund.res)
#skewed left

##d) summary of data
anova(shrub.abund.glm, test= "F")
summary(shrub.abund.glm)

##e) getting p-values
#find Tukeys (HSD) and use for this data

#use for finding z scores for info below
summary(glht(shrub.abund.glm, mcp(treatment="Tukey")))

#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for pema-hial (-0.226)
ptukey(abs(-0.226)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.995

#contrast for viko-hial (0.068)
ptukey(abs(0.068)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.999

#contrast for vogu-hial (-1.209)
ptukey(abs(-1.209)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.634

#contrast for viko-pema (0.293)
ptukey(abs(0.293)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.991

#contrast for vogu-pema (-1.005)
ptukey(abs(-1.005)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.751

#contrast for vogu-viko (-1.270)
ptukey(abs(-1.270)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.604



################## Tree Abundance Analysis #########################
########### Data Exploration for specific analysis#########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(tree_a_analysis$seednum)
boxplot(tree_a_analysis$seednum~ tree_a_analysis$treatment, xlab="treatment", ylab="seednum", main= "tree life form abundance per treatment")

#tree mean similar across treatments but variance higher in pema and viko

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use plot? to make sure we have all combinations)
with(tree_a_analysis, plot(treatment, seednum))

##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(seednum~ treatment +block, data = tree_a_analysis))

# model shows interactions when include the treatment*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(tree_a_analysis, aes(block, seednum))+
  geom_bar(stat = "identity", aes(fill=treatment))
# higher abundance in blocks 2(viko) and 4(pema).

ggplot(tree_a_analysis, aes(treatment, seednum))+
  geom_bar(stat = "identity", aes(fill=block))
# hial is the only plot to have such a large number of trees but in other plots, the nubmers are relatively low.

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(tree_a_analysis, aes(treatment, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(tree_a_analysis, aes(block, seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~treatment)

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(tree_a_analysis, table(block, treatment))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
#library(dplyr); library(ggplot2)
#Find outliers
ggplot(tree_a_analysis, aes(treatment, seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

tree_a_analysis %>% 
  filter(!is.na(seednum)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(tree_a_analysis, aes(block, seednum))+
  geom_boxplot(aes(fill=block))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")
#summary
# block 2 and Hial have more variability than other block and canopsysp treatments
# can go ahead looking at total seed abundance relative to block and treatment. 


#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)
#removed above
# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of tree life form seeds differ between planted tree species treatments? 

##a)changed model to glmer because this will account for overdispersion with the poisson distribution.
tree.abund.glm <- glmer(seednum ~ block+treatment+(1|plot), data=tree_a_analysis, family = poisson)

##b)plot residuals to look at homogeneity
tree.abund.res <- resid(tree.abund.glm) #pearson or deviance?
tree.abund.pred <- predict(tree.abund.glm)
# abund.pred_count <- exp(predict(abund.glm))
# r_test <- tree_a_analysis$total_seednum-abund.pred_count
# plot(r_test, abund.res)

plot(tree.abund.pred, tree.abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
qqnorm(tree.abund.res)
qqline(tree.abund.res, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(tree.abund.res)
#skewed left

##d) summary of data
anova(tree.abund.glm, test= "F")
summary(tree.abund.glm)

##e) getting p-values
#find Tukeys (HSD) and use for this data

#use for finding z scores for info below
summary(glht(tree.abund.glm, mcp(treatment="Tukey")))

#Did not do because did not look significant.

