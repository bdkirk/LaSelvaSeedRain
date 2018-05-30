# Started on 12 May 2017
# Will have a section for richness, shannon-wiener diversity and evenness


################################# Richness ############################

# Load libraries
library(ggplot2); library(car); library(lsmeans); library(stats);library(multcomp); library(lme4)

#bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")
divanalysis <- read.csv("div_sub_notrtsp_nw.csv")

tail(divanalysis)
str(divanalysis)
divanalysis$block <- as.factor(divanalysis$block)
##### Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(divanalysis$richness)
boxplot(divanalysis$richness~ divanalysis$treatment, main= "Seed species richness per treatment", xlab="Treatment", ylab="Species richness")

#created dotplot as suggested by Dr. Dixon on 8 Aug meeting.
ggplot(divanalysis, aes(treatment, richness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = 1)+
  xlab("Planted Tree Species")+
  ylab("Species Richness")
#pema appears to have the lowest species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(divanalysis, table(treatment, richness))
boxplot(divanalysis$richness~divanalysis$treatment, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ treatment+block, data = divanalysis))
vif(glm(richness~ treatment*block, data = divanalysis))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(divanalysis, aes(treatment, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(divanalysis, aes(treatment, richness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(divanalysis, aes(block, richness, color=block))+
  geom_boxplot()+
  facet_grid(.~treatment)

with(divanalysis, ftable(treatment, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(divanalysis, table(block, treatment))

############ Katie Rey Data Visualization####
library(dplyr); library(ggplot2)
#Find outliers
ggplot(divanalysis, aes(treatment, richness))+
  geom_point()
#Appears to be more variance in pema

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

divanalysis %>% 
  filter(!is.na(richness)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(divanalysis, aes(block, richness))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

#################### Data Tidying ####################################
# Fix up dataframe
# a.	Remove missing values (NA’s)

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

#############################  Analysis   ###############

#1) Does species richness of seeds differ between overstory treatments? 

##a) model development
richnessmod1<-lm(richness~block+treatment, data=divanalysis)

# This model is not necessary because the residual df is not large.
#richnessmod2 <- glmer(richness ~ block + treatment + (1|plot), family = poisson, data = divanalysis)

##b) plot residuals to look for homogeneity
plot(richnessmod1)
rich.res <-  resid(richnessmod1)
rich.pred <-  predict(richnessmod1)

plot(rich.pred, rich.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c) plot histogram and qq plot to check for normality
hist(rich.res)

#check for normality with q-qplot
qqnorm(rich.res)
qqline(rich.res, col = 'red')

##d) summary of data
summary(richnessmod1)
anova(richnessmod1, test= "F")

#summary(richnessmod2)
#anova(richnessmod2, test = "F")

##e) getting p-values
lsmeans(richnessmod1, "treatment", contr= "pairwise")
#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#use for finding z scores for info below
summary(glht(richnessmod1, mcp(treatment="Tukey")))

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)
#IMPORTANT: need to use abs(zscore) for negative values.

#contrast for hial-pema (3.244) with K.R. method (-3.264)
ptukey((abs(-3.244)*sqrt(2)), nmeans= 4, df=8, lower = F)
#=0.0471

#contrast for hial-viko (0.311) with K.R. method (-0.104)
ptukey(abs(-0.104)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.999

#contrast for hial-vogu (-0.092) with K.R. method (0.211)
ptukey(abs(-0.211)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.996

#contrast for viko-pema (-3.163) with K.R. method 3.143
ptukey(abs(-3.143)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.0543

#contrast for vogu-pema (-3.266) with K.R. method 3.184
ptukey(3.184*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.051

#contrast for viko-vogu (-0.372) with K.R. method 0.305
ptukey(abs(-0.305)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.994


################################################################################
######################### Diversity #############################################
###############################################################################
##### Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(divanalysis$diversity)
boxplot(divanalysis$diversity~ divanalysis$treatment, main= "Species Diversity per treatment", xlab="treatment", ylab="species diversity")

#vogu and hial have greatest diversity 


#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(divanalysis, aes(treatment, diversity))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .1)+
  xlab("Planted Tree Species")+
  ylab("Species Diversity")

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(divanalysis, table(treatment, diversity))
boxplot(divanalysis$diversity~divanalysis$treatment, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ treatment+block, data = divanalysis))
#vif(glm(diversity~ treatment*block, data = divanalysis))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(divanalysis, aes(treatment, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(divanalysis, aes(treatment, diversity, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(divanalysis, aes(block, diversity, color=block))+
  geom_boxplot()+
  facet_grid(.~treatment)

with(divanalysis, ftable(treatment, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(divanalysis, table(block, treatment))

########### Katie Rey Data Visualization####

library(dplyr); library(ggplot2)
#Find outliers
ggplot(divanalysis, aes(treatment, diversity))+
  geom_point()
#Appears to be more variance in pema

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

divanalysis %>% 
  filter(!is.na(diversity)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(divanalysis, aes(block, diversity))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

################### Fix up dataframe #####
# a.	Remove missing values (NA’s)

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########################  Analysis   ###############

#1) Does abundance of seeds differ between overstory treatments? 

##a) model development
diversitymod1<-lm(diversity~block+treatment, data=divanalysis)

##b)plot residuals to look at homogeneity
plot(diversitymod1)

div.res <- resid(diversitymod1)
div.pred <- predict(diversitymod1)

plot(div.pred, div.res,  ylab="Residuals", xlab="predicted values", main="resid vs pred")
abline(0,0)

plot(divanalysis$diversity, div.res, ylab="Residuals", xlab="Seed Species diversity", main="diversity pred by resid") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to check for normality
hist(div.res)

#check for normality with q-qplot
qqnorm(div.res)
qqline(div.res, col = 'red')

##d) summary of analysis
summary(diversitymod1)
anova(diversitymod1, test = "F") #should not use if unbalanced

##e) look for p-values

###Found no difference on 31 May between using glm or lm for shannon wiener diversity. lm is simpler so I will use this.  Talked with Dr. Dixon on 30 June and glm will revert to lm if the family is Guassian.

lsmeans(diversitymod1, "treatment", contr= "pairwise")
#These p-values are not significant but wanted to see how close PEMA was to significance
 

################################################################################
######################## EVENNESS ##############################################
###############################################################################

##### Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(divanalysis$evenness)
boxplot(divanalysis$evenness~ divanalysis$treatment, main= "Species evenness per treatment", xlab="treatment", ylab="species evenness")

#More of a normal distribution in histogram
#pema and hial have largest amounts of variability


#created dotplot as suggested by Dr. Dixon on 8 Aug meeting. Better to use dots when so few observations.
ggplot(divanalysis, aes(treatment, evenness))+
  geom_dotplot(binaxis = "y", method="histodot", binwidth = .025)+
  xlab("Planted Tree Species")+
  ylab("Species Evenness")
##b.	Examine Zero inflation Y
#Not applicable for species evenness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(divanalysis, table(treatment, evenness))

#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(evenness~ treatment+block, data = divanalysis))
vif(glm(evenness~ treatment*block, data = divanalysis))
# model shows interactions when include the treatment*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(divanalysis, aes(treatment, evenness))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(divanalysis, aes(treatment, evenness, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(divanalysis, aes(block, evenness, color=block))+
  geom_boxplot()+
  facet_grid(.~treatment)

with(divanalysis, ftable(treatment, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(divanalysis, table(block, treatment))

######### Katie Rey Data Visualization####
library(dplyr); library(ggplot2)
#Find outliers
ggplot(divanalysis, aes(treatment, evenness))+
  geom_point()
#Appears to be more variance in pema

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

divanalysis %>% 
  filter(!is.na(evenness)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(divanalysis, aes(block, evenness))+
  geom_boxplot(aes(fill=treatment))+
  facet_grid(.~treatment)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

################ Fix up dataframe #####
# a.	Remove missing values (NA’s)

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

#########################  Analysis   ########################################
#1) Does abundance of seeds differ between overstory treatments? 

##a) model development
evennessmod1<-lm(evenness~block+treatment, data=divanalysis)

##b)plot residuals to look at homogeneity
plot(evennessmod1)

even.res <- resid(evennessmod1)
even.pred <- predict(evennessmod1)
plot(even.pred, even.res,  ylab="Residuals", xlab="predicted values", main="resid vs pred")
abline(0,0)

plot(divanalysis$evenness, even.res, ylab="Residuals", xlab="Seed Species evenness", main="evenness pred by resid") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
hist(even.res) #This histogram has a normal distribution

qqnorm(even.res)
qqline(even.res, col = 'red')

##d) model summary analysis
summary(evennessmod1)
anova(evennessmod1, test="F") #should not use if unbalanced

##e) p-values
lsmeans(evennessmod1, "treatment", contr= "pairwise")
#not significant
