# Started on 12 May 2017
# Will have a section for richness, shannon-wiener diversity and evenness

#######################################################################
################################# Richness ############################
######################################################################
# Load libraries
library(ggplot2); library(car); library(lsmeans); library(stats)

#bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")
divanalysis <- read.csv("div_sub_nocpy.csv")

tail(divanalysis)
str(divanalysis)
divanalysis$block <- as.factor(divanalysis$block)
########### Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(divanalysis$richness)
boxplot(divanalysis$richness~ divanalysis$canopysp, main= "Seed species richness per treatment", xlab="canopysp", ylab="species richness")

#pema appears to have the lowest species richness

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(divanalysis, table(canopysp, richness))
boxplot(divanalysis$richness~divanalysis$canopysp, main= "Seed species richness per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(richness~ canopysp+block, data = divanalysis))
vif(glm(richness~ canopysp*block, data = divanalysis))
# model shows interactions when include the canopysp*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(divanalysis, aes(canopysp, richness))+
  geom_boxplot()
#more variance with Hial

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(divanalysis, aes(canopysp, richness, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~block)

ggplot(divanalysis, aes(block, richness, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~canopysp)

with(divanalysis, ftable(canopysp, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(divanalysis, table(block, canopysp))

###Katie Rey Data Visualization####
library(dplyr); library(ggplot2)
#Find outliers
ggplot(divanalysis, aes(canopysp, richness))+
  geom_point()
#Appears to be more variance in pema

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

divanalysis %>% 
  filter(!is.na(richness)) %>%
  ggplot()+geom_bar(aes(canopysp,fill=as.factor(canopysp)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(divanalysis, aes(block, richness))+
  geom_boxplot(aes(fill=canopysp))+
  facet_grid(.~canopysp)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

#################################################
# Fix up dataframe
# a.	Remove missing values (NA’s)

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of seeds differ between overstory treatments? 

#using all data
#total_seednum~ block+canopysp, family=gaussian  #by default, an identity link

richnessmod1<-glm(richness~block+canopysp, data=divanalysis)
summary(richnessmod1)
anova(richnessmod1) #should not use if unbalanced

richnessmod2 <- glm(richness~plot, data= divanalysis)
richnessmod3 <- glm(richness~canopysp*block, data = divanalysis)
richnessmod4 <- glm(richness~canopysp, data = divanalysis)
richnessmod5 <- glm(richness~block, data = divanalysis)
richnessmodnull <- glm(richness~1, data = divanalysis)
anova(richnessmod4)
#check out the design matrix
head(model.matrix(abundmod1))

# Opinions on model selection- much disagreement about which is best. 

# 1. Classical Hypothesis testing: drop all nonsignificant predictors, then report final model and interpret differences between levels of a predictor in final model. 

# anova(model) gives Type I sums of squares, which means the reference level is tested first and then other levels, and then interactions. R defaults to treatment contrasts. Can get different results for unbalanced datasets depending on which factor is entered first in the model and thus considered first. 

with(abundanalysis,tapply(total_seednum, list(canopysp, block), mean))

##can use car package to do Type II or III sums of squares
#Type III can be used with interactions
Anova(abundmod1, type="III") 

#explore contrasts
options('contrasts') #shows what contrasts R is using
#can set contrasts to SAS default. 
abundmod1a<-glm(total_seednum~block+canopysp, data=, contrasts = list(canopysp = "contr.SAS", block="contr.SAS"))
summary(abundmod1a)

#Type II
Anova(glm(total_seednum~block+canopysp, data=abundanalysis), type="II")  #note - type II can't handle interactions
#compare against 
anova(glm(total_seednum~canopysp+block, data=abundanalysis))
anova(glm(total_seednum~block+canopysp, data=abundanalysis))

#lsmeans
cpyblk<-pairs(lsmeans(abundmod1, ~canopysp | block)) # in lsmeans package
blkcpy <- pairs(lsmeans(abundmod1, ~block | canopysp))
rbind(cpyblk, blkcpy)

####### 2. Classic model selection: Create all sub-models. Use LRT to come up with best model. #####
abundmod1<-glm(logsum~block+canopysp, data=abundanalysis)
abundmod2<-glm(total_seednum~canopysp+block, data=abundanalysis)
abundmod3<-glm(logsum~canopysp, data=abundanalysis)
abundmod4<-glm(logsum~block, data=abundanalysis)
abundmod5 <- glm(total_seednum~plot, data=abundanalysis)
abundmod_null<-glm(total_seednum~1, data=abundanalysis)
abundmod6 <- glm(logsum~canopysp*block, data=abundanalysis)

summary(abundmod5)
anova(abundmod1, abundmod2, test = "Rao")  #model 1 not sig better than 2
anova(abundmod1, abundmod3)  #model 2 not sig better than 3
anova(abundmod1, abundmod4) 
anova.glm(abundmod4, abundmod5)#can't run this, because not sub-model - needs to be nested to compare with LRT
anova(abundmod3, abundmod_null) #model 3 sig better fit than null model

# 3. Information theoretic approach- compare models using AIC- get competing models. AIC is a measure of the goodness of fit of an estimated statistical model. It is -2*log-likelihood + 2*npar, where npar is the number of effective parameters in the model. More complex models get penalized. 

AIC(abundmod1, abundmod6, abundmod3, abundmod4, abundmod5, abundmod_null) #abundmod3 has lowest AIC, by almost 2 points. Might consider model averaging. 

#check out packages MuMIn and AICcmodavg for a variety of useful tools. 

# 4. If you do an experiment, don’t do any model selection at all- fit model that you think makes sense, and keep everything as it is, even non-significant parameters. Some might choose to do some model selection to keep only significant interactions, but once fit model with main effect terms, then stick with it. 

confint(abundmod3) #Saipan abunds are significantly smaller than Guam (confidence intervals do not overlap 0)

#######Model validation######
#A. Look at homogeneity: plot fitted values vs residuals
#B. Look at influential values: Cook
#C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense 
#D. Look at normality of residuals: histogram

#for glm can use plot(model)
plot(abundmod1)

#extract residuals
E1 <- resid(abundmod1, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(abundmod1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x=abundanalysis$canopysp, y=F1) #heterogeneity in residuals bt canopysp
plot(x=abundanalysis$block, y=F1) #heterogeneity in residuals wrt Native
plot(x=abundanalysis$plot, y=F1) #residual variance larger at Guam sites than Saipan sites, but homogeneity bt sites within an island

########### RESIDUALS #################
richnessmod1<-glmer(richness~block+canopysp+(1|plot), family = poisson, data=divanalysis)
summary(richnessmod1)
anova(richnessmod1, test= "F") 


plot(richnessmod1)
rich.res = resid(richnessmod1)
rich.pred = predict(richnessmod1)

plot(rich.pred, rich.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

 
#chedk for normality
hist(rich.res)

################################################################################
######################### Diversity #############################################
################################################################################
########### Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(divanalysis$diversity)
boxplot(divanalysis$diversity~ divanalysis$canopysp, main= "Species Diversity per treatment", xlab="canopysp", ylab="species diversity")

#vogu and hial have greatest diversity 

##b.	Examine Zero inflation Y
#Not applicable for species richness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(divanalysis, table(canopysp, diversity))
boxplot(divanalysis$diversity~divanalysis$canopysp, main= "Seed species diversity per treatment")
#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(diversity~ canopysp+block, data = divanalysis))
vif(glm(diversity~ canopysp*block, data = divanalysis))
# model shows interactions when include the canopysp*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(divanalysis, aes(canopysp, diversity))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(divanalysis, aes(canopysp, diversity, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(divanalysis, aes(block, diversity, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~canopysp)

with(divanalysis, ftable(canopysp, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(divanalysis, table(block, canopysp))

###Katie Rey Data Visualization####
library(dplyr); library(ggplot2)
#Find outliers
ggplot(divanalysis, aes(canopysp, diversity))+
  geom_point()
#Appears to be more variance in pema

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

divanalysis %>% 
  filter(!is.na(diversity)) %>%
  ggplot()+geom_bar(aes(canopysp,fill=as.factor(canopysp)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(divanalysis, aes(block, diversity))+
  geom_boxplot(aes(fill=canopysp))+
  facet_grid(.~canopysp)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

##### Fix up dataframe #####
# a.	Remove missing values (NA’s)

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of seeds differ between overstory treatments? 

#using all data
#total_seednum~ block+canopysp, family=gaussian  #by default, an identity link

diversitymod1<-glm(diversity~block+canopysp, data=divanalysis)
summary(diversitymod1)
anova(diversitymod1) #should not use if unbalanced

diversitymod2 <- glm(diversity~plot, data= divanalysis)
diversitymod3 <- glm(diversity~canopysp*block, data = divanalysis)
diversitymod4 <- glm(diversity~canopysp, data = divanalysis)
diversitymod5 <- glm(diversity~block, data = divanalysis)
diversitymodnull <- glm(diversity~1, data = divanalysis)
anova(diversitymod4)
#check out the design matrix
head(model.matrix(abundmod1))

# Opinions on model selection- much disagreement about which is best. 

# 1. Classical Hypothesis testing: drop all nonsignificant predictors, then report final model and interpret differences between levels of a predictor in final model. 

# anova(model) gives Type I sums of squares, which means the reference level is tested first and then other levels, and then interactions. R defaults to treatment contrasts. Can get different results for unbalanced datasets depending on which factor is entered first in the model and thus considered first. 

with(abundanalysis,tapply(total_seednum, list(canopysp, block), mean))

##can use car package to do Type II or III sums of squares
#Type III can be used with interactions
Anova(abundmod1, type="III") 

#explore contrasts
options('contrasts') #shows what contrasts R is using
#can set contrasts to SAS default. 
abundmod1a<-glm(total_seednum~block+canopysp, data=, contrasts = list(canopysp = "contr.SAS", block="contr.SAS"))
summary(abundmod1a)

#Type II
Anova(glm(total_seednum~block+canopysp, data=abundanalysis), type="II")  #note - type II can't handle interactions
#compare against 
anova(glm(total_seednum~canopysp+block, data=abundanalysis))
anova(glm(total_seednum~block+canopysp, data=abundanalysis))

#lsmeans
cpyblk<-pairs(lsmeans(abundmod1, ~canopysp | block)) # in lsmeans package
blkcpy <- pairs(lsmeans(abundmod1, ~block | canopysp))
rbind(cpyblk, blkcpy)

####### 2. Classic model selection: Create all sub-models. Use LRT to come up with best model. #####
abundmod1<-glm(logsum~block+canopysp, data=abundanalysis)
abundmod2<-glm(total_seednum~canopysp+block, data=abundanalysis)
abundmod3<-glm(logsum~canopysp, data=abundanalysis)
abundmod4<-glm(logsum~block, data=abundanalysis)
abundmod5 <- glm(total_seednum~plot, data=abundanalysis)
abundmod_null<-glm(total_seednum~1, data=abundanalysis)
abundmod6 <- glm(logsum~canopysp*block, data=abundanalysis)

summary(abundmod5)
anova(abundmod1, abundmod2, test = "Rao")  #model 1 not sig better than 2
anova(abundmod1, abundmod3)  #model 2 not sig better than 3
anova(abundmod1, abundmod4) 
anova.glm(abundmod4, abundmod5)#can't run this, because not sub-model - needs to be nested to compare with LRT
anova(abundmod3, abundmod_null) #model 3 sig better fit than null model

# 3. Information theoretic approach- compare models using AIC- get competing models. AIC is a measure of the goodness of fit of an estimated statistical model. It is -2*log-likelihood + 2*npar, where npar is the number of effective parameters in the model. More complex models get penalized. 

AIC(abundmod1, abundmod6, abundmod3, abundmod4, abundmod5, abundmod_null) #abundmod3 has lowest AIC, by almost 2 points. Might consider model averaging. 

#check out packages MuMIn and AICcmodavg for a variety of useful tools. 

# 4. If you do an experiment, don’t do any model selection at all- fit model that you think makes sense, and keep everything as it is, even non-significant parameters. Some might choose to do some model selection to keep only significant interactions, but once fit model with main effect terms, then stick with it. 

confint(abundmod3) #Saipan abunds are significantly smaller than Guam (confidence intervals do not overlap 0)

#######Model validation######
#A. Look at homogeneity: plot fitted values vs residuals
#B. Look at influential values: Cook
#C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense 
#D. Look at normality of residuals: histogram

#for glm can use plot(model)
plot(abundmod1)

#extract residuals
E1 <- resid(abundmod1, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(abundmod1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x=abundanalysis$canopysp, y=F1) #heterogeneity in residuals bt canopysp
plot(x=abundanalysis$block, y=F1) #heterogeneity in residuals wrt Native
plot(x=abundanalysis$plot, y=F1) #residual variance larger at Guam sites than Saipan sites, but homogeneity bt sites within an island

########### RESIDUALS #################
diversitymod1<-lm(diversity~block+canopysp, data=divanalysis)
summary(diversitymod1)
anova(diversitymod1, test = "F") #should not use if unbalanced


plot(diversitymod1)
div.res <- resid(diversitymod1)
div.pred <- predict(diversitymod1)

plot(div.pred, div.res,  ylab="Residuals", xlab="predicted values", main="resid vs pred")
abline(0,0)
#get residuals and plot residuals vs predicted values
div.glm <- glm(diversity ~ block+canopysp, data=divanalysis) 
div.res1 <- resid(div.glm) 
div.pred1 <- predict(div.glm)
plot (div.pred1, div.res1)

anova(div.glm, test = "F")
summary(div.glm)

##Not necessary to use, read below
div.res2 <- residuals.glm(div.glm)
div.pred2 <- predict.glm(div.glm)
plot(div.pred2, div.res2)
##did not find any difference using the glm specific functions

plot(divanalysis$diversity, div.res, ylab="Residuals", xlab="Seed Species diversity", main="diversity pred by resid") 
abline(0, 0) 

#chedk for normality
hist(div.res)

###Found no difference on 31 May between using glm or lm for shannon wiener diversity. lm is simpler so I will use this.

################################################################################
######################## EVENNESS ##############################################
###############################################################################

########### Data Exploration #########
##a.  Outliers in Y / Outliers in X 
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(divanalysis$evenness)
boxplot(divanalysis$evenness~ divanalysis$canopysp, main= "Species evenness per treatment", xlab="canopysp", ylab="species evenness")

#More of a normal distribution in histogram
#pema and hial have largest amounts of variability

##b.	Examine Zero inflation Y
#Not applicable for species evenness because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(divanalysis, table(canopysp, evenness))

#have all combinations here. 
#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(evenness~ canopysp+block, data = divanalysis))
vif(glm(evenness~ canopysp*block, data = divanalysis))
# model shows interactions when include the canopysp*block, shows collinearity

#d.	Homogeneity of variance?
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(divanalysis, aes(canopysp, evenness))+
  geom_boxplot()
#more variance within vogu, one outlier in pema

#e.	Independence Y - are Y's independent? 
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(divanalysis, aes(canopysp, evenness, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~block)

###this is the better one
ggplot(divanalysis, aes(block, evenness, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~canopysp)

with(divanalysis, ftable(canopysp, block))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is similar, can't think of anything else that might matter. 

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with really one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(divanalysis, table(block, canopysp))

###Katie Rey Data Visualization####
library(dplyr); library(ggplot2)
#Find outliers
ggplot(divanalysis, aes(canopysp, evenness))+
  geom_point()
#Appears to be more variance in pema

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

divanalysis %>% 
  filter(!is.na(evenness)) %>%
  ggplot()+geom_bar(aes(canopysp,fill=as.factor(canopysp)))+
  facet_grid(block~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(divanalysis, aes(block, evenness))+
  geom_boxplot(aes(fill=canopysp))+
  facet_grid(.~canopysp)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

##### Fix up dataframe #####
# a.	Remove missing values (NA’s)

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of seeds differ between overstory treatments? 

#using all data
#total_seednum~ block+canopysp, family=gaussian  #by default, an identity link

evennessmod1<-glm(evenness~block+canopysp, data=divanalysis)
summary(evennessmod1)
anova(evennessmod1) #should not use if unbalanced

evennessmod2 <- glm(evenness~plot, data= divanalysis)
evennessmod3 <- glm(evenness~canopysp*block, data = divanalysis)
evennessmod4 <- glm(evenness~canopysp, data = divanalysis)
evennessmod5 <- glm(evenness~block, data = divanalysis)
evennessmodnull <- glm(evenness~1, data = divanalysis)
anova(evennessmod4)
#check out the design matrix
head(model.matrix(abundmod1))

# Opinions on model selection- much disagreement about which is best. 

# 1. Classical Hypothesis testing: drop all nonsignificant predictors, then report final model and interpret differences between levels of a predictor in final model. 

# anova(model) gives Type I sums of squares, which means the reference level is tested first and then other levels, and then interactions. R defaults to treatment contrasts. Can get different results for unbalanced datasets depending on which factor is entered first in the model and thus considered first. 

with(abundanalysis,tapply(total_seednum, list(canopysp, block), mean))

##can use car package to do Type II or III sums of squares
#Type III can be used with interactions
Anova(abundmod1, type="III") 

#explore contrasts
options('contrasts') #shows what contrasts R is using
#can set contrasts to SAS default. 
abundmod1a<-glm(total_seednum~block+canopysp, data=, contrasts = list(canopysp = "contr.SAS", block="contr.SAS"))
summary(abundmod1a)

#Type II
Anova(glm(total_seednum~block+canopysp, data=abundanalysis), type="II")  #note - type II can't handle interactions
#compare against 
anova(glm(total_seednum~canopysp+block, data=abundanalysis))
anova(glm(total_seednum~block+canopysp, data=abundanalysis))

#lsmeans
cpyblk<-pairs(lsmeans(abundmod1, ~canopysp | block)) # in lsmeans package
blkcpy <- pairs(lsmeans(abundmod1, ~block | canopysp))
rbind(cpyblk, blkcpy)

####### 2. Classic model selection: Create all sub-models. Use LRT to come up with best model. #####
abundmod1<-glm(logsum~block+canopysp, data=abundanalysis)
abundmod2<-glm(total_seednum~canopysp+block, data=abundanalysis)
abundmod3<-glm(logsum~canopysp, data=abundanalysis)
abundmod4<-glm(logsum~block, data=abundanalysis)
abundmod5 <- glm(total_seednum~plot, data=abundanalysis)
abundmod_null<-glm(total_seednum~1, data=abundanalysis)
abundmod6 <- glm(logsum~canopysp*block, data=abundanalysis)

summary(abundmod5)
anova(abundmod1, abundmod2, test = "Rao")  #model 1 not sig better than 2
anova(abundmod1, abundmod3)  #model 2 not sig better than 3
anova(abundmod1, abundmod4) 
anova.glm(abundmod4, abundmod5)#can't run this, because not sub-model - needs to be nested to compare with LRT
anova(abundmod3, abundmod_null) #model 3 sig better fit than null model

# 3. Information theoretic approach- compare models using AIC- get competing models. AIC is a measure of the goodness of fit of an estimated statistical model. It is -2*log-likelihood + 2*npar, where npar is the number of effective parameters in the model. More complex models get penalized. 

AIC(abundmod1, abundmod6, abundmod3, abundmod4, abundmod5, abundmod_null) #abundmod3 has lowest AIC, by almost 2 points. Might consider model averaging. 

#check out packages MuMIn and AICcmodavg for a variety of useful tools. 

# 4. If you do an experiment, don’t do any model selection at all- fit model that you think makes sense, and keep everything as it is, even non-significant parameters. Some might choose to do some model selection to keep only significant interactions, but once fit model with main effect terms, then stick with it. 

confint(abundmod3) #Saipan abunds are significantly smaller than Guam (confidence intervals do not overlap 0)

#######Model validation######
#A. Look at homogeneity: plot fitted values vs residuals
#B. Look at influential values: Cook
#C. Look at independence: 
#      plot residuals vs each covariate in the model
#      plot residuals vs each covariate not in the model
#      Common sense 
#D. Look at normality of residuals: histogram

#for glm can use plot(model)
plot(abundmod1)

#extract residuals
E1 <- resid(abundmod1, type = "pearson")

#plot fitted vs residuals
F1 <- fitted(abundmod1, type = "response")

par(mfrow = c(2,2), mar = c(5,5,2,2))
plot(x = F1, 
     y = E1, 
     xlab = "Fitted values",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

plot(x=abundanalysis$canopysp, y=F1) #heterogeneity in residuals bt canopysp
plot(x=abundanalysis$block, y=F1) #heterogeneity in residuals wrt Native
plot(x=abundanalysis$plot, y=F1) #residual variance larger at Guam sites than Saipan sites, but homogeneity bt sites within an island

########### RESIDUALS #################
evennessmod1<-lm(evenness~block+canopysp, data=divanalysis)
summary(evennessmod1)
anova(evennessmod1, test="F") #should not use if unbalanced


plot(evennessmod1)
#this model has the logsum

even.res <- resid(evennessmod1)
even.pred <- predict(evennessmod1)
plot(even.pred, even.res,  ylab="Residuals", xlab="predicted values", main="resid vs pred")

#get residuals and plot residuals vs predicted values
even.glm = glm(evenness ~ block+canopysp, data=divanalysis) 
even.res = resid(even.glm) 

plot(divanalysis$evenness, even.res, ylab="Residuals", xlab="Seed Species evenness", main="evenness pred by resid") 
abline(0, 0) 

#chedk for normality
hist(even.res)

#This histogram has a normal distribution