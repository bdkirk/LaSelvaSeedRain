#This file contains abundance data exploration before the analysis.  This file was created to remove code that was not directly pertinent to the analysis

## File created on 8 Feb 18

##Abundance exploration
#After beginning creation of this, I decided to keep this included in the original Abundance_Analysis.R file and rather to put extra code here that I wasn't using.

#load libraries
library(ggplot2); library(car); library(lsmeans); library(stats); library(lme4); library(dplyr)

#can look at response variables. 
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/RawData")
abundattrib <- read.csv("Plot_attributes.csv")

#Bind columns together to look at attributes
abund <- cbind(abundanalysis, abundattrib)

#Make sure all columns are appropriate characters/numbers, etc.
str(abund)
abund$Dist <- as.factor(abund$Dist)
abund$ï..Plot <- NULL



########Analysis #########
# This includes extras from the model development.
#using all data
#total_seednum~ block+treatment, family=gaussian  #by default, an identity link

abundmod1<-glm(total_seednum~block+treatment, data=abundanalysis)
summary(abundmod1)
anova(abundmod1) #should not use if unbalanced


#check out the design matrix
head(model.matrix(abundmod1))

# Opinions on model selection- much disagreement about which is best. 

# 1. Classical Hypothesis testing: drop all nonsignificant predictors, then report final model and interpret differences between levels of a predictor in final model. 

# anova(model) gives Type I sums of squares, which means the reference level is tested first and then other levels, and then interactions. R defaults to treatment contrasts. Can get different results for unbalanced datasets depending on which factor is entered first in the model and thus considered first. 

with(abundanalysis,tapply(total_seednum, list(treatment, block), mean))

##can use car package to do Type II or III sums of squares
#Type III can be used with interactions
Anova(abundmod1, type="III") 

#explore contrasts
options('contrasts') #shows what contrasts R is using
#can set contrasts to SAS default. 
abundmod1a<-glm(total_seednum~block+treatment, data=, contrasts = list(treatment = "contr.SAS", block="contr.SAS"))
summary(abundmod1a)

#Type II
Anova(glm(total_seednum~block+treatment, data=abundanalysis), type="II")  #note - type II can't handle interactions
#compare against 
anova(glm(total_seednum~treatment+block, data=abundanalysis))
anova(glm(total_seednum~block+treatment, data=abundanalysis))

#lsmeans
cpyblk<-pairs(lsmeans(abundmod1, ~treatment | block)) # in lsmeans package
blkcpy <- pairs(lsmeans(abundmod1, ~block | treatment))
rbind(cpyblk, blkcpy)

####### 2. Classic model selection: Create all sub-models. Use LRT to come up with best model. #####
abundmod1<-glm(logsum~block+treatment, data=abundanalysis)
abundmod2<-glm(total_seednum~treatment+block, data=abundanalysis)
abundmod3<-glm(logsum~treatment, data=abundanalysis)
abundmod4<-glm(logsum~block, data=abundanalysis)
abundmod5 <- glm(total_seednum~plot, data=abundanalysis)
abundmod_null<-glm(total_seednum~1, data=abundanalysis)
abundmod6 <- glm(logsum~treatment*block, data=abundanalysis)

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

plot(x=abundanalysis$treatment, y=F1) #heterogeneity in residuals bt treatment
plot(x=abundanalysis$block, y=F1) #heterogeneity in residuals wrt Native
plot(x=abundanalysis$plot, y=F1) #residual variance larger at Guam sites than Saipan sites, but homogeneity bt sites within an island

########### RESIDUALS #################
plot(abundmod1)
#this model has the logsum

plot(abundmod2) 
#This part involves determining which model is most appropriate
#get residuals and plot residuals vs predicted values
abund.glm <-  glm(total_seednum ~ block+treatment, data=abundanalysis,  family = poisson) 
abund.res = resid(abund.glm) 

plot(abundanalysis$total_seednum, abund.res, ylab="Residuals", xlab="Seed Abundance", main="Abundance pred by resid") 
abline(0, 0) 

anova(abund.glm)
head(abundanalysis)
length(unique(abundanalysis$plot))

### explored adding in distance to model

str(abund)
abund$Dist <- as.factor(abund$Dist)
#Incorporating distance into the model: None of the below works right now.  Need to reread email from Dr. Dixon on 3 July.
abunddist.glm <- glmer(total_seednum~treatment+Dist+(1|plot), data= abund, family = poisson)

summary(abunddist.glm)
anova(abunddist.glm)

library(lsmeans)
lsmeans(abunddist.glm, "Dist", contr="pairwise")