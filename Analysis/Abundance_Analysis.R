# Started on 12 May 2017
# Data exploration will follow Haldre's outline which is based on Zuur paper
# This is an analysis of effects of planted tree species on the abundance of seed rain.  The experimental unit is the plot.  We are looking at 15 plots with 4 treatment groups of planted trees.
#This will look at the abundance for all species seeds summed over a year.

################### Abundance Analysis ##########

#Load libraries
library(ggplot2); library(car); library(lsmeans); library(stats); library(lme4)

#Bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")
abundanalysis <- read.csv("abund_sub_notrtsp.csv")
str(abundanalysis) #manually added in two columns after making the tidy data file to distinguish the block and plot
abundanalysis$block <- as.factor(abundanalysis$block)
str(abundanalysis) #verify block is a factor

#bring in data for other y response variables
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/RawData")
abundattrib <- read.csv("Plot_attributes.csv")

#Bind columns together to look at attributes
abund <- cbind(abundanalysis, abundattrib)

#Make sure all columns are appropriate characters/numbers, etc.
str(abund)
abund$Dist <- as.factor(abund$Dist)
abund$ï..Plot <- NULL

#Plotting to look at how the data relates
plot(abund$total_seednum, abund$Slope)
plot(abund$total_seednum, abund$Aspect)
plot(abund$Dist, abund$total_seednum)
#plot will vary directly with these covariates


#add in se and mean to dataset
library(dplyr)
abund_summary <- abundanalysis %>% # the names of the new data frame and the data frame to be summarised
  group_by(canopysp) %>%   # the grouping variable
  summarise(mean_PL = mean(total_seednum),  # calculates the mean of each group
            sd_PL = sd(total_seednum), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(total_seednum)/sqrt(n())) # calculates the standard error of each group
  
#make plot
library(ggplot2)
#ggplot(abund_summary, aes(canopysp, mean_PL, fill= canopysp))+
  #geom_boxplot()+
  #ggtitle("Total Abundance Across Treatments")+
  #geom_errorbar(abund_summary, aes(ymin=mean_PL-SE_PL, ymax=mean_PL+SE_PL), width=0.2)+
  #xlab("Planted Tree Treatments")+
  #ylab("Abundance")+
  #guides(fill=FALSE)

#looking for trend between distance from forest and total seed num across treatments, none found.
ggplot(abund, aes (Slope, total_seednum))+
  geom_point(aes(color = canopysp))

#Log transform the data because the numbers are very large. Don't really need to do this step
#decided not to logtransform the data because it is count data that will have overdispersion that will be taken into account with 
#abundanalysis$logsum <- log(abundanalysis$total_seednum)
#str(abundanalysis)
#abundanalysis$block <- as.factor(abundanalysis$block)
#abundanalysis$canopysp <- as.factor(abundanalysis$canopysp)

########### Data Exploration #########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(abundanalysis$total_seednum)
boxplot(abundanalysis$total_seednum~ abundanalysis$canopysp, xlab="treatment", ylab="seednum", main= "Seed abundance per treatment")


#look at distance
boxplot(abund$total_seednum~abund$Dist)
#no clear trend seen here

#No difference found when looking at logsum of abundance.
#boxplot(abundanalysis$logsum~ abundanalysis$canopysp, xlab="canopysp", ylab="seednum")

#Much more variance in hial
#one outlier in Y, hial 2 but will keep, X is categorical

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(abundanalysis, table(canopysp, total_seednum))
with(abund, table(Dist, total_seednum))

#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(total_seednum~ canopysp +block + Dist, data = abund))
vif(glm(total_seednum~canopysp +Dist, data = abund))
# model shows interactions when include the canopysp*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(abundanalysis, aes(canopysp, total_seednum, color=block))+
  geom_boxplot()
#more variance with Hial, minimal variance with vogu. pema and viko have a similar variance.

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(abundanalysis, aes(canopysp, total_seednum, color=canopysp))+
  geom_boxplot()+
  facet_grid(.~block)

#2. Perhaps consider distance to the mature forest as a factor.
ggplot(abund, aes(Dist, total_seednum))+
  geom_point()

hist(abund$Dist)
boxplot(abund$Dist ~ abund$total_seednum)

with(abund, ftable(canopysp, Dist))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(abundanalysis, table(block, canopysp))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
library(dplyr); library(ggplot2)
#Find outliers
ggplot(abundanalysis, aes(canopysp, total_seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

abundanalysis %>% 
  filter(!is.na(total_seednum)) %>%
  ggplot()+geom_bar(aes(canopysp,fill=as.factor(canopysp)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(abundanalysis, aes(block, total_seednum))+
  geom_boxplot(aes(fill=block))+
  facet_grid(.~canopysp)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")
#summary
# block 2 and Hial have more variability than other block and canopsysp treatments
# can go ahead looking at total seed abundance relative to block and canopysp. 


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

abundmod1<-glm(total_seednum~block+canopysp, data=abundanalysis)
summary(abundmod1)
anova(abundmod1) #should not use if unbalanced


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
plot(abundmod1)
#this model has the logsum

plot(abundmod2) 
#This part involves determining which model is most appropriate
#get residuals and plot residuals vs predicted values
abund.glm <-  glm(total_seednum ~ block+canopysp, data=abundanalysis,  family = poisson) 
abund.res = resid(abund.glm) 

plot(abundanalysis$total_seednum, abund.res, ylab="Residuals", xlab="Seed Abundance", main="Abundance pred by resid") 
abline(0, 0) 

anova(abund.glm)
head(abundanalysis)
length(unique(abundanalysis$plot))




#change model to glmer because this will account for overdispersion with the poisson distribution.
abund.glm <- glmer(total_seednum ~ block+canopysp+(1|plot), data=abundanalysis, 
                  family = poisson)

abund.res <- resid(abund.glm)
abund.pred <- predict(abund.glm)

plot(abund.pred, abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

anova(abund.glm, test= "F")
summary(abund.glm)

#find Tukeys (HSD) and use for this data
#if p-value is greater than 0.05, don't need to do HSD
lsmeans(abund.glm, "canopysp", contr= "pairwise")
#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for hial-pema (-0.343)
ptukey((-0.343*sqrt(2)), nmeans= 4, df=8, lower = F)
#=1

#contrast for hial-viko (-0.163)
ptukey(-0.163*sqrt(2), nmeans= 4, df=8, lower = F)
#=1

#contrast for hial-vogu (2.247)
ptukey(2.247*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.19

#contrast for viko-pema (0.181)
ptukey(0.181*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.9977

#contrast for vogu-pema (2.558)
ptukey(2.558*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.124

#contrast for viko-vogu (2.394)
ptukey(2.394*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.1556

####Have not figured out how to more efficiently export output and do a conversion.
temp <- lsmeans(abund.glm, "canopysp", contr= "pairwise")

str(abund)
abund$Dist <- as.factor(abund$Dist)
#Incorporating distance into the model: None of the below works right now.  Need to reread email from Dr. Dixon on 3 July.
abunddist.glm <- glmer(total_seednum~canopysp+Dist+(1|plot), data= abund, family = poisson)

summary(abunddist.glm)
anova(abunddist.glm)

library(lsmeans)
lsmeans(abunddist.glm, "Dist", contr="pairwise")

qqnorm(abundanalysis$total_seednum)
qqline(abundanalysis$total_seednum, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(abund.res)
