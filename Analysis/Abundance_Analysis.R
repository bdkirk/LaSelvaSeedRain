# Started on 12 May 2017
# Data exploration will follow Haldre's outline which is based on Zuur paper
# This is an analysis of effects of planted tree species on the abundance of seed rain.  The experimental unit is the plot.  We are looking at 15 plots with 4 treatment groups of planted trees.
#This will look at the abundance for all species seeds summed over a year.

################### Abundance Analysis ##########

#Load libraries
library(ggplot2); library(car); library(lsmeans); library(stats); library(lme4); library(dplyr);library(multcomp)

#Bring in data
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")
abundanalysis <- read.csv("abund_sub_notrtsp_nw.csv")
str(abundanalysis)
abundanalysis$block <- as.factor(abundanalysis$block)
str(abundanalysis) #verify block is a factor

#--------------------------------------------------------------------
######## Exploration of Plot Data #######
#This section looks at how distance, slope and aspect vary in relation to seed size.
#NOTE: Analysis did not end up including these variables.  Distance would not explain what's happening after 26 years. Aspect and slope are assumed to be similar between treatments rather than within blocks because of the experimental design.

# bring in y variables to describe plot (aspect, slope, dist from mature forest.)
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
plot(abund$total_seednum, abund$Dist)
#plot will vary directly with these covariates

#look at distance
boxplot(abund$total_seednum~abund$Dist)
#no clear trend seen here

#add in se and mean to dataset
abund_summary <- abundanalysis %>% # the names of the new data frame and the data frame to be summarised
  group_by(treatment) %>%   # the grouping variable
  summarise(mean_PL = mean(total_seednum),  # calculates the mean of each group
            sd_PL = sd(total_seednum), # calculates the standard deviation of each group
            n_PL = n(),  # calculates the sample size per group
            SE_PL = sd(total_seednum)/sqrt(n())) # calculates the standard error of each group
  
#make plot
#below does not work.
#ggplot(abund_summary, aes(treatment, mean_PL, fill= treatment))+
 # geom_boxplot()+
  #ggtitle("Total Abundance Across Treatments")+
  #geom_errorbar(abund_summary, aes(ymin=mean_PL-SE_PL, ymax=mean_PL+SE_PL), width=0.2)+
  #xlab("Planted Tree Species")+
  #ylab("Abundance")+
  #guides(fill=FALSE)

#looking for trend between distance from forest and total seed num across treatments, none found.
ggplot(abund, aes (Dist, total_seednum))+
  geom_point(aes(color = treatment))

#Log transform the data because the numbers are very large. Don't really need to do this step
#decided not to logtransform the data because it is count data that will have overdispersion that will be taken into account with 
#abundanalysis$logsum <- log(abundanalysis$total_seednum)
#str(abundanalysis)
#abundanalysis$block <- as.factor(abundanalysis$block)
#abundanalysis$treatment <- as.factor(abundanalysis$treatment)


#Look at collinearity for distance
with(abund, table(Dist, total_seednum))

#variance inflation (vif)
vif(glm(total_seednum~treatment +Dist, data = abund))

#2. Perhaps consider distance to the mature forest as a factor.
ggplot(abund, aes(Dist, total_seednum))+
  geom_point()

########### Data Exploration for specific analysis#########
##a.  Outliers in Y / Outliers in X (Step 1)
#i.	plot response and predictors to check for outliers  (only with continuous data)
#1.	Use Mydotplot or dotplot or boxplot, identify outliers
hist(abundanalysis$total_seednum)
boxplot(abundanalysis$total_seednum~ abundanalysis$treatment, xlab="treatment", ylab="seednum", main= "Seed abundance per treatment")

#No difference found when looking at logsum of abundance.
#boxplot(abundanalysis$logsum~ abundanalysis$treatment, xlab="treatment", ylab="seednum") #large controversy about whether to use log or not but generally, you do not use log for count data.

#Much more variance in hial
#one outlier in Y, hial 2 but will keep, X is categorical

##b.	Examine Zero inflation Y (Step 4)
#Not applicable for abundance question because there are no zeros when summed to the plot level and response is continuous (>0)

##c.	Collinearity X: correlation between covariates (Step 5)
#i.	Plot each predictor against each other (since categorical, will use table to make sure we have all combinations)
with(abundanalysis, table(treatment, total_seednum))

#Missing one plot, vogu1.
##good = balanced, bad = unequal sample sizes, ugly = one or more combination is missing
#check variance inflation factor
vif(glm(total_seednum~ treatment +block, data = abundanalysis))

# model shows interactions when include the treatment*block, shows collinearity
#vif was less than 10

#d.	Homogeneity of variance? (Step 2)
#Look at relationships of Y vs X’s to see if homogenous variances at each X value, linear relationships
# i.	Plot response against each predictor and random effect. 
ggplot(abundanalysis, aes(block, total_seednum, color=treatment))+
  geom_boxplot()
#more variance with Hial, minimal variance with vogu. pema and viko have a similar variance.

#e.	Independence Y - are Y's independent? (Step 8)
#1.	Is there a pattern across time or space that is not incorporated in the model? 
ggplot(abundanalysis, aes(treatment, total_seednum, color=treatment))+
  geom_boxplot()+
  facet_grid(.~block)

#Characters are not correct for the following two lines
#hist(abund$Dist)
#boxplot(abund$Dist ~ abund$total_seednum)

with(abund, ftable(treatment, Dist))
#This table is not really useful in this case

#More variance with block 2 and more variance with Hial 

#ii. Are there other potential factors that reduce independence of Y’s? 
#timing is the same.

#f.	Sufficient data?  
##As a rule of thumb, (all models), should have 15 to 20 observations for each parameter. So, if have 50 observations, should really only have 3 parameters. 
# There are 15 observations with one parameter, total_seednum

#i.	Do all levels of Island have adequate samples of at each level of Native? 	Examine interactions
#Is the quality of the data good enough to include them? (i.e. do we have enough samples for each level of the interaction?) 
with(abundanalysis, table(block, treatment))

###Katie Rey Data Visualization####
# This is an alternative way of exploring the data.
#library(dplyr); library(ggplot2)
#Find outliers
ggplot(abundanalysis, aes(treatment, total_seednum))+
  geom_point()
#Appears to be an outlier for hial2

#find out if there is any missing data:

cbPalette <- c("#999999", "#56B4E9", "#F0E442", "#CC79A7")

abundanalysis %>% 
  filter(!is.na(total_seednum)) %>%
  ggplot()+geom_bar(aes(treatment,fill=as.factor(treatment)))+
  facet_grid(plot~.)+
  scale_fill_manual(values=cbPalette)+
  theme(legend.position="none")

# one plot missing for vogu

#Non constant variance: helps to identify if all of the treatments have the same variance
ggplot(abundanalysis, aes(block, total_seednum))+
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

# Not applicable

# b. Standardize continuous predictors (if necessary)
### Why?? You would center (standardize) continuous covariates to remove correlation between slope and intercept. Centering is just putting each value around the mean, whereas standardizing is dividing by SD too. Standardizing puts all variables on the same scale (e.g. temperature, pH etc.), which can be useful for comparing effect sizes of variables, and it can help with model convergence. May be necessary with small datasets with lots of covariates, especially. 

# #in this situation, not necessary, bc no continuous predictors

###########  Analysis   ###############

#1) Does abundance of seeds differ between overstory treatments? 

##a)changed model to glmer because this will account for overdispersion with the poisson distribution.
abund.glm <- glmer(total_seednum ~ block+treatment+(1|plot), data=abundanalysis, family = poisson)

##b)plot residuals to look at homogeneity
abund.res <- resid(abund.glm) #pearson or deviance?
abund.pred <- predict(abund.glm)
# abund.pred_count <- exp(predict(abund.glm))
# r_test <- abundanalysis$total_seednum-abund.pred_count
# plot(r_test, abund.res)

plot(abund.pred, abund.res, ylab="Residuals", xlab="predicted values", main="resid vs pred") 
abline(0, 0) 

##c)plot histogram and Q-Q plot to look at normality
qqnorm(abund.res)
qqline(abund.res, col = 'red')

#the above text produces a Normal Quantile Plot and this indicates that the data is not normally distributed as the points do not line up exactly on the line.  In this case it is better to use non-parametric methods for testing.
hist(abund.res)
#skewed left (check)

##d) summary of data
anova(abund.glm, test= "F")
summary(abund.glm)

##e) getting p-values
#find Tukeys (HSD) and use for this data
#if p-value is greater than 0.05, don't need to do HSD
lsmeans(abund.glm, "treatment", contr= "pairwise")

#use for finding z scores for info below
summary(glht(abund.glm, mcp(treatment="Tukey")))

#These p-values are not t-based p-values that account for df but you can get those by using the code below:

#ptukey(Zscore*sqrt(2), nmeans=4, df=8, lower = F)

#contrast for hial-pema (-0.344)
ptukey(abs(-0.344)*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.985

#contrast for hial-viko (-0.163)
ptukey(abs(-0.163)*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.998

#contrast for hial-vogu (2.25)
ptukey(2.25*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.189

#contrast for viko-pema (0.181)
ptukey(0.181*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.9977

#contrast for vogu-pema (2.561)
ptukey(2.561*sqrt(2), nmeans= 4, df=8, lower = F)
#= 0.123

#contrast for viko-vogu (2.398)
ptukey(2.398*sqrt(2), nmeans= 4, df=8, lower = F)
#=0.154

####Have not figured out how to more efficiently export output and do a conversion.
temp <- lsmeans(abund.glm, "treatment", contr= "pairwise")


