#Analysis of seedrain data
##attempts started 1 April 2017
#load libraries
library(ggplot2)
library(vegan)

###Find the diversity and the abundance
#set wd
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")
#bring in NMDS data
seedrain<-read.csv("species_data.csv")
str(seedrain[,140:144])
#This gives the number of columns
ncol(seedrain)

##Already had canopysp in the dataset with block
#bring in another dataset to match
#seedrain2 <- read.csv("seedrain_alltidy.csv")

#This adds in canopy species by matching them with the trap from the other data
#original$column <- new$column[match(original$column, new$column)]
#seedrain$canopysp <- seedrain2$canopysp[match(seedrain$trap, seedrain2$trap)]
#str(seedrain[,140:144])
#Makes a data frame for the sum of each row which will then be added as a column in the dataset
seedabundance <- as.data.frame(rowSums(seedrain[,-c(1:4,143,144)]))
str(seedabundance)
View(seedabundance)

#Creates a dataframe of diversity of all species and excludes the grouping factors or rows
seeddiversity <- as.data.frame(diversity(seedrain[,-c(1:4,143,144)], index="shannon"))
#check to see what these look like
str(seeddiversity)
View(seeddiversity)

#species richness analysis
seedrich <- as.data.frame(specnumber(seedrain[,-c(1:4,143,144)]))
#If you have margin=2 then it finds the species frequencies

#bind these columns into one dataset because they have the same number of rows
seedrain3 <- cbind(seedrain, seedabundance, seeddiversity, seedrich)
str(seedrain3[,141:147])

#This is not necessary as part of the analysis but keeps the dataset available for future use/tweaking
#Create a new object based off the original object
#seedrain4 <- seedrain

#rename the columns with more meaningful names
seedrain4$Abundance <- seedabundance$`rowSums(seedrain[, -c(1:4, 143, 144)])`
seedrain4$Diversity <- seeddiversity$`diversity(seedrain[, -c(1:4, 143, 144)], index = "shannon")`
seedrain4$Richness <- seedrich$`specnumber(seedrain[, -c(1:4, 143, 144)])`
str(seedrain4[,141:147])

#write as a csv
write.csv(seedrain4, "div_ab_rich.csv")

#aov is an anova calculation of the diversity across the four canopy species
summary(aov(Diversity ~ canopysp, data = seedrain4))

###This is not the appropriate analysis but it does show significance between treatment and diversity
pairwise.t.test(seedrain4$Diversity, seedrain4$canopysp)


###FIGURE OUT HOW TO PLOT WITH TWO DIFFERENT VARIABLES i.e.canopy and block where it is averaged within the canopy for the block
#plot of diversity
library(ggplot2)
ggplot(seedrain4, aes(canopysp, Diversity))+
  geom_violin()

#plot of abundance
ggplot(seedrain4, aes(canopysp, Abundance))+
  geom_boxplot()

#plot of richness
ggplot(seedrain4, aes(canopysp, Richness))+
  geom_violin()


###NEED TO ADD IN TOTAL_SUM, this is not known.
#summed across all seed rain
sumseedrain<-ddply(seedrain, .(canopysp,block), summarize, totalseed=sum(total_seednum))
ggplot(sumseedrain, aes(canopysp, totalseed))+
  geom_violin()


#####NMDS (started 30 Mar 17)
#This analysis is done in the permanova_nmds data file
#install.packages("vegan")
library(vegan)
set.seed(2)
community_matrix<-matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))

example_NMDS<-metaMDS(community_matrix, # Our community-by-species matrix
                      k=2) # The number of reduced dimensions, k is the number of dimensions
example_NMDS<-metaMDS(community_matrix,k=2,trymax=100)
#Shepard plot, which shows scatter around the regression between the interpoint distances in the final configuration (i.e., the distances between each pair of communities) against their original dissimilarities.
stressplot(example_NMDS)


