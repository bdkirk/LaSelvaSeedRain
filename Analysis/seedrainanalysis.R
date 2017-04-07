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
#This gives the number of columns
ncol(seedrain)
#This shows what those columns will look like
str(seedrain[,1:10])


#This gives abundance per trap [may need to change so this is abundance per canopy sp]
seedabundance <- as.data.frame(rowSums(seedrain[,-c(1:6)]))
str(seedabundance)
View(seedabundance)

#This gives diversity per trap [need to change so this is diversity per canpy sp]
seeddiversity <- as.data.frame(diversity(seedrain[,-c(1:6)], index="shannon"))
#check to see what these look like
str(seeddiversity)
View(seeddiversity)

#species richness analysis
seedrich <- as.data.frame(specnumber(seedrain[,-c(1:6)]))
View(seedrich)

#If you have margin=2 then it finds the species frequencies
seedrain2 <- seedrain

#rename the columns with more meaningful names and  put them in the other dataset
seedrain2$Abundance <- seedabundance$`rowSums(seedrain[, -c(1:6)])`
seedrain2$Diversity <- seeddiversity$`diversity(seedrain[, -c(1:6)], index = "shannon")`
seedrain2$Richness <- seedrich$`specnumber(seedrain[, -c(1:6)])`
str(seedrain2[,141:147])

#write as a csv
write.csv(seedrain2, "div_ab_rich.csv")

#aov is an anova calculation of the diversity across the four canopy species
summary(aov(Diversity ~ canopysp, data = seedrain2))

###This is not the appropriate analysis but it does show significance between treatment and diversity
pairwise.t.test(seedrain3$Diversity, seedrain2$canopysp)


###FIGURE OUT HOW TO PLOT WITH TWO DIFFERENT VARIABLES i.e.canopy and block where it is averaged within the canopy for the block
#plot of diversity
library(ggplot2)
ggplot(seedrain2, aes(canopysp, Diversity))+
  geom_violin()

#plot of abundance
ggplot(seedrain2, aes(canopysp, Abundance))+
  geom_boxplot()

#plot of richness
ggplot(seedrain2, aes(canopysp, Richness))+
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


