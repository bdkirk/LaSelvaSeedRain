#Analysis of seedrain data
#bring in seedraintidy data
seedrain<-read.csv("seedraintidy.csv")

#summed across all seed rain
sumseedrain<-ddply(seedrain, .(canopysp,block), summarize, totalseed=sum(seednum))
ggplot(sumseedrain, aes(canopysp, totalseed))+
  geom_boxplot()


#diversity analysis
library(vegan)
diversity(x, index = "shannon", MARGIN = 1, base = exp(1))


#richness analysis

specnumber(x, MARGIN = 1)
#If you have margin=2 then it finds the species frequencies

#####NMDS (started 30 Mar 17)
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

#Practice plot
plot(example_NMDS)

#add text to the plot
ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

#cluster based on treatments
treat=c(rep("Treatment1",5),rep("Treatment2",5))
ordiplot(example_NMDS,type="n")
ordihull(example_NMDS,groups=treat,draw="polygon",col="grey90",label=F)
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),rep("blue",5)),
         air=0.01,cex=1.25)

#color convex hulls by treatment


# First, create a vector of color values corresponding of the 
# same length as the vector of treatment values
colors=c(rep("red",5),rep("blue",5))
ordiplot(example_NMDS,type="n")
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(example_NMDS$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),
                                            rep("blue",5)),air=0.01,cex=1.25)


