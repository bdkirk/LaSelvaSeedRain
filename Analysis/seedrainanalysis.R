#Analysis of seedrain data
#bring in seedraintidy data
seedrain<-read.csv("seedraintidy.csv")

#summed across all seed rain
sumseedrain<-ddply(seedrain, .(canopysp,block), summarize, totalseed=sum(seednum))
ggplot(sumseedrain, aes(canopysp, totalseed))+
  geom_boxplot()


#diversity analysis



#abundance analysis