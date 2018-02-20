# Started file on 19 Feb 2018

# this will look at the top ten species for the 12 months

#load libraries
library(plyr); library(ggplot2); library(readr); library(reshape2); library(BiodiversityR); library(dplyr); library(scales); library(base); library(tidyr); library(vegan)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#read data
seeds <- read.csv("yearsub_no_trtsp_nw.csv", header = TRUE)

#Rank abundance.  1) Does this differ across treatments?  2) Add in seed species traits (dispersal and life form) 3)

#summarise the data by plot
seeds_sum<- ddply(seeds, .(plot, species), summarise, seednum=sum(total_seednum))

# Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
seeds_data <- dcast(seeds_sum, plot ~ species, value.var="seednum")
#This sorts the data after it is created
seeds_data <- seeds_data[,c(names(seeds_data)[1],sort(names(seeds_data)[2:ncol(seeds_data)]))]
#identifies all seed rain species that are NA
seeds_data <- seeds_data[, -which(names(seeds_data)=="NA")]
#Replace NA's with zeros
seeds_data[is.na(seeds_data)] <- 0

head(seeds_data)

#do a rank abundance plot to see which seeds have the most
x <- seeds_data[,2:119]
y <- seeds_data[,1]
y <- as.data.frame(y)
names(y)[names(y) == "y"] <- "plot"
y <- separate(y, col=plot, into=c("treatment", "block"), remove=F, sep= -1)
str(y)
y$treatment <- as.factor(y$treatment)

seeds_rankdata <- rankabundance(x, y="y",factor="species", "plot", t=qt(0.975,df=14))

seeds_rankdata <- as.data.frame(seeds_rankdata)

seeds_rankdata2 <- seeds_rankdata %>% mutate(species = row.names(seeds_rankdata))

write.csv(seeds_rankdata2, "seeds_rankdata.csv", row.names = FALSE)

seeds_rankplot <- rankabunplot(seeds_rankdata, addit=FALSE, labels="species",scale="abundance", specnames=c(1:5), type="o")

# look across treatments
seeds_rankcomp <- rankabuncomp(x, y= y, factor='treatment', scale ='abundance')

seeds_rankcomp <- rankabuncomp(x, y= y, factor='treatment', scale ='proportion')

############### Treatment rather than plot-- no differences found####

# Look at from a treatment perspective. I think this will make things vary a little.

#summarise the data by treatment
trt_sum<- ddply(seeds, .(treatment, species), summarise, seednum=sum(total_seednum))

# Create species columns to have data in wide format
#dcast makes this long data go wide.  You specify the dcast(datafile, columns + you  + want + to + stay + long ~column you want to go wide, value.var="column you want to be the variable")
trt_data <- dcast(trt_sum, treatment ~ species, value.var="seednum")
#This sorts the data after it is created
trt_data <- trt_data[,c(names(trt_data)[1],sort(names(trt_data)[2:ncol(trt_data)]))]
#identifies all seed rain species that are NA
trt_data <- trt_data[, -which(names(trt_data)=="NA")]
#Replace NA's with zeros
trt_data[is.na(trt_data)] <- 0

head(trt_data)

#do a rank abundance plot to see which seeds have the most
a <- trt_data[,2:119]
b <- trt_data[,1]
b <- as.matrix.data.frame(b)
names(b)[names(b) == "b"] <- "treatment"
b$treatment <- as.factor(b$treatment)

trt_rankdata <- rankabundance(a, y="b",factor="species", "treatment", t=qt(0.975,df=14))

write.csv(trt_rankdata, "trt_rankdata.csv", row.names = FALSE)

trt_rankplot <- rankabunplot(trt_rankdata, addit=FALSE, labels="species",scale="abundance", specnames=c(1:10), type="o")

trt_rankcomp <- rankabuncomp(a, y= "b", factor= "treament", scale ="abundance", type ="o", rainbow = T, legend = T)


###### EXAMPLE ######
library(vegan)
data(dune.env)
data(dune)
RankAbun.1 <- rankabundance(dune)
RankAbun.1
rankabunplot(RankAbun.1,scale='abundance', addit=FALSE, specnames=c(1,2,3))
rankabuncomp(dune, y=dune.env, factor='Use', 
             scale='proportion', legend=FALSE)




# Match in Ecology file for top ten species
ecology <- read.csv("seedtraits_tidy_sub_notrt_nw.csv", header = TRUE)

# get subset of top ten seed species 

#1miconia affinis
#2piper colonense
#3miconia multispicata
#4alchorneopsis floribunda
#5piper auritifolium
#6piptocarpha poeppigiana
#7clidemia japurensis
#8cecropia obtusifolia
#9sabicea villosa
#10piper multiplinervium

topspecies <- c("miconia affinis", "piper colonense", "miconia multispicata", "alchorneopsis floribunda", "piper auritifolium", "piptocarpha poeppigiana", "cecropia obtusifolia", "clidemia japurensis", "sabicea villosa", "piper multiplinervium")


ecology_top <- filter(ecology, species %in% topspecies)

ecology_top_abund <- right_join(seeds_rankdata2, ecology_top, by = "species")

ecology_top_abund2 <- subset(ecology_top_abund, select =c(1:3 & 9:11))

ecology_top_abund2 <- subset(ecology_top_abund, select =c("rank","abundance", "proportion", "species", "dispersal", "lifeform"))

write.csv(ecology_top_abund2, "tidy_ecology_top_abund.csv", row.names = FALSE)
