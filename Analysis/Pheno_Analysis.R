## Phenology Analysis
## began on 2 June 17


#Start with visually looking at average monthly abundance across top 10 species
#1miconia affinis
#2piper colonense
#3miconia multispicata
#4alchorneopsis floribunda
#5piper auritifolium
#6piptocarpha poeppigiana
#7cecropia obtusifolia
#8clidemia japurensis
#9sabicea villosa
#10piper multiplinervium


#load libraries
library(plyr); library(ggplot2); library(readr); library(reshape2); library(BiodiversityR); library(dplyr); library(scales); library(base)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#read data
pheno <- read.csv("seedrain_all_tidy.csv")

########Create monthly summaries for the plots######
pheno$date <- as.Date(pheno$date, format = ("%Y-%m-%d"))
pheno$month <- as.Date(cut(pheno$date, breaks = "month"))

#create a monthly seed species, plot total
month_plot <- ddply(pheno, .(plot, species, month), summarise, seednum=sum(total_seednum))

#create a monthly seed species, treatment total
month_treatment <- ddply(pheno, .(canopysp, species, month), summarise, seednum=sum(total_seednum))


#create a monthly seed species, block total
month_block <- ddply(pheno, .(block, canopysp, species, month), summarise, seednum=sum(total_seednum))

#####using summaries to make graphics######

#1)miconia affinis
miaf <- filter(month_plot, species == "miconia affinis")

ggplot(data = miaf, aes(month, seednum, color = plot)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar")+ # or "line"
  scale_x_date(date_breaks = "1 month", labels=date_format ("%b-%Y")) # custom x-axis labels

#plots the totals for each plot compared side by side
ggplot(data = miaf, aes(month, seednum)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar")+ # or "line"
  facet_wrap(~plot, scales = "free_y")


#2)top ten
topspecies <- c("miconia affinis", "piper colonense", "miconia multispicata", "alchorneopsis floribunda", "piper auritifolium", "piptocarpha poeppigiana", "cecropia obtusifolia", "clidemia japurensis", "sabicea villosa", "piper multiplinervium")
               
topten <- filter(month_plot, species %in% topspecies)

ggplot(data = topten, aes(month, seednum, color = species)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "bar")+ # or "line"
  scale_x_date(date_breaks = "1 month", labels=date_format ("%b-%Y")) # custom x-axis labels

#plots the totals for each plot compared side by side
ggplot(data = topten, aes(month, seednum)) +
  stat_summary(fun.y = sum, # adds up all observations for the month
               geom = "line")+ # or "line"
  facet_wrap(~plot, scales = "free_y")

#try to plot monthly totals for each species within each plot
ggplot(data = topten, aes(month, seednum), fill= species) +
  facet_grid(species~plot, scales = "free_y")+
  geom_bar(position = "dodge")
