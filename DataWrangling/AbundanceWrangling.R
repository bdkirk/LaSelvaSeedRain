#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species abundance can be found
# Abundance is defined as the total number of seeds found in each plot.  Will have 15 observations
#Experimental design: RCBD, C. Total df = 14


#Load libraries
library(readr); library(plyr); library(ggplot2); library(reshape2)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#naming datafile seedrain from excel file
abundance <- read.csv("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData/abundance_plot.csv")

str(abundance)
abund <- ddply(abundance, .(plot), summarise, total_seednum=sum(seednum))

write.csv (abund, "abund_analysis.csv", row.names = FALSE)



#######KEEP EDITING BELOW

sr_final <- sr_final_tidy
#still working on figuring this out
sr_final$plot <- NA

#Create list of plot numbers
plot_numbers <- seq(1:75)
#Create list of plots with canopysp_block combo
hial1 <- c(1, 2, 3, 4, 5)
viko1 <- c(6, 7, 8, 9, 10)
pema1 <- c(11, 12, 13, 14, 15)
hial2 <- c(16, 17, 18, 19, 20)
viko2 <- c(21, 22, 23, 24, 25)
pema2 <- c(26, 27, 28, 29, 30)
vogu2 <- c(31, 32, 33, 34, 35)
hial3 <- c(36, 37, 38, 39, 40)
viko3 <- c(41, 42, 43, 44, 45)
pema3 <- c(46, 47, 48, 49, 50)
vogu3 <- c(51, 52, 53, 54, 55)
hial4 <- c(56, 57, 58, 59, 60)
viko4 <- c(61, 62, 63, 64, 65)
pema4 <- c(66, 67, 68, 69, 70)
vogu4 <- c(71, 72, 73, 74, 75)

#Assign plots to traps (15 plots)
sr_final$plot[sr_final$trap %in% hial1] <- "hial1"
sr_final$plot[sr_final$trap %in% hial2] <- "hial2"
sr_final$plot[sr_final$trap %in% hial3] <- "hial3"
sr_final$plot[sr_final$trap %in% hial4] <- "hial4"
sr_final$plot[sr_final$trap %in% pema1] <- "pema1"
sr_final$plot[sr_final$trap %in% pema2] <- "pema2"
sr_final$plot[sr_final$trap %in% pema3] <- "pema3"
sr_final$plot[sr_final$trap %in% pema4] <- "pema4"
sr_final$plot[sr_final$trap %in% viko1] <- "viko1"
sr_final$plot[sr_final$trap %in% viko2] <- "viko2"
sr_final$plot[sr_final$trap %in% viko3] <- "viko3"
sr_final$plot[sr_final$trap %in% viko4] <- "viko4"
sr_final$plot[sr_final$trap %in% vogu2] <- "vogu2"
sr_final$plot[sr_final$trap %in% vogu3] <- "vogu3"
sr_final$plot[sr_final$trap %in% vogu4] <- "vogu4"

#Practice summarize functions
sr_sum <- ddply(sr_final, .(plot, species), summarise, total_seednum=sum(seednum))
