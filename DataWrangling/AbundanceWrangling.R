#Started on 11 May 2017
# This involves wrangling of data so that tree effects on species abundance can be found
# Abundance is defined as the total number of seeds found in each plot.  Will have 15 observations
#Experimental design: RCBD, C. Total df = 14


#Load libraries
library(readr); library(plyr); library(ggplot2); library(reshape2); library(tidyr); library(dplyr)

#set working directory to folder
setwd("~/M.S. Thesis/Data/GitHubProjects/LaSelvaSeedRain/Data/TidyData")

#naming datafile seedrain from excel file
abundance <- read.csv("yearsub_no_trtsp_nw.csv")

#sums up by treatment, block and species to look at what's going on within each block and treatment combo
block_abund <- ddply(abundance, .(treatment, block, species), summarise, seednum=sum(total_seednum))
write.csv(block_abund, "trt_blk_spp_abund.csv", row.names= FALSE)

#sums up by treatment, block, trap, species
trap_abund <- ddply(abundance, .(treatment, block, trap, species), summarise, seednum=sum(total_seednum))
write.csv(trap_abund, "trt_blk_trap_spp_abund.csv", row.names = FALSE)

#This sums up the plots according to species for abundance calculations
str(abundance)
abund <- ddply(abundance, .(plot), summarise, total_seednum=sum(total_seednum))

#add in columns for block and treatment
abund2 <- separate(abund, col=plot, into=c("treatment", "block"), remove=F, sep= -1)

## Add in a seed density component.####
abund2$density <- ((abund2$total_seednum)/(2.6))

# these calculations should use different numbers based on the number of traps per plot so we will use 0.52 * 5 traps = 2.6 m2 and because all plots have the same number of traps, we can justify just all total seed number values by 2.6

write.csv(abund2, "abund_sub_notrtsp_nw.csv", row.names=FALSE)

# finished on 15 May 2017 and then on 5 March 2018

### Add in mean, sd and se for figure in sigmaplot 26 Feb 18 ####
abund3 <- ddply(abund2, c("treatment"), summarise, mean=mean(total_seednum), sd= sd(total_seednum), SE = (sd(total_seednum)/sqrt(length(total_seednum))))

write.csv(abund3, "abund_sd_se_nw.csv")

#### get data for summary table ####

##check to see how many species should actually be in summary table 
seedrain_plot <- ddply(abundance, .(plot, species), summarise, seednum=sum(total_seednum))
seedrain_treatment <- ddply(abundance, .(treatment, species), summarise, seednum=sum(total_seednum))

#spread the data so that 
abund_summary <- spread(seedrain_treatment, treatment, seednum)

#add in zeros for NA
abund_summary[is.na(abund_summary)] <- 0
#this works but will not change the column/unknowns to zero


#add seeds per m2/year to table
#  this number was calculated by taking area of traps (0.52)m2 and multiplying by number of traps per treatment which was 20, 20, 20 and 15.
#a) first add in other columns
abund_summary$Hial_R <- (abund_summary$Hial)/(10.4)
abund_summary$Pema_R <- (abund_summary$Pema)/(10.4)
abund_summary$Viko_R <- (abund_summary$Viko)/(10.4)
abund_summary$Vogu_R <- (abund_summary$Vogu)/(7.8)
abund_summary$spp_total <- abund_summary$Hial+abund_summary$Pema + abund_summary$Viko + abund_summary$Vogu
write.csv(abund_summary, "abund_summary_year_notrtsp_nw.csv", row.names=FALSE)

#####Notes
# Redone on 22 Jan 18 because names needed to be changed.

# redone on 2 Feb 18 to add in columns for block and treatment

# redone on 12 Feb 18 to correct species names that were not corrected and removed three woody species.

# redone on 3 March to have density as part of the analysis


### get the monthly totals for plotting ####
monthtotal <- abundance
monthtotal$date <- as.Date(monthtotal$date, format = ("%Y-%m-%d"))
monthtotal$month <- as.Date(cut(monthtotal$date, breaks = "month"))

month_plot <- ddply(monthtotal, .(plot, month), summarise, seednum=sum(total_seednum)) # Not useful now. could use later if we want to go by a plot basis rather than a treatment basis in which case, you would have to follow similar protocol as above.

month_treatment2 <- ddply(monthtotal, .(month, treatment), summarise, seednum=sum(total_seednum))


#spread the data so that 
month_treatment3 <- spread(month_treatment2, treatment, seednum)

#get the density included
month_treatment3$Hial_D <- (month_treatment3$Hial)/(10.4)
month_treatment3$Pema_D <- (month_treatment3$Pema)/(10.4)
month_treatment3$Viko_D <- (month_treatment3$Viko)/(10.4)
month_treatment3$Vogu_D <- (month_treatment3$Vogu)/(7.8)
month_treatment3$spp_total <- month_treatment3$Hial+month_treatment3$Pema + month_treatment3$Viko + month_treatment3$Vogu

write.csv(month_treatment3, "month_treatment_sub_notrt_nw.csv", row.names=FALSE)


##################################################################################
 #  Do the same thing but without subsetting for the year but with conspecific treatment seeds removed from plots.  This is justification for taking out some of the seeds


month_all <- read.csv("seedrain_notrtsp_tidy_nw.csv")

month_all$date <- as.Date(month_all$date, format = ("%Y-%m-%d"))
month_all$month <- as.Date(cut(month_all$date, breaks = "month"))

month_plot_all <- ddply(month_all, .(plot, month), summarise, seednum=sum(total_seednum)) # Not useful now. could use later if we want to go by a plot basis rather than a treatment basis in which case, you would have to follow similar protocol as above.

month_all2 <- ddply(month_all, .(month, treatment), summarise, seednum=sum(total_seednum))


#spread the data so that 
month_all3 <- spread(month_all2, treatment, seednum)

#get the density included
month_all3$Hial_D <- (month_all3$Hial)/(10.4)
month_all3$Pema_D <- (month_all3$Pema)/(10.4)
month_all3$Viko_D <- (month_all3$Viko)/(10.4)
month_all3$Vogu_D <- (month_all3$Vogu)/(7.8)
month_all3$spp_total <- month_all3$Hial+month_all3$Pema + month_all3$Viko + month_all3$Vogu

write.csv(month_all3, "month_all_notrt_nw.csv", row.names=FALSE)


# Plotting density across months
month_all4 <- subset(month_all3, select = c(1, 6, 7, 8, 9))
month_all5 <- melt(month_all4, id=c("month"))
str(month_all5)
month_all5$date <- as.Date(month_all5$month, format = ("%b%Y")) # Not working
month_all5$month <- as.character(month_all5$month)

ggplot(month_all5, aes(month, value, fill = variable))+
  geom_bar(position = "dodge", stat = "identity")+
  labs( x= "Months", y= "Seed Density (seeds m-2 year-1)")+
  scale_fill_discrete(name = "Overstory Tree Species", labels= c("Hieronyma", "Pentaclethra", "Virola", "Vochysia"))+
  theme_bw()+
  theme(axis.text.x= element_text(angle = 65, hjust =1))+
  scale_fill_grey()

# line rather than bar # not working well
ggplot(month_all5, aes(month, value, color = variable))+
  geom_point()+
  geom_path()+
  labs( x= "Months", y= "Seed Density (seeds/m2 year-1)")+
  scale_fill_discrete(name = "Treatment", labels= c("Hieronyma", "Pentaclethra", "Virola", "Vochysia"))+
  theme(axis.text.x= element_text(angle = 65, hjust =1))
