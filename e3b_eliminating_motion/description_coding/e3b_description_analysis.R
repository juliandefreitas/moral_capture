## clear workspace
rm(list = ls()) 

## necessary libraries
if (!require(psych)) {install.packages("psych"); require(psych)}
if (!require(fmsb)) {install.packages("fmsb"); require(fmsb)}
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}

##================ import description codes =====================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/moral_capture/e3b_eliminating_motion/description_coding")
data <- read.csv('e3b_coded_descriptions.csv')
labels(data)

manUncertainty_codes <- data$julian_manUncertainty_codes
carHitMan_codes <- data$julian_carHitMan_codes

##=================== proportions ==============================

#proportion of people who were uncertain what happened to the man at the end
(sum(manUncertainty_codes)/length(manUncertainty_codes))*100

#proportion of people who clearly indicated that they saw a car hit the man
(sum(carHitMan_codes)/length(manUncertainty_codes))*100

##end ==========================================================





