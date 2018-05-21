## clear workspace
rm(list = ls()) 

## necessary libraries
if (!require(psych)) {install.packages("psych"); require(psych)}
if (!require(fmsb)) {install.packages("fmsb"); require(fmsb)}
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}

##================ import description codes ====================================================================================

dir <- setwd("/Users/Julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/e1_illusory_wrongs/description_coding")
data <- read.csv('e1_coded_descriptions.csv')
labels(data)

pt_causal_codes <- data$pt_causal_code
jdf_causal_codes <- data$jdf_causal_code
causal_agreement <- data$causal_agreement

pt_realistic_codes <- data$pt_realistic_code
jdf_realistic_codes <- data$jdf_realistic_code
realistic_agreement <- data$realistic_agreement

##=================== causal codes ==============================

#agreement between raters (cohen's kappa)
Kappa.test(pt_causal_codes, jdf_causal_codes)

#number of sentences where raters agreed
causal <- data$CAUSAL[causal_agreement == 1]
blame <- data$blame_scaled[causal_agreement == 1]
length(causal) #375

#proportion of sentences rated causal
length(causal[causal == 1])/length(causal)

#t-test: causal vs. non-causal sentence on average blame
tapply(blame, causal, mean)
tapply(blame, causal, sd)
tapply(blame, causal, length)

t.test(blame ~ causal, var.equal=TRUE, paired=FALSE)
tes(-25.173,111,264) #2.85

##=================== 'realistic' codes ===========================

#agreement between raters (cohen's kappa)
Kappa.test(pt_realistic_codes, jdf_realistic_codes) #1.00

#proportion that were unrealistic
realistic <- data$REALISTIC[realistic_agreement == 1]
blame <- data$blame_scaled[realistic_agreement == 1]

length(realistic[realistic == 0])/length(realistic)





