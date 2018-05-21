## clear workspace
rm(list = ls())  

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)} 

##================ import no context and motion conditions from E3a =========================================================================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github_anonymized/e3a_grouping/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data_motion <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data_motion <- (data_motion <- do.call(rbind, data_motion))
data_motion <- data_motion[data_motion$trialStruct.cond==0 | data_motion$trialStruct.cond==1,]
data_motion$trialStruct.cond <- data_motion$trialStruct.cond + 1
data_motion$experiment <- 1

#exclusions
data_motion <- subset(data_motion,(data_motion$trialStruct.comprehension == "E"))
data_motion <- subset(data_motion,(data_motion$trialStruct.displayTime < 4225 & data_motion$trialStruct.displayTime > 4175))
data_motion <- subset(data_motion, (data_motion$trialStruct.whichCar_choice == 2))
dim(data_motion) #202 (same as in E3a, obviously)

#add extra column, so that we can later rbind these data with that for e3b
data_motion$carHitMan <- 0

##================ import data from e3b ==================================================================================================================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github_anonymized/e3b_eliminating_motion/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data_static <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data_static <- (data_static <- do.call(rbind, data_static))
data_static$trialStruct.cond <- data_static$trialStruct.cond + 1

data_static$experiment <- 2

# data after comprehension exclusion
data_static <- subset(data_static,(data_static$trialStruct.comprehension == "E"))
dim(data_static) #280

#demographics
sex <- as.factor(data_static$trialStruct.sex)
table(sex)[2]/sum(table(sex))
age <- as.numeric(levels(data_static$trialStruct.age))[data_static$trialStruct.age] 
mean(age,na.rm=TRUE)

# data after which car exclusion
data_static <- subset(data_static,(data_static$trialStruct.whichCar_choice == 2)) 
dim(data_static) #264

# proportion excluded by which car measure
100 - (((280-264)/280)*100)

#add 'car hit man' codes from separate description coding
dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github_anonymized/e3b_eliminating_motion/description_coding")
descriptions <- read.csv('e3b_coded_descriptions.csv')

#before adding these codes, make sure that both data frames are ordered by participant id
data_static <- data_static[order(data_static$workerID),] 
descriptions <- descriptions[order(descriptions$workerID),] 

data_static$carHitMan <- descriptions$julian_carHitMan_codes

##======================== processing ===============================================================

data <- rbind(data_motion,data_static); dim(data) 

## assign variable names
cond <- as.factor(data$trialStruct.cond)
exp <- as.factor(data$experiment)
table(cond)
blame <- as.numeric(levels(data$trialStruct.blame_scaled))[data$trialStruct.blame_scaled]
blame_choice <- as.factor(data$trialStruct.blame_choice)

#subset of participants who didn't mention a car hitting a man
noHitData <- subset(data, data$carHitMan == 0)
noHit_exp <- as.factor(noHitData$experiment)
noHit_cond <- as.factor(noHitData$trialStruct.cond)
noHit_blame <- as.numeric(levels(noHitData$trialStruct.blame_scaled))[noHitData$trialStruct.blame_scaled]
noHit_blame_choice <- as.factor(noHitData$trialStruct.blame_choice)

##============================= plot ===================================================================== 

alpha = .05
numExps = 2
numConds = 2

blame_mat <- matrix(NA,4,7)
colnames(blame_mat) <- c('counter','exp','cond','mean','sd','n','sem') 

counter = 1
for (j in 1:numExps) {
  for (i in 1:numConds) {
    blame_mat[counter,] <- c(counter, j, i, mean(blame[cond == i & exp == j]),sd(blame[cond==i & exp == j]), length(blame[cond == i & exp == j]),0) 
    blame_mat[counter,7] <- blame_mat[counter,5]/sqrt(blame_mat[counter,6]) 
    counter = counter + 1
  }
}

blame.summary <- as.data.frame(blame_mat, stringsAsFactors=F)
condNames <- c("Motion","Static")

title <- c('Blame Judgments') 
p1<-ggplot(blame.summary,aes(x=factor(exp),y=mean,fill=factor(cond)))+
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1, 8)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")

##============================= analysis: blame scaled ======================================================================== 

#blame scaled
scaled_mod <- aov(blame ~ cond*exp)
summary(scaled_mod)
etaSquared(scaled_mod)

#replicate analysis with subset of participants who didn't mention a car hitting the man
noHit_scaled_mod <- aov(noHit_blame ~ noHit_cond*noHit_exp)
summary(noHit_scaled_mod)
etaSquared(noHit_scaled_mod)

##============================= plot: blame choice ======================================================================== 

blameTable <- matrix(NA,4)
condNames <- c('no context-m','grouping-m','no context-s', 'grouping-s')

blameTable[1] <- length(blame_choice[exp==1 & cond==1 & blame_choice==1])/length(blame_choice[exp==1 & cond==1]) 
blameTable[2] <- length(blame_choice[exp==1 & cond==2 & blame_choice==1])/length(blame_choice[exp==1 & cond==2])
blameTable[3] <- length(blame_choice[exp==2 & cond==1 & blame_choice==1])/length(blame_choice[exp==2 & cond==1])
blameTable[4] <- length(blame_choice[exp==2 & cond==2 & blame_choice==1])/length(blame_choice[exp==2 & cond==2])

barplot(blameTable, beside=TRUE,ylim=c(0,1.0),
        legend.text=TRUE, main="Proportion Blaming Driver of Red Car", cex.axis=1.0, cex.main = 1.5,names = condNames,ylab="Proportion")

##============================= analysis: blame choice ======================================================================== 

blame_mod <- glm(blame_choice ~ cond*exp, family = binomial)
summary(blame_mod)

#replicate analysis with subset of participants who didn't mention a car hitting the man
noHit_blame_mod <- glm(noHit_blame_choice ~ noHit_cond*noHit_exp, family = binomial)
summary(noHit_blame_mod)

## end ====================================================================================================================