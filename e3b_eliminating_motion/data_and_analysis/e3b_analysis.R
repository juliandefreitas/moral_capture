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
if (!require(lsmeans)) {install.packages("lsmean"); require(lsmeans)}

##================ import no context motion baseline from E3a =========================================================================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github_anonymized/e3a_grouping/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data_motion_baseline <- (data_motion_baseline <- do.call(rbind, data))
data_motion_baseline <- data_motion_baseline[data_motion_baseline$trialStruct.cond==0,]
data_motion_baseline$trialStruct.cond <- 2

#exclusions
data_motion_baseline <- subset(data_motion_baseline,(data_motion_baseline$trialStruct.comprehension == "E"))
data_motion_baseline <- subset(data_motion_baseline,(data_motion_baseline$trialStruct.displayTime < 4225 & data_motion_baseline$trialStruct.displayTime > 4175))
data_motion_baseline <- subset(data_motion_baseline,(data_motion_baseline$trialStruct.whichCar_choice == 2))

dim(data_motion_baseline) #89 (same as in E3a, obviously)

#add extra column, so that we can later rbind these data with that for e3b
data_motion_baseline$carHitMan <- 0

##================ import data for E3b ==================================================================================================================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github_anonymized/e3b_eliminating_motion/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

# num ss recruited
dim(data) #283

# data after comprehension exclusion
data <- subset(data,(data$trialStruct.comprehension == "E"))
dim(data) #280

#demographics
sex <- as.factor(data$trialStruct.sex)
table(sex)[2]/sum(table(sex))
age <- as.numeric(levels(data$trialStruct.age))[data$trialStruct.age] 
mean(age,na.rm=TRUE)

# data after which car exclusion
data <- subset(data,(data$trialStruct.whichCar_choice == 2)) 
dim(data) #264

# proportion excluded by which car measure
100 - (((280-264)/280)*100)

###add 'car hit man' codes from separate description coding
dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github_anonymized/e3b_eliminating_motion/description_coding")
descriptions <- read.csv('e3b_coded_descriptions.csv')

#before adding these codes, make sure that both data frames are ordered by participant id
data <- data[order(data$workerID),] 
descriptions <- descriptions[order(descriptions$workerID),] 

data$carHitMan <- descriptions$julian_carHitMan_codes

##======================== processing ===============================================================

data <- rbind(data,data_motion_baseline); dim(data)

numConds = 1

## assign variable names
cond <- as.factor(data$trialStruct.cond)
table(cond)
blame <- as.numeric(levels(data$trialStruct.blame_scaled))[data$trialStruct.blame_scaled]
blame_choice <- as.factor(data$trialStruct.blame_choice)
descrip <- data$trialStruct.descrip

#subset of participants who didn't mention a car hitting a man
noHitData <- subset(data, data$carHitMan == 0)
noHit_cond <- as.factor(noHitData$trialStruct.cond)
noHit_blame <- as.numeric(levels(noHitData$trialStruct.blame_scaled))[noHitData$trialStruct.blame_scaled]
noHit_blame_choice <- as.factor(noHitData$trialStruct.blame_choice)

#baseline
baseline_mean <- mean(blame[cond == 0]); baseline_mean
baseline_sd <- sd(blame[cond==0]); baseline_sd
baseline_length <- length(blame[cond == 0]); baseline_length
baseline_sem <- baseline_sd/sqrt(baseline_length); baseline_sem
baseline_add_sem <- baseline_mean + baseline_sem
baseline_minus_sem <- baseline_mean - baseline_sem

##============================= plot ===================================================================== 

alpha = .05;

blame_mat <- matrix(NA,numConds,5)
colnames(blame_mat) <- c('cond','mean','sd','n','sem') 

for (i in 1:numConds) {
  blame_mat[i,] <- c(i,mean(blame[cond == i]),sd(blame[cond==i]), length(blame[cond == i]),0) 
  blame_mat[i,5] <- blame_mat[i,3]/sqrt(blame_mat[i,4]) 
}

blame.summary <- as.data.frame(blame_mat, stringsAsFactors=F)
condNames <- c("Many Logs")

group.colors <- c("1" = "gray")

## bar plot - blame scaled
title <- c('Blame Judgments') 
p1<-ggplot(blame.summary,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  scale_fill_manual(values=group.colors) +
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1, 8)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean") + geom_hline(yintercept = baseline_add_sem) + 
  geom_hline(yintercept = baseline_minus_sem) + geom_hline(yintercept = baseline_mean, lty=5)

## bar plot - blame mcq
blameTable <- matrix(NA,2)

blameTable[1] <- length(blame_choice[cond==1 & blame_choice==1])/length(blame_choice[cond==1]) #many logs
blameTable[2] <- length(blame_choice[cond==0 & blame_choice==1])/length(blame_choice[cond==0]) #no context 

barplot(blameTable[1], beside=TRUE,ylim=c(0,1.0), col=group.colors,
        legend.text=TRUE, main="Proportion Blaming Driver of Red Car", cex.axis=1.0, cex.main = 1.5,names = condNames,ylab="Proportion")
abline(a= blameTable[2], b = 0)

##blame table for subset of participants who didn't mention a car hitting th eman
noHit_blameTable <- matrix(NA, 2)
noHit_blameTable[1] <- length(noHit_blame_choice[noHit_cond==1 & noHit_blame_choice==1])/length(noHit_blame_choice[noHit_cond==1]) #many logs
noHit_blameTable[2] <- length(noHit_blame_choice[noHit_cond==0 & noHit_blame_choice==1])/length(noHit_blame_choice[noHit_cond==0]) #no context 

##============================= analysis: blame scaled ======================================================================== 
#conds
# 0 = interrupted, no context; 1 = interrupted, motion; 2 = no context, motion

# num subjects in each cond
tapply(blame,cond,length)
tapply(blame,cond,mean)
tapply(blame,cond,sd)

# interrupted motion v interrupted no context
t.test(blame[cond == 1 | cond == 0] ~ cond[cond == 1 | cond == 0], var.equal=TRUE, paired=FALSE)
tes(-1.3754, 137, 127) #0.17

# interrupted motion v no context motion  
t.test(blame[cond == 1 | cond == 2] ~ cond[cond == 1 | cond == 2], var.equal=TRUE, paired=FALSE)
tes(1.0123, 137, 89) #d=0.14

#[see separate script for experiment* condition interaction]

### replicate analysis with subset of participants who didn't mention a car hitting the man

# num subjects in each cond
tapply(noHit_blame,noHit_cond,length)
tapply(noHit_blame,noHit_cond,mean)
tapply(noHit_blame,noHit_cond,sd)

# interrupted motion v interrupted no context
t.test(noHit_blame[noHit_cond == 1 | noHit_cond == 0] ~ noHit_cond[noHit_cond == 1 | noHit_cond == 0], var.equal=TRUE, paired=FALSE)
tes(-0.90041, 59, 38) #0.19

# interrupted motion v no context motion  
t.test(noHit_blame[noHit_cond == 1 | noHit_cond == 2] ~ noHit_cond[noHit_cond == 1 | noHit_cond == 2], var.equal=TRUE, paired=FALSE)
tes(1.0102, 59, 89) #d=0.17

##============================= analysis: blame mcq ======================================================================== 

#[see separate script for experiment* condition interaction]
blameTable
blame_noContext_motion <- length(blame_choice[cond==2 & blame_choice==1])/length(blame_choice[cond==2]); blame_noContext_motion #proportion for E3a no context cond

blameChoiceTable <- table(blame_choice,cond); blameChoiceTable

# many logs v no context
length(cond[cond == 1 | cond == 0])
chisq.test(cond[cond == 1 | cond == 0], blame_choice[cond == 1 | cond == 0])
chisq.test(blameChoiceTable[,1:2])
cramersV(blameChoiceTable[,1:2])

# many logs v no context motion
length(cond[cond == 1 | cond == 2])
chisq.test(cond[cond == 1 | cond == 2], blame_choice[cond == 1 | cond == 2])
chisq.test(blameChoiceTable[,c(2,3)])
cramersV(blameChoiceTable[,c(2,3)])

######## replicate analysis with subset of participants who didn't mention a car hitting the man
noHit_blameTable

noHit_blameChoiceTable <- table(noHit_blame_choice,noHit_cond); noHit_blameChoiceTable

# many logs v no context
length(noHit_cond[noHit_cond == 1 | noHit_cond == 0])
chisq.test(noHit_cond[noHit_cond == 1 | noHit_cond == 0], noHit_blame_choice[noHit_cond == 1 | noHit_cond == 0])
chisq.test(noHit_blameChoiceTable[,1:2])
cramersV(noHit_blameChoiceTable[,1:2])

# many logs v no context motion
length(noHit_cond[noHit_cond == 1 | noHit_cond == 2])
chisq.test(noHit_cond[noHit_cond == 1 | noHit_cond == 2], noHit_blame_choice[noHit_cond == 1 | noHit_cond == 2])
chisq.test(noHit_blameChoiceTable[,c(2,3)])
cramersV(noHit_blameChoiceTable[,c(2,3)])

## end ====================================================================================================================