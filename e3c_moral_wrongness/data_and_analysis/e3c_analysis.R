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

##================ import data ================================================================================================

dir <- setwd("/Users/Julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/e3c_moral_wrongness/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

# num ss recruited
dim(data) #281

#subset who got comprehension question wrong. 
data <- subset(data,(data$trialStruct.comprehension == "E")); 
dim(data) 
data <-subset(data,(data$trialStruct.displayTime < 4225 & data$trialStruct.displayTime > 4175))
dim(data) #243

281-243 #38 excluded

sex <- as.factor(data$trialStruct.sex)
table(sex)[1]/sum(table(sex))
age <- as.numeric(levels(data$trialStruct.age))[data$trialStruct.age] 
mean(age)

# exclusions after 'which car' exclusion
data <- subset(data,(data$trialStruct.whichCar_choice == 2)) 
dim(data) #163

#% who got which car q correct
100 - (((243 - 163)/243)*100)

# conditions:
#0 = no context
#1 = motion (few logs)

##======================= processing =======================================================================================

condNames <- c('Grouping')

## assign variable names
cond <- as.factor(data$trialStruct.cond)
wrong <- as.numeric(levels(data$trialStruct.wrongness_scaled))[data$trialStruct.wrongness_scaled]
wrong_choice <- as.factor(data$trialStruct.wrongness_choice)

# baseline
baseline_mean <- mean(wrong[cond == 0]); baseline_mean
baseline_sd <- sd(wrong[cond==0]); baseline_sd
baseline_length <- length(wrong[cond == 0]); baseline_length
baseline_sem <- baseline_sd/sqrt(baseline_length); baseline_sem
baseline_add_sem <- baseline_mean + baseline_sem
baseline_minus_sem <- baseline_mean - baseline_sem

##============================= plot ============================================================================================ 

alpha = .05;
numConds = 1;

wrong_mat <- matrix(NA,numConds,5)
colnames(wrong_mat) <- c('cond','mean','sd','n','sem') 

wrong_mat[1,] <- c(1,mean(wrong[cond == 1]),sd(wrong[cond==1]), length(wrong[cond == 1]),0) 
wrong_mat[1,5] <- wrong_mat[1,3]/sqrt(wrong_mat[1,4]) 

wrong.summary <- as.data.frame(wrong_mat, stringsAsFactors=F); wrong.summary
condNames <- c('Grouping')

group.colors <- c("1" = "#808080")

## bar plot - wrongness scaled
title <- c('Wrongness Judgments') 
p1<-ggplot(wrong.summary,aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  scale_fill_manual(values=group.colors) +
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1, 8)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean") + geom_hline(yintercept = baseline_add_sem) + 
  geom_hline(yintercept = baseline_minus_sem) + geom_hline(yintercept = baseline_mean, lty=5)

## bar plot - blame mcq
wrongTable <- matrix(NA,6)

wrongTable[1] <- length(wrong_choice[cond==1 & wrong_choice==1])/length(wrong_choice[cond==1])
wrongTable[2] <- length(wrong_choice[cond==0 & wrong_choice==1])/length(wrong_choice[cond==0])

barplot(wrongTable[1], beside=TRUE,ylim=c(0,1.0), col=group.colors,
        legend.text=TRUE, main="Proportion Saying Driver is Wrong", cex.axis=1.0, cex.main = 1.5,names = condNames,ylab="Proportion")
abline(a= wrongTable[2], b = 0)

##============================= analysis: blame scaled ================================================================================= 

table(cond)

t.test(wrong ~ cond, var.equal=TRUE, paired=FALSE)
tes(-4.9106,67,96) #0.78

##========================== analysis: blame mcq ===================================================================================================

wrongTable
wrongChoiceTable <- table(wrong_choice,cond); wrongChoiceTable

# no context v. grouping
length(cond)
chisq.test(cond, wrong_choice)
chisq.test(wrongChoiceTable)
cramersV(wrongChoiceTable)

##end ====================================================================================================