## clear workspace  
rm(list = ls()) 

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 

##================ import data: baseline from E1 ==================================================================================================================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/moral_capture/e1_illusory_wrongs/data_and_analysis/data_all_except_async")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data_baseline <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data_baseline <- (data_baseline <- do.call(rbind, data_baseline))
data_baseline <- data_baseline[data_baseline$trialStruct.cond == 4,]
data_baseline$trialStruct.cond <- 0

#exclusions
data_baseline <- subset(data_baseline,(data_baseline$trialStruct.comprehension == "E"))
data_baseline <- subset(data_baseline,(data_baseline$trialStruct.displayTime < 4225 & data_baseline$trialStruct.displayTime > 4175))
data_baseline <- subset(data_baseline,(data_baseline$trialStruct.whichCar_choice == 2)) 
dim(data_baseline) #91 (same as in E1, obviously)

##================ import main data ====================================================================

dir <- setwd("/Users/julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/moral_capture/e2a_context_duration/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))
data <- data[,c(1:20,22)]

# num ss recruited
dim(data) #562

#data before which car exclusion question
data <- subset(data,(data$trialStruct.comprehension == "E"))
dim(data) 
data <-subset(data,(data$trialStruct.displayTime < 4225 & data$trialStruct.displayTime > 4175))
dim(data) #510

562 - 510 #52 excluded

sex <- as.factor(data$trialStruct.sex)
table(sex)[1]/sum(table(sex))
age <- as.numeric(levels(data$trialStruct.age))[data$trialStruct.age] 
mean(age) 

#data after which car exclusion question
data <- subset(data,(data$trialStruct.whichCar_choice == 2)) 
dim(data) #470

100 - (((510-379)/510)*100) #74%

##======================== processing ===========================================================================================================================

#bind data into single dataset
data <- rbind(data,data_baseline)

## assign variable names
cond <- as.factor(data$trialStruct.cond)
table(cond)
blame <- as.numeric(levels(data$trialStruct.blame_scaled))[data$trialStruct.blame_scaled]
blame_choice <- as.factor(data$trialStruct.blame_choice)

#baseline
baseline_mean <- mean(blame[cond == 0]); baseline_mean
baseline_sd <- sd(blame[cond==0]); baseline_sd
baseline_length <- length(blame[cond == 0]); baseline_length
baseline_sem <- baseline_sd/sqrt(baseline_length); baseline_sem
baseline_add_sem <- baseline_mean + baseline_sem
baseline_minus_sem <- baseline_mean - baseline_sem

##============================= plots ==================================== 

alpha = .05;

blame_mat <- matrix(NA,4,6)
colnames(blame_mat) <- c('cond','condNumbers','mean','sd','n','sem') 
condNumbers <- c(1200,500,100,50)

for (i in 1:4) {
  blame_mat[i,] <- c(i,condNumbers[i],mean(blame[cond == i]),sd(blame[cond==i]), length(blame[cond == i]),0) 
  blame_mat[i,6] <- blame_mat[i,4]/sqrt(blame_mat[i,5]) 
}

blame.summary <- as.data.frame(blame_mat, stringsAsFactors=F)
condNames <- c('(1200)ms','500ms','100ms','50ms')

## line plot - scaled blame
condNums <- blame.summary$condNumbers
blameMeans <- blame.summary$mean
blameSems <- blame.summary$sem

#quartz(width=7)
plot(condNums,blameMeans,xaxt="n",yaxt="n",pch=16, ylim=c(1,9),xlim=c(0,1200), xlab="Context Duration",ylab="Mean Blame Judgment",main="E2 - Context Duration",col="black", cex=1.6)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
abline(baseline_add_sem,0, lty=1)
abline(baseline_mean,0, lty=5)
abline(baseline_minus_sem,0, lty=1)
lines(blame.summary[,2],blame.summary[,3],col='firebrick1',lwd=3)
segments(condNums,blameMeans-blameSems,condNums,blameMeans+blameSems,lwd=2,col="light gray")

## bar plot - blame choice
blameTable <- matrix(NA,5)

blameTable[1] <- length(blame_choice[cond==1 & blame_choice==1])/length(blame_choice[cond==1]) #1200ms
blameTable[2] <- length(blame_choice[cond==2 & blame_choice==1])/length(blame_choice[cond==2]) #500ms
blameTable[3] <- length(blame_choice[cond==3 & blame_choice==1])/length(blame_choice[cond==3]) #100ms
blameTable[4] <- length(blame_choice[cond==4 & blame_choice==1])/length(blame_choice[cond==4]) #50ms
blameTable[5] <- length(blame_choice[cond==0 & blame_choice==1])/length(blame_choice[cond==0]) #no context

barplot(blameTable[1:4], beside=TRUE,ylim=c(0,1.0), 
        legend.text=TRUE, main="Proportion Blaming Driver of Red Car", cex.axis=1.0, cex.main = 1.5,names = condNames,ylab="Proportion")
abline(a= blameTable[5], b = 0)

##========================== analysis: blame scaled ===================================================================================================

## anova of blame scaled for all conditions except baseline from e1
model <- aov(blame[cond!=0] ~ cond[cond!=0])
summary(model)
etaSquared(model)

# means and sd
blame_mat

##---- full duration v other conds

# 1200ms v 500ms
t.test(blame[cond==1 | cond==2] ~ cond[cond==1 | cond==2], var.equal=TRUE, paired=FALSE)
tes(0.88693,108,97) #0.12

# 1200ms v 100ms
t.test(blame[cond==1 | cond==3] ~ cond[cond==1 | cond==3], var.equal=TRUE, paired=FALSE)
tes(3.1753,108,92) #0.45

# 1200ms v 50ms
t.test(blame[cond==1 | cond==4] ~ cond[cond==1 | cond==4], var.equal=TRUE, paired=FALSE)
tes(5.45,108,82) #0.80

##---- no context v other conds
length(cond[cond==0]) #length of no context condition

#no context vs. 1200ms
t.test(blame[cond==0 | cond==1] ~ cond[cond==0 | cond==1], var.equal=TRUE, paired=FALSE)
tes(-3.3927,91,108) #0.48

#no context vs. 500ms
t.test(blame[cond==0 | cond==2] ~ cond[cond==0 | cond==2], var.equal=TRUE, paired=FALSE)
tes(-2.5572,91,97) #0.37

#no context vs. 100ms
t.test(blame[cond==0 | cond==3] ~ cond[cond==0 | cond==3], var.equal=TRUE, paired=FALSE)
tes(-0.38555,91,92) 

#no context vs. 50ms
t.test(blame[cond==0 | cond==4] ~ cond[cond==0 | cond==4], var.equal=TRUE, paired=FALSE)
tes(1.7136,91,82) 

##========================== analysis: blame mcq ===================================================================================================

blameChoiceTable <- table(blame_choice,cond); blameChoiceTable

#1 = 1200, 2 = 500, 3 = 100, 4 = 50, 5 = no context

blameTable

##---- full duration v other conds

# 1200ms v 500ms
length(cond[cond == 1 | cond == 2])
chisq.test(cond[cond == 1 | cond == 2], blame_choice[cond== 1 | cond == 2])
chisq.test(blameChoiceTable[,2:3])
cramersV(blameChoiceTable[,2:3])

# 1200ms v 100ms
length(cond[cond == 1 | cond == 3])
chisq.test(cond[cond == 1 | cond == 3], blame_choice[cond== 1 | cond == 3])
chisq.test(blameChoiceTable[,c(2,4)])
cramersV(blameChoiceTable[,c(2,4)])

# 1200ms v 50ms
length(cond[cond == 1 | cond == 4])
chisq.test(cond[cond == 1 | cond == 4], blame_choice[cond== 1 | cond == 4])
chisq.test(blameChoiceTable[,c(2,5)])
cramersV(blameChoiceTable[,c(2,5)])

##---- no context v other conds

#no context v 1200ms
length(cond[cond == 0 | cond == 1])
chisq.test(cond[cond == 0 | cond == 1], blame_choice[cond== 0 | cond == 1])
chisq.test(blameChoiceTable[,1:2])
cramersV(blameChoiceTable[,1:2])

#no context v 500ms
length(cond[cond == 0 | cond == 2])
chisq.test(cond[cond == 0 | cond == 2], blame_choice[cond== 0 | cond == 2])
chisq.test(blameChoiceTable[,c(1,3)])
cramersV(blameChoiceTable[,c(1,3)])

#no context v 100ms
length(cond[cond == 0 | cond == 3])
chisq.test(cond[cond == 0 | cond == 3], blame_choice[cond == 0 | cond == 3])
chisq.test(blameChoiceTable[,c(1,4)])
cramersV(blameChoiceTable[,c(1,4)])

#no context v 50ms
length(cond[cond == 0 | cond == 4])
chisq.test(cond[cond == 0 | cond == 4], blame_choice[cond== 0 | cond == 4])
chisq.test(blameChoiceTable[,c(1,5)])
cramersV(blameChoiceTable[,c(1,5)])

## end ========================================================================================================================================================= 
