## clear workspace
rm(list = ls()) 

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(lsr)) {install.packages("lsr"); require(lsr)} 

##================ import data: baseline from E1 =================================================================================================================

dir <- setwd("/Users/Julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/e1_illusory_wrongs/data_and_analysis/data_all_except_async")

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

##================ import: data ==========================================================================================================================

dir <- setwd("/Users/Julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/e2b_temporal_asynchrony/data_and_analysis/data")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

# num ss recruited
dim(data) #699

# data after comprehension and done before exclusions
data <- subset(data,(data$trialStruct.comprehension == "E"))
dim(data) 
data <-subset(data,(data$trialStruct.displayTime < 4225 & data$trialStruct.displayTime > 4175))
dim(data) #635

#excluded
699 - 635 #64

# demographics
sex <- as.factor(data$trialStruct.sex)
table(sex)[1]/sum(table(sex))
age <- as.numeric(levels(data$trialStruct.age))[data$trialStruct.age] 
mean(age)

# data after which car exclusions
data <- subset(data,(data$trialStruct.whichCar_choice == 2)) 
dim(data) #494

100 - (((635 - 494)/635)*100)

##================ processing =========================================================================================================

data <- rbind(data,data_baseline)
dim(data)

numConds = 5

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

##============================= plot ====================================================================================================

alpha = .05;

blame_mat <- matrix(NA,numConds,6)
colnames(blame_mat) <- c('cond','condNumbers','mean','sd','n','sem') 
condNumbers <- c(0,-100,-300,-500,-700)

for (i in 1:numConds) {
  blame_mat[i,] <- c(i,condNumbers[i],mean(blame[cond == i]),sd(blame[cond==i]), length(blame[cond == i]),0) 
  blame_mat[i,6] <- blame_mat[i,4]/sqrt(blame_mat[i,5]) 
}

blame.summary <- as.data.frame(blame_mat, stringsAsFactors=F)

condNames <- c('0ms','-100ms','-300ms','-500ms','-700ms')

group.colors <- c("1" = "#808080", "2" = "#236B8E", "3" = "#0680f9", "4" = "#0680f9","5" = "#0680f9")

## line plot - scaled blame
condNums <- blame.summary$condNumbers
blameMeans <- blame.summary$mean
blameSems <- blame.summary$sem

quartz(width=6)
plot(condNums,blameMeans,xaxt="n",yaxt="n",pch=16, ylim=c(4.5,8),xlim=c(-800,0), xlab="Temporal Asynchrony",ylab="Mean Blame Judgment",main="E3 - Temporal Asynchrony",col="black", cex=1.6)
axis(2,cex.axis=1.5)
axis(1,cex.axis=1.5)
abline(baseline_add_sem,0, lty=1)
abline(baseline_mean,0, lty=5)
abline(baseline_minus_sem,0, lty=1)
lines(blame.summary[,2],blame.summary[,3],col='firebrick1',lwd=3)
segments(condNums,blameMeans-blameSems,condNums,blameMeans+blameSems,lwd=2,col="light gray")

## bar plot - forced choice blame
blameTable <- matrix(NA,numConds)

blameTable[1] <- length(blame_choice[cond==1 & blame_choice==1])/length(blame_choice[cond==1]) #0ms
blameTable[2] <- length(blame_choice[cond==2 & blame_choice==1])/length(blame_choice[cond==2]) #-100ms
blameTable[3] <- length(blame_choice[cond==3 & blame_choice==1])/length(blame_choice[cond==3]) #-300ms
blameTable[4] <- length(blame_choice[cond==4 & blame_choice==1])/length(blame_choice[cond==4]) #-500ms
blameTable[5] <- length(blame_choice[cond==5 & blame_choice==1])/length(blame_choice[cond==5]) #-700ms
blameTable[6] <- length(blame_choice[cond==0 & blame_choice==1])/length(blame_choice[cond==0]) #no context

barplot(blameTable[1:numConds], beside=TRUE,ylim=c(0,1.0), col=group.colors,
        legend.text=TRUE, main="Proportion Blaming Driver of Red Car", cex.axis=1.0, cex.main = 1.5,names = condNames,ylab="Proportion")
abline(a= blameTable[6], b = 0)

##============================= analysis: blame scaled ================================================================================== 

#--anova of blame scaled for all conditions except baseline from e1
model <- aov(blame[cond!=0] ~ cond[cond!=0])
summary(model)
etaSquared(model)

#num subjects in each cond
blame_mat
length(cond[cond==0]) #length of no context condition

#1 = 0ms; #2 = -100ms; #3 = -300ms; #4 = -500ms; #5 = -700ms

##---- no asynchrony v other asynchronies

# 0ms v -100ms
t.test(blame[cond==1 | cond==2] ~ cond[cond==1 | cond==2], var.equal=TRUE, paired=FALSE)
tes(-0.15813,116,106) #0.02

# 0ms v -300ms
t.test(blame[cond==1 | cond==3] ~ cond[cond==1 | cond==3], var.equal=TRUE, paired=FALSE)
tes(1.9594,116,94) #0.27

# 0ms v -500ms
t.test(blame[cond==1 | cond==4] ~ cond[cond==1 | cond==4], var.equal=TRUE, paired=FALSE)
tes(2.2591,116,88) #0.32

# 0ms v -700ms
t.test(blame[cond==1 | cond==5] ~ cond[cond==1 | cond==5], var.equal=TRUE, paired=FALSE)
tes(1.5884,116,90) 

##---- no context v other asynchronies
length(cond[cond==0]) #length of no context condition

#no context vs. 0ms
t.test(blame[cond==0 | cond==1] ~ cond[cond==0 | cond==1], var.equal=TRUE, paired=FALSE)
tes(-3.8519,91,116) #0.54

#no context vs. -100ms
t.test(blame[cond==0 | cond==2] ~ cond[cond==0 | cond==2], var.equal=TRUE, paired=FALSE)
tes(-3.9614,91,106) #0.57

#no context vs. -300ms
t.test(blame[cond==0 | cond==3] ~ cond[cond==0 | cond==3], var.equal=TRUE, paired=FALSE)
tes(-1.9104,91,94) #0.28

#no context vs. -500ms
t.test(blame[cond==0 | cond==4] ~ cond[cond==0 | cond==4], var.equal=TRUE, paired=FALSE)
tes(-1.6182,91,88) #0.24

#no context vs. -700ms
t.test(blame[cond==0 | cond==5] ~ cond[cond==0 | cond==5], var.equal=TRUE, paired=FALSE)
tes(-2.2325,91,90) #0.33

##========================== analysis: blame mcq ===================================================================================================

blameChoiceTable <- table(blame_choice,cond); blameChoiceTable

#1 = 0ms; #2 = -100ms; #3 = -300ms; #4 = -500ms; #5 = -700ms

blameTable

##---- no asynchrony v other asynchronies

# 0ms v -100ms
length(cond[cond == 1 | cond == 2])
chisq.test(cond[cond == 1 | cond == 2], blame_choice[cond== 1 | cond == 2])
chisq.test(blameChoiceTable[,2:3])
cramersV(blameChoiceTable[,2:3])

# 0ms v -300ms
length(cond[cond == 1 | cond == 3])
chisq.test(cond[cond == 1 | cond == 3], blame_choice[cond== 1 | cond == 3])
chisq.test(blameChoiceTable[,c(2,4)])
cramersV(blameChoiceTable[,c(2,4)])

# 0ms v -500ms
length(cond[cond == 1 | cond == 4])
chisq.test(cond[cond == 1 | cond == 4], blame_choice[cond== 1 | cond == 4])
chisq.test(blameChoiceTable[,c(2,5)])
cramersV(blameChoiceTable[,c(2,5)])

# 0ms v -700ms
length(cond[cond == 1 | cond == 5])
chisq.test(cond[cond == 1 | cond == 5], blame_choice[cond== 1 | cond == 5])
chisq.test(blameChoiceTable[,c(2,6)])
cramersV(blameChoiceTable[,c(2,6)])

##---- no context v other asynchronies

# no context v 0ms
length(cond[cond == 0 | cond == 1])
chisq.test(cond[cond == 0 | cond == 1], blame_choice[cond== 0 | cond == 1])
chisq.test(blameChoiceTable[,1:2])
cramersV(blameChoiceTable[,1:2])

# no context v -100ms
length(cond[cond == 0 | cond == 2])
chisq.test(cond[cond == 0 | cond == 2], blame_choice[cond == 0 | cond == 2])
chisq.test(blameChoiceTable[,c(1,3)])
cramersV(blameChoiceTable[,c(1,3)])

# no context v -300ms
length(cond[cond == 0 | cond == 3])
chisq.test(cond[cond == 0 | cond == 3], blame_choice[cond == 0 | cond == 3])
chisq.test(blameChoiceTable[,c(1,4)])
cramersV(blameChoiceTable[,c(1,4)])

# no context v -500ms
length(cond[cond == 0 | cond == 4])
chisq.test(cond[cond == 0 | cond == 4], blame_choice[cond == 0 | cond == 4])
chisq.test(blameChoiceTable[,c(1,5)])
cramersV(blameChoiceTable[,c(1,5)])

# no context v -700ms
length(cond[cond == 0 | cond == 5])
chisq.test(cond[cond == 0 | cond == 5], blame_choice[cond == 0 | cond == 5])
chisq.test(blameChoiceTable[,c(1,6)])
cramersV(blameChoiceTable[,c(1,6)])

## end ========================================================================================================================================================= 



