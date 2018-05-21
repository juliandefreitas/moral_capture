## clear workspace
rm(list = ls()) 

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(lsr)) {install.packages("lsr"); require(lsr)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)} 
if (!require(xlsx)) {install.packages("xlsx"); require(xlsx)} 
if (!require(openxlsx)) {install.packages("openxlsx"); require(openxlsx)} 

##================ import data: all conditions except asynchronous condition ====================================================================================

dir <- setwd("/Users/Julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/e1_illusory_wrongs/data_and_analysis/data_all_except_async")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
first_data <- (data <- do.call(rbind, data))

##================ import data: asynchronous context =======================================================================================================

dir <- setwd("/Users/Julian/Dropbox (alvarezlab)/Research-DeFreitas/Projects/moralCapture/moralCapture_data_github/e1_illusory_wrongs/data_and_analysis/data_async")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(file=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
second_data <- (data <- do.call(rbind, data))

second_data$trialStruct.cond <- 3

##======================== counts and exclusions ===========================================================================================================================

#bind data into single dataset
data <- rbind(first_data,second_data)

#num ss recruited
dim(data) #562

#data after comprehension & timing exclusions
data <-subset(data,(data$trialStruct.comprehension == "E"))
dim(data) 

#stimulus duration exclusion boundaries:
#launch: 3936 +-25ms [3911, 3961]
#other conditions: 4200 +-25ms [4175, 4225]

data <-subset(data,((data$trialStruct.cond != 1 & data$trialStruct.displayTime < 4225 & data$trialStruct.displayTime > 4175) |
                      (data$trialStruct.cond == 1 & data$trialStruct.displayTime < 3961 & data$trialStruct.displayTime > 3911)))
dim(data) #490

562-490 #72 excluded

#demographics
sex <- as.factor(data$trialStruct.sex)
table(sex)[1]/sum(table(sex))
age <- as.numeric(levels(data$trialStruct.age))[data$trialStruct.age] 
mean(age)

#data after whichCarExclusions
data <- subset(data, (data$trialStruct.whichCar_choice == 2)) 
dim(data) #397

#proportion passed which car q
100 - (((490-397)/490)*100)

##======================== processing =========================================================================================================================

## assign variable names
cond <- as.factor(data$trialStruct.cond)
blame <- as.numeric(levels(data$trialStruct.blame_scaled))[data$trialStruct.blame_scaled]
blame_choice <- as.factor(data$trialStruct.blame_choice)

##============================= plots =========================================================================================================================== 

## set up table for plotting
alpha = .05;

blame_mat <- matrix(NA,4,5)
colnames(blame_mat) <- c('cond','mean','sd','n','sem') 

for (i in 1:4) {
  blame_mat[i,] <- c(i,mean(blame[cond == i]),sd(blame[cond==i]), length(blame[cond == i]),0) 
  blame_mat[i,5] <- blame_mat[i,3]/sqrt(blame_mat[i,4]) 
}

blame.summary <- as.data.frame(blame_mat, stringsAsFactors=F)
condNames <- c('Launching','Launching Context', 'Offset Context', 'No Context')
group.colors <- c("1" = "#808080", "2" = "#236B8E", "3" = "#0680f9", "4" = "#0680f9")

## bar plot - blame scaled
quartz(width=5)
title <- c('Blame Judgments') 
p1<-ggplot(blame.summary[1:4,],aes(x=factor(cond),y=mean,fill=factor(cond))) +  
  scale_fill_manual(values=group.colors) +
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+theme_bw()+coord_cartesian(ylim=c(1, 8)) 
p1+geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+ggtitle(title)+
  scale_x_discrete(breaks = 1:length(condNames), labels=condNames)+
  theme(panel.grid.major = element_blank(),legend.position="none", panel.grid.minor = element_blank(), axis.title.y=element_blank())+
  theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust=1))+
  xlab("Condition")+ylab("Mean")

## bar plot - blame mcq
blameTable <- matrix(NA,4)

blameTable[1] <- length(blame_choice[cond==1 & blame_choice==1])/length(blame_choice[cond==1])
blameTable[2] <- length(blame_choice[cond==2 & blame_choice==1])/length(blame_choice[cond==2])
blameTable[3] <- length(blame_choice[cond==3 & blame_choice==1])/length(blame_choice[cond==3])
blameTable[4] <- length(blame_choice[cond==4 & blame_choice==1])/length(blame_choice[cond==4])

condNames_bar <- c('Launching',' Launching Context', 'Offset Context', 'No Context')

barplot(blameTable[1:4], beside=TRUE,ylim=c(0,1.0), col=group.colors,
        legend.text=TRUE, main="Proportion Blaming Driver of Red Car", cex.axis=1.0, cex.main = 1.5,names = condNames_bar,ylab="Proportion")

##============================= analysis: blame scaled ============================================================================================================================ 

#1 = launch, 2 = launch context, 3 = asynchronous, 4 = no context

## anova
model <- aov(blame ~ cond)
summary(model)
etaSquared(model)

#means and sds
blame.summary

##--- blame scaled

# launching v launching context
t.test(blame[cond==1 | cond==2] ~ cond[cond==1 | cond==2], var.equal=TRUE, paired=FALSE)
tes(2.263,124,108) #0.3

# launching context v no context
t.test(blame[cond==2 | cond==4] ~ cond[cond==2 | cond==4], var.equal=TRUE, paired=FALSE)
tes(4.1112,108,91) #0.59

# launching context v asynchronous context
t.test(blame[cond==2 | cond==3] ~ cond[cond==2 | cond==3], var.equal=TRUE, paired=FALSE)
tes(4.0009,108,74) #0.6

##============================= analysis: blame mcq ============================================================================================================================ 

#1 = launch, 2 = launch context, 3 = asynchronous, 4 = no context

blameChoiceTable <- table(blame_choice,cond); blameChoiceTable

#proportions
blameTable

# launching v launching context
length(cond[cond == 1 | cond == 2])
chisq.test(cond[cond == 1 | cond == 2], blame_choice[cond==1 | cond == 2])
cramersV(blameChoiceTable[,1:2])

# launching context v no context
length(cond[cond == 2 | cond == 4])
chisq.test(cond[cond == 2 | cond == 4], blame_choice[cond==2 | cond == 4])
cramersV(blameChoiceTable[,c(2,4)])

# launching context v asynchronous context
length(cond[cond == 2 | cond == 3])
chisq.test(cond[cond == 2 | cond == 3], blame_choice[cond==2 | cond == 3])
cramersV(blameChoiceTable[,c(2,3)])

## end ========================================================================================================================================================= 














