library(reshape2)
library(MASS)
library(dplyr)
library(plyr)
library(ggplot2)
library(lmerTest)
library(influence.ME)

setwd("/Users/eoldfiel/Data/github/SIBSTUDY_slots/4.CGT/analysis/model2_bet/GDvsCON/ascending")
data <- read.csv("../../../data_for_analysis/long_regression_data.csv")
data<-subset(data, participant < 300)

data<-rename(data, c("data.ratio"="ratio",
                     "data.a.or.d"="a.or.d",
                     "data.rational.decision"="rational", 
                     "data.percentage.staked"="bet.percent",
                     "data.choice.latency"="choice.latency"))

data$bet.linear<-data$bet.percent
data$participant.as.num<-data$participant
# remove 5:5 trials
data<-subset(data,ratio<5)

data$ratio<-data$ratio-1

data<- within (data, {
  participant<-factor(participant)
  group<-factor(group,levels=c(2,1),labels = c("Gambling disorder","Controls"))
  bet.percent<-ordered(bet.percent,labels = c("5","25","50","75","95"))
  a.or.d<-factor(a.or.d,levels=c("a","d"),labels = c("a","d"))
  ascending_first<-factor(ascending_first,levels=0:1,labels = c("A1","D1"))
})

nlevels(data$participant)

#### ascending linear model 

data<-subset(data,a.or.d==c("a"))

baseline<-lmer(bet.linear ~ 1 + (1|participant),data=data,REML=FALSE)
ratio<-lmer(bet.linear ~ 1 +ratio + (ratio|participant),data=data,REML=FALSE)
group<-update(ratio, .~. + group)
ratio_group<-update(group, .~. + ratio:group)

anova(baseline,ratio,group,ratio_group)

model<-group
summary(model)

confint(model)

## check subset of data where made rational decision is the same result.
data_temp<-subset(data,rational==1)
baseline<-lmer(bet.linear ~ 1 + (1|participant),data=data_temp,REML=FALSE)
ratio<-lmer(bet.linear ~ 1 +ratio + (ratio|participant),data=data_temp,REML=FALSE)
group<-update(ratio, .~. + group)
ratio_group<-update(group, .~. + ratio:group)
anova(baseline,ratio,group,ratio_group)
##

data$pp<-predict(model)

dataCON<-subset(data,participant.as.num<199)
dataCON$participant<-factor(dataCON$participant)
dataGD<-subset(data,participant.as.num>200)
dataGD$participant<-factor(dataGD$participant)

gridCON<-expand.grid(participant=factor(c(levels(dataCON$participant))),group=factor(c("Controls")),ratio=c(0:3))
gridGD<-expand.grid(participant=factor(c(levels(dataGD$participant))),group=factor(c("Gambling disorder")),ratio=c(0:3))

predicted_grid<-rbind(gridCON,gridGD)
predicted_grid$predicted<-predict(model,predicted_grid)

temp<-ddply(predicted_grid, c("ratio","group"), transform, grp.mean.values = mean(predicted))
pCON<-subset(temp,participant==104)
pGD<-subset(temp,participant==203)

pALL<-rbind(pCON,pGD)

p<-pALL[,c("ratio","group","grp.mean.values")]

write.csv(p,file ='GDvsCON_bet_model_predict_ascending.csv',row.names = FALSE)
