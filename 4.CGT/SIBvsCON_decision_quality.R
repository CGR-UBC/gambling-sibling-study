require(ggplot2)
require(reshape2)
require(lme4)
library(lmerTest)
library(dplyr)
library(plyr)
library(influence.ME)

setwd("/Users/eoldfiel/Data/github/SIBSTUDY_slots/4.CGT/analysis/model1_rational/SIBvsCON")
data <- read.csv("../../data_for_analysis/long_regression_data.csv")

data<-subset(data, participant > 300 | participant < 200)

data<-subset(data, participant != 301)

data<-rename(data, c("data.ratio"="ratio",
                     "data.a.or.d"="a.or.d",
                     "data.rational.decision"="rational", 
                     "data.percentage.staked"="bet.percent",
                     "data.choice.latency"="choice.latency"))

data<-subset(data,ratio<5)

data$ratio<-data$ratio-1

data<- within (data, {
  participant<-factor(participant)
  group<-factor(group,levels=c(1,3),labels = c("Controls","Siblings"))
})

  # create frequncy counts of trial types for each participant (check model balanced)
  tally_ratio<-data %>% group_by(participant,ratio) %>% tally()
  tally_ratio<-dcast(tally_ratio,participant ~ ratio,value.var="n")
  write.csv(tally_ratio,file ='SIBvsCONrational_model_trials_per_participant.csv',row.names = FALSE)
  
  
  cat("Test for linearity of the logit \nSignificant values for interaction term mean assumption has been violated.\n")
  data$log.ratio.int<-log(data$ratio)*data$ratio
  testmodel.linearitytest<-glmer(rational ~ (1|participant) + group * ratio+ log.ratio.int,data=data,
                            family=binomial,control = glmerControl(optimizer = "bobyqa"), nAGQ=10)
  summary(testmodel.linearitytest)

  
  baseline<-glmer(rational ~ (1|participant),data=data,family=binomial,control = glmerControl(optimizer = "bobyqa"))
  ratio<-glmer(rational ~ (ratio|participant) + ratio,data=data,family=binomial,control = glmerControl(optimizer = "bobyqa"))
  group<-glmer(rational ~ (ratio|participant) + ratio+group,data=data,family=binomial,control = glmerControl(optimizer = "bobyqa"))
  int<-glmer(rational ~ (ratio|participant) + ratio*group,data=data,family=binomial,control = glmerControl(optimizer = "bobyqa"))
  
  anova(baseline,ratio,group,int)
  summary(group)
  summary(int)
  
  model<-group
  
  confint(model)
  
  
  model_SE<-sqrt(diag(vcov(model)))
  
  tab<-cbind(Est=fixef(model), LL = fixef(model) - 1.96 * model_SE, UL = fixef(model) + 1.96 * model_SE)
  odd_ratios<-exp(tab)
  write.csv(tab,file ='SIBvsCONrational_model_betas.csv',row.names = FALSE)
  write.csv(odd_ratios,file ='SIBvsCONrational_model_OR.csv',row.names = FALSE)
  
  # calc predicted probabilities by group and ratio
  
  pp_ratio_group_data<-data[,c("participant","group","ratio","rational")]
  ratio_values<-c(0,1,2,3)
  
  #group trials by group and ratio, then calc mean and quantiles for predicted probabilities
  pp_ratio_group <- lapply(levels(data$group), function(gp){
    pp_ratio_group_data$group[] <- gp
    lapply(ratio_values, function(rt){
      pp_ratio_group_data$ratio<-rt
      predict(model,newdata=pp_ratio_group_data,type="response")
    })
  })
  
  rational.plot.data <- lapply(pp_ratio_group, function(X) {
    temp <- t(sapply(X, function(x) {
      c(M=mean(x), quantile(x, c(.25, .75)))
    }))
    temp <- as.data.frame(cbind(temp, ratio_values))
    colnames(temp) <- c("Predictedprobability", "Lower", "Upper", "Ratio")
    return(temp)
  })
  
  rational.plot.data<-do.call(rbind,rational.plot.data)
  rational.plot.data$group<-factor(rep(levels(data$group), each = length(ratio_values)))
  write.csv(rational.plot.data,file ='SIBvsCONrational_model_pp.csv',row.names = FALSE)
  


  