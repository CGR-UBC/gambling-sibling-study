library(lme4)
library(lmerTest)
library(car)
library(nlme)
library(pastecs)
# trial types:
# 1 WIN
# 2 NM1 (before)
# 3 NM5
# 4 FM
# 5 too late

setwd("~/Data/github/SIBSTUDY_slots/2.task_behaviour")

data<-read.csv("ratings.csv",header=TRUE)

data<-subset(data, participant > 300 | participant < 200)

data$group <- ifelse(data$participant>200, 2, ifelse(data$participant<200, 1,NA))

data<-subset(data,participant!=0) #remove first row
data<-subset(data,trial.type!=5) #remove too late trials where no rating provided
data$previous.trial.type<- c(NA,data$trial.type[-nrow(data)]) 

data$trial.type_NMcombined<-data$trial.type
data$trial.type_NMcombined[data$trial.type_NMcombined == 2] <- 23
data$trial.type_NMcombined[data$trial.type_NMcombined == 3] <- 23
data$previous.trial.type_NMcombined<- c(NA,data$trial.type_NMcombined[-nrow(data)]) 

data$trial.type<-factor(data$trial.type)
data$trial.type <- relevel(data$trial.type, ref="4")
data$trial.type_NMcombined<-factor(data$trial.type_NMcombined)
data$trial.type_NMcombined <- relevel(data$trial.type_NMcombined, ref="4")

### descriptives
temp<-subset(data,group==1)
by(temp$again,temp$trial.type,stat.desc,basic=TRUE,norm=TRUE)
by(temp$chance,temp$previous.trial.type,stat.desc,basic=TRUE,norm=TRUE)
by(temp$again,temp$trial.type_NMcombined,stat.desc,basic=TRUE,norm=TRUE)
by(temp$chance,temp$previous.trial.type_NMcombined,stat.desc,basic=TRUE,norm=TRUE)

temp<-subset(data,group==2)
by(temp$again,temp$trial.type,stat.desc,basic=TRUE,norm=TRUE)
by(temp$chance,temp$previous.trial.type,stat.desc,basic=TRUE,norm=TRUE)
by(temp$again,temp$trial.type_NMcombined,stat.desc,basic=TRUE,norm=TRUE)
by(temp$chance,temp$previous.trial.type_NMcombined,stat.desc,basic=TRUE,norm=TRUE)

### again model
intercept<-gls(again ~ 1, data = data, method = "ML")
random.intercept<-lme(again ~1, data=data, random = ~ 1|participant,  method = "ML")
anova(intercept,random.intercept)

addintercept<-lmer(again~  (1|participant) , data=data,  REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
addtype<-lmer(again ~ trial.type_NMcombined +  (1|participant) , data=data, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
addgroup <- lmer(again ~ group + trial.type_NMcombined + (1|participant) , data=data, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
addint <- lmer(again ~ trial.type_NMcombined*group +  (1|participant) , data=data, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(addintercept,addtype,addgroup,addint)

summary(addgroup)
summary(addint)

confint.merMod(addgroup,method=c("boot"))

##### chance model
data<-subset(data,previous.trial.type>0)
data$previous.trial.type_NMcombined<-factor(data$previous.trial.type_NMcombined)
data$previous.trial.type_NMcombined <- relevel(data$previous.trial.type_NMcombined, ref="4")

intercept<-gls(chance ~ 1, data = data, method = "ML")
random.intercept<-lme(chance ~1, data=data, random = ~ 1|participant,  method = "ML")
anova(intercept,random.intercept)

addintercept<-lmer(chance ~  (1|participant) , data=data,  REML = FALSE) 
addtype<-lmer(chance ~ previous.trial.type_NMcombined +  (1|participant) , data=data, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
addgroup <- lmer(chance ~ previous.trial.type_NMcombined + group +  (1|participant) , data=data, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
addint <- lmer(chance ~ previous.trial.type_NMcombined*group +  (1|participant) , data=data, REML = FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(addintercept,addtype,addgroup,addint)

summary(addgroup)
summary(addint)
