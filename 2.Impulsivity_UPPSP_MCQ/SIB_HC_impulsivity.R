library(pastecs)
library(reshape)
library(lmerTest)
library(ggplot2)

setwd("~/Data/github/SIBSTUDY_slots/3.impulsivity")
data_all=read.csv("data_for_R.csv")

data<-subset(data_all, participant > 300 | participant < 200)
data<-subset(data_all, SIBHC==1)
N=nrow(data)

#### KIRBY MCQ####
kirby.data<-subset(data,select=c(participant,Group,K.large.raw,K.medium.raw,K.small.raw))

colnames(kirby.data)[3] <- "3"
colnames(kirby.data)[4] <- "2"
colnames(kirby.data)[5] <- "1"

#convert to long format 
kirby.longdata<-melt(kirby.data,id=c("participant","Group"),measured=c(3,2,1))
names(kirby.longdata)<-c("participant","Group","Magnitude","K")
kirby.longdata$K<-log(kirby.longdata$K) #log(k)
kirby.longdata$Magnitude<-as.numeric(levels(kirby.longdata$Magnitude))[kirby.longdata$Magnitude]
options(scipen=999)

# descrptives
by(kirby.longdata$K,list(kirby.longdata$Magnitude,kirby.longdata$Group),stat.desc,basic=TRUE)
kirby.longdata$Group=factor(kirby.longdata$Group)
kirby.longdata$participant=factor(kirby.longdata$participant)

SIB_HC<-c(1,-1)
contrasts(kirby.longdata$Group)<-cbind(SIB_HC)

baseline<-lmer(K  ~ (1|participant), data = kirby.longdata, REML = FALSE)
magnitudeM<-update(baseline, .~. + Magnitude)
groupM<-update(magnitudeM, .~. + Group)
magnitude_group<-update(groupM, .~. + Magnitude:Group)

anova(baseline,magnitudeM,groupM,magnitude_group)

summary(magnitude_group)
confint(magnitude_group)

### UPPS-P ###

NU<-data.frame(data$Group,data$NU)
by(NU$data.NU,NU$data.Group,stat.desc,basic=TRUE,norm=TRUE)
NUmodel<-t.test(data.NU~data.Group,data=NU,paired=FALSE)
NUmodel
t<-NUmodel$statistic[[1]]
df<-NUmodel$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r

loP<-data.frame(data$Group,data$loP)
by(loP$data.loP,loP$data.Group,stat.desc,basic=TRUE,norm=TRUE)
loPmodel<-t.test(data.loP~data.Group,data=loP,paired=FALSE)
loPmodel
t<-loPmodel$statistic[[1]]
df<-loPmodel$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r

loPe<-data.frame(data$Group,data$loPe)
by(loPe$data.loPe,loPe$data.Group,stat.desc,basic=TRUE,norm=TRUE)
loPemodel<-t.test(data.loPe~data.Group,data=loPe,paired=FALSE)
loPemodel
t<-loPemodel$statistic[[1]]
df<-loPemodel$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r

SS<-data.frame(data$Group,data$SS)
by(SS$data.SS,SS$data.Group,stat.desc,basic=TRUE,norm=TRUE)
SSmodel<-t.test(data.SS~data.Group,data=SS,paired=FALSE)
SSmodel
t<-SSmodel$statistic[[1]]
df<-SSmodel$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r

PUmodel<-t.test(data.PU~data.Group,data=PU,paired=FALSE)
PUmodel
t<-PUmodel$statistic[[1]]
df<-PUmodel$parameter[[1]]
r<-sqrt(t^2/(t^2+df))
r
