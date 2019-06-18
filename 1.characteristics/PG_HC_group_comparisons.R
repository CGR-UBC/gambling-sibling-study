library(pastecs)


rfromwilcox<-function(wilcoxModel,N){
  z<-qnorm(wilcoxModel$p.value/2)
  r<-z/sqrt(N)
  cat(wilcoxModel$data.name,"Effect size, r = " , r)
}

setwd("~/Data/github/SIBSTUDY_slots/1.characteristics")
data_all=read.csv("data_for_R_new.csv")

data<-subset(data_all, GDHC==1)

data$Group <- ifelse(substr(data$participant, 1, 1) == 2, 1,0)

data_smokers<-subset(data,Smoke==1)
data_drug_users<-subset(data,DAST>0)

################ NON-PARAMETRIC #######################

BDI<-data.frame(data$Group,data$BDI)
by(BDI$data.BDI,BDI$data.Group,stat.desc,basic=TRUE,norm=TRUE)
BDImodel<-wilcox.test(BDI$data.BDI~BDI$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
BDImodel
rfromwilcox(BDImodel,nrow(BDI))

Age<-data.frame(data$Group,data$Age)
by(Age$data.Age,Age$data.Group,stat.desc,basic=TRUE,norm=TRUE)
Agemodel<-wilcox.test(Age$data.Age~Age$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Agemodel
rfromwilcox(Agemodel,nrow(Age))

BAI<-data.frame(data$Group,data$BAI)
by(BAI$data.BAI,BAI$data.Group,stat.desc,basic=TRUE,norm=TRUE)
BAImodel<-wilcox.test(BAI$data.BAI~BAI$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
BAImodel
rfromwilcox(BAImodel,nrow(BAI))

Alcohol<-data.frame(data$Group,data$Alcohol)
by(Alcohol$data.Alcohol,Alcohol$data.Group,stat.desc,basic=TRUE,norm=TRUE)
Alcoholmodel<-wilcox.test(Alcohol$data.Alcohol~Alcohol$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Alcoholmodel
rfromwilcox(Alcoholmodel,nrow(Alcohol))

IQ<-data.frame(data$Group,data$IQ)
by(IQ$data.IQ,IQ$data.Group,stat.desc,basic=TRUE,norm=TRUE)
IQmodel<-wilcox.test(IQ$data.IQ~IQ$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
IQmodel
rfromwilcox(IQmodel,nrow(IQ))

Emotional.abuse<-data.frame(data$Group,data$Emotional.abuse)
by(Emotional.abuse$data.Emotional.abuse,Emotional.abuse$data.Group,stat.desc,basic=TRUE,norm=TRUE)
Emotional.abusemodel<-wilcox.test(Emotional.abuse$data.Emotional.abuse~Emotional.abuse$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Emotional.abusemodel
rfromwilcox(Emotional.abusemodel,nrow(Emotional.abuse))

Physical.Abuse<-data.frame(data$Group,data$Physical.Abuse)
by(Physical.Abuse$data.Physical.Abuse,Physical.Abuse$data.Group,stat.desc,basic=TRUE,norm=TRUE)
Physical.Abusemodel<-wilcox.test(Physical.Abuse$data.Physical.Abuse~Physical.Abuse$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Physical.Abusemodel
rfromwilcox(Physical.Abusemodel,nrow(Physical.Abuse))

Sexual.Abuse<-data.frame(data$Group,data$Sexual.Abuse)
by(Sexual.Abuse$data.Sexual.Abuse,Sexual.Abuse$data.Group,stat.desc,basic=TRUE,norm=TRUE)
Sexual.Abusemodel<-wilcox.test(Sexual.Abuse$data.Sexual.Abuse~Sexual.Abuse$data.Group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Sexual.Abusemodel
rfromwilcox(Sexual.Abusemodel,nrow(Sexual.Abuse))

by(data_drug_users$DAST,data_drug_users$Group,stat.desc,basic=TRUE,norm=TRUE)
DAST_model<-wilcox.test(data_drug_users$DAST~data_drug_users$Group,paired=FALSE)
DAST_model
rfromwilcox(DAST_model,nrow(data_drug_users))

Fagerstrom<-data.frame(data_smokers$Group,data_smokers$Fagerstrom)
by(Fagerstrom$data_smokers.Fagerstrom,Fagerstrom$data_smokers.Group,stat.desc,basic=TRUE,norm=TRUE)
Fagerstrom_model<-wilcox.test(data_smokers$Fagerstrom~data_smokers$Group,paired=FALSE)
Fagerstrom_model
rfromwilcox(Fagerstrom_model,nrow(Fagerstrom))

PGSI<-data.frame(data$Group,data$PGSI)
by(PGSI$data.PGSI,PGSI$data.Group,stat.desc,basic=TRUE,norm=TRUE)
