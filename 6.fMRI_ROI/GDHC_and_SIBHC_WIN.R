library(reshape)
library(pastecs)
library(nlme)
library(Hmisc)


setwd("/Users/eoldfiel/data/github/SIBSTUDY_slots/2.fMRI/featquery_2018")

#ROIs from WBIC peaks
# t test of win-miss for three ROIs - GDHC and SIBHC

# 5 = win
# 6 = NM1
# 7 = NM5
# 8 = FM

########## putamen #################

### GDHC

bilat_put <- read.csv("bilat_put_summary_new_correct.csv")
bilat_put <- subset(bilat_put,select=c(Participant,GDHC,SIBHC,X5,X6,X7,X8))

GDHC_bilat_put <- subset(bilat_put,GDHC==1)
GDHC_bilat_put$group <- ifelse(GDHC_bilat_put$Participant>200, 'GD', ifelse(GDHC_bilat_put$Participant<200, 'HC',NA))
GDHC_bilat_put$SIBHC<-NULL
GDHC_bilat_put$GDHC<-NULL
GDHC_bilat_put$all_miss<-rowMeans(GDHC_bilat_put[, c(3:5)], na.rm = TRUE)
GDHC_bilat_put$win_all_miss<-GDHC_bilat_put$X5 - GDHC_bilat_put$all_miss



#convert to long format for descriptive
GDHC_bilat_put.long<-melt(GDHC_bilat_put,id=c("Participant","group"),measured=c("X5","X6","X7","X8","all_miss","win_all_miss"))
names(GDHC_bilat_put.long)<-c("Participant","group","Outcome","Signal")
by(GDHC_bilat_put.long$Signal,list(GDHC_bilat_put.long$Outcome,GDHC_bilat_put.long$group),stat.desc,basic=FALSE,norm=TRUE)

GDHC_bilat_put.reg.data<-subset(GDHC_bilat_put.long,Outcome!='win_all_miss')
GDHC_bilat_put.reg.data<-subset(GDHC_bilat_put.reg.data,Outcome!='X6')
GDHC_bilat_put.reg.data<-subset(GDHC_bilat_put.reg.data,Outcome!='X7')
GDHC_bilat_put.reg.data<-subset(GDHC_bilat_put.reg.data,Outcome!='X8')


GDHC_bilat_put.reg.data$group<-factor(GDHC_bilat_put.reg.data$group)
GDHC_bilat_put.reg.data$Outcome<-factor(GDHC_bilat_put.reg.data$Outcome)

baseline<-lme(Signal ~ 1, random = ~1|Participant/Outcome, data = GDHC_bilat_put.reg.data, method = "ML")
outcome<-update(baseline, .~. + Outcome)
group<-update(outcome, .~. + group)
int<-update(group, .~. + Outcome*group)

anova(baseline,outcome,group,int)

summary(int)


### SIBHC

SIBHC_bilat_put <- subset(bilat_put,SIBHC==1)
SIBHC_bilat_put$group <- ifelse(SIBHC_bilat_put$Participant>200, '1SIB', ifelse(SIBHC_bilat_put$Participant<200, '2HC',NA))
SIBHC_bilat_put$GDHC<-NULL
SIBHC_bilat_put$SIBHC<-NULL
SIBHC_bilat_put$all_miss<-rowMeans(SIBHC_bilat_put[, c(3:5)], na.rm = TRUE)
SIBHC_bilat_put$win_all_miss<-SIBHC_bilat_put$X5 - SIBHC_bilat_put$all_miss

#convert to long format for descriptive
SIBHC_bilat_put.long<-melt(SIBHC_bilat_put,id=c("Participant","group"),measured=c("X5","X6","X7","X8","all_miss","win_all_miss"))
names(SIBHC_bilat_put.long)<-c("Participant","group","Outcome","Signal")
by(SIBHC_bilat_put.long$Signal,list(SIBHC_bilat_put.long$Outcome,SIBHC_bilat_put.long$group),stat.desc,basic=FALSE,norm=TRUE)


SIBHC_bilat_put.reg.data<-subset(SIBHC_bilat_put.long,Outcome!='win_all_miss')
SIBHC_bilat_put.reg.data<-subset(SIBHC_bilat_put.reg.data,Outcome!='X6')
SIBHC_bilat_put.reg.data<-subset(SIBHC_bilat_put.reg.data,Outcome!='X7')
SIBHC_bilat_put.reg.data<-subset(SIBHC_bilat_put.reg.data,Outcome!='X8')

SIBHC_bilat_put.reg.data$group<-factor(SIBHC_bilat_put.reg.data$group)
SIBHC_bilat_put.reg.data$Outcome<-factor(SIBHC_bilat_put.reg.data$Outcome)

baseline<-lme(Signal ~ 1, random = ~1|Participant/Outcome, data = SIBHC_bilat_put.reg.data, method = "ML")
outcome<-update(baseline, .~. + Outcome)
group<-update(outcome, .~. + group)
int<-update(group, .~. + Outcome*group)

anova(baseline,outcome,group,int)

summary(int)


########## bilat ofc #################

### GDHC

bilat_ofc <- read.csv("bilat_ofc_summary_new.csv")
bilat_ofc <- subset(bilat_ofc,select=c(Participant,GDHC,SIBHC,X5,X6,X7,X8))

GDHC_bilat_ofc <- subset(bilat_ofc,GDHC==1)
GDHC_bilat_ofc$group <- ifelse(GDHC_bilat_ofc$Participant>200, 'GD', ifelse(GDHC_bilat_ofc$Participant<200, 'HC',NA))
GDHC_bilat_ofc$SIBHC<-NULL
GDHC_bilat_ofc$GDHC<-NULL
GDHC_bilat_ofc$all_miss<-rowMeans(GDHC_bilat_ofc[, c(3:5)], na.rm = TRUE)
GDHC_bilat_ofc$win_all_miss<-GDHC_bilat_ofc$X5 - GDHC_bilat_ofc$all_miss

#convert to long format for descriptive
GDHC_bilat_ofc.long<-melt(GDHC_bilat_ofc,id=c("Participant","group"),measured=c("X5","X6","X7","X8","all_miss","win_all_miss"))
names(GDHC_bilat_ofc.long)<-c("Participant","group","Outcome","Signal")
by(GDHC_bilat_ofc.long$Signal,list(GDHC_bilat_ofc.long$Outcome,GDHC_bilat_ofc.long$group),stat.desc,basic=FALSE,norm=TRUE)

GDHC_bilat_ofc.reg.data<-subset(GDHC_bilat_ofc.long,Outcome!='win_all_miss')
GDHC_bilat_ofc.reg.data<-subset(GDHC_bilat_ofc.reg.data,Outcome!='X6')
GDHC_bilat_ofc.reg.data<-subset(GDHC_bilat_ofc.reg.data,Outcome!='X7')
GDHC_bilat_ofc.reg.data<-subset(GDHC_bilat_ofc.reg.data,Outcome!='X8')


GDHC_bilat_ofc.reg.data$group<-factor(GDHC_bilat_ofc.reg.data$group)
GDHC_bilat_ofc.reg.data$Outcome<-factor(GDHC_bilat_ofc.reg.data$Outcome)

baseline<-lme(Signal ~ 1, random = ~1|Participant/Outcome, data = GDHC_bilat_ofc.reg.data, method = "ML")
outcome<-update(baseline, .~. + Outcome)
group<-update(outcome, .~. + group)
int<-update(group, .~. + Outcome*group)

anova(baseline,outcome,group,int)

summary(int)

### SIBHC

SIBHC_bilat_ofc <- subset(bilat_ofc,SIBHC==1)
SIBHC_bilat_ofc$group <- ifelse(SIBHC_bilat_ofc$Participant>200, '1SIB', ifelse(SIBHC_bilat_ofc$Participant<200, '2HC',NA))
SIBHC_bilat_ofc$GDHC<-NULL
SIBHC_bilat_ofc$SIBHC<-NULL
SIBHC_bilat_ofc$all_miss<-rowMeans(SIBHC_bilat_ofc[, c(3:5)], na.rm = TRUE)
SIBHC_bilat_ofc$win_all_miss<-SIBHC_bilat_ofc$X5 - SIBHC_bilat_ofc$all_miss

#convert to long format for descriptive
SIBHC_bilat_ofc.long<-melt(SIBHC_bilat_ofc,id=c("Participant","group"),measured=c("X5","X6","X7","X8","all_miss","win_all_miss"))
names(SIBHC_bilat_ofc.long)<-c("Participant","group","Outcome","Signal")
by(SIBHC_bilat_ofc.long$Signal,list(SIBHC_bilat_ofc.long$Outcome,SIBHC_bilat_ofc.long$group),stat.desc,basic=FALSE,norm=TRUE)

SIBHC_bilat_ofc.reg.data<-subset(SIBHC_bilat_ofc.long,Outcome!='win_all_miss')
SIBHC_bilat_ofc.reg.data<-subset(SIBHC_bilat_ofc.reg.data,Outcome!='X6')
SIBHC_bilat_ofc.reg.data<-subset(SIBHC_bilat_ofc.reg.data,Outcome!='X7')
SIBHC_bilat_ofc.reg.data<-subset(SIBHC_bilat_ofc.reg.data,Outcome!='X8')

SIBHC_bilat_ofc.reg.data$group<-factor(SIBHC_bilat_ofc.reg.data$group)
SIBHC_bilat_ofc.reg.data$Outcome<-factor(SIBHC_bilat_ofc.reg.data$Outcome)

baseline<-lme(Signal ~ 1, random = ~1|Participant/Outcome, data = SIBHC_bilat_ofc.reg.data, method = "ML")
outcome<-update(baseline, .~. + Outcome)
group<-update(outcome, .~. + group)
int<-update(group, .~. + Outcome*group)

anova(baseline,outcome,group,int)

summary(int)


########## paraC #################

### GDHC

paraC <- read.csv("paraC_summary_new.csv")
paraC <- subset(paraC,select=c(Participant,GDHC,SIBHC,X5,X6,X7,X8))

GDHC_paraC <- subset(paraC,GDHC==1)
GDHC_paraC$group <- ifelse(GDHC_paraC$Participant>200, 'GD', ifelse(GDHC_paraC$Participant<200, 'HC',NA))
GDHC_paraC$SIBHC<-NULL
GDHC_paraC$GDHC<-NULL
GDHC_paraC$all_miss<-rowMeans(GDHC_paraC[, c(3:5)], na.rm = TRUE)
GDHC_paraC$win_all_miss<-GDHC_paraC$X5 - GDHC_paraC$all_miss

#convert to long format for descriptive
GDHC_paraC.long<-melt(GDHC_paraC,id=c("Participant","group"),measured=c("X5","X6","X7","X8","all_miss","win_all_miss"))
names(GDHC_paraC.long)<-c("Participant","group","Outcome","Signal")
by(GDHC_paraC.long$Signal,list(GDHC_paraC.long$Outcome,GDHC_paraC.long$group),stat.desc,basic=FALSE,norm=TRUE)
GDHC_paraC.reg.data<-subset(GDHC_paraC.long,Outcome!='win_all_miss')
GDHC_paraC.reg.data<-subset(GDHC_paraC.reg.data,Outcome!='X6')
GDHC_paraC.reg.data<-subset(GDHC_paraC.reg.data,Outcome!='X7')
GDHC_paraC.reg.data<-subset(GDHC_paraC.reg.data,Outcome!='X8')


GDHC_paraC.reg.data$group<-factor(GDHC_paraC.reg.data$group)
GDHC_paraC.reg.data$Outcome<-factor(GDHC_paraC.reg.data$Outcome)

baseline<-lme(Signal ~ 1, random = ~1|Participant/Outcome, data = GDHC_paraC.reg.data, method = "ML")
outcome<-update(baseline, .~. + Outcome)
group<-update(outcome, .~. + group)
int<-update(group, .~. + Outcome*group)

anova(baseline,outcome,group,int)

summary(int)

### SIBHC

SIBHC_paraC <- subset(paraC,SIBHC==1)
SIBHC_paraC$group <- ifelse(SIBHC_paraC$Participant>200, '1SIB', ifelse(SIBHC_paraC$Participant<200, '2HC',NA))
SIBHC_paraC$GDHC<-NULL
SIBHC_paraC$SIBHC<-NULL
SIBHC_paraC$all_miss<-rowMeans(SIBHC_paraC[, c(3:5)], na.rm = TRUE)
SIBHC_paraC$win_all_miss<-SIBHC_paraC$X5 - SIBHC_paraC$all_miss

#convert to long format for descriptive
SIBHC_paraC.long<-melt(SIBHC_paraC,id=c("Participant","group"),measured=c("X5","X6","X7","X8","all_miss","win_all_miss"))
names(SIBHC_paraC.long)<-c("Participant","group","Outcome","Signal")
by(SIBHC_paraC.long$Signal,list(SIBHC_paraC.long$Outcome,SIBHC_paraC.long$group),stat.desc,basic=FALSE,norm=TRUE)

SIBHC_paraC.reg.data<-subset(SIBHC_paraC.long,Outcome!='win_all_miss')
SIBHC_paraC.reg.data<-subset(SIBHC_paraC.reg.data,Outcome!='X6')
SIBHC_paraC.reg.data<-subset(SIBHC_paraC.reg.data,Outcome!='X7')
SIBHC_paraC.reg.data<-subset(SIBHC_paraC.reg.data,Outcome!='X8')

SIBHC_paraC.reg.data$group<-factor(SIBHC_paraC.reg.data$group)
SIBHC_paraC.reg.data$Outcome<-factor(SIBHC_paraC.reg.data$Outcome)

baseline<-lme(Signal ~ 1, random = ~1|Participant/Outcome, data = SIBHC_paraC.reg.data, method = "ML")
outcome<-update(baseline, .~. + Outcome)
group<-update(outcome, .~. + group)
int<-update(group, .~. + Outcome*group)

anova(baseline,outcome,group,int)

summary(int)

