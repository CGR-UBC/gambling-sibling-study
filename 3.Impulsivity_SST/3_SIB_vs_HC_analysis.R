library(pastecs)
library(ggplot2)

setwd("~/Data/github/SIBSTUDY_slots/5.SST/analysis")
data_all=read.csv("extracted_data.csv")
data<-subset(data_all, Participant > 300 | Participant < 200)

data$group <- ifelse(substr(data$Participant, 1, 1) == 3, 1,0)

pSuccStop<-data.frame(data$group,data$p.Succesful.stop.)
pSuccStop.stats<-by(pSuccStop$data.p.Succesful.stop.,pSuccStop$data.group,stat.desc,basic=TRUE,norm=TRUE)
pSuccStop.stats
pSuccStop.model<-wilcox.test(pSuccStop$data.p.Succesful.stop.~pSuccStop$data.group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
pSuccStop.model
rfromwilcox(pSuccStop.model,nrow(pSuccStop))

Correct.go.median.RT<-data.frame(data$group,data$Correct.go.median.RT)
Correct.go.median.RT.stats<-by(Correct.go.median.RT$data.Correct.go.median.RT,Correct.go.median.RT$data.group,stat.desc,basic=TRUE,norm=TRUE)
Correct.go.median.RT.stats
Correct.go.median.RT.model<-wilcox.test(Correct.go.median.RT$data.Correct.go.median.RT~Correct.go.median.RT$data.group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Correct.go.median.RT.model
rfromwilcox(Correct.go.median.RT.model,nrow(Correct.go.median.RT))

SSRT<-data.frame(data$group,data$SSRT)
SSRT.stats<-by(SSRT$data.SSRT,SSRT$data.group,stat.desc,basic=TRUE,norm=TRUE)
SSRT.stats
SSRT.model<-wilcox.test(SSRT$data.SSRT~SSRT$data.group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
SSRT.model
rfromwilcox(SSRT.model,nrow(SSRT))

Incorrect.go.N<-data.frame(data$group,data$Incorrect.go.N)
Incorrect.go.N.stats<-by(Incorrect.go.N$data.Incorrect.go.N,Incorrect.go.N$data.group,stat.desc,basic=TRUE,norm=TRUE)
Incorrect.go.N.stats
Incorrect.go.N.model<-wilcox.test(Incorrect.go.N$data.Incorrect.go.N~Incorrect.go.N$data.group,paired=FALSE,exact=FALSE)# turn off exact as get error that cant do it
Incorrect.go.N.model
rfromwilcox(Incorrect.go.N.model,nrow(Incorrect.go.N))
