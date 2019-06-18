


setwd("/Users/eoldfiel/data/github/SIBSTUDY_slots/2.fMRI/featquery_2018")

data <- read.csv("NU_corr.csv")

GD<-subset(data,participant<300)
SIB<-subset(data,participant>300)

cor(GD$NU,GD$averagebet,use = "pairwise.complete.obs")
cor.test(GD$NU,GD$averagebet,use = "pairwise.complete.obs")
cor(GD$NU,GD$Win.allmiss.putamen,use = "pairwise.complete.obs")
cor.test(GD$NU,GD$Win.allmiss.putamen,use = "pairwise.complete.obs")

cor(GD$NM.FM.putamen,GD$PGSI,use = "pairwise.complete.obs")
cor.test(GD$NM.FM.putamen,GD$PGSI,use = "pairwise.complete.obs")
cor(GD$NM.FM.paraC,GD$PGSI,use = "pairwise.complete.obs")
cor.test(GD$NM.FM.paraC,GD$PGSI,use = "pairwise.complete.obs")
cor(GD$NM.FM.OFC,GD$PGSI,use = "pairwise.complete.obs")
cor.test(GD$NM.FM.OFC,GD$PGSI,use = "pairwise.complete.obs")

cor(SIB$NU,SIB$averagebet,use = "pairwise.complete.obs")
cor.test(SIB$NU,SIB$averagebet,use = "pairwise.complete.obs")
cor(SIB$NU,SIB$Win.allmiss,use = "pairwise.complete.obs")
cor.test(SIB$NU,SIB$Win.allmiss,use = "pairwise.complete.obs")