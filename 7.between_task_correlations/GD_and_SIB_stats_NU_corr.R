


setwd("/Users/eoldfiel/data/github/SIBSTUDY_slots/2.fMRI/featquery_2018")

data <- read.csv("NU_corr.csv")

GD<-subset(data,participant<300)
SIB<-subset(data,participant>300)

cor.test(GD$NU,GD$averagebet,use = "pairwise.complete.obs")
cor.test(GD$NU,GD$Win.allmiss.putamen,use = "pairwise.complete.obs")
cor.test(GD$Win.allmiss,GD$averagebet,use = "pairwise.complete.obs")

cor.test(SIB$NU,SIB$averagebet,use = "pairwise.complete.obs")
cor.test(SIB$NU,SIB$Win.allmiss,use = "pairwise.complete.obs")
cor.test(SIB$Win.allmiss,SIB$averagebet,use = "pairwise.complete.obs")
