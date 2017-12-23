##4/19/16

#########################################################################################
##Put together a wide dataset. For every person, have the baseline variables:
##age, gender, education, CRP, smoking
#########################################################################################

#########################################################################################
##Load dataset

rm(list=ls())

setwd("~/Dropbox/research/Ravi/paper/CHS_frailty")
mainData <- read.csv("huang_main.csv")
##This dataset includes many baseline variables

nrow(mainData) #5888

#########################################################################################
##Restrict to looking at the original cohort

mainData <- subset(mainData, perstat == 1)
nrow(mainData) ##5201

#########################################################################################
##Age, gender, education, CRP, smoking

##Use original CRP rather than adjusted CRP (adjusted CRP is useful if you are comparing CRP between years)
baselineData <- data.frame(idno = mainData$idno,
                           age01 = mainData$age01, ##self-reported age at baseline
                           grade01 = mainData$grade01, ##a lot of categories (see Meeting20160425.pptx)
                           gend01 = mainData$gend01,  ##gender (0 = female, 1 = male)
                           smoke2 = mainData$smoke2, ##1 = never, 2 = former, 3 = current
                           crpblorg = mainData$crpblorg) ##C-reactive protein, original values (mg/L)

apply(baselineData, 2, function(x){sum(is.na(x))})

save(baselineData, file = "baselineData.Rdata")

