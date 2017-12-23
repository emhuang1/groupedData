##5/5/16

#########################################################################################
##Exclusions:
##the minority cohort
##people who have a missing value for frailty at Year 2
##people who have a missing value at Year 5 but not at Year 9
##people who are frail at Year 2

#########################################################################################
##Put together a wide dataset. For every person, have the following information:
##delta: an indicator of whether or not the person was observed to be frail (1 if yes, 0 if no)
##if delta = 1: V is the first visit at which frailty was observed (either 5 or 9);
##if delta = 0: V is the last visit at which frailty status is measured (either 2, 5, or 9)

#########################################################################################
##Big decisions:
##we use frailimp as the frailty measure
##our outcome is frail, and death is treated as non-informative censoring

#########################################################################################


#########################################################################################
##Open the datasets
rm(list=ls())

setwd("~/Dropbox/research/Ravi/paper/CHS_frailty")
mainData <- read.csv("huang_main.csv")
##This dataset includes the baseline variable perstat, which indicates
##which subjects were in the original cohort.


frailtyData <- read.csv("Arking_frailty.csv")
##This dataset includes frailty over time

nrow(mainData) #5888

nrow(frailtyData) #16977
length(unique(frailtyData$idno)) #5888
16977/3 #5659, which indicates not everyone has 3 records
##This could be because the new cohort entered in Year 5

##Let's restrict to looking at the original cohort
mainData <- subset(mainData, perstat == 1)
participants <- mainData$idno
frailtyData <- subset(frailtyData, idno %in% participants)

nrow(mainData) ##5201
nrow(frailtyData) ##15603
length(unique(frailtyData$idno)) ##5201
15603/3 ##5201
##Everyone in the original cohort has three rows in the frailtyData
##dataset

#########################################################################################
##Let's focus on exploring and formatting the frailty data 
head(frailtyData)

##Decide whether to use fscore, frail, fscoreimp, or frailimp
sum(is.na(frailtyData$fscore)) ##4253
sum(is.na(frailtyData$frail)) ##4253
sum(is.na(frailtyData$fscoreimp)) ##4132
sum(is.na(frailtyData$frailimp)) ##4132

sum(is.na(frailtyData$fscore)&is.na(frailtyData$frail)) ##4253
sum(is.na(frailtyData$fscoreimp)&is.na(frailtyData$frailimp)) ##4132

sum(is.na(frailtyData$fscore)&is.na(frailtyData$frail)&frailtyData$year == 2) ##473
sum(is.na(frailtyData$fscoreimp)&is.na(frailtyData$frailimp)&frailtyData$year == 2) ##466

sum(is.na(frailtyData$fscore)&is.na(frailtyData$frail)&frailtyData$year == 5) ##1381
sum(is.na(frailtyData$fscoreimp)&is.na(frailtyData$frailimp)&frailtyData$year == 5) ##1326

sum(is.na(frailtyData$fscore)&is.na(frailtyData$frail)&frailtyData$year == 9) ##2399
sum(is.na(frailtyData$fscoreimp)&is.na(frailtyData$frailimp)&frailtyData$year == 9) ##2340

##either fscore and frail are both NA or neither is NA
##either fscoreimp and frailimp are both NA or neither is NA

##fscore is the frailty score based on observed data
##fscoreimp is the frailty score using imputed data
##frail is frailty based on observed data
##frailimp is frailty using imputed data

##The determination of frailty is based on the corresponding frailty score:
##0 = not frail (score = 0)
##1 = intermediate frailty (score = 1, 2)
##2 = frail (score =3,4,5)

table(frailtyData$fscore,frailtyData$frail)


table(frailtyData$fscoreimp,frailtyData$frailimp)



table(frailtyData$fscore, frailtyData$fscoreimp, useNA = "ifany")


table(frailtyData$frail, frailtyData$frailimp, useNA = "ifany")


frailty2 <- subset(frailtyData, year == 2) ##nrow = 5201
frailty5 <- subset(frailtyData, year == 5) ##nrow = 5201
frailty9 <- subset(frailtyData, year == 9) ##nrow = 5201

table(frailty2$fscore, frailty2$fscoreimp, useNA = "ifany")
table(frailty5$fscore, frailty5$fscoreimp, useNA = "ifany")
table(frailty9$fscore, frailty9$fscoreimp, useNA = "ifany")

rm(frailty2, frailty5, frailty9)

#########################################################################################
##USE FRAILIMP
frailtyData <- data.frame(idno = frailtyData$idno, year = frailtyData$year, frail = frailtyData$frailimp)
frailtyData$frail <- 1*(frailtyData$frail==2) ##we don't consider intermediate frailty as frailty
                                              ##NA's remain as NA's

frailtyData <-  reshape(frailtyData, 
                timevar = c("year"),
                idvar = c("idno"),
                direction = "wide")
##This reshapes the dataset so that there is one row per person.

#########################################################################################
##Check that frail = NA (not = 1) if a person is dead

statusData <-   data.frame(idno = mainData$idno,
                           dthyr3 = mainData$dthyr3,
                           dthyr4 = mainData$dthyr4,
                           dthyr5 = mainData$dthyr5,
                           dthyr6 = mainData$dthyr6,
                           dthyr7 = mainData$dthyr7,
                           dthyr8 = mainData$dthyr8,
                           dthyr9 = mainData$dthyr9,
                           dthyr10 = mainData$dthyr10,
                           dthyr11 = mainData$dthyr11)

##Coding of dthyr3, dthyr4, etc
##0 no
##1 yes

##We can simplify dthyr3, dthyr4, etc. to a single
##dthyr which gives the first year where the person is dead
##and NA if the person is alive up through Year 11
statusData$dthyr <- NA
for (i in 1:nrow(statusData)){
  if(statusData$dthyr3[i]==1){
    if(statusData$dthyr4[i]==1 & statusData$dthyr5[i]==1 & statusData$dthyr6[i]==1 & statusData$dthyr7[i]==1 & statusData$dthyr8[i]==1 & statusData$dthyr9[i]==1 & statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 3
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr4[i]==1){
    if(statusData$dthyr5[i]==1 & statusData$dthyr6[i]==1 & statusData$dthyr7[i]==1 & statusData$dthyr8[i]==1 & statusData$dthyr9[i]==1 & statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 4
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr5[i]==1){
    if(statusData$dthyr6[i]==1 & statusData$dthyr7[i]==1 & statusData$dthyr8[i]==1 & statusData$dthyr9[i]==1 & statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 5
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr6[i]==1){
    if(statusData$dthyr7[i]==1 & statusData$dthyr8[i]==1 & statusData$dthyr9[i]==1 & statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 6
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr7[i]==1){
    if(statusData$dthyr8[i]==1 & statusData$dthyr9[i]==1 & statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 7
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr8[i]==1){
    if(statusData$dthyr9[i]==1 & statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 8
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr9[i]==1){
    if(statusData$dthyr10[i]==1 & statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 9
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr10[i]==1){
    if(statusData$dthyr11[i]==1){
      statusData$dthyr[i] <- 10
    } else {
      warning(paste("miscoding for person in row", i))
    }
  } else if(statusData$dthyr11[i]==1){
    statusData$dthyr[i] <- 11
  } else {
    statusData$dthyr[i] <- NA    
  }
}
##No warnings 

statusData <- merge(statusData, frailtyData, by = "idno")

##table(statusData$dthyr, statusData$frail.2, useNA = "ifany")
table(statusData$dthyr, statusData$frail.5, useNA = "ifany")
table(statusData$dthyr, statusData$frail.9, useNA = "ifany")

##if the person is dead, frail = NA

rm(statusData)
#########################################################################################
##Exclusions

##people who have a missing value for frailty at Year 2
sum(is.na(frailtyData$frail.2))
frailtyData <- subset(frailtyData, !is.na(frail.2))

##people who are frail at Year 2
sum(frailtyData$frail.2 == 1)
frailtyData <- subset(frailtyData, frail.2 == 0)

##people who have a missing value at Year 5 but not at Year 9
sum(is.na(frailtyData$frail.5)&!is.na(frailtyData$frail.9))
frailtyData <- subset(frailtyData, !(is.na(frailtyData$frail.5)&!is.na(frailtyData$frail.9)))

#########################################################################################
##Get V and delta for each participant

##delta: an indicator of whether or not the person was observed to be frail (1 if yes, 0 if no)
frailtyData$delta <- 0
frailtyData$delta[frailtyData$frail.5 == 1 | frailtyData$frail.9 == 1] <- 1

table(frailtyData$frail.5, frailtyData$frail.9, useNA = "ifany")
##used this code to check delta

##if delta = 1: V is the first visit at which frailty was observed (either 5 or 9);
##if delta = 0: V is the last visit at which frailty status is measured (either 2, 5, or 9)
frailtyData$V <- NA
for (i in 1:nrow(frailtyData)){
  if(frailtyData$delta[i] == 1){
    if(frailtyData$frail.5[i] == 1){
      frailtyData$V[i] <- 5
    } else {
      frailtyData$V[i] <- 9
    }
  } else {
      if(!is.na(frailtyData$frail.5[i]) & !is.na(frailtyData$frail.9[i]) & frailtyData$frail.5[i]==0 & frailtyData$frail.9[i]==0){
        frailtyData$V[i] <- 9
      } else if(!is.na(frailtyData$frail.5[i]) & frailtyData$frail.5[i]==0 & is.na(frailtyData$frail.9[i])){
        frailtyData$V[i] <- 5
      } else {
        frailtyData$V[i] <- 2
      }
  }
}


#########################################################################################
##Save outcome dataset
outcome <- data.frame(idno = frailtyData$idno,
                      V = frailtyData$V,
                      delta = frailtyData$delta)


save(outcome, file = "frail.Rdata")

















# #########################################################################################
# ##See why people have missing values
# 
# unique(frailtyData$status2)
# ##1 = CLINIC VISIT
# table(frailtyData$status2)/nrow(frailtyData)*100
# 
# sort(unique(frailtyData$status5))
# # 1	CLINIC VISIT
# # 2	HOME VISIT
# # 3	PHONE INFO.
# # 12	REFUSED-REASON UNKNOWN
# # 13	REFUSED-NON HEALTH
# # 14	REFUSED-ILL, NO PROXY
# # 15	OUT OF AREA
# # 16	DECEASED
# # 17	CAN'T LOCATE
# # 18	INCAPACITATED-PROXY
# # 19	HOSPITAL
# # 20	NURSING HOME
# # 21	ILL AT HOME
# # 22	MOVED AWAY
# # 23	NO SHOW
# # 24	CARING FOR ILL PERSON
# # 30	DO NOT CONTACT AGAIN
# table(frailtyData$status5)/nrow(frailtyData)*100
# 
# sort(unique(frailtyData$status9))
# # 1	CLINIC VISIT    
# # 2	HOME VISIT      
# # 3	PHONE INFO.     
# # 4	NURS.HOME VISIT 
# # 5	FORMS MAILED    
# # 6	SPLIT VISIT     
# # 16	DECEASED
# # 30	DO NOT CONTACT AGAIN    
# # 40	SOFT DO NOT CONTACT     
# # 41	REFUSED-PROB. WITH STUDY
# # 42	REFUSED-LIFE SITUATION  
# # 43	REFUSED-LACK OF TIME    
# # 44	REFUSED-OTHER REASON    
# # 45	ILL AT HOME     
# # 46	NURSING HOME    
# # 47	HOSPITAL
# # 48	COGNITIVE OR MENTAL REAS.       
# # 49	CARING FOR ILL PERSON   
# # 50	OUT OF AREA     
# # 51	CAN'T LOCATE    
# # 52	NO SHOW 
# # 54	NOT DONE FOR OTHER REAS.
# table(frailtyData$status9)/nrow(frailtyData)*100   
# 
# miss5 <- subset(frailtyData, is.na(outcome5))
# miss9 <- subset(frailtyData, is.na(outcome9))
# 
# nrow(miss5)
# nrow(miss9)
# 
# table(miss5$status5)/nrow(miss5)*100
# table(miss9$status9)/nrow(miss9)*100
# 
# rm(miss5, miss9)
# 
# outcome <- frailtyData
# rm(frailtyData)