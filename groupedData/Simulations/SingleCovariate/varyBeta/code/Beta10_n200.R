##1/13/16
##Update Beta, n, seed, and file name

rm(list=ls())

seed <- 827728188

###################################################################################
##LIBRARIES##
###################################################################################
require(survival)
require(MASS)
require(gaussquad)
require(numDeriv)

currWorkDir <- getwd()

setwd("/users/emhuang/Ravi/paper/genericCode")
source("allCode.R")

setwd(currWorkDir)

############################################################################################################################################
##FUNCTIONS##
############################################################################################################################################
hazard <- function(t, a, b, exb) exb * a * (t/b)^(a-1) 
cumhaz <- function(t, a, b, exb) exb * b * (t/b)^a 
froot <- function(x, u, ...) sqrt(cumhaz(x, ...)) - sqrt(u)




############################################################################################################################################
##INPUTS##
############################################################################################################################################

K <- 10000 ##number of randomized trials to be simulated
m <- 3 ##number of follow-up visits
pTreatment <- 0.5 ##probability of being assigned to treatment group

trueBeta <- 1 ##treatment effect
n <- 200 ##sample size of each simulated randomized trial
set.seed(seed)
simSeeds <- sample(10^9,K)

visit1Time <- 2
visit2Time <- 4
visit3Time <- 6


###################################################################################
##SIMULATIONS##
###################################################################################

nMethods <- 5
result <- matrix(NA, nrow = K, ncol = nMethods * 2)
nties <- matrix(NA, nrow = K, ncol = 3)


for(k in 1:K){
  set.seed(simSeeds[k])
  ###################################################################################
  ##Generation of Z (random treatment assignment: 1 if control, 0 if treatment)##
  ###################################################################################
  data <- data.frame(Z=rbinom(n,size=1,prob=pTreatment))
  
  ###################################################################################
  ##Generation of C (censoring time, the last visit the subject attends)##
  ###################################################################################
  data$C <- sample(x=0:m,size=n,replace = TRUE, prob = c(.08,.1,.1,.72))
  
  ###################################################################################
  ##Generation of T (frailty time)##
  ###################################################################################
  
  ##Failure time generated from the Weibull hazard
  a <- 2
  b <- 100
  u <- -log(runif(n))
  exb <- exp(trueBeta*data$Z)
  data$Tcont <- NA
  for(i in 1:n){
    data$Tcont[i] <- uniroot(froot, interval=c(1.e-14, 1e04), u = u[i], exb = exb[i], a=a, b=b)$root
  }
  rm(a,b,u,exb)
  #hist(data$Tcont)
  #summary(data$Tcont)
  
  ###################################################################################
  ##Generation of T' (grouped frailty time)##
  ###################################################################################
  data$Tgrouped <- 10000
  for(i in 1:n){
    if(data$Tcont[i]==0){
      data$Tgrouped[i] <- 0
    }else if(0<data$Tcont[i]&data$Tcont[i]<=visit1Time){
      data$Tgrouped[i] <- 1
    }else if(visit1Time<data$Tcont[i] & data$Tcont[i]<=visit2Time){
      data$Tgrouped[i] <- 2
    }else if(visit2Time<data$Tcont[i] & data$Tcont[i]<=visit3Time){
      data$Tgrouped[i] <- 3
    }
  }
  
  ###################################################################################
  ##Calculate delta (censoring indicator) and V (visit time depending on delta)##
  ###################################################################################
  data$delta <- 1
  data$delta[data$Tgrouped>data$C] <- 0
  data$V <- data$delta*data$Tgrouped + (1-data$delta)*data$C
  
  #table(data$C)/n
  #table(data$Tgrouped)/n
  #table(data$delta,data$V)/n
  #temp <- table(data$delta,data$V)/n
  #sum(temp[1,1:3]) #proportion of n subjects who dropout early
  #temp[1,4] #proportion of n subjects who are adminstratively censored
  #sum(temp[2,]) #proportion who are observed to be frail
  
  data <- data.frame(delta = data$delta, V = data$V, Z = data$Z)
  
  data <- subset(data, V!=0)
  
  nties[k,] <- table(data$delta, data$V)[2,]
  
  temp <- table(data$delta,data$V)/n
  if (nrow(temp)!=2) {
    result[k,] <- rep(NA,times=nMethods * 2)
    warning(paste(k, ":Either no censoring or no failure."))
  } else {
    result[k,] <- applyMethods(data)
  }
}
    
save(nties, result, file="Beta10_n200.Rdata")