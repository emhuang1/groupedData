##6/14/17

##For random samples of size n from the CHS dataset, get the Efron, Exact (Felix, Laguerre, Laplace), Breslow, and PG estimates
##Do 10000 simulations

##n = 2000


rm(list=ls())
#########################################################################################
##PG and Cox models
#########################################################################################

library(survival)
library(numDeriv)
library(gaussquad)

currWorkDir <- getwd()

setwd("/users/emhuang/Ravi/paper/genericCode")
source("allCode.R")

#########################################################################################
##Load datasets

setwd("/users/emhuang/Ravi/paper/CHS_frailty")

load("data.Rdata")

#########################################################################################
##Run simulations
nmethods <- 5
nsim <- 10000
n <- 2000
nties <- matrix(NA, nrow = nsim, ncol = 2) ##store the number of ties at visits 1 and 2
result <- matrix(NA, nrow = nsim, ncol = nmethods * 2 * 5) 

set.seed(890724)
seeds <- sample(10^6, nsim)

for (k in 1:nsim){
  #########################################################################################
  ##Take a random sample of CHS dataset
  set.seed(seeds[k])
  samp <- data[sample(1:nrow(data), size = n, replace = TRUE),]
  
  nties[k,] <- table(samp$delta, samp$V)[2,]
  result[k,] <- applyMethods(samp)
}

setwd(currWorkDir)

save(result, nties, file = "n2000.Rdata")

