#####################################################################################################
##CHS FRAILTY##
#####################################################################################################

library(xtable)

rm(list=ls())

nMethod <- 5

setwd("~/Dropbox/research/Ravi/paper/Simulations/processResults")
source("processingResults.R")

setwd("~/Dropbox/research/Ravi/paper/CHS_frailty")
load("data.Rdata")

setwd("~/Dropbox/research/Ravi/paper/genericCode")
source("allCode.R")

Beta <- applyMethods(data)[21:25] ##apply PG to CHS data set

numCov <- 5
nMethod <- 5


setwd("~/Dropbox/research/Ravi/paper/Simulations/CHS_frailty/results")


for (i in 1:5){
  #######################################
  n <- 500
  #######################################
  
  load("n500.Rdata")
  temp <- result[,i + numCov*(0:(nMethod*2-1))]
  fullResult <- c(n, estimator(Beta[i],temp), ci(Beta[i],temp))
  
  
  
  #######################################
  n <- 1000
  #######################################
  
  load("n1000.Rdata")
  temp <- result[,i + numCov*(0:(nMethod*2-1))]
  fullResult <- rbind(fullResult, c(n, estimator(Beta[i],temp), ci(Beta[i],temp)))
  
  
  
  #######################################
  n <- 2000
  #######################################
  load("n2000.Rdata")
  temp <- result[,i + numCov*(0:(nMethod*2-1))]
  fullResult <- rbind(fullResult, c(n, estimator(Beta[i],temp), ci(Beta[i],temp)))
  
  
  
  if (i == 1){
    age <- fullResult
  } else if (i == 2){
    educ <- fullResult
  } else if (i == 3){
    gender <- fullResult
  } else if (i == 4){
    everSmoke <- fullResult
  } else {
    crp <- fullResult
  }
  
  rm(fullResult)
  
}


age <- cbind(1, age)
educ <- cbind(2, educ)
gender <- cbind(3, gender)
everSmoke <- cbind(4, everSmoke)
crp <- cbind(5, crp)

fullResult <- rbind(age, educ, gender, everSmoke, crp)

fullResult <- data.frame(fullResult)
names(fullResult) <- c("covariate", "n",
                       "Efron.bias", "Breslow.bias", "Laplace.bias", "Felix.bias", "PG.bias",
                       "Efron.se", "Breslow.se", "Laplace.se", "Felix.se", "PG.se",
                       "Efron.rmse", "Breslow.rmse", "Laplace.rmse", "Felix.rmse", "PG.rmse",
                       "Efron.cov", "Breslow.cov", "Laplace.cov", "Felix.cov", "PG.cov"
                      )

result500 <- subset(fullResult, n == 500)
result1000 <- subset(fullResult, n == 1000)
result2000 <- subset(fullResult, n == 2000)


plotForPaper <- function(data){
  par(mfrow=c(2,2))
  min.y <- min(pmin(data$Efron.bias, data$Laplace.bias, data$PG.bias, data$Felix.bias, data$Breslow.bias)) 
  max.y <- max(pmax(data$Efron.bias, data$Laplace.bias, data$PG.bias, data$Felix.bias, data$Breslow.bias))
  plot(data$covariate, data$Efron.bias, pch = 0, xaxt = "n", xlab = "Covariate", ylab = "Absolute Bias", ylim = c(min.y,max.y))
  par(new = TRUE)
  plot(data$covariate, data$Breslow.bias, pch = 1, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Laplace.bias, pch = 2, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Felix.bias, pch = 3, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$PG.bias, pch = 4, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  axis(1, at=1:5, padj = 0, labels=c("age", "educ", "gender", "smoke", "CRP"))
  abline(h = 0, lty = 2)
  
  min.y <- min(pmin(data$Efron.se, data$Laplace.se, data$PG.se, data$Felix.se, data$Breslow.se))
  max.y <- max(pmax(data$Efron.se, data$Laplace.se, data$PG.se, data$Felix.se, data$Breslow.se))
  plot(data$covariate, data$Efron.se, pch = 0, xaxt = "n", xlab = "Covariate", ylab = "Standard error", ylim = c(min.y,max.y))
  par(new = TRUE)
  plot(data$covariate, data$Breslow.se, pch = 1, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Laplace.se, pch = 2, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Felix.se, pch = 3, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$PG.se,  pch = 4, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  axis(1, at=1:5, padj = 0, labels=c("age", "educ", "gender", "smoke", "CRP"))
  
  min.y <- min(pmin(data$Efron.rmse, data$Laplace.rmse, data$PG.rmse, data$Felix.rmse, data$Breslow.rmse))
  max.y <- max(pmax(data$Efron.rmse, data$Laplace.rmse, data$PG.rmse, data$Felix.rmse, data$Breslow.rmse))
  plot(data$covariate, data$Efron.rmse, pch = 0, xaxt = "n", xlab = "Covariate", ylab = "Root-mean-square error", ylim = c(min.y,max.y))
  par(new = TRUE)
  plot(data$covariate, data$Breslow.rmse, pch = 1, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Laplace.rmse, pch = 2, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Felix.rmse, pch = 3, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$PG.rmse, pch = 4, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  axis(1, at=1:5, padj = 0, labels=c("age", "educ", "gender", "smoke", "CRP"))
  
  min.y <- 0.89 #min(pmin(data$Efron.cov, data$Laplace.cov, data$PG.cov, data$Felix.cov, data$Breslow.cov))
  max.y <- max(pmax(data$Efron.cov, data$Laplace.cov, data$PG.cov, data$Felix.cov, data$Breslow.cov))
  plot(data$covariate, data$Efron.cov, pch = 0, xaxt = "n", xlab = "Covariate", ylab = "Coverage probability", ylim = c(min.y,max.y))
  par(new = TRUE)
  plot(data$covariate, data$Breslow.cov, pch = 1, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Laplace.cov, pch = 2, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$Felix.cov, pch = 3, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$covariate, data$PG.cov, pch = 4, xaxt = "n", ylim = c(min.y,max.y), ylab = "", xlab = "")
  abline(h = 0.95, lty = 2)
  legend("bottomright", legend = c("Laplace", "Analytic", "Efron", "Breslow", "PG"), 
         pch = c(2,3,0,1,4))
  axis(1, at=1:5, padj = 0, labels=c("age", "educ", "gender", "smoke", "CRP"))
}

setwd("~/Dropbox/research/Ravi/paper/Simulations/processResults/plotsForPaper/CHS_frailty")
pdf("CHS_n500.pdf")
plotForPaper(result500)
dev.off()

pdf("CHS_n1000.pdf")
plotForPaper(result1000)
dev.off()

pdf("CHS_n2000.pdf")
plotForPaper(result2000)
dev.off()

tableForPaper <- function(data){
  tab <- data.frame(Covariate = data$covariate,
                    Laplace.Bias = data$Laplace.bias,
                    Laplace.SE = data$Laplace.se,
                    Laplace.RMSE = data$Laplace.rmse,
                    Laplace.CP = data$Laplace.cov,
                    Analytic.Bias = data$Felix.bias,
                    Analytic.SE = data$Felix.se,
                    Analytic.RMSE = data$Felix.rmse,
                    Analytic.CP = data$Felix.cov,
                    Efron.Bias = data$Efron.bias,
                    Efron.SE = data$Efron.se,
                    Efron.RMSE = data$Efron.rmse,
                    Efron.CP = data$Efron.cov,
                    Breslow.Bias = data$Breslow.bias,
                    Breslow.SE = data$Breslow.se,
                    Breslow.RMSE = data$Breslow.rmse,
                    Breslow.CP = data$Breslow.cov,
                    PG.Bias = data$PG.bias,
                    PG.SE = data$PG.se,
                    PG.RMSE = data$PG.rmse,
                    PG.CP = data$PG.cov)
  result <- xtable(tab, align = "cccccccccccccccccccccc")
  digits(result) <- 3
  print(result, include.rownames=FALSE)
}

tableForPaper(result500)
tableForPaper(result1000)
tableForPaper(result2000)

