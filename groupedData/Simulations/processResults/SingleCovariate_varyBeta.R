#####################################################################################################
##SINGLE COVARIATE I: VARY BETA##
#####################################################################################################

library(xtable)

rm(list=ls())

setwd("~/Dropbox/research/Ravi/paper/Simulations/processResults")
source("processingResults.R")




setwd("~/Dropbox/research/Ravi/paper/Simulations/SingleCovariate/varyBeta/results")


#######################################
Beta <- 0
#######################################

n <- 200
load("Beta0_n200.Rdata")
fullResult <- c(Beta, n, estimator(Beta,result), ci(Beta,result))


n <- 500
load("Beta0_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta0_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


#######################################
Beta <- 0.2
#######################################

n <- 200
load("Beta02_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 500
load("Beta02_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta02_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))



#######################################
Beta <- 0.4
#######################################
n <- 200
load("Beta04_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 500
load("Beta04_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta04_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


#######################################
Beta <- 0.6
#######################################
n <- 200
load("Beta06_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 500
load("Beta06_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta06_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


#######################################
Beta <- 0.8
#######################################

n <- 200
load("Beta08_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 500
load("Beta08_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta08_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


#######################################
Beta <- 1
#######################################
n <- 200
load("Beta10_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 500
load("Beta10_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta10_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


#######################################
Beta <- 2
#######################################
n <- 200
load("Beta20_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 500
load("Beta20_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))


n <- 2000
load("Beta20_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, estimator(Beta,result), ci(Beta,result)))




#####################################################################################################
##Format results##
#####################################################################################################
row.names(fullResult) <- 1:nrow(fullResult)

fullResult <- data.frame(fullResult)

names(fullResult) <- c("Beta", "n", 
                       "Efron.bias", "Breslow.bias", "Laplace.bias", "Felix.bias", "PG.bias",
                       "Efron.se", "Breslow.se", "Laplace.se", "Felix.se", "PG.se",
                       "Efron.rmse", "Breslow.rmse", "Laplace.rmse", "Felix.rmse", "PG.rmse",
                       "Efron.cov", "Breslow.cov", "Laplace.cov", "Felix.cov", "PG.cov"
                       )

result200 <- subset(fullResult, n == 200)
result500 <- subset(fullResult, n == 500)
result2000 <- subset(fullResult, n == 2000)

plotForPaper <- function(data){
  par(mfrow=c(2,2))
  min.y <- min(pmin(data$Efron.bias, data$Laplace.bias, data$PG.bias, data$Felix.bias))#, data$Breslow.bias)) 
  max.y <- max(pmax(data$Efron.bias, data$Laplace.bias, data$PG.bias, data$Felix.bias))#, data$Breslow.bias))
  plot(data$Beta, data$Efron.bias, pch = 0, xlab = "Beta", ylab = "Absolute Bias", ylim = c(min.y,max.y))
  par(new = TRUE)
#  plot(data$Beta, data$Breslow.bias, pch = 1, ylim = c(min.y,max.y), ylab = "", xlab = "")
#  par(new = TRUE)
  plot(data$Beta, data$Laplace.bias, pch = 2, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$Felix.bias, pch = 3, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$PG.bias, pch = 4, ylim = c(min.y,max.y), ylab = "", xlab = "")
  abline(h = 0, lty = 2)
  
  min.y <- min(pmin(data$Efron.se, data$Laplace.se, data$PG.se, data$Felix.se))##, data$Breslow.se))
  max.y <- max(pmax(data$Efron.se, data$Laplace.se, data$PG.se, data$Felix.se))##, data$Breslow.se))
  plot(data$Beta, data$Efron.se, pch = 0, xlab = "Beta", ylab = "Standard error", ylim = c(min.y,max.y))
  par(new = TRUE)
#  plot(data$Beta, data$Breslow.se, pch = 1, ylim = c(min.y,max.y), ylab = "", xlab = "")
#  par(new = TRUE)
  plot(data$Beta, data$Laplace.se, pch = 2, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$Felix.se, pch = 3, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$PG.se,  pch = 4, ylim = c(min.y,max.y), ylab = "", xlab = "")
  
  min.y <- min(pmin(data$Efron.rmse, data$Laplace.rmse, data$PG.rmse, data$Felix.rmse))##, data$Breslow.rmse))
  max.y <- max(pmax(data$Efron.rmse, data$Laplace.rmse, data$PG.rmse, data$Felix.rmse))##, data$Breslow.rmse))
  plot(data$Beta, data$Efron.rmse, pch = 0, xlab = "Beta", ylab = "Root-mean-square error", ylim = c(min.y,max.y))
  par(new = TRUE)
#  plot(data$Beta, data$Breslow.rmse, pch = 1, ylim = c(min.y,max.y), ylab = "", xlab = "")
#  par(new = TRUE)
  plot(data$Beta, data$Laplace.rmse, pch = 2, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$Felix.rmse, pch = 3, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$PG.rmse, pch = 4, ylim = c(min.y,max.y), ylab = "", xlab = "")
  
  min.y <- min(pmin(data$Efron.cov, data$Laplace.cov, data$PG.cov, data$Felix.cov))##, data$Breslow.cov))
  max.y <- max(pmax(data$Efron.cov, data$Laplace.cov, data$PG.cov, data$Felix.cov))##, data$Breslow.cov))
  plot(data$Beta, data$Efron.cov, pch = 0, xlab = "Beta", ylab = "Coverage probability", ylim = c(min.y,max.y))
  par(new = TRUE)
#  plot(data$Beta, data$Breslow.cov, pch = 1, ylim = c(min.y,max.y), ylab = "", xlab = "")
#  par(new = TRUE)
  plot(data$Beta, data$Laplace.cov, pch = 2, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$Felix.cov, pch = 3, ylim = c(min.y,max.y), ylab = "", xlab = "")
  par(new = TRUE)
  plot(data$Beta, data$PG.cov, pch = 4, ylim = c(min.y,max.y), ylab = "", xlab = "")
  abline(h = 0.95, lty = 2)
  legend("bottomleft", legend = c("Laplace", "Analytic", "Efron", "PG"),
         pch = c(2,3,0,4))
}

setwd("~/Dropbox/research/Ravi/paper/Simulations/processResults/plotsForPaper/SingleCovariate")

pdf("VaryBeta_n200.pdf")
plotForPaper(result200)
dev.off()

pdf("VaryBeta_n500.pdf")
plotForPaper(result500)
dev.off()

pdf("VaryBeta_n2000.pdf")
plotForPaper(result2000)
dev.off()


tableForPaper <- function(data){
  tab <- data.frame(Beta = data$Beta,
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
tableForPaper(result200)
tableForPaper(result2000)
