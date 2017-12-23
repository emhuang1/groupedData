##8/4/17  

##Tables of number of ties


############################################################################
##Data Based Simulations##
############################################################################

load("~/Dropbox/research/Ravi/paper/Simulations/CHS_frailty/results/n500.Rdata")

n <- 500
hist(nties[,1])
hist(nties[,2])
tab <- c(n, colMeans(nties))


load("~/Dropbox/research/Ravi/paper/Simulations/CHS_frailty/results/n1000.Rdata")

n <- 1000
hist(nties[,1])
hist(nties[,2])
tab <- rbind(tab, c(n, colMeans(nties)))

load("~/Dropbox/research/Ravi/paper/Simulations/CHS_frailty/results/n2000.Rdata")

n <- 2000
hist(nties[,1])
hist(nties[,2])
tab <- rbind(tab, c(n, colMeans(nties)))

library(xtable)
tab <- xtable(tab, align = "cccc")
digits(tab) <- 0
print(tab, include.rownames=FALSE)


############################################################################
##Single Covariate Simulations##
############################################################################

setwd("~/Dropbox/research/Ravi/paper/Simulations/SingleCovariate/varyBeta/results")


#######################################
Beta <- 0
#######################################

n <- 200
load("Beta0_n200.Rdata")
fullResult <- c(Beta, n, colMeans(nties))


n <- 500
load("Beta0_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta0_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

#######################################
Beta <- 0.2
#######################################

n <- 200
load("Beta02_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 500
load("Beta02_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta02_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))


#######################################
Beta <- 0.4
#######################################
n <- 200
load("Beta04_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 500
load("Beta04_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta04_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

#######################################
Beta <- 0.6
#######################################
n <- 200
load("Beta06_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 500
load("Beta06_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta06_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

#######################################
Beta <- 0.8
#######################################

n <- 200
load("Beta08_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 500
load("Beta08_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta08_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

#######################################
Beta <- 1
#######################################
n <- 200
load("Beta10_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 500
load("Beta10_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta10_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

#######################################
Beta <- 2
#######################################
n <- 200
load("Beta20_n200.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 500
load("Beta20_n500.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

n <- 2000
load("Beta20_n2000.Rdata")
fullResult <- rbind(fullResult, c(Beta, n, colMeans(nties)))

fullResult <- data.frame(fullResult)
names(fullResult) <- c("Beta",
           "n",
           "Visit1",
           "Visit2",
           "Visit3")
result200 <- subset(fullResult, n == 200)
result500 <- subset(fullResult, n == 500)
result2000 <- subset(fullResult, n == 2000)

fullResult <- cbind(result200,result500[,-c(1,2)],result2000[,-c(1,2)])
fullResult <- fullResult[,-2]

tab <- xtable(fullResult, align = "ccccccccccc")
digits(tab) <- c(0,1,rep(0,9))
print(tab, include.rownames=FALSE)

