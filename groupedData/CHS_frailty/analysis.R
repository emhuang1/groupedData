##6/12/17


##Load datasets

rm(list=ls())

setwd("/Users/emhuang/Dropbox/research/Ravi/paper/CHS_frailty")
load("baselineData.Rdata")
load("frail.Rdata")

data <- merge(baselineData, outcome, by = "idno")

rm(baselineData, outcome)

##Simplify education variable to <= high school education versus > high school education
data$educ <- as.numeric(data$grade01 > 13)
table(data$grade01, data$educ, useNA = "ifany")

##Simplify smoking to ever or never
data$everSmoke <- as.numeric(data$smoke2 > 1) 
table(data$everSmoke, data$smoke2, useNA = "ifany")

##Omit subjects who do not have all of the baseline variables
data <- data[complete.cases(data),]
nrow(data)

##Recenter V so that V = 0 corresponds to baseline, instead of V = 2 corresponds to baseline
data$V = data$V - 2

##Remove people whose frailty status was only assessed at baseline
data <- subset(data, !(V == 0)) 

##Standardize age
data$age01 <- (data$age01-65)/10

##Standardize CRP
iqr <- quantile(data$crpblorg, .75)-quantile(data$crpblorg, .25)
data$crpblorg <- (data$crpblorg)/iqr 


##Format data so we can apply our functions directly to it
data <- data.frame(delta = data$delta,
                   V = data$V, 
                   age01 = data$age01, 
                   educ = data$educ,
                   gend01 = data$gend01, 
                   everSmoke = data$everSmoke,
                   crpblorg = data$crpblorg)

save(data, file = "data.Rdata")

##Plot of marginal distributions of the baseline variables
setwd("~/Dropbox/research/Ravi/paper/CHS_frailty")


h.age = hist(data$age01, breaks = 25)
h.crp = hist(data$crpblorg, breaks = 50)

pdf("CHSfrailty_margDist.pdf")

par(mfrow=c(3,2))
h.age$density = h.age$counts/sum(h.age$counts)
plot(h.age,freq=FALSE,main = "Histogram of Standardized Age", xlab = "Standardized Age", ylab = "Proportion of subjects")


prop <- table(data$educ)/nrow(data)
barplot(prop, main = "Education Beyond High School", names.arg = c("no", "yes"), ylim = c(0,1), ylab = "Proportion of subjects")


prop <- table(data$gend01)/nrow(data)
barplot(prop, main = "Gender", names.arg = c("female","male"), ylim = c(0,1), ylab = "Proportion of subjects")


prop <- table(data$everSmoke)/nrow(data)
barplot(prop, main = "Smoking Habits", names.arg = c("never smoker", "current or former smoker"), ylim = c(0,1), ylab = "Proportion of subjects")


h.crp$density = h.crp$counts/sum(h.crp$counts)
plot(h.crp,freq = FALSE,main = "Histogram of Standardized CRP Level", xlab = "Standardized CRP Level", ylab = "Proportion of subjects")

dev.off()


## Get regression coefficient estimates for Laplace, Analytic, Breslow, Efron, and PG

setwd("~/Dropbox/research/Ravi/paper/genericCode")
source("allCode.R")

result <- applyMethods(data)

coefs.efron <- result[1:5]
coefs.breslow <- result[6:10]
coefs.Laplace <- result[11:15]
coefs.Felix  <- result[16:20]
coefs.pg <- result[21:25]

se.efron <- result[26:30]
se.breslow <- result[31:35]
se.Laplace <- result[36:40]
se.Felix <- result[41:45]
se.pg <- result[46:50]

result <- cbind(coefs.pg, se.pg, coefs.Laplace, se.Laplace, coefs.Felix, se.Felix, coefs.efron, se.efron, coefs.breslow, se.breslow)

library(xtable)
result <- xtable(result, align = "ccccccccccc")
digits(result) <- 4
print(result, include.rownames=FALSE)

summary(c(coefs.Laplace-coefs.pg, se.Laplace-se.pg))
summary(c((coefs.Laplace-coefs.pg)/coefs.pg*100, (se.Laplace-se.pg)/se.pg*100))
