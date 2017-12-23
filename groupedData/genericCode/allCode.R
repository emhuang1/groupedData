##5/31/17##

##Code for:
#PG
#Laplace
#Felix
#Efron
#Breslow


###############################################################################
##Libraries##
###############################################################################
library(survival)
library(numDeriv)
library(dfoptim)

###############################################################################
##INPUT##
###############################################################################
##data set entitled "data": 
##one row per person, first two columns are (delta,V), 
##the following columns are the covariates

##delta: 1 if failure was observed, 0 otherwise 

#if delta = 1: V is the follow-up visit that failure was first observed
#if delta = 0: V is the follow-up visit that person was last assessed (at which the person
#had not failed yet)

##visits should be numbered so that V = 0 is baseline
##the numbering should also coincide with the timing of the visits
##e.g., if the visits are not evenly spaced, they should not be labeled as 0,1,2,3,4,..

##remove people who had already experienced the event at baseline
##remove people who were only observed at baseline, never showed up to follow-ups
##there should be no V = 0 in the data set

##Test data set
##n <- 200
##data <- data.frame(delta = sample(0:1,n,replace=TRUE), V = sample(1:6,n,replace = TRUE), X = sample(0:1,n,replace=TRUE), Z = rnorm(n, mean = 0, sd = 1), K = rnorm(n, mean = 20, sd = 10))

applyMethods <- function(data){
  ###############################################################################
  ##VARIABLES THAT WILL BE USED##
  ###############################################################################
  
  xnam <- names(data)[3:ncol(data)]
  
  fmla <- as.formula(paste("Surv(V,delta) ~ ", paste(xnam, collapse= "+")))
  
  numCov <- ncol(data) - 2
  
  n <- nrow(data)
  
  failureVisits <- sort(unique(data$V[data$delta == 1])) ##get the follow-up visits where failures are observed
  ###############################################################################
  ##Efron##
  ###############################################################################
  mod.efron <- coxph(fmla, ties = "efron", data=data)
  coefs.efron <- summary(mod.efron)$coef[,1]
  se.efron <- summary(mod.efron)$coef[,3]
  
  ###############################################################################
  ##Breslow##
  ###############################################################################
  mod.breslow <- update(mod.efron, ties = "breslow") 
  coefs.breslow <- summary(mod.breslow)$coef[,1]
  se.breslow <- summary(mod.breslow)$coef[,3]
  
  ###############################################################################
  ##R-exact##
  ###############################################################################
  #mod.Rexact <- update(mod.efron, ties = "exact")
  #coefs.Rexact <- summary(mod.Rexact)$coef[,1]
  #se.Rexact <- summary(mod.Rexact)$coef[,3]
  
  ###############################################################################
  ##PG##
  ###############################################################################
  PGloglik <- function(x, data){
    k <- Nfollowup
    p <- length(x) - k 
    pterm <- rep(0, nrow(data))
    for (i in 1:p) pterm <- pterm + (data[,i+1] * x[i]) ##first column of data is id  
    sterm <- rep(0, nrow(data))
    for (i in 1:k) sterm <- sterm + (data$seqvar == i) * x[i+p]  
    h.jx <- 1 - exp(-exp(pterm + sterm))
    val <-  sum(data$y.ij*log(h.jx) + (1-data$y.ij)*log(1-h.jx))
    return(val)
  }
  
  ##Gradient to help optim estimate PG model parameters
  PGgradient <- function(x, data){
    grad <- rep(NA, length(x))
    k <- Nfollowup
    p <- length(x) - k 
    pterm <- rep(0, nrow(data))
    for (i in 1:p) pterm <- pterm + (data[,i+1] * x[i]) ##first column is id 
    sterm <- rep(0, nrow(data))
    for (i in 1:k) sterm <- sterm + (data$seqvar == i) * x[i+p]
    alpha.ij <- exp(pterm + sterm)
    h.ij <- 1 - exp(-alpha.ij)
    for (i in 1:p) grad[i] <- sum(data$y.ij*exp(-alpha.ij)*alpha.ij/(h.ij)*data[,i+1] - (1-data$y.ij)*exp(-alpha.ij)*alpha.ij/(1-h.ij)*data[,i+1])
    for (i in 1:k) grad[p+i] <- sum((data$y.ij * exp(-alpha.ij)*alpha.ij/h.ij - (1-data$y.ij)*exp(-alpha.ij)*alpha.ij/(1-h.ij))*(data$seqvar == i))
    return(grad)
  }
  
  ##Hessian for getting standard errors
  PGhessian <- function(data, coefs.pg){
    hessian <- matrix(NA, nrow = Nfollowup + numCov, ncol = Nfollowup + numCov)
    pterm <- rep(0, nrow(data))
    for (i in 1:numCov) pterm <- pterm + (data[,i+1] * coefs.pg[i]) ##first column is id 
    sterm <- rep(0, nrow(data))
    for (i in 1:Nfollowup) sterm <- sterm + (data$seqvar == i) * coefs.pg[i+numCov]
    alpha.ij <- exp(pterm + sterm)
    h.ij <- 1 - exp(-alpha.ij)
    x.ij <- 1/(exp(alpha.ij)-1)-alpha.ij*exp(alpha.ij)/(exp(alpha.ij)-1)^2
    
    for (i in 1:numCov){
      for (j in 1:numCov){
        hessian[i,j] <- sum(data$y.ij*data[,1+i]*data[,1+j]*alpha.ij*x.ij-(1-data$y.ij)*data[,1+i]*data[,1+j]*alpha.ij)
      }
    }
    
    for (i in 1:Nfollowup){
      for (j in 1:Nfollowup){
        if (i != j) {
          hessian[numCov+i,numCov+j] <- 0 
        } else {
          hessian[numCov+i,numCov+i]<-sum((data$seqvar==i) * (data$y.ij*alpha.ij*x.ij-alpha.ij*(1-data$y.ij)))
        }
      }
    }
    
    for (i in 1:numCov){
      for (j in 1:Nfollowup){
        hessian[i,j+numCov] <- hessian[j+numCov, i] <- sum((data$seqvar==j)*(data$y.ij*alpha.ij*data[,1+i]*x.ij-(1-data$y.ij)*alpha.ij*data[,1+i]))
      }
    }
    
    return(hessian)
    
  }
  
  ek <- unname(with(data,table(V, delta))[,2])
  
  if (sum(ek == 0)>0){
    print(paste("Simulation",k,"PG: zero failures at some visit."))
  }
  
  followup_visits <- sort(unique(data$V))
  Nfollowup <- length(followup_visits)
  

  
  dat <- data.frame(id = 1:nrow(data), data) ##dat will be data in long format
  
  ##Relabel V so it is 1, 2, ... This is just for PG, which is why we don't do it to data.
  dat$V <- as.numeric(as.factor(dat$V))
  
  seqvar <- 1:dat$V[1] ##will define as in Jenkins STB
  for(i in 2:nrow(dat)){
    seqvar <- c(seqvar,1:dat$V[i])
  }
  dat <- dat[rep(1:nrow(dat),times=dat$V),] 
  dat$seqvar <- seqvar
  dat$y.ij <- 0 ##we will define y.ij as in Jenkin's STB
  dat$y.ij[dat$delta == 1 & dat$seqvar == dat$V] <- 1
  
  dat <- dat[,-c(2,3)] ##remove delta and V columns
  
  modPG <-  optim(c(rep(0, numCov), log(-log(1-(1+ek)/n))), PGloglik, gr = PGgradient, method="BFGS", data=dat, control=list(fnscale=-1), hessian = FALSE)
  coefs.pg <-  modPG$par
  se.pg <- sqrt(diag(solve(-PGhessian(dat, coefs.pg))))
  coefs.pg <- coefs.pg[1:numCov]
  se.pg <- se.pg[1:numCov]
  
  ###############################################################################
  ##Felix##
  ###############################################################################
  ##Took out the gradient code
  
  felixPL <- function(Beta, data){
    logPL <- 0
    data$r <- exp(as.matrix(data[,3:(2+numCov)])%*%Beta)
    for (v in 1:length(failureVisits)){
      riskSet <- subset(data, V >= failureVisits[v])
      riskSet.top <- subset(riskSet, V == failureVisits[v] & delta == 1)
      d <- nrow(riskSet.top)
      riskSet.bottom <- subset(riskSet, !(V == failureVisits[v] & delta == 1))
      riskSet <- rbind(riskSet.top, riskSet.bottom)
      sumRisk <- sum(riskSet$r[(d+1):nrow(riskSet)])
      logPL <- logPL + sum(log(1-exp(-riskSet$r[1:d]/sumRisk*(d+1))))
    }
    return(logPL)
  }
  
  mod.Felix <- optim(coefs.pg, felixPL, method="BFGS", data=data, control=list(fnscale=-1), hessian = FALSE)
  coefs.Felix <- mod.Felix$par
  numHess.Felix <- hessian(func = felixPL, x = coefs.Felix, method = "Richardson", data = data)
  se.Felix <- sqrt(diag(solve(-numHess.Felix)))
  
  ###############################################################################
  ##Laplace##
  ###############################################################################
  laplaceFun <- function(x, a.j){
    val <- rep(0,length(x))
    for (i in 1:length(x)){
      val[i] <- sum(a.j/(exp(a.j*x[i])-1)) - 1
    }
    return(val)
  }
  
  laplacePL <- function(Beta, data){
    logPL <- 0
    for (v in 1:length(failureVisits)){ 
      ##consider only those who are at risk 
      riskSet <- subset(data, V >= failureVisits[v]) 
      
      ##compute every person's risk score
      riskSet$r <- exp(as.matrix(riskSet[,3:(2+numCov)])%*%Beta) 
      
      #people who failed at visit v
      riskSet.top <- subset(riskSet, V == failureVisits[v] & delta == 1) 
      d <- nrow(riskSet.top) ##number of people who failed at visit v 
      
      #the people who didn't fail
      riskSet.bottom <- subset(riskSet, !(V == failureVisits[v] & delta == 1))
      
      ##order so that the first d rows are the people who failed, the following
      ##rows are the people in the risk set who didn't fail
      riskSet <- rbind(riskSet.top, riskSet.bottom) 
      
      ##add the risk scores of the people in the risk set who didn't fail
      sumRisk <- sum(riskSet$r[(d+1):nrow(riskSet)]) 
      
      if (d > 1){
        ##vector of risk scores of people who failed divided by sum of risk scores of people
        ##who didn't fail
        a.j <- riskSet$r[1:d]/sumRisk
        
        ##to prevent exp(a.j*x) - 1 from being 0
        a.j[a.j < 1e-16] <- 1e-16
        
        ##compute lower and upper bounds for tStar
        res <- laplaceFun(1:d, a.j)
        lowerLim <- max(which(res > 0))
        upperLim <- min(which(res < 0))
        
        ##compute tStar
        tStar <- uniroot(f = laplaceFun, interval = c(lowerLim,upperLim), extendInt = "no", a.j = a.j)$root
        
        sigmaStar <- sum(a.j^2 * exp(a.j*tStar)/(exp(a.j*tStar) - 1)^2)
        
        logPL <- logPL - 1/2*log(sigmaStar) + sum(log(1-exp(-a.j*tStar))) - tStar + pnorm(0, mean = tStar, sd = 1/sqrt(sigmaStar), lower.tail = FALSE, log.p = TRUE)
      } else {
        logPL <- logPL + log(riskSet$r[1]) - log(riskSet$r[1]+sumRisk)
      }
      
    }
    return(logPL)
  }

  if (numCov > 1){
    mod.Laplace <- tryCatch(optim(coefs.pg, laplacePL, method = "BFGS", data = data, control = list(fnscale=-1), hessian = FALSE),
                            error = function(e){
                              nmk(coefs.pg, laplacePL, control = list(maximize = TRUE), data = data)
                            })
    
    coefs.Laplace <- mod.Laplace$par
    numHess.Laplace <- hessian(func = laplacePL, x = coefs.Laplace, method = "Richardson", data = data)
    se.Laplace <- sqrt(diag(solve(-numHess.Laplace)))
    if (sum(is.nan(se.Laplace))>0){
      mod.Laplace <- optim(coefs.pg, laplacePL, method = "BFGS", data = data, control = list(fnscale=-1), hessian = TRUE)
      coefs.Laplace <- mod.Laplace$par
      se.Laplace <- sqrt(diag(solve(-mod.Laplace$hessian)))
    }
  } else {
    mod.Laplace <- optimize(laplacePL, interval = c(-10,10), maximum = TRUE, data = data)
    coefs.Laplace <- mod.Laplace$maximum
    numHess.Laplace <- hessian(func = laplacePL, x = coefs.Laplace, method = "Richardson", data = data)
    se.Laplace <- sqrt(diag(solve(-numHess.Laplace)))
    if (is.nan(se.Laplace)){
      mod.Laplace <- optim(coefs.pg, laplacePL, method = "BFGS", data = data, control = list(fnscale=-1), hessian = TRUE)
      coefs.Laplace <- mod.Laplace$par
      se.Laplace <- sqrt(diag(solve(-mod.Laplace$hessian)))
    }
  }

  
  
  ###############################################################################
  ##Assemble all results##
  ###############################################################################
  
  beta <- c(coefs.efron, coefs.breslow, coefs.Laplace, coefs.Felix, coefs.pg)
  
  se <- c(se.efron, se.breslow, se.Laplace, se.Felix, se.pg)
  
  return(c(beta,se))
  
}

