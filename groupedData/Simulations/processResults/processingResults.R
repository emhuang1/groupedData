##6/28/17

rm(list=ls())

#####################################################################################################
##FUNCTIONS##
#####################################################################################################

SE.fun <- function(x){sqrt(mean((x-mean(x))^2))}
RMSE.fun <- function(x){sqrt(mean((x-Beta)^2))}

estimator <- function(Beta,result){
  estimates <- result[,1:(ncol(result)/2)]
  if (sum(is.na(estimates)) > 0){
    warning(paste("There are NA's in the regression coefficient estimates."))
  }
  bias <- colMeans(estimates)-Beta
  se <- apply(estimates,2,SE.fun)
  rmse <- apply(estimates,2,RMSE.fun)
  return(c(bias,se,rmse))
}

ci <- function(Beta, result){
 estimates <- result[,1:(ncol(result)/2)]
 se <- result[,((ncol(result)/2)+1):ncol(result)]
 leftLim <- estimates - 1.96*se ##left limit of CI
 rightLim <- estimates + 1.96*se ##right limit of CI
 containBeta <- (Beta >= leftLim) & (Beta <= rightLim) ##indicator of whether CI includes Beta
 return(colMeans(containBeta))
}




