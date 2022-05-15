library(GauPro) #Gaussian Process
library(kernlab) #Gaussian Process (gausspr)
library(GPfit) #Gaussian Process
library(fields) #Thin plate spline regression
library(tidyverse)

lagvec <- function(x,lag = 1) dplyr::lag(x, n = lag, default = 0) #to avoid conflict
pred <- function(fit) kernlab::predict(fit)

causality2_gp <- function(x, y, thres = 0.02, alpha = 1, confound = "linear", verbose = FALSE, method = "rbfdot"){ 
  #checking causality between two variables using kernel regression assuming AR(1) using Gaussian Process
  #thres: threshold to accept/reject hypothesis (0.02 is the default in the paper)
  #alpha: optional arg to correct the significance (bonferroni correction)
  #confound: optional arg for confounding correction (from residual regression, according to FWL theorem)
  #method: you should use gausspr package as it is fast
  #options: rbfdot, polydot,vanilladot, tanhdot, laplacedot,besseldot,anovadot,splinedot
  if (confound == "linear"){
    x <- resid(lm(x ~ lagvec(x)))
    #y <- resid(lm(y ~ lagvec(x))) #other options
    #y <- resid(lm(y ~ lagvec(y)))
  } else if (confound == "nonlinear"){
    fit <- gausspr(x ~ lagvec(x), kernel = method, cross = 300)
    resid <- y - pred(fit)  
  }
  
  if (method == "GP_fit") {
    fit <- GP_fit(x, y)
    resid <- y - predict(fit)$Y_hat
  } else if (method == "GauPro") {
    fit <- GauPro(x, y, parallel=FALSE)
    resid <- y - fit$predict(x)
  } else if (method == "TPS") {
    fit <- Tps(x,y)
    resid <- y - predict(fit)
  }  else {
    fit <- gausspr(y ~ x, kernel = method, cross = 300)
    resid <- y - pred(fit)
  } 
  test <- cor.test(resid, x)
  if (verbose){print(test$p.value)}
  if (test$p.value > thres/alpha){
    return(1) #residuals are independent of x (no endogeneity)
  } else {
    return(0) #residuals are dependent of x (endogeneity)
  }
}