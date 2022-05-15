########### Simulating Data ###########
#Case 1: X -> Y and both X and Y are time sequence (AR(1))
case1_norm <- function (q1 = 1, q2 = 1, nonlinear = 0, 
                        ar = c(0,0), ar3 = c(0,0),
                        linear = 1.0, n = 300){ 
  #ar: list of ar parameters (we should assume non unit-root otherwise we suffer from spurious correlation)
  #ar3: list of ar parameters (non-linear)
  #q: error size (q = 1: normal/ q > 1 or q < 1 yields super- or sub-Gaussian)
  #linear: linear effect of x -> y
  #nonlinear: strength of non-linearity
  #n: sample size (default: 300 as in the paper)
  x <- c(); y <- c()
  xnoise <- rnorm(1); ynoise <- rnorm(1)
  x[1] <- sign(xnoise)*abs(xnoise)^q1
  y[1] <- sign(ynoise)*abs(ynoise)^q2 + linear*as.numeric(x[1]) + nonlinear*as.numeric(x[1])^3
  for (i in 2:n){
    xnoise <- rnorm(1); ynoise <- rnorm(1)
    x[i] <- sign(xnoise)*abs(xnoise)^q1 + ar[1]*as.numeric(x[i-1]) + ar3[1]*as.numeric(x[i-1])^3
    y[i] <- sign(ynoise)*abs(ynoise)^q2 + ar[2]*as.numeric(y[i-1]) + ar3[1]*as.numeric(y[i-1])^3+
      linear*as.numeric(x[i]) + nonlinear*as.numeric(x[i])^3
  }
  df <- data.frame(x = x, y = y)
  return(df)
}