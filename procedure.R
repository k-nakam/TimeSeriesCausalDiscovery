proc_xy <- function(p){
  data <- case1_norm(q1 = p[1], q2 = p[1], nonlinear = p[2], 
                     ar = c(p[3],p[4]), ar3 = c(p[5],p[6]),linear = p[7])
  sum1 <- causality2_gp(data$x, data$y, confound = FALSE) #forward without confounding correction
  sum2 <- causality2_gp(data$y, data$x, confound = FALSE) #backward without confounding correction
  sum3 <- causality2_gp(data$x, data$y) #forward with confounding correction
  sum4 <- causality2_gp(data$y, data$x) #backward with confounding correction
  sum5 <- causality2_gp(data$x, data$y, confound = "nonlinear") #forward with confounding correction
  sum6 <- causality2_gp(data$y, data$x, confound = "nonlinear") #backward with confounding correction
  return(c(p[1], p[2], p[3], p[4], p[5], p[6], p[7], sum1,sum2,sum3,sum4,sum5,sum6))
}