#####################################################
#####    Intertemporal Causal Discovery         #####
#####################################################
#Last Update: May/15/2022 
#Written by Kentaro Nakamura
#Environment: R version 4.2.0
#required packages: Gaupro, kernlab, GPfit, fields, tidyverse

rm(list = ls())
#setwd("/Users/kentaro/Dropbox/UChicago/3. 2022Spring/STAT37795 Causal Inference for Machine Learning/STAT37795 final project/experiment")
#setwd("C:\Users\tarok\Dropbox\UChicago\3. 2022Spring\STAT37795 Causal Inference for Machine Learning\STAT37795 final project\experiment")
getwd()

###########Installing packages/functions ###########
source("algorithm.R") #algorithm to check causality
source("simulation.R") #simulation
source("procedure.R") #contains process of experiment
library(parallel)

########### Hyperparameters ###########
qrange = 1 #q2
brange = 1
arx = seq(0,1,0.1)
ary = seq(0,1,0.1)
ar3x = 0
ar3y = 0
linear = 1
iter = 3
param <- expand_grid(q2 = qrange, b = brange, arx = arx, ary = ary, 
                     ar3x = ar3x, ar3y = ar3y,
                     linear = linear)

#######################################
#warning: the code below might take some time
core = detectCores()
cl <- makeCluster(getOption("cl.cores", core))
clusterExport(cl=cl, varlist=c("lagvec","pred","proc_xy","param","gausspr","case1_norm", "causality2_gp"))
parLapply(cl, 1:iter,function(i) {apply(param, 1, proc_xy)})
stopCluster(cl)
#(for mac)res <- mclapply(1:iter, function(i) {apply(param, 1, proc_xy)}, mc.cores = core)

########### Plot ###########
res_formatted<- sapply(1:(11*nrow(param)), 
                       FUN = function(i){sapply(res, function(x){sum(x[i])}) %>% sum()}) %>%
  matrix(nrow = 11) %>% t() %>% data.frame()
colnames(res_formatted) <- c("q2", "nonlinear", "arx","ary","ar3x","ar3y","linear", "forward","backward","forward_c","backward_c")
res_formatted <- res_formatted/iter

res_formatted %>%
  select(arx,ary,forward,backward,forward_c,backward_c) %>%
  pivot_longer(!c(arx,ary), names_to = "method",values_to = "Pcorrect") %>%
  ggplot() +
  geom_line(aes(x = arx, y = Pcorrect, color = method), alpha = 0.5) +
  labs(x = "Coef of x:AR(1)",
       y = expression(P[correct])) +
  theme_bw() +
  facet_wrap(~ary)