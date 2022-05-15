rm(list = ls())
library(truncnorm)
library(mvtnorm)



trials = c(50,200)
pars = c(32,64)
subjects = c(32,64,256)


tmp<-expand.grid(trials,pars,subjects)
names(tmp)<-c("trials", "pars","subjects")

for(permutation in 1:nrow(tmp)){
  n_pars <- tmp$pars[permutation]
  n_subj <-tmp$subjects[permutation]
  n_trials <- tmp$trials[permutation]     #number trials per subject per conditions
  #n_factors <- tmp$factors[permutation]
  
  parNames <- paste0("V", 1:n_pars)
  covMat <- MCMCpack::riwish(n_pars*1.1, diag(n_pars))
  x = seq(-5, 5, length.out = n_pars)
  all_data <- data.frame()
  all_means <- data.frame()
  for(j in 1:n_subj){
    sub_mean <- mvtnorm::rmvnorm(1, x, covMat)
    sub_data <- data.frame(subject = rep(j, n_trials))
    sub_data[,2:(n_pars + 1)] <- data.frame(mvtnorm::rmvnorm(n_trials, sub_mean, diag(1, nrow = n_pars)))
    all_data <- rbind(all_data, sub_data)
    names(sub_mean) <- parNames
    subj_mean <- cbind(subject = j, sub_mean)
    all_means <- rbind(all_means,subj_mean)
  }
  setwd(r"(D:\Psychology\Master\Research Internship\Data\simData)")
  save(all_data,file=paste0(paste("data",n_subj,"subs",n_pars,"pars",n_trials,"trials" ,sep="_"),".RData"))
  setwd(r"(D:\Psychology\Master\Research Internship\Data\generating)")
  save(all_means,file=paste0(paste("alpha",n_subj,"subs",n_pars,"pars",n_trials,"trials" ,sep="_"),".RData"))
  save(x,file=paste0(paste("theta",n_subj,"subs",n_pars,"pars",n_trials,"trials" ,sep="_"),".RData"))
  save(covMat,file=paste0(paste("sigma",n_subj,"subs",n_pars,"pars",n_trials,"trials" ,sep="_"),".RData"))
  
}
