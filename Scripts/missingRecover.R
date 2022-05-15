rm(list = ls())
#setwd("~/Documents/Research/Amsterdam/Code/FA/factorAnalysisTest/missingData")

setwd(r"(D:\Psychology\Master\Research Internship\Data)")

ll_old <- function(x, data){
  n_pars <- length(x)
  out <- numeric(length(x))
  for(i in 2:ncol(data)){
    print(i)
    if(any(is.na(data[,i]))) {
      out[i-1] <- 1e-10
    }else{
      out[i-1] <- sum(dnorm(data[,i], mean = x[i-1], sd = 1, log=T))
    }
  }
    return(sum(out))
  #return(sum(dnorm(data[,2:(1 + n_pars)], mean = x, sd = 1, log=T)))
  #return(sum(mvtnorm::dmvnorm(as.matrix(data[,2:(1 + n_pars)]), x, diag(1, nrow = n_pars), log = T)))
}


ll <- function(x, data, sample= F){
  # x<-pars
  # remove <- as.numeric(which(is.na(data[1,])))-1
  # data <- data[ , apply(data, 2, function(x) !any(is.na(x)))]
  # x <- x[-remove]
  
  return(sum(na.omit(as.vector(mapply(dnorm, data[,-1], x, MoreArgs = list(sd=1,log=T))))))
  
  #return(sum(na.omit(as.vector(apply(data[,-1], 2, function(p) dnorm(p, mean=x, sd=1, log=T))))))
}
  


LO.s = c(.25,.5,.75)
LO.d = c(.25,.5,.75)
files = list.files("simData")

perm <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
perm <- 51

tmp<-expand.grid(LO.s,LO.d, files)
names(tmp)<-c("subs", "data", "files")


load(paste0("simData/",tmp$files[perm]))
leaveOut <- tmp[perm,]
missingSubs <- max(all_data$subject)*leaveOut$subs
missingData <- (ncol(all_data)-1)*leaveOut$data
leaveOutSubs <- sample(x= unique(all_data$subject), size = missingSubs, replace = F)
for(i in 1:missingSubs){
  leaveOutData = sample(x= ncol(all_data[-1]), size = missingData, replace = F)
  for(j in 1:missingData){
    removeSubject = which(all_data$subject==leaveOutSubs[i])
    removeColumn = as.numeric(leaveOutData[j])+1
    all_data[removeSubject,removeColumn] <- NA  
  }
}

parNames = names(all_data[,-1])
#setwd("~/Documents/Research/Amsterdam/Code")
setwd(r"(D:\pmwg_new)")
source("pmwg/variants/standard.R")
sampler <- pmwgs(
    data = all_data,
    pars = parNames,
    ll_func = ll,
)

# start the sampler ---------------------------------------------------------
sampler <- init(sampler, n_cores = 1) # i don't use any start points here
    
# Sample! -------------------------------------------------------------------
burned <- run_stage(sampler, stage = "burn",iter = 250, particles = 150, n_cores = 1, pstar = 0.6)
adapted <- run_stage(burned, stage = "adapt",iter = 150, particles = 150, n_cores =1, pstar = .6)
sampled <- run_stage(adapted, stage = "sample",iter = 100, particles = 150, n_cores =1, pstar = .6)

setwd(r"(D:\Psychology\Master\Research Internship\Data)")
save(sampled, file = "test_simulation.Rdata")
save(sampled, file = paste0("samples/MVN_", n_subj, "S_", n_pars, "P_.RData"))
