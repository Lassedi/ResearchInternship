rm(list=ls())
library(dplyr)
library(rtdists)
library(pmwg)

# setwd("~/Documents/Research/Amsterdam/Code/pmwg_new-master")
# source("pmwg/sampling_factors.R")
#setwd("~/Documents/Research/Honours_2021/output/Missing_Data")
setwd(r"(D:\Psychology\Master\Research Internship\Data)")
load("fullData.Rdata")
df <- data[1:2,]
nBack_ll <- function(x, data, sample = FALSE){                   # x = named vector of parameters, sample = sample values from the posterior distribution?
  names(x)[1]<-"C"
  if (sample) {
    data$responseSample <- NA                                            #if we are sampling, clear the 'response' column
    for (i in 1:nrow(data)){
      if (data$cond[i] == "target"){                                          # if this is a target
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["target.d"], sd = 1) > x["C"],"target","nontarget")
      }else if (data$cond[i] == "lure"){                            # Now if this is a lure
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["lure.d"], sd = 1) > x["C"],"target","nontarget") 
      }else{                                                        # and if this is a nontarget
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["nonlure.d"], sd = 1) > x["C"],"target","nontarget")
        } 
    }
    return(data)
  } else {
    out <- numeric(nrow(data))                                     #otherwise, create a vector containing the log-likelihood value for each trial
    for (i in 1:nrow(data)){
      if (data$cond[i] == "target"){                                # if this is a target
        if (data$response[i] == "target"){                          # and they responded "target"
          out[i] <- pnorm(x["C"], mean = x["target.d"], sd = 1,     # find the probability of x being   
                          log.p = TRUE, lower.tail = FALSE)         # above the criterion
        } else {                                                    # but if they responded "not-target"
          out[i] <- pnorm(x["C"], mean = x["target.d"], sd = 1,     # find the probability of x being  
                          log.p = TRUE, lower.tail = TRUE)          # below the criterion
        } 
      }else if (data$cond[i] == "lure"){                            # Now if this is a lure
        if (data$response[i] == "target"){                          # and they responded "target"
          out[i] <- pnorm(x["C"], mean = x["lure.d"], sd = 1,       # find the probability of x being 
                          log.p = TRUE, lower.tail = FALSE)         # above the criterion
        } else {                                                    # but if they responded "non-target"
          out[i] <- pnorm(x["C"], mean = x["lure.d"], sd = 1,       # find the probability of x being
                          log.p = TRUE, lower.tail = TRUE)          # below the criterion
        } 
      }else{                                                        # and if this is a nontarget
        if (data$response[i] == "target"){                          # and they responded "target"
          out[i] <- pnorm(x["C"], mean = x["nonlure.d"], sd = 1,  # find the probability of x being  
                          log.p = TRUE, lower.tail = FALSE)         # above the criterion
        } else {                                                    # but if they responded "nontarget"
          out[i] <- pnorm(x["C"], mean = x["nonlure.d"], sd = 1,  # find the probability of x being
                          log.p = TRUE, lower.tail = TRUE)          # below the criterion
        } 
      }
    }
    sum(out)
  }
}


pdt_SDT_ll <- function(x, data, sample = FALSE){                   # x = named vector of parameters, sample = sample values from the posterior distribution?
  names(x)[1]<-"C"
  if (sample) {
    data$responseSample <- NA                                            #if we are sampling, clear the 'response' column
    for (i in 1:nrow(data)){
      if (data$difficulty[i] == "easy"){
        if (data$cond[i]=="blue"){                                          # if this is a target
          data$responseSample[i] <- ifelse(rnorm(1, mean = x["B_E.d"], sd = 1) > x["C"],"orange","blue")
        } else {                                          # if this is a target
          data$responseSample[i] <- ifelse(rnorm(1, mean = x["O_E.d"], sd = 1) > x["C"],"orange","blue")
        }
      } else if (data$difficulty[i]=="medium"){
        if (data$cond[i]=="blue"){                                          # if this is a target
          data$responseSample[i] <- ifelse(rnorm(1, mean = x["B_M.d"], sd = 1) > x["C"],"orange","blue")
        } else {                                          # if this is a target
          data$responseSample[i] <- ifelse(rnorm(1, mean = x["O_M.d"], sd = 1) > x["C"],"orange","blue")
        }
      }else {
        if (data$cond[i]=="blue"){                                          # if this is a target
          data$responseSample[i] <- ifelse(rnorm(1, mean = x["B_H.d"], sd = 1) > x["C"],"orange","blue")
        } else {                                          # if this is a target
          data$responseSample[i] <- ifelse(rnorm(1, mean = 0, sd = 1) > x["C"],"orange","blue")
        }
      }
    }
    return(data)
  } else {
    out <- numeric(nrow(data))                                     #otherwise, create a vector containing the log-likelihood value for each trial
    for (i in 1:nrow(data)){
      if (data$cond[i] == "blue"){
        if (data$difficulty[i] == "easy"){
          if (data$response[i] == "blue"){
            out[i] <- pnorm(x["C"], mean = x["B_E.d"], sd = 1,         #<-If blue and easy and they response "blue"     
                            log.p = TRUE, lower.tail = TRUE)
          } else {
            out[i] <- pnorm(x["C"], mean = x["B_E.d"], sd = 1,         #<-If blue and easy and they response "orange"       
                            log.p = TRUE, lower.tail = FALSE)
          }
        } else if (data$difficulty[i] == "medium"){
          if (data$response[i] == "blue"){
            out[i] <- pnorm(x["C"], mean = x["B_M.d"], sd = 1,        #<-<-If blue and medium and they response "blue"      
                            log.p = TRUE, lower.tail = TRUE)
          } else {
            out[i] <- pnorm(x["C"], mean = x["B_M.d"], sd = 1,        #<- If blue and medium and they response "orange"      
                            log.p = TRUE, lower.tail = FALSE)
          }
        } else {
          if (data$response[i] == "blue"){
            out[i] <- pnorm(x["C"], mean = x["B_H.d"], sd = 1,        #<-<-If blue and hard and they response "blue"      
                            log.p = TRUE, lower.tail = TRUE)
          } else {
            out[i] <- pnorm(x["C"], mean = x["B_H.d"], sd = 1,        #<- If blue and hard and they response "orange"      
                            log.p = TRUE, lower.tail = FALSE)
          }
        }
      } else{
        if (data$difficulty[i] == "easy"){
          if (data$response[i] == "orange"){
            out[i] <- pnorm(x["C"], mean = x["O_E.d"], sd = 1,        #<-<-If orange and easy and they response "orange"      
                            log.p = TRUE, lower.tail = FALSE)
          } else {
            out[i] <- pnorm(x["C"], mean = x["O_E.d"], sd = 1,        #<- If orange and easy and they response "blue"      
                            log.p = TRUE, lower.tail = TRUE)
          }
        } else if (data$difficulty[i] == "medium"){
          if (data$response[i] == "orange"){
            out[i] <- pnorm(x["C"], mean = x["O_M.d"], sd = 1,        #<-<-If orange and medium and they response "orange"      
                            log.p = TRUE, lower.tail = FALSE)
          } else {
            out[i] <- pnorm(x["C"], mean = x["O_M.d"], sd = 1,        #<- If orange and medium and they response "blue"      
                            log.p = TRUE, lower.tail = TRUE)
          }
        } else {
          if (data$response[i] == "orange"){
            out[i] <- pnorm(x["C"], mean = 0, sd = 1,                   #<-<-If orange and hard and they response "orange"      
                            log.p = TRUE, lower.tail = FALSE)
          } else {
            out[i] <- pnorm(x["C"], mean = 0, sd = 1,                   #<- If orange and hard and they response "blue"      
                            log.p = TRUE, lower.tail = TRUE)
          }
        }
      }
    }
    sum(out)
  }
}


edt_SDT_ll <- function(x, data, sample = FALSE){                   # x = named vector of parameters, sample = sample values from the posterior distribution?
  names(x)[1]<-"C"
  if (sample) {
    data$responseSample <- NA                                            #if we are sampling, clear the 'response' column
    for (i in 1:nrow(data)){
      if (data$cond[i] == "Happy"){                                
        if (data$Gender[i] == "male"){
          if (data$Mouth[i] == "open") {                                           # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["H_M_O.d"], sd = 1) > x["C"],"Angry","Happy")
          } else {                                          # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["H_M_C.d"], sd = 1) > x["C"],"Angry","Happy")
          }
        } else{
          if (data$Mouth[i] == "open") {                                           # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["H_F_O.d"], sd = 1) > x["C"],"Angry","Happy")
          } else {                                          # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["H_F_C.d"], sd = 1) > x["C"],"Angry","Happy")
          }
        } 
      } else  {
        if (data$Gender[i] == "male"){
          if (data$Mouth[i] == "open") {                                           # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["A_M_O.d"], sd = 1) > x["C"],"Angry","Happy")
          } else {                                          # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["A_M_C.d"], sd = 1) > x["C"],"Angry","Happy")
          }
        } else{
          if (data$Mouth[i] == "open") {                                           # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = x["A_F_O.d"], sd = 1) > x["C"],"Angry","Happy")
          } else {                                          # if this is a target
            data$responseSample[i] <- ifelse(rnorm(1, mean = 0, sd = 1) > x["C"],"Angry","Happy")
          }
        } 
      }
    }
    return(data)
  } else {
    out <- numeric(nrow(data))                                     #otherwise, create a vector containing the log-likelihood value for each trial
    for (i in 1:nrow(data)){
      if (data$cond[i] == "Happy"){                                
        if (data$Gender[i] == "male"){
          if (data$Mouth[i] == "open") { 
            if (data$response[i] == "Happy"){
              out[i] <- pnorm(x["C"], mean = x["H_M_O.d"], sd = 1,   #<-If happy, male and open and they response "happy"     
                              log.p = TRUE, lower.tail = TRUE)
            } else {
              out[i] <- pnorm(x["C"], mean = x["H_M_O.d"], sd = 1, #<-If happy, male and open and they response "sad"       
                              log.p = TRUE, lower.tail = FALSE)
            }
          } else {
            if (data$response[i] == "Happy"){
              out[i] <- pnorm(x["C"], mean = x["H_M_C.d"], sd = 1,   #<-If happy, male and closed and they response "happy"     
                              log.p = TRUE, lower.tail = TRUE)
            } else {
              out[i] <- pnorm(x["C"], mean = x["H_M_C.d"], sd = 1,   #<-If happy, male and closed and they response "sad"       
                              log.p = TRUE, lower.tail = FALSE)
            } 
          }
        } else {
          if (data$Mouth[i] == "open") {
            if (data$response[i] == "Happy"){
              out[i] <- pnorm(x["C"], mean = x["H_F_O.d"], sd = 1,   #<-If happy, female and open and they response "happy"     
                              log.p = TRUE, lower.tail = TRUE)
            } else {
              out[i] <- pnorm(x["C"], mean = x["H_F_O.d"], sd = 1,   #<-If happy, female and open and they response "sad"       
                              log.p = TRUE, lower.tail = FALSE)
            }
          } else {
            if (data$response[i] == "Happy"){
              out[i] <- pnorm(x["C"], mean = x["H_F_C.d"], sd = 1,   #<-If happy, female and closed and they response "happy"     
                              log.p = TRUE, lower.tail = TRUE)
            } else {
              out[i] <- pnorm(x["C"], mean = x["H_F_C.d"], sd = 1,   #<-If happy, female and closed and they response "sad"       
                              log.p = TRUE, lower.tail = FALSE)
            }                                                       
          }
        }
      } else {
        if (data$Gender[i] == "male"){
          if (data$Mouth[i] == "open") {
            if (data$response[i] == "Angry"){
              out[i] <- pnorm(x["C"], mean = x["A_M_O.d"], sd = 1,         # if Angry, male and open and they respond "Angry"  
                              log.p = TRUE, lower.tail = FALSE)          
            } else {                                                     
              out[i] <- pnorm(x["C"], mean = x["A_M_O.d"], sd = 1,         #  if Angry, male and open and they respond "Happy" 
                              log.p = TRUE, lower.tail = TRUE)
            }
          } else{
            if (data$response[i] == "Angry"){
              out[i] <- pnorm(x["C"], mean = x["A_M_C.d"], sd = 1,         # if Angry, male and closed and they respond "Angry"  
                              log.p = TRUE, lower.tail = FALSE)          
            } else {                                                     
              out[i] <- pnorm(x["C"], mean = x["A_M_C.d"], sd = 1,         #  if Angry, male and closed and they respond "Happy" 
                              log.p = TRUE, lower.tail = TRUE)
            }
          }
        } else {
          if (data$Mouth[i] == "open"){
            if (data$response[i] == "Angry"){
              out[i] <- pnorm(x["C"], mean = x["A_F_O.d"], sd = 1,         # if Angry, female and open and they respond "Angry"  
                              log.p = TRUE, lower.tail = FALSE)          
            } else {                                                     
              out[i] <- pnorm(x["C"], mean = x["A_F_O.d"], sd = 1,         #  if Angry, female and open and they respond "Happy" 
                              log.p = TRUE, lower.tail = TRUE)
            }
          } else {
            if (data$response[i] == "Angry"){
              out[i] <- pnorm(x["C"], mean = 0, sd = 1,         # if Angry, female and open and they respond "Angry"  
                              log.p = TRUE, lower.tail = FALSE)          
            } else {                                                     
              out[i] <- pnorm(x["C"], mean = 0, sd = 1,         #  if Angry, female and open and they respond "Happy" 
                              log.p = TRUE, lower.tail = TRUE)
            }
          }
        }
      }
    }
    sum(out)
  } 
}

lex_SDT_ll <- function(x, data, sample = FALSE){                   # x = named vector of parameters, sample = sample values from the posterior distribution?
  names(x)[1]<-"C"
  if (sample) {
    data$responseSample <- NA                                            #if we are sampling, clear the 'response' column
    for (i in 1:nrow(data)){
      if (data$cond[i] == "HF"){                                          # if this is a target
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["HF.d"], sd = 1) > x["C"],"word","nonword")
      }else if (data$cond[i] == "LF"){                            # Now if this is a lure
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["LF.d"], sd = 1) > x["C"],"word","nonword")
      }else if (data$cond[i] == "VLF"){                            # Now if this is a lure
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["VLF.d"], sd = 1) > x["C"],"word","nonword")
      }else{                                                        # and if this is a nontarget
        data$responseSample[i] <- ifelse(rnorm(1, mean = 0, sd = 1) > x["C"],"word","nonword")
      } 
    }
    return(data)
  }  else {
    out <- numeric(nrow(data))                                     #otherwise, create a vector containing the log-likelihood value for each trial
    for (i in 1:nrow(data)){
      if (data$cond[i] == "HF"){                                    # if this is a hi frequency word
        if (data$response[i] == "word"){                                  # and they responded "word"
          out[i] <- pnorm(x["C"], mean = x["HF.d"], sd = 1,         # find the probability of x being   
                          log.p = TRUE, lower.tail = FALSE)         # above the criterion
        } else {                                                    # but if they responded "non-word"
          out[i] <- pnorm(x["C"], mean = x["HF.d"], sd = 1,         # find the probability of x being  
                          log.p = TRUE, lower.tail = TRUE)          # below the criterion
        } 
      }else if (data$cond[i] == "LF"){                            # Now if this is a low freq word
        if (data$response[i] == "word"){                                # and they responded "word"
          out[i] <- pnorm(x["C"], mean = x["LF.d"], sd = 1,       # find the probability of x being 
                          log.p = TRUE, lower.tail = FALSE)       # above the criterion
        } else {                                                  # but if they responded "non-word"
          out[i] <- pnorm(x["C"], mean = x["LF.d"], sd = 1,       # find the probability of x being
                          log.p = TRUE, lower.tail = TRUE)        # below the criterion
        } 
      }else if (data$cond[i] == "VLF"){                         # and if this is a very low freq word
        if (data$response[i] == "word"){                              # and they responded "word"
          out[i] <- pnorm(x["C"], mean = x["VLF.d"], sd = 1,    # find the probability of x being  
                          log.p = TRUE, lower.tail = FALSE)     # above the criterion
        } else {                                                # but if they responded "non-word"
          out[i] <- pnorm(x["C"], mean = x["VLF.d"], sd = 1,    # find the probability of x being
                          log.p = TRUE, lower.tail = TRUE)      # below the criterion
        } 
      }else {                                                 # aaaaand if this is not a word
        if (data$response[i] == "word"){                            # but they responded "word"
          out[i] <- pnorm(x["C"], mean = 0, sd = 1,           # find the probability of x being
                          log.p = TRUE, lower.tail = FALSE)         # above the criterion
        } else {                                              # but if they correctly said "nonword"
          out[i] <- pnorm(x["C"], mean = 0, sd = 1,           # find the probability of x being
                          log.p = TRUE, lower.tail = TRUE)          # below the criterion
        }
      }
    }
    sum(out)
  }
}


ll_joint <- function(x,data,sample=FALSE) {
  edt = x[1:8]
  pdt = x[9:14]
  nb = x[15:18]
  lex = x[19:22]
  
  sub = data$subject
  data.edt = as.data.frame(data$edt)
  data.pdt = as.data.frame(data$pdt)
  data.nb = as.data.frame(data$nback)
  data.lex = as.data.frame(data$ldt)
  
  if (sample) {
    eData = edt_SDT_ll(edt, data.edt,sample = T)
    pData = pdt_SDT_ll(pdt, data.pdt,sample = T)
    nbData = nBack_ll(nb, data.nb,sample = T)
    lData = lex_SDT_ll(lex, data.lex,sample = T)
    
    df <- tibble(
      subject = sub,
      edt = list(
        tibble(eData)
      ),
      pdt = list(
        tibble(pData)
      ),
      nback = list(
        tibble(nbData)
      ),
      ldt = list(
        tibble(lData)
      )
    )
    return(df)
  } else {
    if(nrow(data.edt)==0){
      out.edt=1e-10
    }else{
      out.edt = edt_SDT_ll(edt, data.edt,sample = F)
    }
    
    if(nrow(data.pdt)==0){
      out.pdt=1e-10
    }else{
      out.pdt = pdt_SDT_ll(pdt, data.pdt,sample = F)
    }
    
    if(nrow(data.nb)==0){
      out.nb=1e-10
    }else{
      out.nb = nBack_ll(nb, data.nb,sample = F)
    }
    
    if(nrow(data.lex)==0){
      out.lex=1e-10
    }else{
      out.lex = lex_SDT_ll(lex, data.lex,sample = F)
    }
    
    
    out = sum(out.edt,out.pdt,out.nb,out.lex)
    return(out)
  }
  
}

edt <- c("C.edt" , "H_M_O.d" , "H_M_C.d" , "H_F_O.d" , "H_F_C.d" , "A_M_O.d" ,"A_M_C.d" , "A_F_O.d" ) #EDT
pdt <- c("C.pdt", "B_E.d", "B_M.d", "B_H.d", "O_E.d", "O_M.d") #PDT
nb <- c("C.nb" , "target.d", "lure.d", "nonlure.d")      #nback
lex <- c("C.lex", "HF.d", "LF.d", "VLF.d")      #lex

pars<-c(edt,pdt,nb,lex)
# tmp <- rnorm(length(x),mean=0,sd=1)
# names(tmp)<-x
# ll_joint(tmp,data[3,])

priors <- list(
  theta_mu_mean = rep(0, length(pars)),
  theta_mu_var = diag(rep(1, length(pars)))
)
print(priors)
# k=2
# #For identifiability constraints (with diagonals). 
# #Now the package will recognize that the diagonals are fixed, and no sign switching will be applied.
# constraintMat <- matrix(1, nrow = length(pars), ncol = k)
# constraintMat[upper.tri(constraintMat, diag = T)] <- 0 
  
  
  # Create the Particle Metropolis within Gibbs sampler object ------------------
sampler <- pmwgs(
    data = df,
    pars = pars,
    prior = priors,
    ll_func = ll_joint
  )

sampler <- init(sampler) # i don't use any start points here

# Sample! -------------------------------------------------------------------
sampled <- run_stage(sampler, stage = "burn",iter = 10, particles = 20, n_cores = 1, epsilon = .1)
sampled <- run_stage(sampled, stage = "adapt",iter = 10, particles = 20, n_cores =1, epsilon = .1)
save(sampled, file = "SDT_first3_.RData")
sampled <- run_stage(sampled, stage = "sample",iter = 100, particles = 20, n_cores =1, epsilon = .1)
save(sampled, file = "SDT_full_.RData")

sampled$samples$last_theta_sig_inv
