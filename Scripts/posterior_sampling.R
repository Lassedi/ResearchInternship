rm(list=ls())
library(dplyr)
library(rtdists)
library(ggplot2)
library(jtools)
library(scales)
library(ggpubr)

## load dataset 
setwd(r"(D:\Psychology\Master\Research Internship\Data)")

load("SDT_full_.Rdata")
full_posterior <- sampled
full_posterior$data <- full_posterior$data[-c(3,52),]
full_posterior$n_subjects <- 56

load("SDT_full_noCon.Rdata")
full_data <- sampled$data

load("SDT_full_noCon_1.Rdata")
half_MD_1 <- sampled
half_MD_1$data <- full_data

load("SDT_full_noCon_2.Rdata")
half_MD_2 <- sampled
half_MD_2$data <- full_data

load("SDT_full_noCon_3.Rdata")
half_MD_3 <- sampled
half_MD_3$data <- full_data

load("SDT_full_noCon_al11.Rdata")
every1_MD_1 <- sampled
every1_MD_1$data <- full_data

load("SDT_full_noCon_al12.Rdata")
every1_MD_2 <- sampled
every1_MD_2$data <- full_data

load("SDT_full_noCon_al13.Rdata")
every1_MD_3 <- sampled
every1_MD_3$data <- full_data

load("SDT_prop.Rdata")
prop_MD <- sampled
prop_MD$data <- full_data

#### generate data ####
generate.posterior <- function(sampled, n){
  n.posterior=n # Number of parameter samples from posterior distribution.
  pp.data=list()
  S = sampled$n_subjects
  data=sampled$data
  data$subject <- as.factor(data$subject)
  sampled_stage = length(sampled$samples$stage[sampled$samples$stage=="sample"])
  for (s in 1:S) {
    cat(s," ")
    iterations=round(seq(from=(sampled$samples$idx-sampled_stage) , to=sampled$samples$idx, length.out=n.posterior))
    for (i in 1:length(iterations)) {
      x <- sampled$samples$alpha[,s,iterations[i]]
      names(x) <- sampled$par_names
      
      tmp=sampled$ll_func(x=x,data=data[s,],sample=TRUE)
      if (i==1) {
        pp.data[[s]]=cbind(i,tmp)
      } else {
        pp.data[[s]]=rbind(pp.data[[s]],cbind(i,tmp))
      }
    }
    
  }
  return(pp.data)
}

nBack_ll <- function(x, data, sample = FALSE){                   # x = named vector of parameters, sample = sample values from the posterior distribution?
  names(x)[1]<-"C"
  if (sample) {
    data$responseSample <- NA                                            #if we are sampling, clear the 'response' column
    for (i in 1:nrow(data)){
      if (data$cond[i] == "target"){                                          # if this is a target
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["target.d"], sd = 1) > x["C"],"target","nontarget")
      }else if (data$cond[i] == "lure"){                            # Nonrow(w if this is a lure
        data$responseSample[i] <- ifelse(rnorm(1, mean = x["lure.d"], sd = 1) > x["C"],"target","nontarget") 
      }else{                                                        # and if this is a nontarget
        data$responseSample[i] <- ifelse(rnorm(1, mean = 0, sd = 1) > x["C"],"target","nontarget")
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
          out[i] <- pnorm(x["C"], mean = 0, sd = 1,  # find the probability of x being  
                          log.p = TRUE, lower.tail = FALSE)         # above the criterion
        } else {                                                    # but if they responded "nontarget"
          out[i] <- pnorm(x["C"], mean = 0, sd = 1,  # find the probability of x being
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



tmp<-generate.posterior(sampled, 20)
tmp=do.call(rbind,tmp)

save(tmp, file = "samp_full.Rdata")

save(tmp, file = "samp_half_MD_1.Rdata")
save(tmp, file = "samp_half_MD_2.Rdata")
save(tmp, file = "samp_half_MD_3.Rdata")

save(tmp, file = "samp_every1_MD_1.Rdata")
save(tmp, file = "samp_every1_MD_2.Rdata")
save(tmp, file = "samp_every1_MD_3.Rdata")

save(tmp, file = "samp_prop_MD.Rdata")
