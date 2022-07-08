###Script used to calculate log-likelihood sums###
rm(list=ls())
library(dplyr)

setwd(r"(D:\Psychology\Master\Research Internship\Data\2ndrun)")
load("SDT_full_.Rdata")
sampled_full <- sampled

load("SDT_half_1.Rdata")
sampled_h1 <- sampled

load("SDT_half_2.Rdata")
sampled_h2 <- sampled

load("SDT_half_3.Rdata")
sampled_h3 <- sampled

load("SDT_all1.Rdata")
sampled_a1 <- sampled

load("SDT_prop_.Rdata")
sampled_prop <- sampled



#identify which tasks are missing for which individuals and return the list
id_MD <- function(sampled_obj){
  task_list <- c(2,3,4,5)
  tasks_MD <- sampled_obj$data$task
  count <- 0
  index <- 1
  MD <- list()
  for (sub in tasks_MD){
    count <- count + 1
    for (numb in task_list){
      if ((numb %in% sub) == FALSE){
        bind <- c(count, numb)
        MD[[index]] <- bind
        index <- index + 1
      }
    }
  }
  return(MD)
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


calc_ll <- function(sampled, sampled_MD = NULL){
  data_all <- sampled_full$data
  if (substitute(sampled) == "sampled_full"){
    MD <- data.frame("sub" = rep(c(1:53), each=4), "task" = c(2:5))
  }
  else{
    MD <- id_MD(sampled)
    MD <- t(as.data.frame(MD))
    colnames(MD) <- c("sub", "task")
    MD <- as.data.frame(MD)
  }
  mean_pars <- t(apply(sampled$samples$alpha[,, sampled$samples$stage == "sample"], 1:2, mean))
  mean_pars_ll <- numeric()
  
  df_final <- data.frame()
  for (x in unique(MD$sub)) {
    subMDlist <- x
    sub_vector <- as.vector(sampled_full$subjects)
    sub <- sub_vector[[subMDlist]]
    task <- MD[MD$sub == x,]$task
    
    x <- mean_pars[sub, ]
    edt = x[1:8]
    pdt = x[9:14]
    nb = x[15:17]
    lex = x[18:21]
    
    
    df <- as.data.frame(matrix(ncol = 5))
    colnames(df) <- colnames(data_all[1:5])
    df$subject <- sub
    df$sub <- subMDlist
    data = data_all[data_all$subject == sub,]
    cat(" - ")
    if (2 %in% task) {
      df$edt <- edt_SDT_ll(edt, as.data.frame(data$edt))
    }
    else {
      df$edt <- 0
    }
    if (3 %in% task) {
      df$ldt <- lex_SDT_ll(lex, as.data.frame(data$ldt))
    }
    else {
      df$ldt <- 0
    }
    if (4 %in% task) {
      df$pdt <- pdt_SDT_ll(pdt, as.data.frame(data$pdt))
    }
    else {
      df$pdt <- 0
    }
    if (5 %in% task) {
      df$nback <- nBack_ll(nb, as.data.frame(data$nback))
    }
    else {
      df$nback  <- 0
    }
    df$sum <- sum(df[df$subject == sub, 2:5])
    df_final <- rbind(df_final, df)
  }
  return(df_final)
}

return_summary <- function(original_LL, MD_LL, sampled_MD, full = FALSE){
  if (full == TRUE){
    original_MD <- original_LL[2:5]
  }
  else{
    original_MD <- ((original_LL[unique(t(as.data.frame(id_MD(sampled_MD)))[,1]),][2:5] * MD_LL[2:5])
                    /MD_LL[2:5])
  }
                  
  ll_dif <- original_MD - MD_LL[2:5]
  
  median <- median(unname(unlist(ll_dif)), na.rm = TRUE)
  mean <- mean(unname(unlist(ll_dif)), na.rm = TRUE)
  std <- sd(unname(unlist(ll_dif)), na.rm = TRUE)
  min <- min(ll_dif, na.rm = TRUE)
  max <- max(ll_dif, na.rm = TRUE)
  sum_OrigProp_LL <- sum(rowSums(original_MD, na.rm = TRUE))
  sum_MD_LL <- sum(rowSums(MD_LL[2:5], na.rm = TRUE))
  sum_dif <- sum_OrigProp_LL - sum_MD_LL
  
  df <- as.data.frame(matrix(nrow=1, ncol=0))
  df$MD <- deparse(substitute(MD_LL))
  df$Min <- min
  df$Max <- max
  df$Median <- median
  df$Mean <- mean
  df$STD <- std
  df[["Total Missing Task Log-likelihood Sum"]] <- sum_MD_LL
  df[["Total Missing Task Sum Difference"]] <- sum_dif
  
  return(df)
}

id_outliers <- function(original_LL, MD_LL, sampled_MD){
  original_MD <- ((original_LL[unique(t(as.data.frame(id_MD(sampled_MD)))[,1]),][2:5] * MD_LL[2:5])
                / MD_LL[2:5])
  
  ll_ratio <- original_MD - MD_LL[2:5] 
  ll_ratio$sub <- MD_LL$sub
  return(ll_ratio)
}


"create likelihood dataframe"

original_LL <- calc_ll(sampled_full)
df <- return_summary(original_LL, original_LL, full = TRUE)

H1_LL <- calc_ll(sampled_h1)
df <- rbind(df, return_summary(original_LL, H1_LL, sampled_h1))

H2_LL <- calc_ll(sampled_h2)
df <- rbind(df, return_summary(original_LL, H2_LL, sampled_h2))

H3_LL <- calc_ll(sampled_h3)
df <- rbind(df, return_summary(original_LL, H3_LL, sampled_h3))

A1_LL <- calc_ll(sampled_a1)
df <- rbind(df, return_summary(original_LL, A1_LL, sampled_a1))

proportionalFit_LL <- calc_ll(sampled_prop)
df <- rbind(df, return_summary(original_LL, proportionalFit_LL, sampled_prop))

df <-  df[2:8] %>% mutate_all(funs(round(.,2)))

destination <- r"(D:\Psychology\Master\Research Internship\Report\LL_table.csv)"
write.csv(df, destination, row.names = FALSE)

write.table(df, "clipboard", sep = ",")

"ID outliers"
id_prop <- id_outliers(original_LL, proportionalFit_LL, sampled_prop)
