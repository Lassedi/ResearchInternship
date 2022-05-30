rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)

setwd(r"(D:\Psychology\Master\Research Internship\Data\2ndrun)")


#get the data
task_df <- function(tmp, name, MD = FALSE, list = NULL){
  if (class(tmp) != "data.frame"){
    df_tasks <- data.frame(matrix(ncol = ncol(tmp[[1]][[name]][[1]])))
    colnames(df_tasks) <- colnames(tmp[[1]][[name]][[1]])
    df_tasks$sub <- NA
    df_tasks$i <- NA
  } 
  else {
    df_tasks <- data.frame(matrix(ncol = ncol(tmp[[name]][[1]])))
    colnames(df_tasks) <- colnames(tmp[[name]][[1]])
    df_tasks$sub <- NA
    df_tasks$i <- NA
  }
  print(name)
  task_names = c("", "edt", "ldt", "pdt", "nback")
  if(MD == TRUE){
    if (class(tmp) == "data.frame"){
      for (x in 1:length(list)){
        sub <- list[[x]][[1]]
        task <- list[[x]][[2]]
        
        cat(x, " ")
        
        if (task_names[[task]] == name){ 
          print(task)
          task_list <- tmp[tmp$sub == sub,][[name]]
          print(paste("Sub:", sub))
          for (y in 1:length(task_list)){
            df <- task_list[[y]]
            df$sub <- sub
            df$i <- y
            #print(colnames(df) == colnames(df_tasks))
            #print(paste(ncol(df_tasks), colnames(df_tasks)))
            #print(head(df_tasks))
            #print(head(df))
            
            df_tasks <- rbind(df_tasks, df)
          }
        }
      }
    }
    else{
      for (x in 1:length(list)){
        sub <- list[[x]][[1]]
        task <- list[[x]][[2]]
        
        cat(x, " ")
        
        if (task_names[[task]] == name){
          task_list <- tmp[[sub]][[name]]
          print(paste("Sub:", sub))
          for (y in 1:length(task_list)){
            df <- task_list[[y]]
            df$sub <- sub
            df$i <- y
            df_tasks <- rbind(df_tasks, df)
          }
        }
      }
    }
    return (df_tasks)
  }
  else{
    for (x in 1:53){
      cat(x, " ")
      
      if (class(tmp) == "data.frame"){
        task_list <- tmp[tmp$sub == x,][[name]]
      }
      else{
        task_list <- tmp[[x]][[name]]
      }
      #print(task_list)
      for (y in 1:length(task_list)){
        df <- task_list[[y]]
        df$sub <- x
        df$i <- y
        df_tasks <- rbind(df_tasks, df)
      }
    }
    return (df_tasks)
  }
}

##add subject number column to tmp object
id_sub_numb <- function(tmp){
  n <- 1
  tmp$sub <- 1
  for (x in 1:53){
    tmp$sub[n:(n+19)] <- x
    n <- n + 20
  }
  return(tmp)
} # useless if tmp is a list!

#identify which tasks are missing in a sampled object
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

# load different predictive samples
load("samp_full.Rdata")
full_tmp <- tmp
full_tmp <- id_sub_numb(full_tmp)

load("samp_half_MD_1.Rdata")
load("samp_half_MD_2.Rdata")
load("samp_half_MD_3.Rdata")

load("samp_every1_MD_1.Rdata")
load("samp_every1_MD_2.Rdata")
load("samp_every1_MD_3.Rdata")

load("samp_prop_MD.Rdata")

#useless if tmp is a list/not necessary
tmp <- id_sub_numb(tmp)

#real data responses for each task
load("SDT_full_.Rdata")
sampled_full <- sampled
rm(sampled)


#get MD_list for alpha
load("SDT_prop_.Rdata")
load("SDT_full_noCon_al11.Rdata")
load("SDT_full_noCon_al12.Rdata")
load("SDT_full_noCon_al13.Rdata")


MD_list <- id_MD(sampled) 

"Alpha - destination"
alpha_dest <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\EvSubMD_1_alpha.pdf)"
alpha_dest <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\EvSubMD_2_alpha.pdf)"
alpha_dest <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\EvSubMD_3_alpha.pdf)"

alpha_dest <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\Prop_alpha.pdf)"

"Theta - destination"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\EvSubMD_3.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\EvSubMD_2.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\EvSubMD_1.pdf)"

destination <- r"(D:\Psychology\Master\Research Internship\Plots\PostPred_V2\Prop.pdf)"


#all data - original fit
df_pdt <- task_df(full_tmp, "pdt")
df_nback <- task_df(full_tmp, "nback")
df_ldt <- task_df(full_tmp, "ldt")
df_edt <- task_df(full_tmp, "edt")


#all data - 0,5/1 removed + prop fits
df_MD_pdt <- task_df(tmp, "pdt")
df_MD_nback <- task_df(tmp, "nback")
df_MD_ldt <- task_df(tmp, "ldt")
df_MD_edt <- task_df(tmp, "edt")

###plot posterior sample vs actual response data####
pdf(file = destination)

##source the plotting file##
setwd(r"(D:\Psychology\Master\Research Internship\Git_directory\Scripts)")
source("post_pred_theta.R", echo = TRUE)
dev.off()

pdf(file = alpha_dest)
source("post_pred_alpha.R", echo = TRUE)
dev.off()

#return to data direct
setwd(r"(D:\Psychology\Master\Research Internship\Data)")
