rm(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
setwd(r"(D:\Psychology\Master\Research Internship\Data)")



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
    for (x in 1:56){
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

#function to subset the original fit for the specific task and people with missing data
full_df <- function(full_data, task=NULL, list){
  #task_list <- list(edt = 2, ldt = 3, pdt = 4, nback = 5)
  data <- full_data[[task]]
  #MD_list <- t(as.data.frame(list))
  #row.names(MD_list) <- NULL
  
  #MD_list <- MD_list[MD_list[,2] == task_list[[task]],][,1]
  #data <- data[MD_list]
  
  x2 <- data.frame()
  for (sub in 1:length(MD_list)){
    x <- data[[sub]]
    x$sub <- MD_list[sub]
    x2 <- rbind(x2, x)
  }
  return(x2)
} #(OUTDATED)

##add subject number column to tmp object
id_sub_numb <- function(tmp){
  n <- 1
  tmp$sub <- 1
  for (x in 1:56){
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




##half S1 MD##
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_halfMD_S1_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_halfMD_S1_onlyMD.pdf)"

##half s2 MD##
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_halfMD_S2_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_halfMD_S2_onlyMD.pdf)"

##half s3 MD##
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_halfMD_S3_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_halfMD_S3_onlyMD.pdf)"

## every1 MD - 1
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_every1MD_S1_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_every1MD_S1_onlyMD.pdf)"

## every1 MD - 2
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_every1MD_S2_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_every1MD_S2_onlyMD.pdf)"

## every1 MD - 3
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_every1MD_S3_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_every1MD_S3_onlyMD.pdf)"


##prop MD##
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_propMD_allDATA.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\post_samp_propMD_onlyMD.pdf)"


# load different predictive samples
load("samp_full.Rdata")
full_tmp <- tmp
full_tmp <- id_sub_numb(full_tmp)

load("samp_half_MD_1.Rdata")
load("samp_half_MD_2.Rdata")
load("samp_half_MD_3.Rdata")

load("samp_every1_MD_1.Rdata")
load("samp_every1_MD_2.Rdata")
load("samp_every1_MD_3:Rdata")

load("samp_prop_MD.Rdata")

#real data responses for each task
load("SDT_full_noCon.Rdata")
sampled_full <- sampled

#useless if tmp is a list/not necessary
tmp <- id_sub_numb(tmp)

#get MD_list
load("SDT_full_noCon_2.Rdata")


MD_list <- id_MD(sampled) 



###create dfs###
"Alpha"
#all data - original fit
df_pdt <- task_df(tmp, "pdt", MD = TRUE, list = MD_list)
df_nback <- task_df(tmp, "nback", MD = TRUE, list = MD_list)
df_ldt <- task_df(tmp, "ldt", MD = TRUE, list = MD_list)
df_edt <- task_df(full_tmp, "edt", MD = TRUE, list = MD_list)

#all data - 0,5/1 removed + prop fits
df_pdt <- task_df(tmp, "pdt", MD = TRUE, list = MD_list)
df_nback <- task_df(tmp, "nback", MD = TRUE, list = MD_list)
df_ldt <- task_df(tmp, "ldt", MD = TRUE, list = MD_list)
df_MD_edt <- task_df(tmp, "edt", MD = TRUE, list = MD_list)

"Theta"
#all data - original fit
tmp <- tmp[tmp$sub != 3 & tmp$sub != 52,] #for now excluding these two since they have MD waiting for 56 fit
tmp <- tmp[,colnames(tmp) != "sub"] #id again after otherwise task_df doesnt work

df_pdt <- task_df(tmp, "pdt")
df_nback <- task_df(tmp, "nback")
df_ldt <- task_df(tmp, "ldt")
df_edt <- task_df(tmp, "edt")

#all data - 0,5/1 removed + prop fits
df_MD_pdt <- task_df(tmp, "pdt")
df_MD_nback <- task_df(tmp, "nback")
df_MD_ldt <- task_df(tmp, "ldt")
df_MD_edt <- task_df(tmp, "edt")






###plot posterior sample vs actual response data####
pdf(file = destination)

"EDT"
#=prepare  the .5/1 removed/prop dataframes
edt <- df_MD_edt[df_MD_edt$responseSample == "Happy",] %>% group_by(i,sub, Gender, cond, Mouth) %>% count(sub, 
       i, cond, Gender, Mouth) %>% summarise_at(vars(n), list(name = mean)) %>% na.exclude()


edt$cond <- paste(edt$Gender, edt$cond, edt$Mouth, sep = "\n")
edt <- edt[, colnames(edt) != "Mouth" & colnames(edt) != "Gender"]


## prepare the real response dataframe
df_realResp_edt <- data.frame()

for (sub in 1:56){
  x <- sampled_full$data$edt[[sub]]
  x$sub <- sub
  df_realResp_edt <- rbind(x, df_realResp_edt)
}

df_realResp_edt <- df_realResp_edt[df_realResp_edt$response == "Happy",] %>% group_by(sub,Gender,cond, Mouth) %>% count(sub,
      cond, Gender, Mouth) %>% summarise_at(vars(n), list(name = mean))


#df_realResp_edt <- na.exclude(df_realResp_edt)
df_realResp_edt$cond <- paste(df_realResp_edt$Gender, df_realResp_edt$cond, df_realResp_edt$Mouth, sep = "\n")
df_realResp_edt <-df_realResp_edt[, colnames(df_realResp_edt) != "Gender" & colnames(df_realResp_edt) != "Mouth"]


## prepare the original fit posterior predicitive dataframe
df_post_pred <- df_edt[df_edt$responseSample == "Happy",] %>% group_by(i,sub, Gender,cond, Mouth) %>% count(sub,i, 
      cond,Gender, Mouth) %>% summarise_at(vars(n), list(name = mean))


df_post_pred <- na.exclude(df_post_pred)
df_post_pred$cond <- paste(df_post_pred$Gender, df_post_pred$cond, df_post_pred$Mouth, sep = "\n")
df_post_pred <- df_post_pred[, colnames(df_post_pred) != "Mouth" & colnames(df_post_pred) != "Gender"]




list <- t(as.data.frame(MD_list)) 
row.names(list) <-  NULL
list <- list[list[,2] == 2,]

for (sub in list[,1]){
  cat(sub, " ")
  print(ggplot()+
          geom_col(df_realResp_edt[df_realResp_edt$sub == sub,], mapping = aes(x = cond, y = name), colour = "sienna3", 
                   fill = "seagreen4", alpha = 0.5, width = 0.5)+
          geom_jitter(edt[edt$sub == sub,],mapping = aes(x = cond, y = name), alpha = 0.4, colour = "tomato", width = 0.15)+
          geom_jitter(df_post_pred[df_post_pred$sub == sub,],mapping = aes(x = cond, y = name), alpha = 0.2, colour = "blue", width = 0.15)+
      
          labs(title = "EDT - posterior sampling means distribution vs real data means",
               subtitle = paste("Response: Happy", "\n", "Subject:", sub),
               x = "Condition",
               y = "mean"))      
}

###LDT###
ldt <- df_ldt[df_ldt$responseSample == "word",] %>% group_by(i, cond) %>% count(sub, 
                                                                                     i, cond) %>% summarise_at(vars(n), list(name = mean)) %>% na.exclude()


x2 <- df_ldt[df_ldt$response == "word",] %>% group_by(cond) %>% count(sub, 
                                                                            i, cond) %>% summarise_at(vars(n), list(name = mean))
x2 <- na.exclude(x2)


ggplot()+
  geom_col(x2, mapping = aes(x = cond, y = name), colour = "sienna3", 
           fill = "seagreen4", alpha = 0.3, width = 0.5)+
  geom_jitter(ldt,mapping = aes(x = cond, y = name), colour = "tomato", width = 0.15)+
  labs(title = "LDT - posterior sampling means distribution vs real data means",
       subtitle = paste("Response: Word", "\n", "Subject:", sub),
       x = "Condition",
       y = "mean")




###nback###
x1 <- df_nback[df_nback$responseSample == "target",] %>% group_by(i, cond) %>% count(sub, 
                                i, cond) %>% summarise_at(vars(n), list(name = mean)) %>% na.exclude()


x2 <- df_nback[df_nback$response == "target",] %>% group_by(cond) %>% count(sub, 
                                                                          i, cond) %>% summarise_at(vars(n), list(name = mean))
x2 <- na.exclude(x2)

ggplot()+
  geom_col(x2, mapping = aes(x = cond, y = name), colour = "sienna3", 
           fill = "seagreen4", alpha = 0.3, width = 0.5)+
  geom_jitter(data = x1,mapping = aes(x = cond, y = name), colour = "tomato", width = 0.15)+
  labs(title = "N-Back - posterior sampling means distribution vs real data means",
       subtitle = "Response: target",
       x = "Condition",
       y = "mean")



####PDT####
pdt <- df_pdt[df_pdt$responseSample == "orange",] %>% group_by(sub, i, 
                                                               cond, difficulty) %>% count() %>% summarise_at(vars(n), list(name = mean)) %>% na.exclude()
# count(sub, i, cond, difficulty) + rm sub from group_by if group means are wanted above

pdt$cond <- paste(pdt$cond, pdt$difficulty, sep = "-")#
pdt <- pdt[, colnames(pdt) != "difficulty"]

data <- full_data$pdt
MD_list <- id_MD(sampled)
MD_list <- t(as.data.frame(MD_list))
row.names(MD_list) <- NULL

MD_list <- MD_list[MD_list[,2] == 4,][,1]
data <- data[MD_list]

x2 <- data.frame()
for (sub in 1:length(MD_list)){
  x <- data[[sub]]
  x$sub <- MD_list[sub]
  x2 <- rbind(x2, x)
}

x2 <- full_df(full_data, "pdt", MD_list)
x2 <- x2[x2$response == "orange",] %>% group_by(sub,cond, difficulty) %>% count() %>% summarise_at(vars(n), list(name = mean))
x2 <- na.exclude(x2)
x2$cond <- paste(x2$cond, x2$difficulty, sep = "-")
x2 <- x2[,colnames(x2) != "difficulty"]


data <- sampled$data$pdt # needs to be of missing data
x3 <- data.frame()
for (sub in 1:56){
  x <- data[[sub]]
  x$sub <- sub
  x3 <- rbind(x, x3)
} 

x3 <- task_df(tmp, "pdt")
x3 <- x3[x3$response == "orange",] %>% group_by(cond, difficulty) %>% count(sub,i, cond, difficulty) %>% na.exclude()
x3 <- x3 %>% summarise_at(vars(n) , list(name = mean))
x3$cond <- paste(x3$cond, x3$difficulty, sep = "-")
x3 <- x3[,colnames(x3) != "difficulty"]

list <- t(as.data.frame(MD_list)) 
row.names(list) <-  NULL
list <- list[list[,2] == 4,]

for (sub in list[,1]){
  cat(sub, " ")
  print(ggplot()+
          geom_col(x2[x2$sub == sub,],mapping =  aes(x = cond, y = name), colour = "sienna3", 
                   fill = "seagreen4", alpha = 0.6, width = 0.5)+
          geom_col(x3,mapping =  aes(x = cond, y = name), colour = "orange", 
                                fill = "blue", alpha = 0.1, width = 0.5)+
          geom_jitter(data = pdt[pdt$sub == sub,],mapping =  aes(x = cond, y = name), colour = "tomato", width = 0.15)+
          labs(title = "PDT - posterior sampling means distribution vs real data means",
               subtitle = paste("Response: Orange", "\n", "Subject:", sub),
               x = "Condition",
               y = "mean"))
}

ggplot()+
    geom_col(x2,mapping =  aes(x = cond, y = name), colour = "sienna3", 
            fill = "seagreen4", alpha = 0.3, width = 0.5)+
    geom_jitter(data = pdt,mapping =  aes(x = cond, y = name), colour = "tomato", width = 0.15)+
    labs(title = "PDT - posterior sampling means distribution vs real data means",
         subtitle = "Response: Orange",
             x = "Condition",
             y = "mean")

dev.off()
