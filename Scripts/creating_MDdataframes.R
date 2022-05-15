rm(list = ls())
setwd(r"(D:\Psychology\Master\Research Internship\Data)")

load("fullData.Rdata")
subjects_allTasks <- data
load("FULLdataset.Rdata")
data <- data

library(dplyr)
data <- data %>% select(-(demo:dd))
data <- data %>% select(-one_of("sart", "riskyGambles"))
data <- data %>% select(-(digit:spatialspan))


#check completeness of the data and remove participants that have less than 50 rows
complete_data_check <- function(data){
  for (participant in 1:56) {
    for (task in 2:5){
      if (nrow(data[[task]][[participant]]) < 50){
        cat("Par:", participant, "Task:", task)
        data <- data[-c(participant), ]
        rownames(data) <- NULL
      }
    }
  }
  return(data)
}

subjects_allTasks <- complete_data_check(subjects_allTasks)


#return list with how man tasks are missing per person in original dataset
MD_length_list <- function(data){
  MD_list <- list()
  for (subject in 1:nrow(data)){
    x = length(MD_list)
    tasks_sub <- c()
    for (task in 2:5){
      #print(paste(subject, task))
      #print(nrow(data[[task]][[subject]]))
      if ((nrow(data[[task]][[subject]]) < 20)){ #| is.na(data[[task]][[subject]]$trial_type[[1]]) <- dont remeber why that was in there
        tasks_sub <- append(tasks_sub, task)
      }
      MD_list[[x+1]] <- c(subject, tasks_sub)
      
      MD <- MD_list[[subject]]
      subject <- MD_list[[subject]][[1]]
      missing_tasks <- length(MD)-1
      MD_list[[x+1]] <- c(subject, missing_tasks)
    }
  }
  MD_list <- t(as.data.frame(MD_list))
  return(MD_list)
}

#return a dataframe with tasks missing based on proportions of original dataset
proportional_MD <- function(data, subs_prop){
  count <- 0
  data$task <- list(2:5)
  
  participants <- c(1:nrow(data))
  for (prop in subs_prop){
    output <- assign_emptyDF(data, prop, ntasks = count, proportional = participants)
    data <- output[[1]]
    participants <- output[[2]]
    print(length(participants))
    count <- count + 1
  }
  return(data)
}


#' randomly select a task to be replaced by an empty dataframe
#'
#' remove data for randomly selected tasks for specified amount of participants
#' @param data the complete cases dataset to remove data from
#' @param npar the number of participants you want to remove data from - 
#' if all participants write "full" - default is 1
#' @param ntasks the number of task you want to remove data - default is 1
#'
#' @return returns a dataframe with missing data
#'
#' @examples  assign_emptyDF(data, npar = 35, ntasks = 2)
assign_emptyDF <- function(data, npar = 1, ntasks = 1, proportional = FALSE) {
  
  #check if only a part or all participants should get a task removed
  if (npar == "full"){
    participants <- c(1:nrow(data))
    print(participants)
  }
  else if (proportional != FALSE){
    participants <- sample(proportional, npar)
    prop <- TRUE
    print(participants)
    print(length(participants))
  }
  else{
    participants <- sample(1:nrow(data), npar)
  }
    
  tasks_count <- c()
  if (ntasks != 0){
    for (par in participants) {
      #sampling the task
      task_list <- data[["task"]][[par]]
      print(task_list)
      if (ntasks > 1){
        tasks <- task_list[sample(1:length(task_list), ntasks)]
      }
      else{
        tasks <- task_list[sample(1:length(task_list), 1)]
        #print("haoiefhEFOPÄRJ")
      }
      tasks_count <- append(tasks_count, tasks)
      
      #deleting the data for the selected task
      print(paste("Participant:", par, " ", "Task index:", tasks))
      for (task in tasks){
        df <- data.frame(matrix(ncol = length(colnames(data[[task]][[par]])), nrow = 0))
        colnames(df) <- colnames(data[[task]][[par]])
        data[[task]][[par]] <- df
        
        
        #updating the task list for the participant
        task_list <- task_list[task_list != task]
        
      }
      data[["task"]][[par]] <- task_list
  }
  
  }
  print(table(tasks_count)) #check missing task distribution for dataset
  if (prop == TRUE){
    participants <- proportional[! proportional %in% participants]
    output <- list(data, participants)
    return(output)
  }
  else{
    return(data)
  }
}


#create the different dataframes - NOT proportional
data$task <- list(2:5)
df_missingTask_half <- assign_emptyDF(data, npar = 26)
df_missingTask_full_1 <- assign_emptyDF(data, npar = "full")
data$task <- df_missingTask_full_1$task
df_missingTask_full_2 <- assign_emptyDF(data, npar = "full")
data$task <- df_missingTask_full_2$task
df_missingTask_full_3 <- assign_emptyDF(data, npar = "full")
data$task <- list(2:5)
df_missingTask_twoTasks <- assign_emptyDF(data, npar = "full", ntasks = 2)




##create proportional dataframe
MD_length <- MD_length_list(data)

# create subs_prop - list with number of praticipants per 0,1,2,3 tasks missing
count_0 <- length(MD_length[MD_length[,2] == 0])/2
count_1 <- length(MD_length[MD_length[,2] == 1])/2
count_2 <- length(MD_length[MD_length[,2] == 2])/2
count_3 <- length(MD_length[MD_length[,2] == 3])/2
count_4 <- length(MD_length[MD_length[,2] == 4])/2

subjects_with_data <- 210 - count_4  #210 not generalizable to other datasets(!)
prop_0 <- count_0/subjects_with_data
prop_1 <- count_1/subjects_with_data
prop_2 <- count_2/subjects_with_data
prop_3 <- count_3/subjects_with_data

subs_0 <- as.integer(count_0*prop_0)
subs_1 <- as.integer(count_0*prop_1)
subs_2 <- as.integer(count_0*prop_2)
subs_3 <- as.integer(count_0*prop_3)
rm(count_0, count_1, count_2, count_3,count_4, prop_0, prop_1, prop_2, prop_3)

subs_prop <- c(subs_0, subs_1, subs_2,subs_3)

#create dataframe
prop_MD <- proportional_MD(subjects_allTasks, subs_prop = subs_prop)

##check how many tasks per participant where removed
MD_length_list(prop_MD)





##check whether the sample from the posterior distribution has NA values
tmp_MD <- tmp[2,colnames(tmp) != "i"]
m <- as.data.frame(MD_length_list(tmp_MD))  
m[[2]][[1]]
