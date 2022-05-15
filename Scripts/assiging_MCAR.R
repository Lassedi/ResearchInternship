rm(list = ls())
setwd(r"(D:\Psychology\Master\Research Internship\Data)")
load("fullData.Rdata")

#check completeness of the data
for (participant in 1:56) {
  for (task in 2:5){
    if (nrow(data[[task]][[participant]]) < 20) {
      print(paste("Par:", participant, "Task:", task))
      data <- data[-c(participant), ]
      rownames(data) <- NULL
    }
  }
}


#randomly selecting some values to be replaced by NA
assigning_NA_MCAR <- function(data, npar = 1,nNArow = 1) {
  participants = sample(1:56, npar)
  for (par in participants) {
    task = sample(2:5, 1)
    print(paste("Participant:", par, " ", "Task:", task))
    df <- data[[task]][[par]]
    rows_na = sample(1:nrow(df), nNArow)
    for (row in rows_na) {
      df[row, "response"] <- NA
      data[[task]][[par]] <- df
    }
  }
  return(data)
}
  
dataNA <- assigning_NA_MCAR(data, npar = 23, nNArow = 10)

print(sum(is.na(dataNA[[5]][[50]])))
print(sum(is.na(data[[5]][[50]])))
print(data[[5]][[53]]) # does not have data at all. there seem to be multiple


#randomly select a task to be replaced by an empty dataframe
assign_emptyDF <- function(data, npar = 1, ntasks = 1) {
  if (npar == "full"){
   participants <- c(1:nrow(data))
   print(participants)
  }
  else{
    participants <- sample(1:nrow(data), npar)
    print(participants)
  }
  tasks_count <- c()
  for (par in participants) {
    
    #sampling the task
    task_list <- data[["task"]][[par]]
    if (ntasks > 1){
      tasks <- task_list[sample(1:length(task_list), ntasks)]
    }
    else{
      tasks <- task_list[sample(1:length(task_list), 1)]
    }
    tasks_count <- append(tasks_count, tasks)
    print(task_list)
    
    #deleting the data for the selected task
    print(paste("Participant:", par, " ", "Task index:", tasks))
    for (task in tasks){
      df <- data.frame(matrix(ncol = length(colnames(data[[task]][[par]])), nrow = 0))
      colnames(df) <- colnames(data[[task]][[par]])
      data[[task]][[par]] <- df
      
      
      #updating the task list for the participant
      task_list <- task_list[task_list != task]
      
    }
    print(task_list)
    data[["task"]][[par]] <- task_list
  }
  print(table(tasks_count)) #check missing task distribution for dataset
  return(data)
}


list = c(1:3)

list2 <- as.list(list, length = )
print(list2)
list <- list[-task]
print(list)
#create the different dataframes
data$task <- list(2:5)
df_missingTask_half <- assign_emptyDF(data, npar = 26)
df_missingTask_full_1 <- assign_emptyDF(data, npar = "full")
data$task <- df_missingTask_full_1$task
df_missingTask_full_2 <- assign_emptyDF(data, npar = "full")
data$task <- df_missingTask_full_2$task
df_missingTask_full_3 <- assign_emptyDF(data, npar = "full")
data$task <- list(2:5)
df_missingTask_twoTasks <- assign_emptyDF(data, npar = "full", ntasks = 2)

hist(df_missingTask$task)
# assigns an empty dataframe for a randomly selected sample of participants on a random task each
print(nrow(data_emptyDF[[3]][[45]]))




