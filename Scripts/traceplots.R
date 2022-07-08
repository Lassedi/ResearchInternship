###Script used in order to create trace and violin plots###
rm(list = ls())

setwd(r"(D:\Psychology\Master\Research Internship\Data\2ndrun)")

load("SDT_half_1.Rdata")
sampled_1 <- sampled
load("SDT_half_2.Rdata")
sampled_2 <- sampled
load("SDT_half_3.Rdata")
sampled_3 <- sampled
load("SDT_all1.Rdata")
sampled_all_1 <- sampled
load("SDT_prop_.Rdata")
sampled_prop <- sampled
load("SDT_full_.Rdata")
sampled_full <- sampled

traceplot_pmwg <- function(mu_matrix, tasks, sub = NULL){
  task_list <- list("edt" = list(1,8), "pdt" = list(9,14), "nb" = list(15,17), "lex" = list(18,21))
  if(is.null(sub) == TRUE){
    for (task in tasks) {
      task_index <- task_list[task]
      a <- task_index[[1]][[1]]
      b <- task_index[[1]][[2]]
      ylabel <- paste("Parameter means -", task)
      xlabel <- paste("Iterrations -", "Subject:", sub)
      matplot(t(mu_matrix)[, a:b], type = "l", ylab = ylabel, xlab = xlabel)
    }
  }
  else{
    for (task in tasks) {
      task_index <- task_list[task]
      a <- task_index[[1]][[1]]
      b <- task_index[[1]][[2]]
      ylabel <- paste("Parameter means -", task)
      xlabel <- "Iterrations"
      matplot(t(mu_matrix)[, a:b], type = "l", ylab = ylabel, xlab = xlabel)
    }
  }
}


tasks <- list("edt", "pdt", "lex", "nb")
sampled_list <- list(sampled_prop)

#group level
destination <- r"(D:\Psychology\Master\Research Internship\Plots\Trace_Violine_Cor_V2\prop_theta_traceplot.pdf)"

pdf(file=destination)
for (sampleing_round in sampled_list){
  theta_mu <- sampleing_round$samples$theta_mu
  traceplot_pmwg(theta_mu, tasks)
}
dev.off()
 
#individual level
destination <- r"(D:\Psychology\Master\Research Internship\Plots\Trace_Violine_Cor_V2\prop_alpha_traceplot.pdf)"

pdf(file= destination)
participants <- c(1:53)
for (subject in participants){
  cat(subject, " ")
  for (sampleing_round in sampled_list){
    alpha <- sampleing_round$samples$alpha
    alpha_mu <- alpha[,subject,]
    traceplot_pmwg(alpha_mu, tasks, subject)
  }
}
dev.off()



#violine plot
library("ggplot2")
library("tidyr")

##prepare theta_mu
prepare_theta_mu <- function(theta_mu){
  df <- as.data.frame(t(theta_mu$samples$theta_mu[, theta_mu$samples$stage=="sample"]))
  df <- df %>% pivot_longer(cols=everything(), names_to = "parameter", values_to = "estimate")
  return(df)
}


df_full <- prepare_theta_mu(sampled_full)
df_full$fit <- "Original"
df_1 <- prepare_theta_mu(sampled_1)
df_1$fit <- "0.5removed1\n Version 1\n"
df_2 <- prepare_theta_mu(sampled_2)
df_2$fit <- "0.5removed1\n Version 2\n"
df_3 <- prepare_theta_mu(sampled_3)
df_3$fit <- "0.5removed1\n Version 3\n"
df_4 <- prepare_theta_mu(sampled_all_1)
df_4$fit <- "1.0removed1\n Version 1\n"
df_7 <- prepare_theta_mu(sampled_prop)
df_7$fit <- "Proportional"
df_out <- rbind(df_full, df_1, df_2, df_3, df_4, df_7)
rm(df_full, df_1, df_2, df_3, df_4, df_7)

#plot
plot_violine <- function(df_full){
  all_para <- sampled$par_names
  task_list <- list("Edt" = c(1:8), "Pdt" = c(9:14), "n-Back" = c(15:17), "Ldt" = c(18:21))
  task_names <- c("Edt", "Pdt", "n-Back", "Ldt")
  count <- 0
  for (task in task_list){

    df_task <- data.frame()
    para_list <- all_para[task]
    for (para in para_list){
      #print(para)
      data <- df_full[df_full$parameter == para,]
    
      df_task <- rbind(df_task, data)
      
    }
    count <- count + 1
    task <- task_names[count]
    print(task)
    print(para_list)
    print(nrow(df_task))
    print(head(df_task))
    print(ggplot(df_task, aes(x = factor(parameter, level = para_list), y=estimate, colour = fit))+
            geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75), scale = "width")+
            labs(x="Parameter", y= "Estimate", title = task)+
            theme_bw())
  }
}


destination <- r"(D:\Psychology\Master\Research Internship\Plots\Trace_Violine_Cor_V2\full_violine.pdf)"
pdf(file = destination)
plot_violine(df_out)
dev.off()
