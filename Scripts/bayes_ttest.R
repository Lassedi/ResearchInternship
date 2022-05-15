rm(list = ls())

library(bayestestR)
library(BayesFactor)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd(r"(D:\Psychology\Master\Research Internship\Data)")
load("SDT_full_noCon.Rdata")
sampled_full <- sampled

#load data - half of participants have one missing task
load("SDT_full_noCon_1.Rdata")
sampled_1 <- sampled
load("SDT_full_noCon_2.Rdata")
sampled_2 <- sampled
load("SDT_full_noCon_3.Rdata")
sampled_3 <- sampled

##load data - every participant has one task removed
load("SDT_full_noCon_al11.Rdata")
sampled_1 <- sampled
load("SDT_full_noCon_al12.Rdata")
sampled_2 <- sampled
load("SDT_full_noCon_al13.Rdata")
sampled_3 <- sampled

#remove unne
rm(sampled)

#return df for vector/distribution creation used in bayes_tests
prepare_df <- function(sampled_obj, alpha_mu = FALSE, part_numb, task){
  task_list <- list("edt" = list(1,8), "lex" = list(18,21), "pdt" = list(9,14), "nb" = list(15,17))
  if (alpha_mu == TRUE){
    task_ident <- task_list[task - 1]
    a <- task_ident[[1]][[1]]
    b <- task_ident[[1]][[2]]
    #print(paste(a, b))
    df <- sampled_obj$samples$alpha[a:b, part_numb, sampled_obj$samples$stage == "sample"]
    pars <- rownames(df)
    print(deparse(substitute(sampled_obj)))
    print(paste(task, a, b))
    print(pars)
    print(df[,1:5])
    df <- as.data.frame(t(df))
    df <- df %>% pivot_longer(cols=everything(), names_to = "parameter", values_to = "estimate")
    return(list(df, pars))
    
  }
  else{
    df <- as.data.frame(t(sampled_obj$samples$theta_mu[, sampled_obj$samples$stage=="sample"]))
    df <- df %>% pivot_longer(cols=everything(), names_to = "parameter", values_to = "estimate")
    return(df)
  }
}

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

#loop through subjects with MD - call bayes_tests for those subjects
alpha_tests <- function(sampled_obj, MD_obj, SD = FALSE, plot = FALSE){
  count <- 0
  df_alpha <- data.frame()
  for (x in 1){ # :length(MD_obj)
    sub <- MD_obj[[x]][1]
    task <- MD_obj[[x]][2]
    print(paste(sub,task))
    print(deparse(substitute(sampled_obj)))
    df <- prepare_df(sampled_obj, alpha_mu = TRUE, sub, task)
    pars <- df[[2]]
    df <- df[[1]]
    
    df_full <- prepare_df(sampled_full, alpha_mu = TRUE, sub, task)
    df_full <- df_full[[1]]
    print(head(df))
    dfs <- list(df, df_full)
    
    print(paste(nrow(df_full),  nrow(df)))
    
    if (SD == TRUE){
      bayes_tests(pars, dfs, alpha = TRUE, SD = TRUE)
      count <- count + 1
    }
    else if (plot == TRUE){
      count <- count + 1
      df_alpha_p <- bayes_tests(pars, dfs, alpha = TRUE, plot = TRUE)
      df_alpha_p$participant <- count
      #print(nrow(df_alpha_p))
      df_alpha <- rbind(df_alpha, df_alpha_p)
    }
    else{
      count <- count + 1
      bayes_tests(pars, dfs, alpha = TRUE)
    }
  }
  cat(count)
  return(df_alpha)
}

#bayesian t-test and CIs of diference between posterior (df with MD) and null(full df)
bayes_tests <- function(pars, df, alpha = FALSE, SD = FALSE, plot = FALSE){
  prior <- distribution_normal(1500, mean = 0, sd = 1)
  count <- 0
  tasks_odd_BF <- 0
  plot_df <- data.frame()
  plot_df_prior <- as.data.frame(prior)
  plot_df_prior$MD <- "prior"
  plot_df_prior <- rename(plot_df_prior, estimate = prior)
  plot_df <- rbind(plot_df, plot_df_prior)
  plot_df$par <- "p"

  if (alpha == TRUE & SD == FALSE){
    for (par in pars){
      count <- count + 1
      alpha_full <- df[[2]][df[[2]]$parameter == par,]$estimate
      alpha_rec <- df[[1]][df[[1]]$parameter == par,]$estimate
      #print(paste("full:", length(alpha_full)))
      #print(paste("rec:", length(alpha_rec)))
      dif <- alpha_full - alpha_rec 
      
      cat(" - ")

      ci <- ci(dif, ci = 0.95)
      if (plot == TRUE){
        plot_df_full <- as.data.frame(alpha_full)
        plot_df_full$MD <- "full"
        plot_df_full <- rename(plot_df_full, estimate = alpha_full)
        plot_df_rec <- as.data.frame(alpha_rec)
        plot_df_rec$MD <- "rec"
        plot_df_rec <- rename(plot_df_rec, estimate = alpha_rec)
        plot_df_par <- rbind(plot_df_full, plot_df_rec)       
        plot_df_par$par <- par
        plot_df <- rbind(plot_df, plot_df_par)
      }
      else {
        bf <- ttestBF(alpha_full, alpha_rec)
        SD <- bayesfactor_parameters(dif, prior)
        if ((ci$CI_low < 0 & ci$CI_high > 0) == FALSE | SD$log_BF > 0){
        cat("\n::::::::::::::::::::::::\n")
        print(SD)
        cat("\n::::::::::::::::::::::::\n")
        print(ci)
        cat("\n::::::::::::::::::::::::\n")
        print(bf)
        cat(":::::::::::::::::::::::\n")
        tasks_odd_BF <- tasks_odd_BF + 1
        }
      }
    }
  }
  else if (alpha == TRUE & SD == TRUE){
    for (par in pars){
      count <- count + 1
      alpha_rec <- df[[1]][df[[1]]$parameter == par,]$estimate
      SD <- bayesfactor_parameters(alpha_rec, prior)
      if (SD$log_BF < 1){
        cat("\n")
        print(SD)
        cat("\n")
        tasks_odd_BF <- tasks_odd_BF + 1
      }
    }
  }
  else {
    for (par in pars){
      count <- count + 1
      theta_full <- df_full[df_full$parameter == par,]$estimate
      theta_rec <- df[df$parameter == par,]$estimate
      dif <- theta_full - theta_rec 
      
      cat(" - ")
      SD <- bayesfactor_parameters(dif, prior)
      ci <- ci(dif, ci = 0.95)
      if ((ci$CI_low < 0 & ci$CI_high > 0) == FALSE | SD$log_BF > 0){
        print(ttestBF(theta_full, theta_rec))
        cat("\n_____________________\n")
        print(SD)
        cat("\n_____________________\n")
        print(ci)
        tasks_odd_BF <- tasks_low_BF + 1
      }
      if (plot == TRUE){
        plot_df_full <- as.data.frame(theta_full)
        plot_df_full$MD <- "full"
        plot_df_full <- rename(plot_df_full, estimate = theta_full)
        plot_df_rec <- as.data.frame(theta_rec)
        plot_df_rec$MD <- "rec"
        plot_df_rec <- rename(plot_df_rec, estimate = theta_rec)
        plot_df_par <- rbind(plot_df_full, plot_df_rec, plot_df_prior)
        plot_df_par$par <- par
        plot_df <- rbind(plot_df, plot_df_par)
      }
    }
  }
  print(paste("Parameter-count:", count, "Odd log_BF/CI:",tasks_odd_BF))
  return(plot_df)
}

#compare prior with estimated distributions - both full and with MD
plot_dist <- function(plot_df, destination, MD, alpha = FALSE ){
  if (alpha == FALSE){
    pdf(file = destination)
    par(mfrow = c(2,2))
    for (par in pars){
      plot_df_par <- plot_df[plot_df$par == par | plot_df$par == "p",]
      print(ggplot(plot_df_par, aes(x=estimate, fill=MD)) +
              geom_histogram(binwidth=.05, alpha=.5, position="identity") + 
              labs(title = paste(par, " - theta")))
    }
    
    dev.off()
  }
  else {
    task_list <- list("edt" = list(1,8), "lex" = list(18,21), "pdt" = list(9,14), "nb" = list(15,17))
    pdf(file = destination)
    par(mfrow = c(2,2))
    for (subs in unique(plot_df$participant)){
      task <- task_list[MD[[subs]][[2]] - 1]
      a <- task[[1]][[1]]
      b <- task[[1]][[2]]
      #print(task)
      pars <- sampled_full$par_names[a:b]
      for (par in pars){
        plot_df_par <- plot_df[(plot_df$par == par) & plot_df$participant == subs,]
        print(ggplot(plot_df_par, aes(x=estimate, fill=MD)) +
                geom_histogram(binwidth=.05, alpha=.5, position="identity") + 
                labs(title = paste(par, subs, " - alpha")))
      }
    }
    dev.off()
  }
}


##group level
df_full <- prepare_df(sampled_full)
df_1 <- prepare_df(sampled_1)
df_2 <- prepare_df(sampled_2)
df_3 <- prepare_df(sampled_3)

pars <- rownames(sampled_1$samples$theta_mu)

##plot theta
plot_df_s1 <- bayes_tests(pars, df_1, plot = TRUE)
plot_df_s2 <- bayes_tests(pars, df_2, plot = TRUE)
plot_df_s3 <- bayes_tests(pars, df_3, plot = TRUE)

destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\theta_s1_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\theta_s2_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\theta_s3_hist.pdf)"

destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\theta_s1_MDevery1_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\theta_s2_MDevery1_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\theta_s3_MDevery1_hist.pdf)"


plot_dist(plot_df_s1, destination_s1)
plot_dist(plot_df_s2, destination_s2)
plot_dist(plot_df_s3, destination_s3)


##individual level with missing tasks only
MD_1 <- id_MD(sampled_1)
MD_2 <- id_MD(sampled_2)
MD_3 <- id_MD(sampled_3)


#test if rec distribution moved away from the prior
alpha_tests(sampled_1, MD_1, SD = TRUE)
alpha_tests(sampled_2, MD_2, SD = TRUE)
alpha_tests(sampled_3, MD_3, SD = TRUE)

#test if rec distribution is the same as full distribution
alpha_tests(sampled_1, MD_1)
alpha_tests(sampled_2, MD_2)
alpha_tests(sampled_3, MD_2)

#plot alpha
alpha_df_s1 <- alpha_tests(sampled_1, MD_1, plot = TRUE)
alpha_df_s2 <- alpha_tests(sampled_2, MD_2, plot = TRUE)
alpha_df_s3 <- alpha_tests(sampled_3, MD_3, plot = TRUE)



destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\alpha_s1_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\alpha_s2_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\alpha_s3_hist.pdf)"

destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\alpha_s1_MDevery1_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\alpha_s2_MDevery1_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\alpha_s3_MDevery1_hist.pdf)"


plot_dist(alpha_df_s1, destination_s1, MD_1, alpha = TRUE)
plot_dist(alpha_df_s2, destination_s2, MD_2, alpha = TRUE)
plot_dist(alpha_df_s3, destination_s3, MD_3, alpha = TRUE)

