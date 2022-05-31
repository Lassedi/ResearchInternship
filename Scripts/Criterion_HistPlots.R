rm(list = ls())

library(bayestestR)
library(dplyr)
library(tidyr)
library(ggplot2)


setwd(r"(D:\Psychology\Master\Research Internship\Data\2ndrun)")
load("SDT_full_.Rdata")
sampled_full <- sampled

#load data - half of participants have one missing task
load("SDT_half_1.Rdata")
sampled_1 <- sampled
load("SDT_half_2.Rdata")
sampled_2 <- sampled
load("SDT_half_3.Rdata")
sampled_3 <- sampled
sampled_obj <- list(sampled_1, sampled_2, sampled_3)

##load data - every participant has one task removed
load("SDT_full_noCon_al11.Rdata")
sampled_1 <- sampled
load("SDT_full_noCon_al12.Rdata")
sampled_2 <- sampled
load("SDT_full_noCon_al13.Rdata")
sampled_3 <- sampled
sampled_obj <- c(sampled_1, sampled_2, sampled_3)

#load data - proportional MD
load("SDT_prop_.Rdata")
sampled_1 <- sampled
sampled_obj <- c(sampled_1)



#remove unne
rm(sampled)

#criterion tests
pmwg_informationCriterion <- function(samples, filter = "sample"){
  ##Gets DIC, BPIC and effective parameters
  # Mean log-likelihood of the overall (samples-stage) model, for each subject
  mean_ll <- apply(samples$samples$subj_ll[, samples$samples$stage == filter], 1, mean)
  # Mean of each parameter across iterations.
  # Keep dimensions for parameters and subjects
  mean_pars <- t(apply(samples$samples$alpha[,, samples$samples$stage == filter], 1:2, mean))
  # Name 'mean_pars' so it can be used by the log_like function
  colnames(mean_pars) <- samples$par_names
  # log-likelihood for each subject using their mean parameter vector
  data <- samples$data
  mean_pars_ll <- numeric() #sloppy but hey, still fast enough
  for (sub in 1:length(samples$subjects)) {
    mean_pars_ll[sub] <- samples$ll_func(mean_pars[sub, ], data = data[data$subject == sub,])
  }
  # mean deviance(-2*ll of all data)
  # effective number of parameters(-2*ll of all data - -2*ll with mean theta)
  pD <- sum(-2 * mean_ll + 2 * mean_pars_ll)
  # DIC = mean deviance + effective number of parameters
  DIC <- sum(-4 * mean_ll + 2 * mean_pars_ll)
  # BPIC = mean deviance + 2*effective number of parameters
  # Note this is the "easy" BPIC, instead of the complex 2007 one
  BPIC <- sum(-6 * mean_ll + 4 * mean_pars_ll)
  return(c("DIC " = DIC, "BPIC" = BPIC, "Effective parameters" = pD))
}

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
    #print(df[,1:5])
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
alpha_tests <- function(sampled_obj, MD_obj, plot = FALSE){
  count <- 0
  df_alpha <- data.frame()
  for (x in 1:length(MD_1)){ # :length(MD_obj)
    sub <- MD_obj[[x]][1]
    task <- MD_obj[[x]][2]
    print(paste(sub,task))
    #print(deparse(substitute(sampled_obj)))
    df <- prepare_df(sampled_obj, alpha_mu = TRUE, sub, task)
    pars <- df[[2]]
    df <- df[[1]]
    
    
    df_full <- prepare_df(sampled_full, alpha_mu = TRUE, sub, task)
    df_full <- df_full[[1]]
    df <- sample_n(df, nrow(df_full))
    
    #print(head(df))
    dfs <- list(df, df_full)
    
    #print(paste(nrow(df_full),  nrow(df)))
    
    if (plot == TRUE){
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
bayes_tests <- function(pars, df, alpha = FALSE){
  count <- 0
  tasks_odd_BF <- 0
  plot_df <- data.frame()

  
  if (alpha == TRUE){
    for (par in pars){
      count <- count + 1
      alpha_full <- df[[2]][df[[2]]$parameter == par,]$estimate
      alpha_rec <- df[[1]][df[[1]]$parameter == par,]$estimate
      
      #print(paste("full:", length(alpha_full)))
      #print(paste("rec:", length(alpha_rec)))

      cat(" - ")
    
        plot_df_full <- as.data.frame(alpha_full)
        plot_df_full$MD <- "Original"
        plot_df_full <- rename(plot_df_full, estimate = alpha_full)
        plot_df_rec <- as.data.frame(alpha_rec)
        plot_df_rec$MD <- "MD"
        plot_df_rec <- rename(plot_df_rec, estimate = alpha_rec)
        plot_df_par <- rbind(plot_df_full, plot_df_rec)       
        plot_df_par$par <- par
        
        cat(nrow(plot_df_full), nrow(plot_df_rec))
        plot_df <- rbind(plot_df, plot_df_par)
      
    }
  }
  else {
    prior <- distribution_normal(1500, mean = 0, sd = 1)
    plot_df_prior <- as.data.frame(prior)
    plot_df_prior$MD <- "prior"
    plot_df_prior <- rename(plot_df_prior, estimate = prior)
    plot_df <- rbind(plot_df, plot_df_prior)
    plot_df$par <- "p"
    for (par in pars){
      count <- count + 1
      theta_full <- df_full[df_full$parameter == par,]$estimate
      theta_rec <- df[df$parameter == par,]$estimate

      cat(" - ")
      
        plot_df_full <- as.data.frame(theta_full)
        plot_df_full$MD <- "Original"
        plot_df_full <- rename(plot_df_full, estimate = theta_full)
        plot_df_rec <- as.data.frame(theta_rec)
        plot_df_rec$MD <- "MD"
        plot_df_rec <- rename(plot_df_rec, estimate = theta_rec)
        
        #cat(nrow(plot_df_full), nrow(plot_df_rec),nrow(plot_df_prior))
        plot_df_par <- rbind(plot_df_full, plot_df_rec, plot_df_prior)
        plot_df_par$par <- par
        plot_df <- rbind(plot_df, plot_df_par)
        
      
    }
  }
  print(paste("Parameter-count:", count))
  return(plot_df)
}

#compare prior with estimated distributions - both full and with MD
plot_dist <- function(plot_df, destination, MD, alpha = FALSE, sampled_obj = NULL){
  if (alpha == FALSE){
    pdf(file = destination)
    par(mfrow = c(2,2))
    for (par in pars){
      plot_df_par <- plot_df[(plot_df$par == par)&(plot_df$MD !="prior"),]
      print(nrow(plot_df_par))
      print(ggplot(plot_df_par, aes(x=estimate, fill=MD)) +
                geom_density(color = "999999", alpha=.6) +
                geom_vline(xintercept= 0, color="red", linetype="dashed", size=0.5) + 
                labs(title = paste(par, " - theta")
                     ))
    }
    
    dev.off()
  }
  else {
    task_list <- list("edt" = list(1,8), "lex" = list(18,21), "pdt" = list(9,14), "nb" = list(15,17))
    pdf(file = destination)
    par(mfrow = c(2,2))
    
    for (subs in unique(plot_df$participant)){
      task <- task_list[MD[[subs]][[2]] - 1]
      sub <- MD[[subs]][[1]]
      a <- task[[1]][[1]]
      b <- task[[1]][[2]]
      #print(task)
      pars <- sampled_full$par_names[a:b]
      theta_all <- prepare_df(sampled_obj)
      for (par in pars){
        plot_df_par_alpha <- plot_df[(plot_df$par == par) & plot_df$participant == subs,]
        print(colnames(theta_all))
        print(colnames(plot_df_par_alpha))
        theta <- theta_all[theta_all$parameter == par,]
        theta$MD <- "theta"
        cat("nrow theta: ", nrow(theta), "nrow alpha: ", nrow(plot_df_par_alpha), "\n")
        print(head(plot_df_par_alpha))
        print(ggplot(NULL, aes(x=estimate, fill=MD)) +
                geom_density(data = plot_df_par_alpha,color = "white", alpha=.6, position="identity") +
                geom_density(data = theta, color= "white", alpha=.6, position="identity") +
                geom_vline(xintercept= 0, color="red", linetype="dashed", size=0.5) + 
                labs(title = paste(par, " - alpha", "\nSubject:", sub)))
      }
    }
    dev.off()
  }
} 


##group level
destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_h1_V2_den.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_h2_V2_den.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_h3_V2_den.pdf)"

destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_s1_MDevery1_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_s2_MDevery1_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_s3_MDevery1_hist.pdf)"

"storage - prop MD"
destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\theta_prop_den.pdf)"

df_full <- prepare_df(sampled_full)
df_1 <- prepare_df(sampled_1)
df_1 <- sample_n(df_1, 31500)

"plot_theta"
df_full <- prepare_df(sampled_full)
df_1 <- prepare_df(sampled_1)
df_2 <- prepare_df(sampled_2)
df_3 <- prepare_df(sampled_3)

pars <- rownames(sampled_1$samples$theta_mu)

##plot theta
plot_df_s1 <- bayes_tests(pars, df_1)
plot_df_s2 <- bayes_tests(pars, df_2)
plot_df_s3 <- bayes_tests(pars, df_3)


plot_dist(plot_df_s1, destination_s1)
plot_dist(plot_df_s2, destination_s2)
plot_dist(plot_df_s3, destination_s3)


"individual level with missing tasks only"
#storage destination - half of subs missing one task
destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_s1_V2_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_s2_V2_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_s3_V2_hist.pdf)"

#storage destination - every sub missing one task
destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_s1_MDevery1_hist.pdf)"
destination_s2 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_s2_MDevery1_hist.pdf)"
destination_s3 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_s3_MDevery1_hist.pdf)"

#storage destination - prop MD
destination_s1 <- r"(D:\Psychology\Master\Research Internship\Plots\Dist_V2\alpha_prop_den.pdf)"


MD_1 <- id_MD(sampled_1)
MD_2 <- id_MD(sampled_2)
MD_3 <- id_MD(sampled_3)

#create dfs to plot - identify which task is missing for which person and get the full and rec for each par of the missing task
alpha_df_s1 <- alpha_tests(sampled_1, MD_1, plot = TRUE)
alpha_df_s2 <- alpha_tests(sampled_2, MD_2, plot = TRUE)
alpha_df_s3 <- alpha_tests(sampled_3, MD_3, plot = TRUE)

#plot the dataframes
plot_dist(alpha_df_s1, destination_s1, MD_1, alpha = TRUE, sampled_1)
plot_dist(alpha_df_s2, destination_s2, MD_2, alpha = TRUE, sampled_2)
plot_dist(alpha_df_s3, destination_s3, MD_3, alpha = TRUE, sampled_3)


"Criterion tests"
#original fit
pmwg_informationCriterion(sampled_full)

#Recovered 0.5/1
load("SDT_full_noCon_1.Rdata")
sampled_1<- sampled
pmwg_informationCriterion(sampled_1)

load("SDT_full_noCon_2.Rdata")
sampled_2<- sampled
pmwg_informationCriterion(sampled_2)

load("SDT_full_noCon_3.Rdata")
sampled_3<- sampled
pmwg_informationCriterion(sampled_3)

load("SDT_full_noCon_al11.Rdata")
sampled_ev_1<- sampled
pmwg_informationCriterion(sampled_ev_1)

load("SDT_full_noCon_al12.Rdata")
sampled_ev_2<- sampled
pmwg_informationCriterion(sampled_ev_2)

load("SDT_full_noCon_al13.Rdata")
sampled_ev_3<- sampled
pmwg_informationCriterion(sampled_ev_3)

#PROP
load("SDT_prop_.Rdata")
sampled$data <- sampled_full$data
pmwg_informationCriterion(sampled_full)
