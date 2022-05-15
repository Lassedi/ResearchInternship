library(dplyr)
library(ggplot2)
library(RColorBrewer)
rm(list = ls())
pdf(file="covPlots.pdf")



pars <- seq(10,50, by=10)
subjects <- seq(25, 100, by =25)
factors <- c("constrained","unconstrained")

listofObjects<-expand.grid(pars,subjects,factors)
names(listofObjects)<-c("pars","subjects","factors")


for(permutation in 1:nrow(listofObjects)){
  print(permutation)
  n_pars = listofObjects$pars[permutation]
  n_subjects = listofObjects$subjects[permutation]
  n_factors = listofObjects$factors[permutation]
  print(paste0("samples/MVN_", n_subjects, "S_", n_pars, "P_.RData"))
  load(paste0("samples/MVN_", n_subjects, "S_", n_pars, "P_.RData"))
  covMat <- stats::cov(aggregate(. ~ subject, sampled$data, base::mean)[1:sampled$n_pars + 1])
  covMVN <- sampled$samples$theta_var
  print(paste0("samples/factor_", n_factors, "_F_", n_subjects, "S_", n_pars, "P_.RData"))
  load(paste0("samples/factor_", n_factors, "_F_", n_subjects, "S_", n_pars, "P_.RData"))
  covFactor <- sampled$samples$theta_var
  
  filterXY <- 1:10
  filterZ <- 2000:2500
  covMVN <- covMVN[filterXY, filterXY, filterZ]
  covFactor <- covFactor[filterXY, filterXY, filterZ]
  covMat <- covMat[filterXY, filterXY]
  # corrplot::corrplot(cov2cor(apply(covFactor, 1:2, mean)))
  # corrplot::corrplot(cov2cor(apply(covMVN, 1:2, mean)))
  # corrplot::corrplot(cov2cor(covMat))
  
  load(paste0("samples/diag_", n_subjects, "S_", n_pars, "P_.RData"))
  covDiag <- stats::cov(t(sampled$samples$theta_mu[, filterZ]))
  covDiag <- covDiag[filterXY, filterXY]
  
  # corrplot::corrplot(cov2cor(covMat))
  
  cov1 <- covFactor #Place holder
  
  for(j in 1:dim(cov1)[3]){
    tmp_factor <- covMVN[,,j]
    tmp_MVN <- covFactor[,,j]
    
    tmp <- tmp_factor #placeholder again
    cor_MVN <- cov2cor(tmp_MVN)
    cor_factor <- cov2cor(tmp_factor)
    tmp[upper.tri(tmp)] <- cor_factor[upper.tri(cor_factor)] 
    tmp[lower.tri(tmp, diag = T)] <- cor_MVN[lower.tri(cor_MVN, diag = T)]
    cov1[,,j] <- tmp
  }
  
  parNames <- expand.grid(colnames(cov1), colnames(cov1))
  
  data <- as.numeric(cov1[,,1])
  for(i in 2:dim(cov1)[3]){
    data <- c(data, as.numeric(cov1[,,i]))
  }
  
  parNames1 <- rep(parNames[,1], dim(cov1)[3])
  parNames2 <- rep(parNames[,2], dim(cov1)[3])
  iteration <- rep(1:dim(cov1)[3], each = nrow(cov1)*nrow(cov1))
  
  all_data <- data.frame(iteration = iteration, parNames1 = parNames1, parNames2 = parNames2, value = data)
  
  
  meanDat <- aggregate(value ~ parNames1*parNames2, data = all_data, base::mean)
  all_data$meanDat <- rep(meanDat$value, dim(cov1)[3])
  all_data$is_diag <- F
  all_data$is_diag[all_data$value == 1] <- T
  
  
  true_cor <- as.numeric(cov2cor(covMat))
  true_data <- data.frame(iteration = rep(-1, length(true_cor)), parNames1 = parNames[,1], parNames2 = parNames[,2], value = true_cor, meanDat = meanDat$value, is_diag = F)
  
  diag_cor <- as.numeric(cov2cor(covDiag))
  diag_data <- data.frame(iteration = rep(-9, length(diag_cor)), parNames1 = parNames[,1], parNames2 = parNames[,2], value = diag_cor, meanDat = meanDat$value, is_diag = F)
  
  
  
  all_data <- rbind(all_data, true_data, diag_data)
  
  
  cols <- colorRampPalette( colors = brewer.pal(9,"RdBu") )
  
  plotto<-ggplot(all_data, aes(value)) +   facet_grid(parNames1 ~ parNames2, switch = "y") + 
    geom_histogram(data = subset(all_data, iteration != -1), aes(y = ..density.., colour=meanDat, fill=meanDat), binwidth=.005)+
    geom_density(data = subset(all_data, iteration != -1), color = "black") + 
    geom_vline(data = subset(all_data, iteration == -1), aes(xintercept = value), size = 1.1, lty = "11") + 
    geom_vline(data = subset(all_data, iteration == -9), aes(xintercept = value), size = 1.1, lty = "11", color = "red") + 
    geom_point(data = subset(all_data, is_diag == T), aes(y = value, colour=meanDat, fill=meanDat), size = 1000) + 
    xlim(c(-1,1)) + 
    ylim(c(-1, 5)) + 
    theme_bw() +
    scale_fill_gradientn(colours=cols(11), limits = c(-1, 1)) + 
    scale_colour_gradientn(colours=cols(11), limits = c(-1, 1)) +
    xlab("correlation") + 
    labs(title = paste0("factor_", n_factors, "F_", n_subjects, "S_", n_pars, "P"))+ 
    theme(
      strip.background = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=2),
      panel.spacing = unit(0, "lines"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y=element_blank(),
      strip.text = element_text(size = 12)
      
    )
  plot(plotto)
  print(paste0(permutation,"_done"))
  
}

dev.off()
