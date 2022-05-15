### Model fit figure for Juanita

rm(list=ls())
library(dplyr)
library(rtdists)
library(ggplot2)
library(jtools)
library(scales)
library(ggpubr)

setwd("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/IS2_keats/FINAL")
#### generate data ####
generate.posterior <- function(sampled, n){
  n.posterior=n # Number of parameter samples from posterior distribution.
  pp.data=list()
  S = sampled$n_subjects
  data=sampled$data
  data$subject<- as.factor(data$subject)
  sampled_stage = length(sampled$samples$stage[sampled$samples$stage=="sample"])
  for (s in 1:S) {
    cat(s," ")
    iterations=round(seq(from=(sampled$samples$idx-sampled_stage) , to=sampled$samples$idx, length.out=n.posterior))
    for (i in 1:length(iterations)) {
      x <- sampled$samples$alpha[,s,iterations[i]]
      names(x) <- sampled$par_names
      tmp=sampled$ll_func(x=x,data=data[as.integer(as.numeric(data$subject))==s,],sample=TRUE)
      if (i==1) {
        pp.data[[s]]=cbind(i,tmp)
      } else {
        pp.data[[s]]=rbind(pp.data[[s]],cbind(i,tmp))
      }
    }
    
  }
  return(pp.data)
}
###MSC
#### winning vs complex vs joint
#complex
load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/MSC/IS2_t_8b.Rdata")
pp.data<-generate.posterior(sampled,20)
tmp=do.call(rbind,pp.data)
tmp$resp <- "long"
tmp$resp[tmp$response==1]<-"short"
#tmp$resp <- as.factor(tmp$resp)
tmp$corr <- 1
tmp$corr[tmp$length!=tmp$resp]<-0
complex=tmp

#winning
load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/MSC/IS2_t_1b.Rdata")
pp.data<-generate.posterior(sampled,20)
tmp=do.call(rbind,pp.data)
tmp$resp <- "long"
tmp$resp[tmp$response==1]<-"short"
#tmp$resp <- as.factor(tmp$resp)
tmp$corr <- 1
tmp$corr[tmp$length!=tmp$resp]<-0
winning=tmp



#plot
summ6 <- complex %>% group_by(i,cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(corr))%>%
  ungroup()
summ6$source <- "MSC_6"


summ2 <- winning %>% group_by(i,cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(corr))%>%
  ungroup()
summ2$source <- "MSC_2"


summD <- data %>% group_by(cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(correct))%>%
  ungroup()
summD$source <- "data"

dat <- rbind(summ2, summ6)

acc1 <- ggplot()+
  geom_col(data=summD, aes(x=trialType, y=ACC, fill=source, group=source), alpha=0.5)+
  geom_point(data=dat,aes(x=trialType, y=ACC, color=source, group=source), position = position_dodge2(width=0.3))+
  #geom_line(data=dat, aes(x=trialType, y=ACC, color=source, group=source))+
  coord_cartesian(ylim = c(0.7, .95))+
  facet_wrap(~cond)+
  theme_bw()

rt1 <- ggplot()+
  geom_col(data=summD, aes(x=trialType, y=MeanRT, fill=source, group=source), alpha=0.5)+
  geom_point(data=dat,aes(x=trialType, y=MeanRT, color=source, group=source), position = position_dodge2(width=0.3))+
  #geom_line(data=dat, aes(x=trialType, y=MeanRT, color=source, group=source))+
  coord_cartesian(ylim = c(0.66, 0.73))+
  facet_wrap(~cond)+
  theme_bw()

save.image("cov_output.Rdata")


##############        Other   ##################
####null vs t0 (needs ll) vs cov
rm(list=ls())


generate.posterior <- function(sampled, n){
  n.posterior=n # Number of parameter samples from posterior distribution.
  pp.data=list()
  S = sampled$n_subjects
  data=sampled$data
  data$subject<- as.factor(data$subject)
  sampled_stage = length(sampled$samples$stage[sampled$samples$stage=="sample"])
  for (s in 1:S) {
    cat(s," ")
    iterations=round(seq(from=(sampled$samples$idx-sampled_stage) , to=sampled$samples$idx, length.out=n.posterior))
    for (i in 1:length(iterations)) {
      x <- sampled$samples$alpha[,s,iterations[i]]
      names(x) <- sampled$par_names
      tmp=sampled$ll_func(x=x,data=data[as.integer(as.numeric(data$subject))==s,],sample=TRUE)
      if (i==1) {
        pp.data[[s]]=cbind(i,tmp)
      } else {
        pp.data[[s]]=rbind(pp.data[[s]],cbind(i,tmp))
      }
    }
    
  }
  return(pp.data)
}
####null vs t0 (needs ll) vs cov
load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/ERP/keats_null.RData")
pp.data<-generate.posterior(sampled,20)
tmp=do.call(rbind,pp.data)
tmp$resp <- "long"
tmp$resp[tmp$response==1]<-"short"
#tmp$resp <- as.factor(tmp$resp)
tmp$corr <- 1
tmp$corr[tmp$length!=tmp$resp]<-0
null=tmp

#winning
#load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/IS2_keats/FINAL/IS2_keats_B.Rdata")
#sampled$ll_func <- function (x, data, sample = FALSE) 
# {
#   x[1:13] = exp(x[1:13])
#   contamProb = pnorm(x["contaminant"])
#   A = x["A"]
#   t0 = x["t0"]
#   out <- numeric(nrow(data))
#   vS = vL = b = trial = numeric(nrow(data))
#   trial <- data$trial
#   for (c in levels(data$cond)) {
#     for (t in levels(data$trialType)) {
#       isDiff = data$cond == c & data$trialType == t
#       b[isDiff] <- ifelse(data$length[isDiff] == "long", 
#                           x["b.long"] + A, x["b.short"] + A)
#       b[isDiff] <- ifelse(data$postDeviant[isDiff] == 1, 
#                           ifelse(data$postDeviant2[isDiff] == 2, b[isDiff] + 
#                                    x["b.post.high.Dev"], b[isDiff] + x["b.post.low.Dev"]), 
#                           b[isDiff])
#       vS[isDiff] <- ifelse(data$length[isDiff] == "long", 
#                            x["v.mean"] - x[paste0("v.diff.", c, ".", trial[isDiff])], 
#                            x["v.mean"] + x[paste0("v.diff.", c, ".", trial[isDiff])])
#       vL[isDiff] <- ifelse(data$length[isDiff] == "long", 
#                            x["v.mean"] + x[paste0("v.diff.", c, ".", trial[isDiff])], 
#                            x["v.mean"] - x[paste0("v.diff.", c, ".", trial[isDiff])])
#     }
#   }
#   if (sample) {
#     data$rt = NA
#     data$resp = NA
#     for (i in 1:nrow(data)) {
#       out = rLBA(n = 1, A = A, b = b[i], t0 = t0, mean_v = list(vS[i], 
#                                                                 vL[i]), sd_v = list(1, 1), distribution = "norm", 
#                  silent = TRUE)
#       data$rt[i] <- out$rt
#       data$response[i] <- out$resp
#     }
#     return(data)
#   }
#   else {
#     out = dLBA(rt = data$rt, response = data$response, A = A, 
#                b = b, t0 = t0, mean_v = list(vS, vL), sd_v = list(1, 
#                                                                   1), distribution = "norm", silent = TRUE)
#     out = ((1 - contamProb) * out) + (contamProb * (1/2) * 
#                                         0.5)
#   }
#   return(sum(log(pmax(out, 1e-10))))
# }

load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/IS2_keats/FINAL/IS2_keats_simple.Rdata")

pp.data<-generate.posterior(sampled,20)
tmp=do.call(rbind,pp.data)
tmp$resp <- "long"
tmp$resp[tmp$response==1]<-"short"
#tmp$resp <- as.factor(tmp$resp)
tmp$corr <- 1
tmp$corr[tmp$length!=tmp$resp]<-0
winning<-tmp

#cov
load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/IS2_keats/FINAL/IS2_keats_CovDev.Rdata")
pp.data<-generate.posterior(sampled,20)
tmp=do.call(rbind,pp.data)
tmp$resp <- "long"
tmp$resp[tmp$response==1]<-"short"
#tmp$resp <- as.factor(tmp$resp)
tmp$corr <- 1
tmp$corr[tmp$length!=tmp$resp]<-0
cov=tmp


#load("~/Documents/Research/Modelling Project/Work/Projects/Todd_Behavioural/Keats/IS2_keats/FINAL/IS2_keats_t0.Rdata")
#plot
summN <- null %>% group_by(cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(corr))%>%
  ungroup()
summN$source <- "Null"

summW <- winning %>% group_by(cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(corr))%>%
  ungroup()
summW$source <- "Model_9"

summE <- cov %>% group_by(cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(corr))%>%
  ungroup()
summE$source <- "ERP_model"

data <- sampled$data
data$correct<- ifelse(data$resp==data$length,1,0)
summD <- data %>% group_by(cond, trialType) %>%
  summarise(MeanRT = mean(rt,na.rm=TRUE),
            ACC = mean(correct))%>%
  ungroup()
summD$source <- "data"

dat <- rbind(summN, summW, summE)

ggplot()+
  geom_col(data=summD, aes(x=trialType, y=ACC, fill=source, group=source), alpha=0.5)+
  geom_point(data=dat,aes(x=trialType, y=ACC, color=source, group=source))+
  geom_line(data=dat, aes(x=trialType, y=ACC, color=source, group=source))+
  coord_cartesian(ylim = c(0.55, 0.93))+
  facet_wrap(~cond)+
  labs(x="Trial Type", y="Accuracy")+
  theme_bw()

ggplot()+
  geom_col(data=summD, aes(x=trialType, y=MeanRT, fill=source, group=source), alpha=0.5)+
  geom_point(data=dat,aes(x=trialType, y=MeanRT, color=source, group=source))+
  geom_line(data=dat, aes(x=trialType, y=MeanRT, color=source, group=source))+
  coord_cartesian(ylim = c(0.66, 0.78))+
  facet_wrap(~cond)+
  labs(x="Trial Type", y="Response Time")+
  theme_bw()

save.image("mod_output.Rdata")


