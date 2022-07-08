###Plotting script for group level posterior predictive plots###
library(berryFunctions)

##check whether all participants have data - if not add 0
dataCheck_addZero <-  function(df, npar, i = FALSE){
  count <- 0
  count2 <- 0
  if (i == TRUE){
    n_it <- 53*20*npar
  }
  else{
    n_it <- 53*npar
  }
  for (x in 1: n_it){
    count <- count + 1
    df_sub <- data.frame()
    df_sub <- df[x,]
    #print(df_sub)
    if(is.na(df_sub$sub)){
      df_sub <- df[(x-10),]
      df_sub$sub <- count
      df_sub$n <- 0
      df <- insertRows(df, x, df_sub)
    }
    else if (count > 35){
      sub <- df_sub$sub
      df_sub <- df[(x-15),]
      df_sub$sub <- sub
    }
    #print(x)
    #print(df_sub)
    #print(df_sub$sub != count)
    if (df_sub$sub != count){
      #print(count)
      df_sub$sub <- count
      df_sub$n <- 0
      df <- insertRows(df, x, df_sub)
    }
    if (count == 53){
      count <- 0
      count2 <- count2+1
      cat(count2, " ")
    }
  }
  return(df)
}



"EDT"
##original fit##
edt <- df_edt[df_edt$responseSample == "Happy",] %>% na.exclude() %>% 
  group_by(i, Gender, cond, Mouth) %>% 
  count(sub, i, cond, Gender, Mouth) %>% dataCheck_addZero(8, i = TRUE) 

edt$cond <- paste(edt$Gender, edt$cond, edt$Mouth, sep = "\n")
edt_happy <- edt[, colnames(edt) != "Mouth" & colnames(edt) != "Gender"]

edt <- df_edt[df_edt$responseSample == "Angry",] %>% na.exclude() %>% 
  group_by(i, Gender, cond, Mouth) %>% 
  count(sub, i, cond, Gender, Mouth) %>% dataCheck_addZero(8, i = TRUE)

edt$cond <- paste(edt$Gender, edt$cond, edt$Mouth, sep = "\n")
edt_angry <- edt[, colnames(edt) != "Mouth" & colnames(edt) != "Gender"]
rm(edt)

df_list <- merge(edt_angry, edt_happy, by =c("i", "cond", "sub")) 
df_list <- df_list[order(df_list$i, df_list$cond, df_list$sub),]

df_list$n_sum <- rowSums(df_list[,c("n.x", "n.y")])

df_list$prop <- df_list$n.y / df_list$n_sum
df_edt_alpha <- df_list
df_list <- df_list %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))

##Missing data fit##
x2 <- df_MD_edt
x2 <- x2[x2$responseSample == "Happy",] %>% na.exclude() %>% 
  group_by(i, Gender, cond, Mouth) %>% 
  count(sub, i, cond, Gender, Mouth) %>% dataCheck_addZero(8, i = TRUE) 
  

x2$cond <- paste(x2$Gender, x2$cond, x2$Mouth, sep = "\n")
x2_happy <- x2[, colnames(x2) != "Mouth" & colnames(x2) != "Gender"]

x2 <- df_MD_edt
x2 <- x2[x2$responseSample == "Angry",] %>% na.exclude() %>% 
  group_by(i, Gender, cond, Mouth) %>% 
  count(sub, i, cond, Gender, Mouth) %>% dataCheck_addZero(8, i = TRUE)

x2$cond <- paste(x2$Gender, x2$cond, x2$Mouth, sep = "\n")
x2_angry <- x2[, colnames(x2) != "Mouth" & colnames(x2) != "Gender"]
rm(x2)

df_list_MD <- merge(x2_angry, x2_happy, by =c("i", "cond", "sub")) 
df_list_MD <- df_list_MD[order(df_list_MD$i,df_list_MD$cond, df_list_MD$sub),]

df_list_MD$n_sum <- rowSums(df_list_MD[,c("n.x", "n.y")])

df_list_MD$prop <- df_list_MD$n.y / df_list_MD$n_sum
df_edt_MD_alpha <- df_list_MD
df_list_MD <- df_list_MD %>% 
  group_by(i, cond) %>% 
  summarise_at(vars(prop), list(name = mean))


##real responses##
df_RR <- sampled_full$data
x3 <- data.frame()

for(x in 1:53){
  y <- df_RR[["edt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3 <- x3[x3$response == "Happy",] %>% na.exclude() %>% 
  group_by( Gender, cond, Mouth) %>% 
  count(sub, cond, Gender, Mouth) %>% dataCheck_addZero(8)

x3$cond <- paste(x3$Gender, x3$cond, x3$Mouth, sep = "\n")
x3_happy <- x3[, colnames(x3) != "Mouth" & colnames(x3) != "Gender"]


x3 <- data.frame()

for(x in 1:53){
  y <- df_RR[["edt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3 <- x3[x3$response == "Angry",] %>% na.exclude() %>% 
  group_by( Gender, cond, Mouth) %>% 
  count(sub, cond, Gender, Mouth) %>% dataCheck_addZero(8)

x3$cond <- paste(x3$Gender, x3$cond, x3$Mouth, sep = "\n")
x3_angry <- x3[, colnames(x3) != "Mouth" & colnames(x3) != "Gender"]
rm(x3)

df_list_RD <- merge(x3_angry, x3_happy, by =c("cond", "sub")) 
df_list_RD <- df_list_RD[order( df_list_RD$cond, df_list_RD$sub),]

df_list_RD$n_sum <- rowSums(df_list_RD[,c("n.x", "n.y")])

df_list_RD$prop <- df_list_RD$n.y / df_list_RD$n_sum
df_edt_RD_alpha <- df_list_RD
df_list_RD <- df_list_RD %>% 
  group_by(cond) %>%
  summarise_at(vars(prop), list(name = mean))

print(ggplot()+
          geom_col(df_list_RD, 
                   mapping = aes(x = cond, y = name, fill = "Real responses"), 
                   alpha = 0.4, width = 0.5)+
          geom_jitter(df_list,
                      mapping = aes(x = cond, y = name, color = "Original fit"), 
                      alpha = 0.35, width = 0.15, size = 1.5, shape = 17, height = 0)+
          geom_point(df_list_MD,
                     mapping = aes(x = cond, y = name, color = "Missing data fit"),
                     alpha = 0.4, size = 2)+
          scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
          scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                   "Missing data fit" = "#FF6666"))+
          theme_classic()+
          theme(legend.key.width = unit(0.3, "cm"),
                legend.text = element_text(size = 10))+
          
          labs(title = "EDT - Generated response mean distributions vs real response mean",
               subtitle = "Response: Happy",
               x = "Condition",
               y = "Mean"))    




"LDT"
#original fit
ldt_word <- df_ldt[df_ldt$responseSample == "word",] %>% na.exclude() %>% 
  group_by(i, cond) %>% count(sub, i, cond) %>% dataCheck_addZero(4, i = TRUE) 


ldt_nonword <- df_ldt[df_ldt$responseSample == "nonword",] %>% na.exclude() %>% 
  group_by(i, cond) %>% count(sub, i, cond) %>% dataCheck_addZero(4, i = TRUE) 

df_list <- merge(ldt_nonword,ldt_word, by =c("i", "cond", "sub")) 
df_list <- df_list[order(df_list$i, df_list$cond, df_list$sub),]

df_list$n_sum <- rowSums(df_list[,c("n.x", "n.y")])

df_list$prop <- df_list$n.y / df_list$n_sum
df_ldt_alpha <- df_list
df_list <- df_list %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))
rm(ldt_word, ldt_nonword)

#MD fit
x2_word <- df_MD_ldt[df_MD_ldt$responseSample == "word",] %>% 
  na.exclude() %>% group_by(i,cond) %>% 
  count(sub, i, cond) %>% dataCheck_addZero(4, TRUE)

x2_nonword <- df_MD_ldt[df_MD_ldt$responseSample == "nonword",] %>% 
  na.exclude() %>% group_by(i,cond) %>% 
  count(sub, i, cond) %>% dataCheck_addZero(4, TRUE)

df_list_MD <- merge(x2_nonword,x2_word, by =c("i", "cond", "sub")) 
df_list_MD <- df_list_MD[order(df_list_MD$i, df_list_MD$cond, df_list_MD$sub),]

df_list_MD$n_sum <- rowSums(df_list_MD[,c("n.x", "n.y")])

df_list_MD$prop <- df_list_MD$n.y / df_list_MD$n_sum
df_ldt_alpha_MD <- df_list_MD
df_list_MD <- df_list_MD %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))
rm(x2_word, x2_nonword)


##real responses##
df_RR <- sampled_full$data
x3 <- data.frame()

for(x in 1:53){
  y <- df_RR[["ldt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3_word <- x3[x3$response == "word",] %>% na.exclude() %>% 
  group_by(cond) %>% 
  count(sub, cond) %>% dataCheck_addZero(4)



x3 <- data.frame()
for(x in 1:53){
  y <- df_RR[["ldt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3_nonword <- x3[x3$response == "non-word",] %>% na.exclude() %>% 
  group_by(cond) %>% 
  count(sub, cond) %>% dataCheck_addZero(4)

df_list_RD <- merge(x3_nonword,x3_word, by =c("cond", "sub")) 
df_list_RD <- df_list_RD[order(df_list_RD$cond, df_list_RD$sub),]

df_list_RD$n_sum <- rowSums(df_list_RD[,c("n.x", "n.y")])

df_list_RD$prop <- df_list_RD$n.y / df_list_RD$n_sum
df_ldt_alpha_RD <- df_list_RD
df_list_RD <- df_list_RD %>% group_by(cond) %>% summarise_at(vars(prop), list(name = mean))
rm(x3_word, x3_nonword)


print(ggplot()+
        geom_col(df_list_RD, 
                 mapping = aes(x = cond, y = name, fill = "Real responses"), 
                 alpha = 0.4, width = 0.5)+
        geom_jitter(df_list,
                    mapping = aes(x = cond, y = name, color = "Original fit"), 
                    alpha = 0.5, width = 0.15, size = 1.5, shape = 17, height = 0)+
        geom_point(df_list_MD,
                   mapping = aes(x = cond, y = name, color = "Missing data fit"),
                   alpha = 0.4, size = 2)+
        scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
        scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                 "Missing data fit" = "#FF6666"))+
        theme_classic()+
        theme(legend.key.width = unit(0.3, "cm"),
              legend.text = element_text(size = 10))+
        
        labs(title = "LDT - Generated response mean distributions vs real response mean",
             subtitle = "Response: Word",
             x = "Condition",
             y = "Mean")) 



"nBack"
#original fit
nback_target <- df_nback[df_nback$responseSample == "target",] %>% group_by(i, cond) %>% 
  count(sub, i, cond)  %>% na.exclude() %>% dataCheck_addZero(3, TRUE)

nback_nontarget <- df_nback[df_nback$responseSample == "nontarget",] %>% group_by(i, cond) %>% 
  count(sub, i, cond)  %>% na.exclude() %>% dataCheck_addZero(3, TRUE)

df_list <- merge(nback_nontarget, nback_target, by =c("i", "cond","sub")) 
df_list <- df_list[order(df_list$cond, df_list$sub),]

df_list$n_sum <- rowSums(df_list[,c("n.x", "n.y")])

df_list$prop <- df_list$n.y / df_list$n_sum
df_nback_alpha <- df_list
df_list <- df_list %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))
rm(nback_nontarget, nback_target)


#data removed fit
x2_target <- df_MD_nback[df_MD_nback$responseSample == "target",] %>% na.exclude() %>% group_by(i,cond) %>% 
  count(sub, i, cond) %>% dataCheck_addZero(3, TRUE)

x2_nontarget <- df_MD_nback[df_MD_nback$responseSample == "nontarget",] %>% na.exclude() %>% group_by(i,cond) %>% 
  count(sub, i, cond) %>% dataCheck_addZero(3, TRUE)

df_list_MD <- merge(x2_nontarget,x2_target, by =c("i", "cond", "sub")) 
df_list_MD <- df_list_MD[order(df_list_MD$cond, df_list_MD$sub),]

df_list_MD$n_sum <- rowSums(df_list_MD[,c("n.x", "n.y")])

df_list_MD$prop <- df_list_MD$n.y / df_list_MD$n_sum
df_nback_alpha_MD <- df_list_MD
df_list_MD <- df_list_MD %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))
rm(x2_nontarget, x2_target)


##real responses##
df_RR <- sampled_full$data
x3 <- data.frame()

for(x in 1:53){
  y <- df_RR[["nback"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3_target <- x3[x3$response == "target",] %>% na.exclude() %>% 
  group_by(cond) %>% 
  count(sub, cond) %>% dataCheck_addZero(3)


x3 <- data.frame()
for(x in 1:53){
  y <- df_RR[["nback"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3_nontarget <- x3[x3$response == "nontarget",] %>% na.exclude() %>% 
  group_by(cond) %>% 
  count(sub, cond) %>% dataCheck_addZero(3)


df_list_RD <- merge(x3_nontarget,x3_target, by =c("cond", "sub")) 
df_list_RD <- df_list_RD[order(df_list_RD$cond, df_list_RD$sub),]

df_list_RD$n_sum <- rowSums(df_list_RD[,c("n.x", "n.y")])

df_list_RD$prop <- df_list_RD$n.y / df_list_RD$n_sum
df_nback_alpha_RD <- df_list_RD
df_list_RD <- df_list_RD %>% group_by(cond) %>% summarise_at(vars(prop), list(name = mean))

print(ggplot()+
        geom_col(df_list_RD, 
                 mapping = aes(x = cond, y = name, fill = "Real responses"), 
                 alpha = 0.4, width = 0.5)+
        geom_jitter(df_list,
                    mapping = aes(x = cond, y = name, color = "Original fit"), 
                    alpha = 0.5, width = 0.15, size = 1.5, shape = 17)+
        geom_point(df_list_MD,
                   mapping = aes(x = cond, y = name, color = "Missing data fit"),
                   alpha = 0.4, size = 2)+
        scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
        scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                 "Missing data fit" = "#FF6666"))+
        theme_classic()+
        theme(legend.key.width = unit(0.3, "cm"),
              legend.text = element_text(size = 10))+
        
        labs(title = "N-back - Generated response mean distributions vs real response mean",
             subtitle = "Response: Target",
             x = "Condition",
             y = "Mean")) 



"PDT"
##original fit##
pdt <- df_pdt[df_pdt$responseSample == "orange",]%>% na.exclude() %>% group_by( i, 
  cond, difficulty) %>% count(sub, i, cond, difficulty) %>% dataCheck_addZero(6, TRUE)
pdt$cond <- paste(pdt$cond, pdt$difficulty, sep = "-")#
pdt_orange <- pdt[, colnames(pdt) != "difficulty"]

pdt <- df_pdt[df_pdt$responseSample == "blue",] %>% na.exclude() %>% 
  group_by(i, cond, difficulty) %>% count(sub, i, cond, difficulty) %>% dataCheck_addZero(6,TRUE)
pdt$cond <- paste(pdt$cond, pdt$difficulty, sep = "-")#
pdt_blue <- pdt[, colnames(pdt) != "difficulty"]

df_list <- merge(pdt_blue,pdt_orange, by =c("i", "cond", "sub")) 
df_list <- df_list[order(df_list$cond, df_list$sub),]

df_list$n_sum <- rowSums(df_list[,c("n.x", "n.y")])

df_list$prop <- df_list$n.y / df_list$n_sum
df_pdt_alpha <- df_list
df_list <- df_list %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))


##Missing data fit##
x2 <- df_MD_pdt
x2 <- x2[x2$responseSample == "orange",] %>% na.exclude() %>% group_by(i,cond, difficulty) %>% 
  count(sub, i, cond, difficulty) %>% dataCheck_addZero(6, TRUE) 
x2$cond <- paste(x2$cond, x2$difficulty, sep = "-")
x2_orange <- x2[,colnames(x2) != "difficulty"]

x2 <- df_MD_pdt
x2 <- x2[x2$responseSample == "blue",] %>% na.exclude() %>% group_by(i,cond, difficulty) %>% 
  count(sub, i, cond, difficulty) %>% dataCheck_addZero(6, TRUE) 
x2$cond <- paste(x2$cond, x2$difficulty, sep = "-")
x2_blue <- x2[,colnames(x2) != "difficulty"]

df_list_MD <- merge(x2_blue,x2_orange, by =c("i", "cond", "sub")) 
df_list_MD <- df_list_MD[order(df_list_MD$cond, df_list_MD$sub),]

df_list_MD$n_sum <- rowSums(df_list_MD[,c("n.x", "n.y")])

df_list_MD$prop <- df_list_MD$n.y / df_list_MD$n_sum
df_pdt_alpha_MD <- df_list_MD
df_list_MD <- df_list_MD %>% group_by(i, cond) %>% summarise_at(vars(prop), list(name = mean))

#real responses##
df_RR <- sampled_full$data
x3 <- data.frame()
for(x in 1:53){
  y <- df_RR[["pdt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3 <- x3[x3$response == "orange",] %>% na.exclude() %>% group_by(cond, difficulty) %>% 
  count(sub, cond, difficulty) %>% dataCheck_addZero(6)
x3$cond <- paste(x3$cond, x3$difficulty, sep = "-")
x3_orange <- x3[,colnames(x3) != "difficulty"]


x3 <- data.frame()
for(x in 1:53){
  y <- df_RR[["pdt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3 <- x3[x3$response == "blue",] %>% na.exclude() %>% group_by(cond, difficulty) %>% 
  count(sub, cond, difficulty) %>% dataCheck_addZero(6)
x3$cond <- paste(x3$cond, x3$difficulty, sep = "-")
x3_blue <- x3[,colnames(x3) != "difficulty"]

df_list_RD <- merge(x3_blue,x3_orange, by =c("cond", "sub")) 
df_list_RD <- df_list_RD[order(df_list_RD$cond, df_list_RD$sub),]

df_list_RD$n_sum <- rowSums(df_list_RD[,c("n.x", "n.y")])

df_list_RD$prop <- df_list_RD$n.y / df_list_RD$n_sum
df_pdt_alpha_RD <- df_list_RD
df_list_RD <- df_list_RD %>% group_by(cond) %>% summarise_at(vars(prop), list(name = mean))


print(ggplot()+
        geom_col(df_list_RD, 
                 mapping = aes(x = cond, y = name, fill = "Real responses"), 
                 alpha = 0.4, width = 0.5)+
        geom_jitter(df_list,
                    mapping = aes(x = cond, y = name, color = "Original fit"), 
                    alpha = 0.5, width = 0.15, size = 1.5, shape = 17)+
        geom_point(df_list_MD,
                   mapping = aes(x = cond, y = name, color = "Missing data fit"),
                   alpha = 0.4, size = 2)+
        scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
        scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                 "Missing data fit" = "#FF6666"))+
        theme_classic()+
        theme(legend.key.width = unit(0.3, "cm"),
              legend.text = element_text(size = 10))+
        
        labs(title = "PDT - Generated response mean distributions vs real response mean",
             subtitle = "Response: Orange",
             x = "Condition",
             y = "Mean")) 

rm(df_list, df_list_MD, df_list_RD)

