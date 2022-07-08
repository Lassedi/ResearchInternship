###Script used to explore dataset for participants which did not complete the task correctly###
df_RR <- sampled_full$data
x3 <- data.frame()

for(x in 1:56){
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

for(x in 1:56){
  y <- df_RR[["edt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3 <- x3[x3$response == "Angry",] %>% na.exclude() %>% 
  group_by( Gender, cond, Mouth) %>% 
  count(sub, cond, Gender, Mouth) %>%
  dataCheck_addZero(8)

x3$cond <- paste(x3$Gender, x3$cond, x3$Mouth, sep = "\n")
x3_angry <- x3[, colnames(x3) != "Mouth" & colnames(x3) != "Gender"]
rm(x3)

df_list_RD <- merge(x3_angry, x3_happy, by =c("cond", "sub")) 
df_list_RD <- df_list_RD[order( df_list_RD$cond, df_list_RD$sub),]

df_list_RD$n_sum <- rowSums(df_list_RD[,c("n.x", "n.y")])

df_list_RD$prop <- df_list_RD$n.y / df_list_RD$n_sum

for (x in 1:nrow(df_list_RD)){
  y <- df_list_RD
  if (y$n_sum[[x]] > 22){
    print(y$sub[[x]])
  }
  if (length(y[y$n.x == 0 & y$sub == x,]$n.x == 0) == 8 | length(y[y$n.y == 0 & y$sub == x,]$n.y == 0) == 8){
    print(y[y$n.x == 0 & y$sub == x,]$n.x == 0)
    print(y$sub[[x]])
  }
}


"ldt"
df_RR <- sampled_full$data
x3 <- data.frame()

for(x in 1:56){
  y <- df_RR[["ldt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3_word <- x3[x3$response == "word",] %>% na.exclude() %>% 
  group_by(cond) %>% 
  count(sub, cond) %>% dataCheck_addZero(4)



x3 <- data.frame()
for(x in 1:56){
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

for (x in 1:nrow(df_list_RD)){
  y <- df_list_RD
  if (y$n_sum[[x]] > 120 & y$cond[[x]] == "NW"){
    print(y$sub[[x]])
  }
  if (y$n_sum[[x]] > 40 & y$cond[[x]] != "NW"){
    print(y$sub[[x]])
  }
  if (length(y[y$n.x == 0 & y$sub == x,]$n.x == 0) == 4 | length(y[y$n.y == 0 & y$sub == x,]$n.y == 0) == 4){
    print(y[y$n.x == 0 & y$sub == x,]$n.x == 0)
    print(y$sub[[x]])
  }
}

"nback"
df_RR <- sampled_full$data
x3 <- data.frame()

for(x in 1:56){
  y <- df_RR[["nback"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3_target <- x3[x3$response == "target",] %>% na.exclude() %>% 
  group_by(cond) %>% 
  count(sub, cond) %>% dataCheck_addZero(3)


x3 <- data.frame()
for(x in 1:56){
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

for (x in 1:nrow(df_list_RD)){
  y <- df_list_RD
  if (y$n_sum[[x]] > 48 & y$cond[[x]] == "nonlure"){
    print(y$sub[[x]])
  }
  if (y$n_sum[[x]] > 16 & y$cond[[x]] != "nonlure"){
    print(y$sub[[x]])
  }
  if (length(y[y$n.x == 0 & y$sub == x,]$n.x == 0) == 3 | length(y[y$n.y == 0 & y$sub == x,]$n.y == 0) == 3){
    print(y[y$n.x == 0 & y$sub == x,]$n.x == 0)
    print(y$sub[[x]])
  }
}


"pdt"
df_RR <- sampled_full$data
x3 <- data.frame()
for(x in 1:56){
  y <- df_RR[["pdt"]][[x]]
  y$sub <- x
  x3 <- rbind(x3, y)
}

x3 <- x3[x3$response == "orange",] %>% na.exclude() %>% group_by(cond, difficulty) %>% 
  count(sub, cond, difficulty) %>% dataCheck_addZero(6)
x3$cond <- paste(x3$cond, x3$difficulty, sep = "-")
x3_orange <- x3[,colnames(x3) != "difficulty"]


x3 <- data.frame()
for(x in 1:56){
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


for (x in 1:nrow(df_list_RD)){
  y <- df_list_RD
  if (y$n_sum[[x]] > 40){
    print(y$sub[[x]])
  }
  if (length(y[y$n.x == 0 & y$sub == x,]$n.x == 0) == 6 | length(y[y$n.y == 0 & y$sub == x,]$n.y == 0) == 6){
    print(y[y$n.x == 0 & y$sub == x,]$n.x == 0)
    print(y$sub[[x]])
  }
}


