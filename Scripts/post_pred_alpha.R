###Plotting script for individual level posterior predictive plots###
"EDT"
list <- t(as.data.frame(MD_list)) 
row.names(list) <-  NULL
list <- list[list[,2] == 2,]

for (sub in list[,1]){
  cat(sub, " ")
  print(ggplot()+
          geom_col(df_edt_RD_alpha[df_edt_RD_alpha$sub == sub,], 
                   mapping = aes(x = cond, y = prop, fill = "Real responses"), 
                   alpha = 0.4, width = 0.5)+
          geom_jitter(df_edt_alpha[df_edt_alpha$sub == sub,],
                      mapping = aes(x = cond, y = prop, color = "Original fit"), 
                      alpha = 0.35, width = 0.15, size = 1.5, shape = 17, height = 0)+
          geom_point(df_edt_MD_alpha[df_edt_MD_alpha$sub == sub,],
                      mapping = aes(x = cond, y = prop, color = "Missing data fit"),
                      alpha = 0.4, size = 2)+
          scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
          scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                   "Missing data fit" = "#FF6666"))+
          theme_classic()+
          theme(legend.key.width = unit(0.3, "cm"),
                legend.text = element_text(size = 10))+
          
          labs(title = "EDT - Generated response mean distributions vs real response mean",
               subtitle = paste("Response: Happy", "\nParticipant:", sub),
               x = "Condition",
               y = "Count"))    
}


"PDT"
list <- t(as.data.frame(MD_list)) 
row.names(list) <-  NULL
list <- list[list[,2] == 4,]

for (sub in list[,1]){
  cat(sub, " ")
  print(ggplot()+
          geom_col(df_pdt_alpha_RD[df_pdt_alpha_RD$sub == sub,], 
                   mapping = aes(x = cond, y = prop, fill = "Real responses"), 
                   alpha = 0.4, width = 0.5)+
          geom_jitter(df_pdt_alpha[df_pdt_alpha$sub == sub,],
                      mapping = aes(x = cond, y = prop, color = "Original fit"), 
                      alpha = 0.5, width = 0.15, size = 1.5, shape = 17, height = 0)+
          geom_point(df_pdt_alpha_MD[df_pdt_alpha_MD$sub == sub,],
                     mapping = aes(x = cond, y = prop, color = "Missing data fit"),
                     alpha = 0.4, size = 2)+
          scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
          scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                   "Missing data fit" = "#FF6666"))+
          theme_classic()+
          theme(legend.key.width = unit(0.3, "cm"),
                legend.text = element_text(size = 10))+
          
          labs(title = "PDT - Generated response mean distributions vs real response mean",
               subtitle = paste("Response: Orange", "\nParticipant:", sub),
               x = "Condition",
               y = "Count"))    
}


"LDT"
list <- t(as.data.frame(MD_list)) 
row.names(list) <-  NULL
list <- list[list[,2] == 3,]

for (sub in list[,1]){
  cat(sub, " ")
  print(ggplot()+
          geom_col(df_ldt_alpha_RD[df_ldt_alpha_RD$sub == sub,], 
                   mapping = aes(x = cond, y = prop, fill = "Real responses"), 
                   alpha = 0.4, width = 0.5)+
          geom_jitter(df_ldt_alpha[df_ldt_alpha$sub == sub,],
                      mapping = aes(x = cond, y = prop, color = "Original fit"), 
                      alpha = 0.5, width = 0.15, size = 1.5, shape = 17, height = 0)+
          geom_point(df_ldt_alpha_MD[df_ldt_alpha_MD$sub == sub,],
                     mapping = aes(x = cond, y = prop, color = "Missing data fit"),
                     alpha = 0.4, size = 2)+
          scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
          scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                   "Missing data fit" = "#FF6666"))+
          theme_classic()+
          theme(legend.key.width = unit(0.3, "cm"),
                legend.text = element_text(size = 10))+
          
          labs(title = "LDT - Generated response mean distributions vs real response mean",
               subtitle = paste("Response: Word", "\nParticipant:", sub),
               x = "Condition",
               y = "Count"))    
}


"nBack"
list <- t(as.data.frame(MD_list)) 
row.names(list) <-  NULL
list <- list[list[,2] == 5,]

for (sub in list[,1]){
  cat(sub, " ")
  print(ggplot()+
          geom_col(df_nback_alpha_RD[df_nback_alpha_RD$sub == sub,], 
                   mapping = aes(x = cond, y = prop, fill = "Real responses"), 
                   alpha = 0.4, width = 0.5)+
          geom_jitter(df_nback_alpha[df_nback_alpha$sub == sub,],
                      mapping = aes(x = cond, y = prop, color = "Original fit"), 
                      alpha = 0.35, width = 0.15, size = 1.5, shape = 17, height = 0)+
          geom_point(df_nback_alpha_MD[df_nback_alpha_MD$sub == sub,],
                     mapping = aes(x = cond, y = prop, color = "Missing data fit"),
                     alpha = 0.4, size = 2)+
          scale_fill_manual(name = "", values = c("Real responses" = "#003366"))+
          scale_color_manual(name = "", values = c("Original fit" = "#003300", 
                                                   "Missing data fit" = "#FF6666"))+
          theme_classic()+
          theme(legend.key.width = unit(0.3, "cm"),
                legend.text = element_text(size = 10))+
          
          labs(title = "N-back - Generated response mean distributions vs real response mean",
               subtitle = paste("Response: Target", "\nParticipant:", sub),
               x = "Condition",
               y = "Count"))    
}


