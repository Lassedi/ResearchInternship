setwd(r"(D:\Psychology\Master\Research Internship\Data)")
load("SDT_full_noCon_1.Rdata")
load("SDT_full_noCon.Rdata")

cov<-apply(sampled$samples$theta_var[,,sampled$samples$stage=="sample"],1:2, mean)
colnames(cov)<-sampled$par_names
rownames(cov)<-sampled$par_names
cor<-cov2cor(cov) #correlation matrix
library(corrplot)
corrplot(cor, method="circle", type = "lower", title = "Parameter Correlations", tl.col = "black",mar=c(0,0,2,0))

