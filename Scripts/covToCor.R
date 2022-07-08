###Script used to plot correlation strengths###
setwd(r"(D:\Psychology\Master\Research Internship\Data)")

setwd(r"(D:\Psychology\Master\Research Internship\Data\2ndrun)")

load("SDT_full_noCon_1.Rdata")
load("SDT_full_noCon.Rdata")
load("SDT_prop_.Rdata")
load("SDT_full_.Rdata")

destination <- r"(D:\Psychology\Master\Research Internship\Plots\Trace_Violine_Cor_V2\prop_corplot.pdf)"
destination <- r"(D:\Psychology\Master\Research Internship\Plots\Trace_Violine_Cor_V2\full_corplot.pdf)"


cov<-apply(sampled$samples$theta_var[,,sampled$samples$stage=="sample"],1:2, mean)
colnames(cov)<-sampled$par_names
rownames(cov)<-sampled$par_names
cor<-cov2cor(cov) #correlation matrix
library(corrplot)
pdf(file = destination)
corrplot(cor, method="circle", type = "lower", title = "Parameter Correlations", tl.col = "black",mar=c(0,0,2,0))
dev.off()
