load(file="Data/params_out_2steps.Rdata")
burn.in=dim(params.out)[1]
library(matrixStats)

#Parameter values
params.in <- data.frame(expand.grid(pn.in = c(0.3,0.5,0.8),pa.in = c(0,0.1,0.5,0.8), pr.in = c(0.001,0.01,0.05,0.1)))
dim(params.out)


#Plot number of affiliates

jpeg("Plots/Metrics_2steps/affiliates_50rep_1to12_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,1400),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/Metrics_2steps/affiliates_50rep_13to24_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,1400),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/affiliates_50rep_25to36_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,1400),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/affiliates_50rep_37to48_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,1400),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

#Plot number of avoidances

jpeg("Plots/Metrics_2steps/avoidances_50rep_1to12_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,1400),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/Metrics_2steps/avoidances_50rep_13to24_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,1400),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/avoidances_50rep_25to36_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,1400),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/avoidances_50rep_37to48_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,1400),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

#Plot modularity

jpeg("Plots/Metrics_2steps/modularity_50rep_1to12_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/Metrics_2steps/modularity_50rep_13to24_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/modularity_50rep_25to36_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/modularity_50rep_37to48_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()



#Plot clustering coeff

jpeg("Plots/Metrics_2steps/clustering_50rep_1to12_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/Metrics_2steps/clustering_50rep_13to24_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/clustering_50rep_25to36_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/Metrics_2steps/clustering_50rep_37to48_2steps.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()
