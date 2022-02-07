load(file="Data/params_out.Rdata")
burn.in=dim(params.out)[1]
library(matrixStats)

#Parameter values
params.in <- data.frame(expand.grid(pn.in = c(0.3,0.5,0.8),pa.in = c(0,0.1,0.5,0.8), pr.in = c(0.001,0.01,0.05,0.1)))
dim(params.out)

#Plot modularity

jpeg("Plots/modularity_50rep_1to12.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/modularity_50rep_13to24.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/modularity_50rep_25to36.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/modularity_50rep_37to48.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$modularity,type='l',ylim=c(0,1),ylab='Modularity',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()


#Plot number of affiliates

jpeg("Plots/affiliates_50rep_1to12.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,900),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/affiliates_50rep_13to24.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,900),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/affiliates_50rep_25to36.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,900),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/affiliates_50rep_37to48.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_pos,type='l',ylim=c(0,900),ylab='N.affiliates',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

#Plot number of avoidances

jpeg("Plots/avoidances_50rep_1to12.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,700),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/avoidances_50rep_13to24.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,700),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/avoidances_50rep_25to36.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,700),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/avoidances_50rep_37to48.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$n_neg,type='l',ylim=c(0,700),ylab='N.avoidances',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

#Plot clustering coeff

jpeg("Plots/clustering_50rep_1to12.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 1:12){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}

dev.off()

jpeg("Plots/clustering_50rep_13to24.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 13:24){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/clustering_50rep_25to36.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 25:36){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()

jpeg("Plots/clustering_50rep_37to48.jpeg",width=600,height=1200,quality=100)
par(mfrow=c(4,3))
for (i in 37:48){
  test1=params.out[,,,i]
  test2=as.data.frame(apply(test1, c(1,2), median))
  plot(test2$cc_pos,type='l',ylim=c(0,0.7),ylab='Clustering coeff',xlab="time steps", cex.lab=1.3, cex.axis=1.3,
       main=paste0("Pn=",as.character(params.in[i,'pn.in'])," Pa=",as.character(params.in[i,'pa.in'])," Pr=",as.character(params.in[i,'pr.in'])))
}
dev.off()



#Look at the number of components at the last time step= average over 50 replicates
dim(params.out)
test1=params.out[200,6,,]
test2=as.data.frame(apply(test1, 2, mean))

test3=cbind(params.in,test2)
names(test3)[4]="mean.n.comp"

test3[test3$pn.in==0.3 & test3$pr.in==0.01,]
test3[test3$pn.in==0.5 & test3$pr.in==0.01,]
test3[test3$pn.in==0.8 & test3$pr.in==0.01,]