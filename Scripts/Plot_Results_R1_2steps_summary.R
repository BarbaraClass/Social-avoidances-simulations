library(matrixStats)
library(vioplot)

load(file="Data/params_out_2steps.Rdata")
dim(params.out)
burn.in=dim(params.out)[1]
cols_Pa=rep(gray.colors(n=4),each=3)
cols_Pa=cols_Pa[12:1]
pch_Pn=rep(c(21,24,22),4)

#Parameter values
params.in <- data.frame(expand.grid(pn.in = c(0.3,0.5,0.8),pa.in = c(0,0.1,0.5,0.8), pr.in = c(0.001,0.01,0.05,0.1)))
dim(params.out)

#Plot number of affiliates

Npos=params.out[burn.in,"n_pos",,]

jpeg("Plots/Metrics_2steps/Affiliates_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(Npos[,1:12],ylim=c(0,2000), main=list("Pr=0.001",cex=2,font=1),ylab=list("N.affiliates",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=2000, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Npos[,13:24],ylim=c(0,2000), main=list("Pr=0.01",cex=2,font=1),ylab=list("N.affiliates",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=2000, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Npos[,25:36],ylim=c(0,2000), main=list("Pr=0.05",cex=2,font=1),ylab=list("N.affiliates",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=2000, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Npos[,37:48],ylim=c(0,2000), main=list("Pr=0.1",cex=2,font=1),ylab=list("N.affiliates",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=2000, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()

#Plot number of avoidances

Nneg=params.out[burn.in,"n_neg",,]

jpeg("Plots/Metrics_2steps/Avoidances_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(Nneg[,1:12],ylim=c(0,1600), main=list("Pr=0.001",cex=2,font=1),ylab=list("N.avoidances",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1600, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Nneg[,13:24],ylim=c(0,1600), main=list("Pr=0.01",cex=2,font=1),ylab=list("N.avoidances",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1600, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Nneg[,25:36],ylim=c(0,1600), main=list("Pr=0.05",cex=2,font=1),ylab=list("N.avoidances",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1600, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Nneg[,37:48],ylim=c(0,1600), main=list("Pr=0.1",cex=2,font=1),ylab=list("N.avoidances",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1600, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()

#Plot modularity

Mod=params.out[burn.in,"modularity",,]

jpeg("Plots/Metrics_2steps/Modularity_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(Mod[,1:12],ylim=c(0,1), main=list("Pr=0.001",cex=2,font=1),ylab=list("Modularity",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Mod[,13:24],ylim=c(0,1), main=list("Pr=0.01",cex=2,font=1),ylab=list("Modularity",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Mod[,25:36],ylim=c(0,1), main=list("Pr=0.05",cex=2,font=1),ylab=list("Modularity",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Mod[,37:48],ylim=c(0,1), main=list("Pr=0.1",cex=2,font=1),ylab=list("Modularity",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()

#Plot clustering coeff

CC=params.out[burn.in,"cc_pos",,]

jpeg("Plots/Metrics_2steps/Clustering_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(CC[,1:12],ylim=c(0,0.8), main=list("Pr=0.001",cex=2,font=1),ylab=list("Clustering coeff",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=0.8, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(CC[,13:24],ylim=c(0,0.8), main=list("Pr=0.01",cex=2,font=1),ylab=list("Clustering coeff",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=0.8, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(CC[,25:36],ylim=c(0,0.8), main=list("Pr=0.05",cex=2,font=1),ylab=list("Clustering coeff",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=0.8, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(CC[,37:48],ylim=c(0,0.8), main=list("Pr=0.1",cex=2,font=1),ylab=list("Clustering coeff",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=0.8, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()

#number of components

Ncomps=params.out[burn.in,6,,]

jpeg("Plots/Metrics_2steps/NComp_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(Ncomps[,1:12],ylim=c(1,17), main=list("Pr=0.001",cex=2,font=1),ylab=list("N.components",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=17, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Ncomps[,13:24],ylim=c(1,17), main=list("Pr=0.01",cex=2,font=1),ylab=list("N.components",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=17, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Ncomps[,25:36],ylim=c(1,17), main=list("Pr=0.05",cex=2,font=1),ylab=list("N.components",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=17, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(Ncomps[,37:48],ylim=c(1,17), main=list("Pr=0.1",cex=2,font=1),ylab=list("N.components",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=17, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()

#Plot variation of individual degree

load(file="Data/Degree_2steps.Rdata")
degrees_sd=as.data.frame(apply(degree.mat, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean

jpeg("Plots/Metrics_2steps/CVDegree_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(degrees_CV[,1:12],ylim=c(0,1.1), main=list("Pr=0.001",cex=2,font=1),ylab=list("CV degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1.1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(degrees_CV[,13:24],ylim=c(0,1.1), main=list("Pr=0.01",cex=2,font=1),ylab=list("CV degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1.1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(degrees_CV[,25:36],ylim=c(0,1.1), main=list("Pr=0.05",cex=2,font=1),ylab=list("CV degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1.1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(degrees_CV[,37:48],ylim=c(0,1.1), main=list("Pr=0.1",cex=2,font=1),ylab=list("CV degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=1.1, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()

#Plot degree distribution in each scenario

degrees_stack=apply(degree.mat, 3, as.vector)

jpeg("Plots/Metrics_2steps/Degree_50rep_2steps.jpeg",width=1200,height=1200,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))
vioplot(degrees_stack[,1:12],ylim=c(0,47), main=list("Pr=0.001",cex=2,font=1),ylab=list("Degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=47, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(degrees_stack[,13:24],ylim=c(0,47), main=list("Pr=0.01",cex=2,font=1),ylab=list("Degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=47, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(degrees_stack[,25:36],ylim=c(0,47), main=list("Pr=0.05",cex=2,font=1),ylab=list("Degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=47, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

vioplot(degrees_stack[,37:48],ylim=c(0,47), main=list("Pr=0.1",cex=2,font=1),ylab=list("Degree",cex=2),xlab=list("Pn",cex=2),xaxt="n",col=cols_Pa,lineCol = cols_Pa, rectCol = "black",border=cols_Pa,pchMed =pch_Pn,colMed="black",colMed2="black" ,cex=1.5,cex.axis=2)
axis(1, at= 1:12, labels=rep(c(0.3,0.5,0.8),4),tick=FALSE,cex.axis=1.5)
arrows(c(3.5,6.5,9.5),-1000,c(3.5,6.5,9.5),2500,lty=2,length=0)
text(x=c(2,5,8,11),y=47, labels=c('Pa=0',"Pa=0.1","Pa=0.5","Pa=0.8"),cex=2)

dev.off()
