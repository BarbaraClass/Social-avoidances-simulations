params.in <- data.frame(expand.grid(pn.in = c(0.3,0.4,0.5,0.6,0.7,0.8),
                                    pa.in = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), 
                                    pr.in = c(0.001,0.01,0.05)))
params.in$scenario=1:nrow(params.in)

library(matrixStats)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
cols=brewer.pal(9,"Blues")

#Load results file
load(file="Data/params_out_1step.Rdata")
dim(params.out)
burn.in=dim(params.out)[1]

#Plot modularity

jpeg("Plots/Metrics_1step/Modularity_lines_Pn.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

modularity=t(params.out[burn.in,"modularity",,])
dim(modularity)
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.001 & modularity$pa.in %in% c(0,0.3,0.6,0.8)) ,]
modularity=modularity[order(modularity$pa.in,modularity$pn.in),]

plot(modularity$pn.in[1:6],modularity$modularity[1:6], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pn", ylab="Modularity",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pn.in[7:12],modularity$modularity[7:12], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[13:18],modularity$modularity[13:18], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[19:24],modularity$modularity[19:24], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")



modularity=t(params.out[burn.in,"modularity",,])
dim(modularity)
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01 & modularity$pa.in %in% c(0,0.3,0.6,0.8)) ,]
modularity=modularity[order(modularity$pa.in,modularity$pn.in),]

plot(modularity$pn.in[1:6],modularity$modularity[1:6], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pn", ylab="Modularity",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pn.in[7:12],modularity$modularity[7:12], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[13:18],modularity$modularity[13:18], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[19:24],modularity$modularity[19:24], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


modularity=t(params.out[burn.in,"modularity",,])
dim(modularity)
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.05 & modularity$pa.in %in% c(0,0.3,0.6,0.8)) ,]
modularity=modularity[order(modularity$pa.in,modularity$pn.in),]

plot(modularity$pn.in[1:6],modularity$modularity[1:6], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pn", ylab="Modularity",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pn.in[7:12],modularity$modularity[7:12], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[13:18],modularity$modularity[13:18], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[19:24],modularity$modularity[19:24], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot Number of Affiliates

jpeg("Plots/Metrics_1step/Naffiliates_lines_Pn.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

Npos=t(params.out[burn.in,"n_pos",,])
Npos=rowMeans(Npos,na.rm=T)
Npos=data.frame(Npos,params.in)
Npos=Npos[which(Npos$pr.in==0.001 & Npos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
Npos=Npos[order(Npos$pa.in,Npos$pn.in),]

plot(Npos$pn.in[1:6],Npos$Npos[1:6], ylim=c(0,1200),col=cols[2],lwd=2,
     xlab="Pn", ylab="Number of affiliates",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Npos$pn.in[7:12],Npos$Npos[7:12], ylim=c(0,1200),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pn.in[13:18],Npos$Npos[13:18], ylim=c(0,1200),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pn.in[19:24],Npos$Npos[19:24], ylim=c(0,1200),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


Npos=t(params.out[burn.in,"n_pos",,])
Npos=rowMeans(Npos,na.rm=T)
Npos=data.frame(Npos,params.in)
Npos=Npos[which(Npos$pr.in==0.01 & Npos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
Npos=Npos[order(Npos$pa.in,Npos$pn.in),]

plot(Npos$pn.in[1:6],Npos$Npos[1:6], ylim=c(0,1200),col=cols[2],lwd=2,
     xlab="Pn", ylab="Number of affiliates",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Npos$pn.in[7:12],Npos$Npos[7:12], ylim=c(0,1200),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pn.in[13:18],Npos$Npos[13:18], ylim=c(0,1200),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pn.in[19:24],Npos$Npos[19:24], ylim=c(0,1200),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


Npos=t(params.out[burn.in,"n_pos",,])
Npos=rowMeans(Npos,na.rm=T)
Npos=data.frame(Npos,params.in)
Npos=Npos[which(Npos$pr.in==0.05 & Npos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
Npos=Npos[order(Npos$pa.in,Npos$pn.in),]

plot(Npos$pn.in[1:6],Npos$Npos[1:6], ylim=c(0,1200),col=cols[2],lwd=2,
     xlab="Pn", ylab="Number of affiliates",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Npos$pn.in[7:12],Npos$Npos[7:12], ylim=c(0,1200),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pn.in[13:18],Npos$Npos[13:18], ylim=c(0,1200),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pn.in[19:24],Npos$Npos[19:24], ylim=c(0,1200),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot Number of Avoidances   

jpeg("Plots/Metrics_1step/Navoidances_lines_Pn.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

Nneg=t(params.out[burn.in,"n_neg",,])
Nneg=rowMeans(Nneg,na.rm=T)
Nneg=data.frame(Nneg,params.in)
Nneg=Nneg[which(Nneg$pr.in==0.001 & Nneg$pa.in %in% c(0,0.3,0.6,0.8)) ,]
Nneg=Nneg[order(Nneg$pa.in,Nneg$pn.in),]

plot(Nneg$pn.in[1:6],Nneg$Nneg[1:6], ylim=c(0,1300),col=cols[2],lwd=2,
     xlab="Pn", ylab="Number of avoidances",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Nneg$pn.in[7:12],Nneg$Nneg[7:12], ylim=c(0,1300),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pn.in[13:18],Nneg$Nneg[13:18], ylim=c(0,1300),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pn.in[19:24],Nneg$Nneg[19:24], ylim=c(0,1300),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

Nneg=t(params.out[burn.in,"n_neg",,])
Nneg=rowMeans(Nneg,na.rm=T)
Nneg=data.frame(Nneg,params.in)
Nneg=Nneg[which(Nneg$pr.in==0.01 & Nneg$pa.in %in% c(0,0.3,0.6,0.8)) ,]
Nneg=Nneg[order(Nneg$pa.in,Nneg$pn.in),]

plot(Nneg$pn.in[1:6],Nneg$Nneg[1:6], ylim=c(0,1300),col=cols[2],lwd=2,
     xlab="Pn", ylab="Number of avoidances",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Nneg$pn.in[7:12],Nneg$Nneg[7:12], ylim=c(0,1300),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pn.in[13:18],Nneg$Nneg[13:18], ylim=c(0,1300),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pn.in[19:24],Nneg$Nneg[19:24], ylim=c(0,1300),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

Nneg=t(params.out[burn.in,"n_neg",,])
Nneg=rowMeans(Nneg,na.rm=T)
Nneg=data.frame(Nneg,params.in)
Nneg=Nneg[which(Nneg$pr.in==0.05 & Nneg$pa.in %in% c(0,0.3,0.6,0.8)) ,]
Nneg=Nneg[order(Nneg$pa.in,Nneg$pn.in),]

plot(Nneg$pn.in[1:6],Nneg$Nneg[1:6], ylim=c(0,1300),col=cols[2],lwd=2,
     xlab="Pn", ylab="Number of avoidances",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Nneg$pn.in[7:12],Nneg$Nneg[7:12], ylim=c(0,1300),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pn.in[13:18],Nneg$Nneg[13:18], ylim=c(0,1300),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pn.in[19:24],Nneg$Nneg[19:24], ylim=c(0,1300),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot clustering coefficient   

jpeg("Plots/Metrics_1step/ClusteringCoeff_lines_Pn.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.001 & CCpos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
CCpos=CCpos[order(CCpos$pa.in,CCpos$pn.in),]

plot(CCpos$pn.in[1:6],CCpos$CCpos[1:6], ylim=c(0.15,0.7),col=cols[2],lwd=2,
     xlab="Pn", ylab="Clustering coefficient",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pn.in[7:12],CCpos$CCpos[7:12], ylim=c(0.15,0.7),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[13:18],CCpos$CCpos[13:18], ylim=c(0.15,0.7),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[19:24],CCpos$CCpos[19:24], ylim=c(0.15,0.7),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01 & CCpos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
CCpos=CCpos[order(CCpos$pa.in,CCpos$pn.in),]

plot(CCpos$pn.in[1:6],CCpos$CCpos[1:6], ylim=c(0.15,0.7),col=cols[2],lwd=2,
     xlab="Pn", ylab="Clustering coefficient",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pn.in[7:12],CCpos$CCpos[7:12], ylim=c(0.15,0.7),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[13:18],CCpos$CCpos[13:18], ylim=c(0.15,0.7),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[19:24],CCpos$CCpos[19:24], ylim=c(0.15,0.7),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.05 & CCpos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
CCpos=CCpos[order(CCpos$pa.in,CCpos$pn.in),]

plot(CCpos$pn.in[1:6],CCpos$CCpos[1:6], ylim=c(0.15,0.7),col=cols[2],lwd=2,
     xlab="Pn", ylab="Clustering coefficient",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pn.in[7:12],CCpos$CCpos[7:12], ylim=c(0.15,0.7),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[13:18],CCpos$CCpos[13:18], ylim=c(0.15,0.7),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[19:24],CCpos$CCpos[19:24], ylim=c(0.15,0.7),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot between-individual variation in degree
load(file="Data/Degree_1step.Rdata")
degrees_sd=as.data.frame(apply(degree.mat, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]
degrees_sd=degrees_sd[,params.in$scenario]


jpeg("Plots/Metrics_1step/CVDegree_lines_Pn.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.001 & degrees$pa.in %in% c(0,0.3,0.6,0.8)) ,]
degrees=degrees[order(degrees$pa.in,degrees$pn.in),]

plot(degrees$pn.in[1:6],degrees$degrees[1:6], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pn", ylab="Degreee CV",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pn.in[7:12],degrees$degrees[7:12], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[13:18],degrees$degrees[13:18], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[19:24],degrees$degrees[19:24], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.01 & degrees$pa.in %in% c(0,0.3,0.6,0.8)) ,]
degrees=degrees[order(degrees$pa.in,degrees$pn.in),]

plot(degrees$pn.in[1:6],degrees$degrees[1:6], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pn", ylab="Degreee CV",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pn.in[7:12],degrees$degrees[7:12], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[13:18],degrees$degrees[13:18], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[19:24],degrees$degrees[19:24], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.05 & degrees$pa.in %in% c(0,0.3,0.6,0.8)) ,]
degrees=degrees[order(degrees$pa.in,degrees$pn.in),]

plot(degrees$pn.in[1:6],degrees$degrees[1:6], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pn", ylab="Degreee CV",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pn.in[7:12],degrees$degrees[7:12], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[13:18],degrees$degrees[13:18], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[19:24],degrees$degrees[19:24], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()