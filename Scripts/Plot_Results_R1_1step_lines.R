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

jpeg("Plots/Metrics_1step/Modularity_lines.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

modularity=t(params.out[burn.in,"modularity",,])
dim(modularity)
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.001 & modularity$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
modularity=modularity[order(modularity$pn.in,modularity$pa.in),]

plot(modularity$pa.in[1:9],modularity$modularity[1:9], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pa", ylab="Modularity",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pa.in[10:18],modularity$modularity[10:18], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[19:27],modularity$modularity[19:27], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[28:36],modularity$modularity[28:36], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")



modularity=t(params.out[burn.in,"modularity",,])
dim(modularity)
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01 & modularity$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
modularity=modularity[order(modularity$pn.in,modularity$pa.in),]

plot(modularity$pa.in[1:9],modularity$modularity[1:9], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pa", ylab="Modularity",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pa.in[10:18],modularity$modularity[10:18], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[19:27],modularity$modularity[19:27], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[28:36],modularity$modularity[28:36], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


modularity=t(params.out[burn.in,"modularity",,])
dim(modularity)
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.05 & modularity$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
modularity=modularity[order(modularity$pn.in,modularity$pa.in),]

plot(modularity$pa.in[1:9],modularity$modularity[1:9], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pa", ylab="Modularity",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pa.in[10:18],modularity$modularity[10:18], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[19:27],modularity$modularity[19:27], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[28:36],modularity$modularity[28:36], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot Number of Affiliates

jpeg("Plots/Metrics_1step/Naffiliates_lines.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

Npos=t(params.out[burn.in,"n_pos",,])
Npos=rowMeans(Npos,na.rm=T)
Npos=data.frame(Npos,params.in)
Npos=Npos[which(Npos$pr.in==0.001 & Npos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
Npos=Npos[order(Npos$pn.in,Npos$pa.in),]

plot(Npos$pa.in[1:9],Npos$Npos[1:9], ylim=c(0,1200),col=cols[2],lwd=2,
     xlab="Pa", ylab="Number of affiliates",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Npos$pa.in[10:18],Npos$Npos[10:18], ylim=c(0,1200),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pa.in[19:27],Npos$Npos[19:27], ylim=c(0,1200),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pa.in[28:36],Npos$Npos[28:36], ylim=c(0,1200),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


Npos=t(params.out[burn.in,"n_pos",,])
Npos=rowMeans(Npos,na.rm=T)
Npos=data.frame(Npos,params.in)
Npos=Npos[which(Npos$pr.in==0.01 & Npos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
Npos=Npos[order(Npos$pn.in,Npos$pa.in),]

plot(Npos$pa.in[1:9],Npos$Npos[1:9], ylim=c(0,1200),col=cols[2],lwd=2,
     xlab="Pa", ylab="Number of affiliates",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Npos$pa.in[10:18],Npos$Npos[10:18], ylim=c(0,1200),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pa.in[19:27],Npos$Npos[19:27], ylim=c(0,1200),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pa.in[28:36],Npos$Npos[28:36], ylim=c(0,1200),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


Npos=t(params.out[burn.in,"n_pos",,])
Npos=rowMeans(Npos,na.rm=T)
Npos=data.frame(Npos,params.in)
Npos=Npos[which(Npos$pr.in==0.05 & Npos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
Npos=Npos[order(Npos$pn.in,Npos$pa.in),]

plot(Npos$pa.in[1:9],Npos$Npos[1:9], ylim=c(0,1200),col=cols[2],lwd=2,
     xlab="Pa", ylab="Number of affiliates",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Npos$pa.in[10:18],Npos$Npos[10:18], ylim=c(0,1200),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pa.in[19:27],Npos$Npos[19:27], ylim=c(0,1200),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Npos$pa.in[28:36],Npos$Npos[28:36], ylim=c(0,1200),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot Number of Avoidances   

jpeg("Plots/Metrics_1step/Navoidances_lines.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

Nneg=t(params.out[burn.in,"n_neg",,])
Nneg=rowMeans(Nneg,na.rm=T)
Nneg=data.frame(Nneg,params.in)
Nneg=Nneg[which(Nneg$pr.in==0.001 & Nneg$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
Nneg=Nneg[order(Nneg$pn.in,Nneg$pa.in),]

plot(Nneg$pa.in[1:9],Nneg$Nneg[1:9], ylim=c(0,1300),col=cols[2],lwd=2,
     xlab="Pa", ylab="Number of avoidances",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Nneg$pa.in[10:18],Nneg$Nneg[10:18], ylim=c(0,1300),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pa.in[19:27],Nneg$Nneg[19:27], ylim=c(0,1300),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pa.in[28:36],Nneg$Nneg[28:36], ylim=c(0,1300),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

Nneg=t(params.out[burn.in,"n_neg",,])
Nneg=rowMeans(Nneg,na.rm=T)
Nneg=data.frame(Nneg,params.in)
Nneg=Nneg[which(Nneg$pr.in==0.01 & Nneg$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
Nneg=Nneg[order(Nneg$pn.in,Nneg$pa.in),]

plot(Nneg$pa.in[1:9],Nneg$Nneg[1:9], ylim=c(0,1300),col=cols[2],lwd=2,
     xlab="Pa", ylab="Number of avoidances",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Nneg$pa.in[10:18],Nneg$Nneg[10:18], ylim=c(0,1300),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pa.in[19:27],Nneg$Nneg[19:27], ylim=c(0,1300),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pa.in[28:36],Nneg$Nneg[28:36], ylim=c(0,1300),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

Nneg=t(params.out[burn.in,"n_neg",,])
Nneg=rowMeans(Nneg,na.rm=T)
Nneg=data.frame(Nneg,params.in)
Nneg=Nneg[which(Nneg$pr.in==0.05 & Nneg$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
Nneg=Nneg[order(Nneg$pn.in,Nneg$pa.in),]

plot(Nneg$pa.in[1:9],Nneg$Nneg[1:9], ylim=c(0,1300),col=cols[2],lwd=2,
     xlab="Pa", ylab="Number of avoidances",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(Nneg$pa.in[10:18],Nneg$Nneg[10:18], ylim=c(0,1300),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pa.in[19:27],Nneg$Nneg[19:27], ylim=c(0,1300),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(Nneg$pa.in[28:36],Nneg$Nneg[28:36], ylim=c(0,1300),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot clustering coefficient   

jpeg("Plots/Metrics_1step/ClusteringCoeff_lines.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.001 & CCpos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
CCpos=CCpos[order(CCpos$pn.in,CCpos$pa.in),]

plot(CCpos$pa.in[1:9],CCpos$CCpos[1:9], ylim=c(0.2,0.6),col=cols[2],lwd=2,
     xlab="Pa", ylab="Clustering coefficient",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pa.in[10:18],CCpos$CCpos[10:18], ylim=c(0.2,0.6),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[19:27],CCpos$CCpos[19:27], ylim=c(0.2,0.6),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[28:36],CCpos$CCpos[28:36], ylim=c(0.2,0.6),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01 & CCpos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
CCpos=CCpos[order(CCpos$pn.in,CCpos$pa.in),]

plot(CCpos$pa.in[1:9],CCpos$CCpos[1:9], ylim=c(0.2,0.6),col=cols[2],lwd=2,
     xlab="Pa", ylab="Clustering coefficient",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pa.in[10:18],CCpos$CCpos[10:18], ylim=c(0.2,0.6),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[19:27],CCpos$CCpos[19:27], ylim=c(0.2,0.6),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[28:36],CCpos$CCpos[28:36], ylim=c(0.2,0.6),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.05 & CCpos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
CCpos=CCpos[order(CCpos$pn.in,CCpos$pa.in),]

plot(CCpos$pa.in[1:9],CCpos$CCpos[1:9], ylim=c(0.2,0.6),col=cols[2],lwd=2,
     xlab="Pa", ylab="Clustering coefficient",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pa.in[10:18],CCpos$CCpos[10:18], ylim=c(0.2,0.6),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[19:27],CCpos$CCpos[19:27], ylim=c(0.2,0.6),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[28:36],CCpos$CCpos[28:36], ylim=c(0.2,0.6),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()

#Plot between-individual variation in degree
load(file="Data/Degree_1step.Rdata")
degrees_sd=as.data.frame(apply(degree.mat, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]
degrees_sd=degrees_sd[,params.in$scenario]


jpeg("Plots/Metrics_1step/CVDegree_lines.jpeg",width=800,height=800,quality=100)
par(mfrow=c(1,3),mar=c(5,5,5,2))

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.001 & degrees$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
degrees=degrees[order(degrees$pn.in,degrees$pa.in),]

plot(degrees$pa.in[1:9],degrees$degrees[1:9], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pa", ylab="Degreee CV",type="l",main=list("Pr=0.001",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pa.in[10:18],degrees$degrees[10:18], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[19:27],degrees$degrees[19:27], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[28:36],degrees$degrees[28:36], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.01 & degrees$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
degrees=degrees[order(degrees$pn.in,degrees$pa.in),]

plot(degrees$pa.in[1:9],degrees$degrees[1:9], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pa", ylab="Degreee CV",type="l",main=list("Pr=0.01",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pa.in[10:18],degrees$degrees[10:18], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[19:27],degrees$degrees[19:27], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[28:36],degrees$degrees[28:36], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.05 & degrees$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
degrees=degrees[order(degrees$pn.in,degrees$pa.in),]

plot(degrees$pa.in[1:9],degrees$degrees[1:9], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pa", ylab="CV Degree",type="l",main=list("Pr=0.05",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pa.in[10:18],degrees$degrees[10:18], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[19:27],degrees$degrees[19:27], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[28:36],degrees$degrees[28:36], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

dev.off()