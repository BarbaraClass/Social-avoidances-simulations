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
burn.in=dim(params.out)[1]
params.out.1=params.out

load(file="Data/params_out_2steps.Rdata")
params.out.2=params.out
rm(params.out)

####Plot modularity####
jpeg("Plots/Modularity_4panels_r001.jpeg",width=800,height=800,quality=100)

par(mfrow=c(2,2),mar=c(5,5,5,2))

modularity=t(params.out.1[burn.in,"modularity",,])
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01 & modularity$pa.in %in% c(0,0.3,0.6,0.8)) ,]
modularity=modularity[order(modularity$pa.in,modularity$pn.in),]

plot(modularity$pn.in[1:6],modularity$modularity[1:6], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pn", ylab="Modularity",type="l",main=list("1 step",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pn.in[7:12],modularity$modularity[7:12], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[13:18],modularity$modularity[13:18], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[19:24],modularity$modularity[19:24], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


modularity=t(params.out.2[burn.in,"modularity",,])
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01 & modularity$pa.in %in% c(0,0.3,0.6,0.8)) ,]
modularity=modularity[order(modularity$pa.in,modularity$pn.in),]

plot(modularity$pn.in[1:6],modularity$modularity[1:6], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pn", ylab="Modularity",type="l",main=list("2 steps",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pn.in[7:12],modularity$modularity[7:12], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[13:18],modularity$modularity[13:18], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pn.in[19:24],modularity$modularity[19:24], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


modularity=t(params.out.1[burn.in,"modularity",,])
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01 & modularity$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
modularity=modularity[order(modularity$pn.in,modularity$pa.in),]

plot(modularity$pa.in[1:9],modularity$modularity[1:9], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pa", ylab="Modularity",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(modularity$pa.in[10:18],modularity$modularity[10:18], ylim=c(0,1),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[19:27],modularity$modularity[19:27], ylim=c(0,1),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(modularity$pa.in[28:36],modularity$modularity[28:36], ylim=c(0,1),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


modularity=t(params.out.2[burn.in,"modularity",,])
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01 & modularity$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
modularity=modularity[order(modularity$pn.in,modularity$pa.in),]

plot(modularity$pa.in[1:9],modularity$modularity[1:9], ylim=c(0,1),col=cols[2],lwd=2,
     xlab="Pa", ylab="Modularity",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
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

####Plot CC####

jpeg("Plots/CC_4panels_r001.jpeg",width=800,height=800,quality=100)

par(mfrow=c(2,2),mar=c(5,5,5,2))
CCpos=t(params.out.1[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01 & CCpos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
CCpos=CCpos[order(CCpos$pa.in,CCpos$pn.in),]

plot(CCpos$pn.in[1:6],CCpos$CCpos[1:6], ylim=c(0.15,0.7),col=cols[2],lwd=2,
     xlab="Pn", ylab="Clustering coefficient",type="l",main=list("1 step",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pn.in[7:12],CCpos$CCpos[7:12], ylim=c(0.15,0.7),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[13:18],CCpos$CCpos[13:18], ylim=c(0.15,0.7),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[19:24],CCpos$CCpos[19:24], ylim=c(0.15,0.7),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

CCpos=t(params.out.2[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01 & CCpos$pa.in %in% c(0,0.3,0.6,0.8)) ,]
CCpos=CCpos[order(CCpos$pa.in,CCpos$pn.in),]

plot(CCpos$pn.in[1:6],CCpos$CCpos[1:6], ylim=c(0.15,0.7),col=cols[2],lwd=2,
     xlab="Pn", ylab="Clustering coefficient",type="l",main=list("2 steps",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pn.in[7:12],CCpos$CCpos[7:12], ylim=c(0.15,0.7),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[13:18],CCpos$CCpos[13:18], ylim=c(0.15,0.7),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pn.in[19:24],CCpos$CCpos[19:24], ylim=c(0.15,0.7),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


CCpos=t(params.out.1[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01 & CCpos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
CCpos=CCpos[order(CCpos$pn.in,CCpos$pa.in),]

plot(CCpos$pa.in[1:9],CCpos$CCpos[1:9], ylim=c(0.2,0.6),col=cols[2],lwd=2,
     xlab="Pa", ylab="Clustering coefficient",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(CCpos$pa.in[10:18],CCpos$CCpos[10:18], ylim=c(0.2,0.6),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[19:27],CCpos$CCpos[19:27], ylim=c(0.2,0.6),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(CCpos$pa.in[28:36],CCpos$CCpos[28:36], ylim=c(0.2,0.6),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")

CCpos=t(params.out.2[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01 & CCpos$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
CCpos=CCpos[order(CCpos$pn.in,CCpos$pa.in),]

plot(CCpos$pa.in[1:9],CCpos$CCpos[1:9], ylim=c(0.2,0.6),col=cols[2],lwd=2,
     xlab="Pa", ylab="Clustering coefficient",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
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


####

####Plot CV degree####
load(file="Data/Degree_1step.Rdata")
degree.mat.1=degree.mat
load(file="Data/Degree_2steps.Rdata")
degree.mat.2=degree.mat

jpeg("Plots/CVDegree_4panels_r001.jpeg",width=800,height=800,quality=100)
par(mfrow=c(2,2),mar=c(5,5,5,2))

degrees_sd=as.data.frame(apply(degree.mat.1, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat.1, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]
degrees_sd=degrees_sd[,params.in$scenario]

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.01 & degrees$pa.in %in% c(0,0.3,0.6,0.8)) ,]
degrees=degrees[order(degrees$pa.in,degrees$pn.in),]

plot(degrees$pn.in[1:6],degrees$degrees[1:6], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pn", ylab="Degreee CV",type="l",main=list("1 step",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pn.in[7:12],degrees$degrees[7:12], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[13:18],degrees$degrees[13:18], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[19:24],degrees$degrees[19:24], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


degrees_sd=as.data.frame(apply(degree.mat.2, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat.2, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]
degrees_sd=degrees_sd[,params.in$scenario]

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.01 & degrees$pa.in %in% c(0,0.3,0.6,0.8)) ,]
degrees=degrees[order(degrees$pa.in,degrees$pn.in),]

plot(degrees$pn.in[1:6],degrees$degrees[1:6], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pn", ylab="Degreee CV",type="l",main=list("2 steps",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pn.in[7:12],degrees$degrees[7:12], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[13:18],degrees$degrees[13:18], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pn.in[19:24],degrees$degrees[19:24], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


degrees_sd=as.data.frame(apply(degree.mat.1, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat.1, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]
degrees_sd=degrees_sd[,params.in$scenario]

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.01 & degrees$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
degrees=degrees[order(degrees$pn.in,degrees$pa.in),]

plot(degrees$pa.in[1:9],degrees$degrees[1:9], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pa", ylab="Degreee CV",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
par(new=TRUE)
plot(degrees$pa.in[10:18],degrees$degrees[10:18], ylim=c(0.25,0.8),col=cols[4],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[19:27],degrees$degrees[19:27], ylim=c(0.25,0.8),col=cols[6],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")
par(new=TRUE)
plot(degrees$pa.in[28:36],degrees$degrees[28:36], ylim=c(0.25,0.8),col=cols[8],lwd=2,
     xaxt="n",yaxt="n",xlab="", ylab="",type="l")


degrees_sd=as.data.frame(apply(degree.mat.2, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat.2, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]
degrees_sd=degrees_sd[,params.in$scenario]

degrees=colMeans(degrees_CV,na.rm=T)
degrees=data.frame(degrees,params.in)
degrees=degrees[which(degrees$pr.in==0.01 & degrees$pn.in %in% c(0.3,0.4,0.5,0.8)) ,]
degrees=degrees[order(degrees$pn.in,degrees$pa.in),]

plot(degrees$pa.in[1:9],degrees$degrees[1:9], ylim=c(0.25,0.8),col=cols[2],lwd=2,
     xlab="Pa", ylab="Degreee CV",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
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

