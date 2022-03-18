params.in <- data.frame(expand.grid(pn.in = c(0.3,0.4,0.5,0.6,0.7,0.8),
                                    pa.in = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), 
                                    pr.in = c(0.001,0.01,0.05)))
params.in$scenario=1:nrow(params.in)

Pr=c(0.001,0.01,0.05)
Pn=c(0.3,0.4,0.5,0.8)
Pa=c(0,0.3,0.5,0.8)

library(matrixStats)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
cols=brewer.pal(9,"Blues")[c(2,4,6,8)]

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END

blue1 <- t_col("#DEEBF7", perc = 50, )
blue2 <- t_col("#9ECAE1", perc = 50, )
blue3 <- t_col("#4292C6", perc = 50, )
blue4 <- t_col("#08519C", perc = 50, )

blues=list(blue1,blue2,blue3,blue4)
#Load results file
load(file="Data/params_out_2steps.Rdata")
dim(params.out)
burn.in=dim(params.out)[1]


#Plot modularity

par(mfrow=c(1,3),mar=c(5,5,5,2))


for (i in c(1:3)){
  par(new=FALSE)
  
  for (j in c(1:4)){
    
    modularity=t(params.out[burn.in,"modularity",,])

    modularity25=apply(modularity,1,quantile,probs=0.025,na.rm=T)
    modularity95=apply(modularity,1,quantile,probs=0.975,na.rm=T)
    modularity=rowMeans(modularity,na.rm=T)
    
    modularity=data.frame(modularity,modularity25,modularity95,params.in)
    modularity=modularity[which(modularity$pr.in==Pr[i] & modularity$pn.in==Pn[j] ) ,]
    modularity=modularity[order(modularity$pn.in,modularity$pa.in),]
    
    if (j==1){
      plot(modularity$pa.in,modularity$modularity, ylim=c(0,1),col=cols[j],lwd=2,
           xlab="Pa", ylab="Modularity",type="l",main=list(paste("Pr=", as.character( Pr[i]),sep=""),cex=2,font=1),cex.axis=1.5,cex.lab=2)
    }

    
    if (j>1){
      plot(modularity$pa.in,modularity$modularity, ylim=c(0,1),col=cols[j],lwd=2,
           xlab="", ylab="",xaxt="n",yaxt="n",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)

    }
    
    polygon(c(modularity$pa.in[1:9],modularity$pa.in[9:1]),
            c(modularity$modularity25[1:9],modularity$modularity95[9:1]),
            col=blues[[j]],border=NA)
    
    par(new=TRUE)
    
    
  }
  
}



for (i in c(1:3)){
  par(new=FALSE)
  
  for (j in c(1:4)){
    
    Npos=t(params.out[burn.in,"n_pos",,])
    
    Npos25=apply(Npos,1,quantile,probs=0.025,na.rm=T)
    Npos95=apply(Npos,1,quantile,probs=0.975,na.rm=T)
    Npos=rowMeans(Npos,na.rm=T)
    
    Npos=data.frame(Npos,Npos25,Npos95,params.in)
    Npos=Npos[which(Npos$pr.in==Pr[i] & Npos$pn.in==Pn[j] ) ,]
    Npos=Npos[order(Npos$pn.in,Npos$pa.in),]
    
    if (j==1){
      plot(Npos$pa.in,Npos$Npos, ylim=c(0,1600),col=cols[j],lwd=2,
           xlab="Pa", ylab="Number of associates",type="l",main=list(paste("Pr=", as.character( Pr[i]),sep=""),cex=2,font=1),cex.axis=1.5,cex.lab=2)
    }
    
    
    if (j>1){
      plot(Npos$pa.in,Npos$Npos, ylim=c(0,1600),col=cols[j],lwd=2,
           xlab="", ylab="",xaxt="n",yaxt="n",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
      
    }
    
    polygon(c(Npos$pa.in[1:9],Npos$pa.in[9:1]),
            c(Npos$Npos25[1:9],Npos$Npos95[9:1]),
            col=blues[[j]],border=NA)
    
    par(new=TRUE)
    
    
  }
  
}


for (i in c(1:3)){
  par(new=FALSE)
  
  for (j in c(1:4)){
    
    Nneg=t(params.out[burn.in,"n_neg",,])
    
    Nneg25=apply(Nneg,1,quantile,probs=0.025,na.rm=T)
    Nneg95=apply(Nneg,1,quantile,probs=0.975,na.rm=T)
    Nneg=rowMeans(Nneg,na.rm=T)
    
    Nneg=data.frame(Nneg,Nneg25,Nneg95,params.in)
    Nneg=Nneg[which(Nneg$pr.in==Pr[i] & Nneg$pn.in==Pn[j] ) ,]
    Nneg=Nneg[order(Nneg$pn.in,Nneg$pa.in),]
    
    if (j==1){
      plot(Nneg$pa.in,Nneg$Nneg, ylim=c(0,1500),col=cols[j],lwd=2,
           xlab="Pa", ylab="Number of avoidances",type="l",main=list(paste("Pr=", as.character( Pr[i]),sep=""),cex=2,font=1),cex.axis=1.5,cex.lab=2)
    }
    
    
    if (j>1){
      plot(Nneg$pa.in,Nneg$Nneg, ylim=c(0,1500),col=cols[j],lwd=2,
           xlab="", ylab="",xaxt="n",yaxt="n",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
      
    }
    
    polygon(c(Nneg$pa.in[1:9],Nneg$pa.in[9:1]),
            c(Nneg$Nneg25[1:9],Nneg$Nneg95[9:1]),
            col=blues[[j]],border=NA)
    
    par(new=TRUE)
    
    
  }
  
}


for (i in c(1:3)){
  par(new=FALSE)
  
  for (j in c(1:4)){
    
    CCpos=t(params.out[burn.in,"cc_pos",,])
    
    CCpos25=apply(CCpos,1,quantile,probs=0.025,na.rm=T)
    CCpos95=apply(CCpos,1,quantile,probs=0.975,na.rm=T)
    CCpos=rowMeans(CCpos,na.rm=T)
    
    CCpos=data.frame(CCpos,CCpos25,CCpos95,params.in)
    CCpos=CCpos[which(CCpos$pr.in==Pr[i] & CCpos$pn.in==Pn[j] ) ,]
    CCpos=CCpos[order(CCpos$pn.in,CCpos$pa.in),]
    
    if (j==1){
      plot(CCpos$pa.in,CCpos$CCpos, ylim=c(0.2,0.8),col=cols[j],lwd=2,
           xlab="Pa", ylab="Clustering Coefficient",type="l",main=list(paste("Pr=", as.character( Pr[i]),sep=""),cex=2,font=1),cex.axis=1.5,cex.lab=2)
    }
    
    
    if (j>1){
      plot(CCpos$pa.in,CCpos$CCpos, ylim=c(0.2,0.8),col=cols[j],lwd=2,
           xlab="", ylab="",xaxt="n",yaxt="n",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
      
    }
    
    polygon(c(CCpos$pa.in[1:9],CCpos$pa.in[9:1]),
            c(CCpos$CCpos25[1:9],CCpos$CCpos95[9:1]),
            col=blues[[j]],border=NA)
    
    par(new=TRUE)
    
    
  }
  
}



load(file="Data/Degree_2steps.Rdata")
degrees_sd=as.data.frame(apply(degree.mat, c(1,3), sd))#variance of degrees for each replicate of each scenario
degrees_mean=as.data.frame(apply(degree.mat, c(1,3), mean))#mean of degrees for each replicate of each scenario
degrees_CV=degrees_sd/degrees_mean
degrees_CV=degrees_CV[,params.in$scenario]#sort by scenario
degrees_CV=t(degrees_CV)


for (i in c(1:3)){
  par(new=FALSE)
  
  for (j in c(1:4)){
    
    degrees25=apply(degrees_CV,1,quantile,probs=0.025,na.rm=T)
    degrees95=apply(degrees_CV,1,quantile,probs=0.975,na.rm=T)
    degrees=rowMeans(degrees_CV,na.rm=T)
    
    degrees=data.frame(degrees,degrees25,degrees95,params.in)
    degrees=degrees[which(degrees$pr.in==Pr[i] & degrees$pn.in==Pn[j] ) ,]
    degrees=degrees[order(degrees$pn.in,degrees$pa.in),]
    
    if (j==1){
      plot(degrees$pa.in,degrees$degrees, ylim=c(0.2,1),col=cols[j],lwd=2,
           xlab="Pa", ylab="Degreee CV",type="l",main=list(paste("Pr=", as.character( Pr[i]),sep=""),cex=2,font=1),cex.axis=1.5,cex.lab=2)
    }
    
    
    if (j>1){
      plot(degrees$pa.in,degrees$degrees, ylim=c(0.2,1),col=cols[j],lwd=2,
           xlab="", ylab="",xaxt="n",yaxt="n",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
      
    }
    
    polygon(c(degrees$pa.in[1:9],degrees$pa.in[9:1]),
            c(degrees$degrees25[1:9],degrees$degrees95[9:1]),
            col=blues[[j]],border=NA)
    
    par(new=TRUE)
    
    
  }
  
}



#Try smoothing the curves: problem is that it makes some values negative when they cannot be
par(mfrow=c(1,3),mar=c(5,5,5,2))


for (i in c(1:3)){
  par(new=FALSE)
  
  for (j in c(1:4)){
    
    modularity=t(params.out[burn.in,"modularity",,])
    
    modularity25=apply(modularity,1,quantile,probs=0.025,na.rm=T)
    modularity95=apply(modularity,1,quantile,probs=0.975,na.rm=T)
    modularity=rowMeans(modularity,na.rm=T)
    
    modularity=data.frame(modularity,modularity25,modularity95,params.in)
    modularity=modularity[which(modularity$pr.in==Pr[i] & modularity$pn.in==Pn[j] ) ,]
    modularity=modularity[order(modularity$pn.in,modularity$pa.in),]
    
    curve_values <- loess(modularity$modularity ~ modularity$pa.in)  
    curve_values25 <- loess(modularity$modularity25 ~ modularity$pa.in)  
    curve_values95 <- loess(modularity$modularity95 ~ modularity$pa.in) 
    
    if (j==1){
      plot(curve_values, ylim=c(0,1),col=cols[j],lwd=2,
           xlab="Pa", ylab="Modularity",type="l",main=list(paste("Pr=", as.character( Pr[i]),sep=""),cex=2,font=1),cex.axis=1.5,cex.lab=2)
    }
    
    
    if (j>1){
      plot(curve_values, ylim=c(0,1),col=cols[j],lwd=2,
           xlab="", ylab="",xaxt="n",yaxt="n",type="l",main=list("",cex=2,font=1),cex.axis=1.5,cex.lab=2)
      
    }
    
    polygon(c(modularity$pa.in[1:9],modularity$pa.in[9:1]),
            c(curve_values25$fitted[1:9],curve_values95$fitted[9:1]),
            col=blues[[j]],border=NA)
    
    par(new=TRUE)
    
    
  }
  
}

