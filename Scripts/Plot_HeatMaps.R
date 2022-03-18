#Make plots

library(lattice)
library(gridExtra)
library(RColorBrewer)

params.in <- data.frame(expand.grid(pn.in = c(0.3,0.4,0.5,0.6,0.7,0.8),
                                    pa.in = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), 
                                    pr.in = c(0.001,0.01,0.05)))
params.in$scenario=1:nrow(params.in)

load(file="Data/params_out_2steps.Rdata")

dim(params.out)
burn.in=dim(params.out)[1]


#Plot Modularity 
modularity=t(params.out[burn.in,"modularity",,])
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01) ,]
modularity$modularity=round(modularity$modularity,2)

coul <- colorRampPalette(brewer.pal(9, "Blues"))(15)

   mod.loess = loess(modularity ~ pa.in*pn.in, data = modularity, degree = 2, span = 0.8)
    
   mod.fit = expand.grid(list(pa.in= seq(0, 0.8, 0.01), pn.in = seq(0.3, 0.8, 0.01)))
    z = predict(mod.loess, newdata = mod.fit)
    mod.fit$modularity = as.numeric(z)
    summary(mod.fit$modularity)
    
    levelplot(modularity ~ pa.in*pn.in, data = mod.fit,
                    xlab = list("Pa",cex=1), ylab = list("Pn",cex=1),
                    xlim=c(0,0.8), ylim=c(0.3,0.8),
                    cuts = 10,contour=FALSE,col.regions=coul,colorkey=list(at=seq(from=0, to=0.7,by=0.05)),
                    panel=function(..., at, contour, region) {
                      panel.levelplot(..., at=seq(from=0, to=0.7,by=0.05), contour = FALSE, region = TRUE,labels=FALSE)
                      panel.contourplot(..., at=seq(from=0, to=0.7,by=0.05), contour = FALSE,labels=FALSE,
                                        region = FALSE)
                      
                    })
    
    
#Plot CCpos    
    CCpos=t(params.out[burn.in,"cc_pos",,])
    CCpos=rowMeans(CCpos,na.rm=T)
    CCpos=data.frame(CCpos,params.in)
    CCpos=CCpos[which(CCpos$pr.in==0.01) ,]
    CCpos$CCpos=round(CCpos$CCpos,2)
    
    coul <- colorRampPalette(brewer.pal(9, "Blues"))(21)
    
    cc.loess = loess(CCpos ~ pa.in*pn.in, data = CCpos, degree = 2, span = 0.8)
    
    cc.fit = expand.grid(list(pa.in= seq(0, 0.8, 0.01), pn.in = seq(0.3, 0.8, 0.01)))
    z = predict(cc.loess, newdata = cc.fit)
    cc.fit$CCpos = as.numeric(z)
    summary(cc.fit$CCpos)
    
    levelplot(CCpos ~ pa.in*pn.in, data = cc.fit,
              xlab = list("Pa",cex=1), ylab = list("Pn",cex=1),
              xlim=c(0,0.8), ylim=c(0.3,0.8),
              cuts = 10,contour=FALSE,col.regions=coul,colorkey=list(at=seq(from=0.34, to=0.56,by=0.02)),
              panel=function(..., at, contour, region) {
                panel.levelplot(..., at=seq(from=0.34, to=0.56,by=0.02), contour = FALSE, region = TRUE,labels=FALSE)
                panel.contourplot(..., at=seq(from=0.34, to=0.56,by=0.02), contour = FALSE,labels=FALSE,
                                  region = FALSE)
                
              })
    

#Look at the 1step model
    
load(file="Data/params_out_1step.Rdata")
    

#Plot Modularity 
modularity=t(params.out[burn.in,"modularity",,])
modularity=rowMeans(modularity,na.rm=T)
modularity=data.frame(modularity,params.in)
modularity=modularity[which(modularity$pr.in==0.01) ,]
modularity$modularity=round(modularity$modularity,2)

coul <- colorRampPalette(brewer.pal(9, "Blues"))(15)

mod.loess = loess(modularity ~ pa.in*pn.in, data = modularity, degree = 2, span = 0.8)

mod.fit = expand.grid(list(pa.in= seq(0, 0.8, 0.01), pn.in = seq(0.3, 0.8, 0.01)))
z = predict(mod.loess, newdata = mod.fit)
mod.fit$modularity = as.numeric(z)
summary(mod.fit$modularity)

levelplot(modularity ~ pa.in*pn.in, data = mod.fit,
          xlab = list("Pa",cex=1), ylab = list("Pn",cex=1),
          xlim=c(0,0.8), ylim=c(0.3,0.8),
          cuts = 10,contour=FALSE,col.regions=coul,colorkey=list(at=seq(from=0.5, to=0.72,by=0.02)),
          panel=function(..., at, contour, region) {
            panel.levelplot(..., at=seq(from=0.5, to=0.72,by=0.02), contour = FALSE, region = TRUE,labels=FALSE)
            panel.contourplot(..., at=seq(from=0.5, to=0.72,by=0.02), contour = FALSE,labels=FALSE,
                              region = FALSE)
            
          })



#Plot CCpos
CCpos=t(params.out[burn.in,"cc_pos",,])
CCpos=rowMeans(CCpos,na.rm=T)
CCpos=data.frame(CCpos,params.in)
CCpos=CCpos[which(CCpos$pr.in==0.01) ,]
CCpos$CCpos=round(CCpos$CCpos,2)
coul <- colorRampPalette(brewer.pal(9, "Blues"))(15)

cc.loess = loess(CCpos ~ pa.in*pn.in, data = CCpos, degree = 2, span = 0.8)

cc.fit = expand.grid(list(pa.in= seq(0, 0.8, 0.01), pn.in = seq(0.3, 0.8, 0.01)))
z = predict(cc.loess, newdata = cc.fit)
cc.fit$CCpos = as.numeric(z)
summary(cc.fit$CCpos)

levelplot(CCpos ~ pa.in*pn.in, data = cc.fit,
          xlab = list("Pa",cex=1), ylab = list("Pn",cex=1),
          xlim=c(0,0.8), ylim=c(0.3,0.8),
          cuts = 10,contour=FALSE,col.regions=coul,colorkey=list(at=seq(from=0.30, to=0.56,by=0.02)),
          
          panel=function(..., at, contour, region) {
            panel.levelplot(..., at=seq(from=0.30, to=0.56,by=0.02), contour = FALSE, region = TRUE,labels=FALSE)
            panel.contourplot(..., at=seq(from=0.30, to=0.56,by=0.02), contour = FALSE,labels=FALSE,
                              region = FALSE)
            
          })
