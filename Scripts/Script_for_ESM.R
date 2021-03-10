library(sna)
library(Matrix)
library(assortnet)
library(igraph)

############### START  OF THE SIMULATING FUNCTION ###############
update.network <- function(network, pn, pa, pr, prn, pra, pb=1) {
  
  #One randomly chosen individual dies
  del = sample(1:N, 1)
  
  network <- network[-del,]
  network <- network[,-del]
  N <- N-1
  
  #One randomly chosen individual reproduces
  repro <- sample(1:N, 1)
  
  # Add this new individual
  N <- N + 1
  
  # Generate the relationships of this new individual: by default, casual
  rel <- rep(0, N)
  rel[N]<-0.5
  # with mother's avoidances: depend on the probability to inherit them from mother Pa
  neg.edges <- which(network[repro,]==-1)
  rel[neg.edges] <- sample(c(0,-1),length(neg.edges),prob=c(1-pa,pa),replace=TRUE)
  
  # with mother'sassociates: depend on the probability to inherit them from mother Pn
  pos.edges <- which(network[repro,]==1)
  rel[pos.edges] <- sample(c(0,1),length(pos.edges),prob=c(1-pn,pn),replace=TRUE)
  
  #Relationship between the new individual and its parent (here coded 1) depends on pb 
  rel[repro] <- sample(c(0,1),1,prob=c(1-pb,pb))
  
  #make new friends or avoidances
  no.edges <- which(rel==0)
  
  
  
  if (length(no.edges)>0){
    all.rel<-cbind(rel[-N],network[,no.edges])
    colnames(all.rel)<-c("offspring", no.edges)
    
    no_rel_prod<-all.rel[,1] * all.rel[,2:ncol(all.rel)]#product=do they have the same rel with the same individuals?
    
    indirect_avoid<-names(which(apply(no_rel_prod,2,function(x)length(which(x==-1))>0)))#column contains -1: indiv that are friends with offspring's inherited avoidance (and vice versa)
    neutral<-names(which(apply(no_rel_prod,2,function(x)length(which(x==0))==N-1)))#column only contains zeros
    indirect_friend<-names(which(apply(no_rel_prod,2,function(x)length(which(x==1))>0 )))#column contains 1: indiv that are  friends with offsprings friends or avoiding offsprings avoidances
    indirect_friend<-indirect_friend[-which(indirect_friend %in% indirect_avoid)]#remove columns with  both 1 and -1 from friends
    
    
    
    if (prn==pr & pra==pr){#balance the density of the networks that have no SB constraint to match that of the network with constraint
      compensate1= pr*length(c(neutral,indirect_friend))/length(no.edges)
      compensate2= pr*length(c(neutral,indirect_avoid))/length(no.edges)
      
      
      rel[as.numeric(no.edges)] <- sample(c(0,1,-1),length(no.edges),prob=c(1-compensate1-compensate2,compensate1,compensate2),replace=TRUE)
    }
    
    if(prn==0 & pra==0){
      rel[as.numeric(neutral)] <- sample(c(0,1,-1),length(neutral),prob=c(1-2*pr,pr,pr),replace=TRUE)
      rel[as.numeric(indirect_avoid)] <- sample(c(0,1,-1),length(indirect_avoid),prob=c(1-pr-prn,prn,pr),replace=TRUE)
      rel[as.numeric(indirect_friend)] <- sample(c(0,1,-1),length(indirect_friend),prob=c(1-pr-pra,pr,pra),replace=TRUE)
      
    }
    
    
    
  }
  
  # update network: add this new individual to the network
  network.new <- rbind(network, rel[-N])
  network.new<- cbind(network.new, rel)
  
  colnames(network.new) <- NULL
  return(list(network.new=network.new))
  
}


############### END  OF THE SIMULATING FUNCTION ###############


############### START  OF THE SIMULATIONS ###############

#Set parameter values
params.in <- data.frame(expand.grid(pn.in = 0.5,pa.in = c(0.1,0.5,0.8), pr.in = 0.1,  prn.in = c(0.1,0) ,pra.in = 0))
params.in$pb.in<-1
params.in$pra.in<-params.in$prn.in

N <- 50			# Number of individuals
n.rep <- 500 # Number of replicates
burn.in <- 500 #Number of generations/time steps (converges when approx 10 times the network size)

#Store population metrics in an array. rows= generations, layers=replicates
params <- c( 'n_pos', 'n_neg','cc_pos', 'assort_pos','modularity','n.comp',
             'shared_pos', 'shared_neg',"n_neg.tri","n_unbal.tri","skew.degree", "skew.betweenness")

params.out<-array (0, c(burn.in,length(params),n.rep,nrow(params.in)))
dimnames(params.out)[[2]]<-params

library(RColorBrewer)
par(mfrow=c(2,3), mar=c(6,6,1,1))
cols=c(brewer.pal(3,"Blues"), brewer.pal(3,"Oranges"))

#Store individual metrics in arrays
degree.mat=array (NA,c(n.rep,N,nrow(params.in)))
betweenness.mat=array (NA,c(n.rep,N,nrow(params.in)))

#Run the loop
for (i in 1:nrow(params.in)){
  
  
  for (rep in 1:n.rep){
    network.orig<-matrix(0,N,N)
    network.orig[lower.tri(network.orig)]<-sample(c(0,1,-1),N*(N-1)/2,replace=T,prob = c(0.8,0.1,0.1))
    diag(network.orig)=0.5
    
    for (zz in 1:burn.in) {
      output <- update.network(network.orig, params.in$pn.in[i], params.in$pa.in[i], params.in$pr.in[i], params.in$prn.in[i], params.in$pra.in[i], params.in$pb.in[i])
      network.orig <- output$network.new#THIS UPDATES THE NETWORK
      
      #Extract values from the simulated network 
      params.out[zz,"n_pos",rep,i] = length(network.orig[network.orig==1])  #Number of associates
      params.out[zz,"n_neg",rep,i] = length(network.orig[network.orig==-1])  #Number of avoidances
      
      #Network of positive relationships. It is the "observed" network
      pos = network.orig
      pos[pos!= 1] = 0
      
      #Network of positive negative relationships (avoidances)
      neg = network.orig
      neg[neg!=-1] = 0
      neg[neg<0] = 1
      
      #Extract network metrics
      params.out[zz,"cc_pos",rep,i] = sna::gtrans(pos, mode = "graph")#CLUSTERING COEFFICIENT FOR POSITIVE REL
      params.out[zz,"assort_pos",rep,i] = assortnet::assortment.discrete(pos,sna::degree(pos, gmode = "graph"),weighted = F)$r#ASSORTATIVITY FOR NEGATIVE REL
      
      g<-igraph::graph_from_incidence_matrix(pos)#transform network into a graph
      wc <- igraph::walktrap.community(g) #looking for structure 
      params.out[zz,"modularity",rep,i] = igraph::modularity(wc)#extract modularity
      n.comp<-sum(igraph::components(g)$csize>2)#extract number of groups ( individuals)
      params.out[zz,"n.comp",rep,i] = n.comp
      
      #Create matrices of shared positive and negative relationships: two individuals that are friends: how many friends or avoidances do they share
      shared_neg = c()
      shared_pos = c()
      unbal.tri = c()
      neg.tri = c()
      for (a in 1:(N-1)){
        for (b in (a+1):N){
          if (pos[a,b]==1) shared_neg = c(shared_neg,sum(neg[a,]==neg[b,] & neg[a,]==1))
          if (pos[a,b]==1) shared_pos = c(shared_pos,sum(pos[a,]==pos[b,] & pos[a,]==1))
          if (pos[a,b]==1) unbal.tri=c(unbal.tri,sum(c(pos[a,]+neg[b,], pos[b,]+neg[a,])==2))
          if (neg[a,b]==1) neg.tri=c(neg.tri,sum(neg[a,]==neg[b,] & neg[a,]==1))
        }
      }
      
      params.out[zz,"shared_pos",rep,i] = length(which(shared_pos>0))#Number of shared positive relationships: +++ triads
      params.out[zz,"shared_neg",rep,i] = length(which(shared_neg>0))#Number of shared negative relationships:+-- triads
      
      #Calculate the number of negative and unbalance triads
      params.out[zz,"n_neg.tri",rep,i] = length(which(neg.tri>0))#Number of --- triads
      params.out[zz,"n_unbal.tri",rep,i] = length(which(unbal.tri>0))#Number of +-+ triads
      
      skew = function(x) mean( ((x - mean(x)) / sd(x))^3 )
      params.out[zz,"skew.degree",rep,i] = skew(sna::degree(pos, gmode = "graph"))
      params.out[zz,"skew.betweenness",rep,i] = skew(sna::betweenness(pos))
      
      if (zz==burn.in){
        #Extract individual metrics on the last iteration
        degree.mat[rep,1:N,i]=sna::degree(pos, gmode = "graph")#Degree distribution
        betweenness.mat[rep,1:N,i]=sna::betweenness(pos)}# Betweenness distribution
      
      if (rep==1& zz==burn.in){
        
        if (i==1){ gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[i],edge.col = cols[i],vertex.border=cols[i],ylab="No SB",cex.lab=2)}
        if (i==2){ gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[i],edge.col = cols[i],vertex.border=cols[i])}
        if (i==3){ gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[i],edge.col = cols[i],vertex.border=cols[i])}
        if (i==4){ gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[i],edge.col = cols[i],vertex.border=cols[i],ylab="SB",xlab="Pa=0.1",cex.lab=2)}
        if (i==5){ gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[i],edge.col = cols[i],vertex.border=cols[i],xlab="Pa=0.5",cex.lab=2)}
        if (i==6){ gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[i],edge.col = cols[i],vertex.border=cols[i],xlab="Pa=0.8",cex.lab=2)}
        
      }
      
    }
    cat(paste0(rep," "))
    
  }
}

############### END  OF THE SIMULATIONS ###############


save(params.out, file="Data/params_out.Rdata")
save(degree.mat, file="Data/Degree.Rdata")
save(betweenness.mat, file="Data/Betweenness.Rdata")

############### PLOT THE RESULTS ###############

load(file="Data/params_out.Rdata")
burn.in=dim(params.out)[1]
library(matrixStats)

#Parameter values
params.in <- data.frame(expand.grid(pn.in = 0.5,pa.in = c(0.1,0.5,0.8), pr.in = 0.1,  prn.in = c(0.1,0),pra.in = 0.1))
params.in$pra.in=params.in$prn.in
params.in$pb.in<-1

#Calculate the median for each metric (across replicates) at each time step 
median_time=matrix(NA, burn.in,54)
CIinf_time=matrix(NA, burn.in,54)
CIsup_time=matrix(NA, burn.in,54)

for ( i in 1:6){
  
  median_time[1:burn.in,seq(from=((i-1)*9)+1 ,to= (i*9))]<-apply(params.out[,c(1:5,7:10),,i],2,rowMedians)#Calculate the mean over 1000 replicates
  CIinf_time[1:burn.in,seq(from=((i-1)*9)+1 ,to= (i*9))]<-apply(params.out[,c(1:5,7:10),,i],2,rowQuantiles,probs=c(0.025))#Calculate the mean over 1000 replicates
  CIsup_time[1:burn.in,seq(from=((i-1)*9)+1 ,to= (i*9))]<-apply(params.out[,c(1:5,7:10),,i],2,rowQuantiles,probs=c(0.975))#Calculate the mean over 1000 replicates
  
}

colnames(median_time)=paste(rep(colnames(params.out[,c(1:5,7:10),1,3]),6),rep(c(1:6), each=9),sep=".")
colnames(CIinf_time)=paste(rep(colnames(params.out[,c(1:5,7:10),1,3]),6),rep(c(1:6), each=9),'CInf',sep=".")
colnames(CIsup_time)=paste(rep(colnames(params.out[,c(1:5,7:10),1,3]),6),rep(c(1:6), each=9),'Csup',sep=".")


#Group by metric
Npos=cbind(median_time[,c(((0:5)*9) +1)], CIinf_time[,c(((0:5)*9) +1)],CIsup_time[,c(((0:5)*9) +1)])
Nneg=cbind(median_time[,c(((0:5)*9) +2)], CIinf_time[,c(((0:5)*9) +2)],CIsup_time[,c(((0:5)*9) +2)])
CC=cbind(median_time[,c(((0:5)*9) +3)], CIinf_time[,c(((0:5)*9) +3)],CIsup_time[,c(((0:5)*9) +3)])
Degree.Assort=cbind(median_time[,c(((0:5)*9) +4)], CIinf_time[,c(((0:5)*9) +4)],CIsup_time[,c(((0:5)*9) +4)])
Modularity=cbind(median_time[,c(((0:5)*9) +5)], CIinf_time[,c(((0:5)*9) +5)],CIsup_time[,c(((0:5)*9) +5)])
Shared.pos=cbind(median_time[,c(((0:5)*9) +6)], CIinf_time[,c(((0:5)*9) +6)],CIsup_time[,c(((0:5)*9) +6)])
Shared.neg=cbind(median_time[,c(((0:5)*9) +7)], CIinf_time[,c(((0:5)*9) +7)],CIsup_time[,c(((0:5)*9) +7)])
Neg.tri=cbind(median_time[,c(((0:5)*9) +8)], CIinf_time[,c(((0:5)*9) +8)],CIsup_time[,c(((0:5)*9) +8)])
Unbal.tri=cbind(median_time[,c(((0:5)*9) +9)], CIinf_time[,c(((0:5)*9) +9)],CIsup_time[,c(((0:5)*9) +9)])

#Calculate mean number of components instead of the median
Ncomp=cbind(rowMeans(params.out[,'n.comp',,1]),
            rowMeans(params.out[,'n.comp',,2]),
            rowMeans(params.out[,'n.comp',,3]),
            rowMeans(params.out[,'n.comp',,4]),
            rowMeans(params.out[,'n.comp',,5]),
            rowMeans(params.out[,'n.comp',,6]))



#Plot population metrics over time
library(RColorBrewer)
par(mfrow=c(5,1), mar=c(2,5,0,0), oma= c(3,0.3,0.3,0.3))
cols=c(brewer.pal(3,"Blues"), brewer.pal(3,"Oranges"))

for ( i in 1:6){
  plot(Npos[,i], type="l",lwd=2, ylim=c(min(Npos[,1:6]),max(Npos[,1:6])),col=cols[i],xlab="",xaxt= "n", ylab="Number of associates",cex.axis=1.5,cex.lab=1.5)
  par(new=TRUE)}
par(new=FALSE)

for ( i in 1:6){
  plot(Nneg[,i], type="l",lwd=2, ylim=c(min(Nneg[,1:6]),max(Nneg[,1:6])),col=cols[i],xlab="",xaxt= "n", ylab="Number of avoidances",cex.axis=1.5,cex.lab=1.5)
  par(new=TRUE)}
par(new=FALSE)

for ( i in 1:6){
  plot(CC[,i], type="l",lwd=2, ylim=c(min(CC[,1:6]),max(CC[,1:6])),col=cols[i],xlab="",xaxt= "n", ylab="Clustering coefficient",cex.axis=1.5,cex.lab=1.5)
  par(new=TRUE)}
par(new=FALSE)

for ( i in 1:6){
  plot(Degree.Assort[,i], type="l",lwd=2, ylim=c(min(Degree.Assort[,1:6]),max(Degree.Assort[,1:6])),col=cols[i],xlab="",xaxt= "n", ylab="Degreee assortativity",cex.axis=1.5,cex.lab=1.5)
  par(new=TRUE)}
abline(0,0,lty=2)
par(new=FALSE)

for ( i in 1:6){
  plot(Modularity[,i], type="l",lwd=2, ylim=c(min(Modularity[,1:6]),max(Modularity[,1:6])),col=cols[i],xlab='', ylab="Modularity",cex.axis=1.5,cex.lab=1.5)
  par(new=TRUE)}
par(new=FALSE)

mtext("Time", side=1, outer=F, line=3.7,at=250,cex=1.5)


#Number of components at the last time step of each scenario
table(params.out[burn.in,"n.comp",,1])/dim(params.out)[3]
table(params.out[burn.in,"n.comp",,2])/dim(params.out)[3]
table(params.out[burn.in,"n.comp",,3])/dim(params.out)[3]
table(params.out[burn.in,"n.comp",,4])/dim(params.out)[3]
table(params.out[burn.in,"n.comp",,5])/dim(params.out)[3]
table(params.out[burn.in,"n.comp",,6])/dim(params.out)[3]


############### END OF PLOT THE RESULTS ###############


############### PLOT RELATIONSHIPS AMONG METRICS ###############
load(file="Data/params_out.Rdata")
burn.in=dim(params.out)[1]
library(matrixStats)

#Parameter values
params.in <- data.frame(expand.grid(pn.in = 0.5,pa.in = c(0.1,0.5,0.8), pr.in = 0.1,  prn.in = c(0.1,0),pra.in = 0.1))
params.in$pra.in=params.in$prn.in
params.in$pb.in<-1

#Calculate the median for each metric at each time step
median_time=matrix(NA, burn.in,54)
CIinf_time=matrix(NA, burn.in,54)
CIsup_time=matrix(NA, burn.in,54)

for ( i in 1:6){
  
  median_time[1:burn.in,seq(from=((i-1)*9)+1 ,to= (i*9))]<-apply(params.out[,c(1:5,7:10),,i],2,rowMedians)#Calculate the mean over 1000 replicates
  CIinf_time[1:burn.in,seq(from=((i-1)*9)+1 ,to= (i*9))]<-apply(params.out[,c(1:5,7:10),,i],2,rowQuantiles,probs=c(0.025))#Calculate the mean over 1000 replicates
  CIsup_time[1:burn.in,seq(from=((i-1)*9)+1 ,to= (i*9))]<-apply(params.out[,c(1:5,7:10),,i],2,rowQuantiles,probs=c(0.975))#Calculate the mean over 1000 replicates
  
}

colnames(median_time)=paste(rep(colnames(params.out[,c(1:5,7:10),1,3]),6),rep(c(1:6), each=9),sep=".")
colnames(CIinf_time)=paste(rep(colnames(params.out[,c(1:5,7:10),1,3]),6),rep(c(1:6), each=9),'CInf',sep=".")
colnames(CIsup_time)=paste(rep(colnames(params.out[,c(1:5,7:10),1,3]),6),rep(c(1:6), each=9),'Csup',sep=".")


#Relationships across time-steps=use median
library(RColorBrewer)
par(mfrow=c(2,3))
cols=c(brewer.pal(3,"Blues"), brewer.pal(3,"Oranges"))

plot(median_time[,'n_pos.1'], median_time[,'modularity.1'],ylim=c(0.15,0.55),xlim=c(100,410), xlab="n_pos", ylab= "Modularity",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.2'], median_time[,'modularity.2'],ylim=c(0.15,0.55),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.3'], median_time[,'modularity.3'],ylim=c(0.15,0.55),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.4'], median_time[,'modularity.4'],ylim=c(0.15,0.55),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.5'], median_time[,'modularity.5'],ylim=c(0.15,0.55),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.6'], median_time[,'modularity.6'],ylim=c(0.15,0.55),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

plot(median_time[,'cc_pos.1'], median_time[,'modularity.1'],ylim=c(0.15,0.55),xlim=c(0.1,0.38), xlab="cc_pos", ylab= "Modularity",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.2'], median_time[,'modularity.2'],ylim=c(0.15,0.55),xlim=c(0.1,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.3'], median_time[,'modularity.3'],ylim=c(0.15,0.55),xlim=c(0.1,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.4'], median_time[,'modularity.4'],ylim=c(0.15,0.55),xlim=c(0.1,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.5'], median_time[,'modularity.5'],ylim=c(0.15,0.55),xlim=c(0.1,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.6'], median_time[,'modularity.6'],ylim=c(0.15,0.55),xlim=c(0.1,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

plot(median_time[,'assort_pos.1'], median_time[,'modularity.1'],ylim=c(0.10,0.50),xlim=c(-0.05,0.01), xlab="assort_pos", ylab= "Modularity",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(median_time[,'assort_pos.2'], median_time[,'modularity.2'],ylim=c(0.10,0.50),xlim=c(-0.05,0.01),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(median_time[,'assort_pos.3'], median_time[,'modularity.3'],ylim=c(0.10,0.50),xlim=c(-0.05,0.01),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(median_time[,'assort_pos.4'], median_time[,'modularity.4'],ylim=c(0.10,0.50),xlim=c(-0.05,0.01),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(median_time[,'assort_pos.5'], median_time[,'modularity.5'],ylim=c(0.10,0.50),xlim=c(-0.05,0.01),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(median_time[,'assort_pos.6'], median_time[,'modularity.6'],ylim=c(0.10,0.50),xlim=c(-0.05,0.01),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

plot(median_time[,'n_pos.1'], median_time[,'cc_pos.1'],ylim=c(0.10,0.38),xlim=c(100,410), xlab="n_pos", ylab= "cc_pos",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.2'], median_time[,'cc_pos.2'],ylim=c(0.10,0.38),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.3'], median_time[,'cc_pos.3'],ylim=c(0.10,0.38),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.4'], median_time[,'cc_pos.4'],ylim=c(0.10,0.38),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.5'], median_time[,'cc_pos.5'],ylim=c(0.10,0.38),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.6'], median_time[,'cc_pos.6'],ylim=c(0.10,0.38),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

plot(median_time[,'n_pos.1'], median_time[,'assort_pos.1'],ylim=c(-0.05,0.01),xlim=c(100,410), xlab="n_pos", ylab= "assort_pos",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.2'], median_time[,'assort_pos.2'],ylim=c(-0.05,0.01),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.3'], median_time[,'assort_pos.3'],ylim=c(-0.05,0.01),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.4'], median_time[,'assort_pos.4'],ylim=c(-0.05,0.01),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.5'], median_time[,'assort_pos.5'],ylim=c(-0.05,0.01),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(median_time[,'n_pos.6'], median_time[,'assort_pos.6'],ylim=c(-0.05,0.01),xlim=c(100,410),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

plot(median_time[,'cc_pos.1'], median_time[,'assort_pos.1'],ylim=c(-0.05,0.01),xlim=c(0.10,0.38), xlab="cc_pos", ylab= "assort_pos",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.2'], median_time[,'assort_pos.2'],ylim=c(-0.05,0.01),xlim=c(0.10,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.3'], median_time[,'assort_pos.3'],ylim=c(-0.05,0.01),xlim=c(0.10,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.4'], median_time[,'assort_pos.4'],ylim=c(-0.05,0.01),xlim=c(0.10,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.5'], median_time[,'assort_pos.5'],ylim=c(-0.05,0.01),xlim=c(0.10,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(median_time[,'cc_pos.6'], median_time[,'assort_pos.6'],ylim=c(-0.05,0.01),xlim=c(0.10,0.38),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

#Relationships on the replicate level= error level
test1=as.data.frame(t(params.out[burn.in,,,1]))
test2=as.data.frame(t(params.out[burn.in,,,2]))
test3=as.data.frame(t(params.out[burn.in,,,3]))
test4=as.data.frame(t(params.out[burn.in,,,4]))
test5=as.data.frame(t(params.out[burn.in,,,5]))
test6=as.data.frame(t(params.out[burn.in,,,6]))

par(mfrow=c(2,3))

plot(test1[,'n_pos'], test1[,'modularity'],ylim=c(0,0.70),xlim=c(150,500), xlab="n_pos", ylab= "Modularity",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(test2[,'n_pos'], test2[,'modularity'],ylim=c(0,0.70),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(test3[,'n_pos'], test3[,'modularity'],ylim=c(0,0.70),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(test4[,'n_pos'], test4[,'modularity'],ylim=c(0,0.70),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(test5[,'n_pos'], test5[,'modularity'],ylim=c(0,0.70),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(test6[,'n_pos'], test6[,'modularity'],ylim=c(0,0.70),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)


plot(test1[,'cc_pos'], test1[,'modularity'],ylim=c(0,0.70),xlim=c(0.1,0.50), xlab="cc_pos", ylab= "Modularity",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(test2[,'cc_pos'], test2[,'modularity'],ylim=c(0,0.70),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(test3[,'cc_pos'], test3[,'modularity'],ylim=c(0,0.70),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(test4[,'cc_pos'], test4[,'modularity'],ylim=c(0,0.70),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(test5[,'cc_pos'], test5[,'modularity'],ylim=c(0,0.70),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(test6[,'cc_pos'], test6[,'modularity'],ylim=c(0,0.70),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)


plot(test1[,'assort_pos'], test1[,'modularity'],ylim=c(0,0.70),xlim=c(-0.1,0.1), xlab="assort_pos", ylab= "Modularity",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(test2[,'assort_pos'], test2[,'modularity'],ylim=c(0,0.70),xlim=c(-0.1,0.1),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(test3[,'assort_pos'], test3[,'modularity'],ylim=c(0,0.70),xlim=c(-0.1,0.1),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(test4[,'assort_pos'], test4[,'modularity'],ylim=c(0,0.70),xlim=c(-0.1,0.1),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(test5[,'assort_pos'], test5[,'modularity'],ylim=c(0,0.70),xlim=c(-0.1,0.1),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(test6[,'assort_pos'], test6[,'modularity'],ylim=c(0,0.70),xlim=c(-0.1,0.1),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)


plot(test1[,'n_pos'], test1[,'cc_pos'],ylim=c(0.1,0.50),xlim=c(150,500), xlab="n_pos", ylab= "cc_pos",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(test2[,'n_pos'], test2[,'cc_pos'],ylim=c(0.1,0.50),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(test3[,'n_pos'], test3[,'cc_pos'],ylim=c(0.1,0.50),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(test4[,'n_pos'], test4[,'cc_pos'],ylim=c(0.1,0.50),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(test5[,'n_pos'], test5[,'cc_pos'],ylim=c(0.1,0.50),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(test6[,'n_pos'], test6[,'cc_pos'],ylim=c(0.1,0.50),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)


plot(test1[,'n_pos'], test1[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(150,500), xlab="n_pos", ylab= "assort_pos",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(test2[,'n_pos'], test2[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(test3[,'n_pos'], test3[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(test4[,'n_pos'], test4[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(test5[,'n_pos'], test5[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(test6[,'n_pos'], test6[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(150,500),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)


plot(test1[,'cc_pos'], test1[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(0.1,0.50), xlab="cc_pos", ylab= "assort_pos",col=cols[1],bg=cols[1],pch=21)
par(new=TRUE)
plot(test2[,'cc_pos'], test2[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[2],bg=cols[2],pch=21)
par(new=TRUE)
plot(test3[,'cc_pos'], test3[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[3],bg=cols[3],pch=21)
par(new=TRUE)
plot(test4[,'cc_pos'], test4[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[4],bg=cols[4],pch=21)
par(new=TRUE)
plot(test5[,'cc_pos'], test5[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[5],bg=cols[5],pch=21)
par(new=TRUE)
plot(test6[,'cc_pos'], test6[,'assort_pos'],ylim=c(-0.1,0.1),xlim=c(0.1,0.50),xaxt="n", yaxt="n", xlab=" ", ylab= " ",col=cols[6],bg=cols[6],pch=21)

############### END OF PLOT RELATIONSHIPS AMONG METRICS ###############
