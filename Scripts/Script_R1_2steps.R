library(sna)
library(Matrix)
library(igraph)
library(here)

#Set parameter values
pn = 0.8
pa = 0.8
pr = 0.1
pb =1
N <- 50 # Number of individuals
n.rep <- 50 # Number of replicates
burn.in <- 500 #Number of generations/time steps (converges when approx 10 times the network size)
network.orig<-matrix(0,N,N)
network.orig[lower.tri(network.orig)]<-sample(c(0,1,-1),N*(N-1)/2,replace=T,prob = c(0.8,0.1,0.1))
network.orig[upper.tri(network.orig)]<-t(network.orig)[upper.tri(network.orig)]#Duplicates the lower triangle part into the upper triangle
diag(network.orig)=0.5
network=network.orig

############### START  OF THE SIMULATING FUNCTION ###############
update.network <- function(network, pn, pa, pr) {
  
  #One randomly chosen individual dies
  del = sample(1:N, 1)
  
  network <- network[-del,]
  network <- network[,-del]
  N <- N-1
  
  #One randomly chosen individual reproduces
  repro <- sample(1:N, 1)
  
  # Add this new individual
  N <- N + 1
  
  # Generate the relationships of this new individual: by default 0
  rel <- rep(0, N)
  rel[N]<-0.5
  
  # offspring inherits mother's avoidances: depends on the probability to inherit them from mother Pa
  neg.edges <- which(network[repro,]==-1)
  rel[neg.edges] <- sample(c(0,-1),length(neg.edges),prob=c(1-pa,pa),replace=TRUE)
  
  # offspring inherits mother's friends: depends on the probability to inherit them from mother Pn
  pos.edges <- which(network[repro,]==1)
  rel[pos.edges] <- sample(c(0,1),length(pos.edges),prob=c(1-pn,pn),replace=TRUE)
  
  #Relationship between the new individual and its parent
  rel[repro] <- 1
  
  #potential new partners (friends or avoidances) an individual can then have (unknown to its mother)
  no.edges <- which(network[repro,]==0)
  
  
  if (length(no.edges)>0){
    #combine offspring's relationships and new potential partners' relationships
    all.rel<-cbind(rel[-N],network[,no.edges])
    all.rel<-all.rel[-repro,]#remove relationships with mother, as these were accounted for in the 1st step
    colnames(all.rel)<-c("offspring", no.edges)
    partner=all.rel[,1]
    partner[which (partner==-1)]=0#individuals cannot inherit relationships from avoidances so put as zero
    all.rel[,1]=partner
    
    #product of relationships A-B and B-C (A= offspring, B= offspring's inherited partners, C= potential new partners)
    no_rel_prod<-all.rel[,1] * all.rel[,2:ncol(all.rel)]
    
    #classify individuals into neutral, indirect friend, or indirect avoidance
    neutral<-names(which(apply(no_rel_prod,2,function(x)length(which(x==0))==nrow(no_rel_prod))))#column only contains zeros
    n_indirect_avoid=apply(no_rel_prod,2,function(x)length(which(x==-1)))#each new potential partner: how many times is it an indirect avoidance
    n_indirect_friend=apply(no_rel_prod,2,function(x)length(which(x==1)))#each new potential partner: how many times is it an indirect friend
    
    n_indirect_avoid=n_indirect_avoid[-which(names(n_indirect_avoid) %in% neutral)]#remove zeros
    n_indirect_friend=n_indirect_friend[-which(names(n_indirect_friend) %in% neutral)]#remove zeros
    relationships=rbind(n_indirect_avoid,n_indirect_friend)
    relationships=(relationships[1,]/colSums(relationships))*100#calculate proportions of indirect friend vs. avoidance for each individual
    
    indirect_avoid<-names(which(relationships>=50))#if >=50% indirect avoidance, it will become an indirect avoidance (inheritance determined by Pa)
    indirect_friend<-names(which(relationships<50))#if less, it will become an indirect friend (inheritance determined by Pn)
    
    #Offspring makes new connections with these individuals based on Pr, Pb, and Pa
    rel[as.numeric(neutral)] <- sample(c(0,1,-1),length(neutral),prob=c(1-2*pr,pr,pr),replace=TRUE)#neutral individuals can become anything
    rel[as.numeric(indirect_avoid)] <- sample(c(0,-1),length(indirect_avoid),prob=c(1-pa,pa),replace=TRUE)#inherit avoidances from friends
    rel[as.numeric(indirect_friend)] <- sample(c(0,1),length(indirect_friend),prob=c(1-pn,pn),replace=TRUE)#inherit friends from friends
    
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
params.in <- data.frame(expand.grid(pn.in = c(0.3,0.4,0.5,0.6,0.7,0.8),pa.in = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), pr.in = c(0.001,0.01,0.05)))

N <- 50			# Number of individuals
n.rep <- 100 # Number of replicates
burn.in <- 1000 #Number of generations/time steps (converges when approx 10 times the network size)

#Store population metrics in an array. rows= generations, layers=replicates
params <- c( 'n_pos', 'n_neg','cc_pos','modularity','n.comp')

params.out<-array (0, c(burn.in,length(params),n.rep,nrow(params.in)))
dimnames(params.out)[[2]]<-params
par(mfrow=c(2,3), mar=c(6,6,1,1))

#Store individual degrees in an array
degree.mat=array (NA,c(n.rep,N,nrow(params.in)))

#Run the loop
for (i in 1:nrow(params.in)){
  
  
  for (rep in 1:n.rep){
    network.orig<-matrix(0,N,N)
    network.orig[lower.tri(network.orig)]<-sample(c(0,1,-1),N*(N-1)/2,replace=T,prob = c(0.8,0.1,0.1))
    network.orig[upper.tri(network.orig)]<-t(network.orig)[upper.tri(network.orig)]#Duplicates the lower triangle part into the upper triangle
    
    diag(network.orig)=0.5
    
    for (zz in 1:burn.in) {
      output <- try(update.network(network.orig, params.in$pn.in[i], params.in$pa.in[i], params.in$pr.in[i]))
      if(is(output,"try-error"))  break
      
      network.orig <- output$network.new#THIS UPDATES THE NETWORK
      
      #Extract values from the simulated network 
      params.out[zz,"n_pos",rep,i] = length(network.orig[network.orig==1])  #Number of friendships
      params.out[zz,"n_neg",rep,i] = length(network.orig[network.orig==-1])  #Number of avoidances
      
      #Network of positive relationships (friendships)
      pos = network.orig
      pos[pos!= 1] = 0
      
      #Network of positive negative relationships (avoidances)
      neg = network.orig
      neg[neg!=-1] = 0
      neg[neg<0] = 1
      
      #Extract network metrics from the positive network. It is the "observed" network
      params.out[zz,"cc_pos",rep,i] = sna::gtrans(pos, mode = "graph")#Clustering coefficient
      
      g<-igraph::graph_from_incidence_matrix(pos)#transform network into a graph
      wc <- igraph::walktrap.community(g) #looking for structure 
      params.out[zz,"modularity",rep,i] = igraph::modularity(wc)#extract modularity
      n.comp<-sum(igraph::components(g)$csize>2)#extract number of groups (individuals)
      params.out[zz,"n.comp",rep,i] = n.comp
      
      if (zz==burn.in){
        #Extract individual degrees at the last iteration
        degree.mat[rep,1:N,i]=sna::degree(pos, gmode = "graph")}#Degree distribution
      
      #if (rep==1& zz==burn.in){
      #  
      #  gplot(pos,usearrows = FALSE,edge.lwd=2,
      #        xlab=paste0("Pn=",as.character(params.in[i,1])),
      #        ylab=paste0("Pa=",as.character(params.in[i,2])),
      #        main=paste0("Pr=",as.character(params.in[i,3])))
      #  
      #}
      
    }
    cat(paste0(rep," "))
    
  }
  save(params.out, file="Data/params_out_2steps.Rdata")
  save(degree.mat, file="Data/Degree_2steps.Rdata")
}

############### END  OF THE SIMULATIONS ###############