#Set parameter values
params.in <- data.frame(expand.grid(pn.in = c(0.4),pa.in = c(0,0.4,0.8), pr.in = 0.01))

N <- 50			# Number of individuals
n.rep <- 1 # Number of replicates
burn.in <- 1000 #Number of generations/time steps (converges when approx 10 times the network size)

#Store population metrics in an array. rows= generations, layers=replicates
params <- c( 'n_pos', 'n_neg','cc_pos','modularity','n.comp')

params.out<-array (0, c(burn.in,length(params),n.rep,nrow(params.in)))
dimnames(params.out)[[2]]<-params
par(mfrow=c(1,3), mar=c(6,6,1,1))

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
      
      if (rep==1& zz==burn.in){
        
        gplot(pos,usearrows = FALSE,edge.lwd=2,
              xlab=paste0("Pn=",as.character(params.in[i,1])),
              ylab=paste0("Pa=",as.character(params.in[i,2]))) #,
              #main=paste0("Pr=",as.character(params.in[i,3])))
        
      }
      
    }
    
  }

}
