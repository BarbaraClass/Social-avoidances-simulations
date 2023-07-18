#Set parameter values
params.in <- data.frame(expand.grid(pn.in = c(0.4),pa.in = c(0,0.4,0.8), pr.in = 0.01))

N <- 50			# Number of individuals
burn.in <- 1000 #Number of generations/time steps (converges when approx 10 times the network size)

library(RColorBrewer)

par(mfrow=c(1,3), mar=c(6,6,1,1))
cols=brewer.pal(3,"Blues")

#Run the loop
for (i in 1:nrow(params.in)){

    network.orig<-matrix(0,N,N)
    network.orig[lower.tri(network.orig)]<-sample(c(0,1,-1),N*(N-1)/2,replace=T,prob = c(0.8,0.1,0.1))
    network.orig[upper.tri(network.orig)]<-t(network.orig)[upper.tri(network.orig)]#Duplicates the lower triangle part into the upper triangle
    
    diag(network.orig)=0.5
    
    for (zz in 1:burn.in) {
      output <- try(update.network(network.orig, params.in$pn.in[i], params.in$pa.in[i], params.in$pr.in[i]))
      if(is(output,"try-error"))  break
      
      network.orig <- output$network.new#THIS UPDATES THE NETWORK
      
      #Network of positive relationships (friendships)
      pos = network.orig
      pos[pos!= 1] = 0
      
      if (zz==burn.in){
        
        gplot(pos,usearrows = FALSE,edge.lwd=2,vertex.col=cols[3],edge.col = cols[3],vertex.border=cols[3],main= " ",ylab=" ",cex.lab=2)
        
      }
      
    }
    
  }


