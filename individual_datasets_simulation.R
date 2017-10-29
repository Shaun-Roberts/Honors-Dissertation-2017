library(sna)
library(igraph)
library(ergm)
library(intergraph)
library(parallel)
library("Matrix")
data = c("sampson", "kapferer", "samplk", "faux.mesa.high", "faux.magnolia.high", "ecoli")
data = "ecoli"
data = "sampson"
f.pos.rate =  c(0, 0.01, 0.05, 0.10, 0.15, 0.20)
fresh = expand.grid(data, f.pos.rate)


indiviual.networks = function(data.set.name, f.pos.rate){
  set.seed(2)
  if(data.set.name == "ecoli"){
    data(ecoli)
    gest = ecoli1
  }
  
  if(data.set.name == "sampson"){
    data(sampson)
    gest = samplike
  }
  if(data.set.name =="kapferer"){
    data(kapferer)
    gest = kapferer2
  }
  if(data.set.name == "samplk"){
    data(samplk)
    gest = samplk2
  }
  if(data.set.name == "faux.mesa.high"){
    data("faux.mesa.high")
    gest = faux.mesa.high
  }
  if(data.set.name == "faux.magnolia.high"){
    data("faux.magnolia.high")
    gest = faux.magnolia.high
  }
  pdf(paste(paste("plot_OG", data.set.name, f.pos.rate, sep = "_"), ".pdf", sep = ""))
  gplot(gest, gmode = "graph")
  dev.off()
  
  #Initializing metric measurments:
  record = list(density.rec = list(), 
                distance.rec = list(), 
                degree.dist.rec = list(), 
                edge.con.rec = list(), 
                degree.rec = list(),
                closeness.rec = list(),
                betweenness.rec = list(),
                bonacich.rec = list(),
                eigen.cent.rec = list(),
                gil.sch.rec = list(),
                info.cent.rec = list(),
                load.rec = list(),
                klein.auth.rec = list()
  )
  #step 2:
  #initial measure metrics:
  {
    #Transformation function to change from sna to igraph:
    gest = intergraph::asIgraph(gest)
    #Dens1ty:
    record$density.rec[1] = tryCatch(igraph::edge_density(gest), error = function(e) NA)
    #Distance:
    record$distance.rec[[1]] = tryCatch(igraph::distances(gest), error = function(e) NA)
    #Degree distribution:
    record$degree.dist.rec[[1]] = tryCatch(igraph::degree.distribution(gest, mode = "total"), error = function(e) NA)
    #Edge connectivity
    record$edge.con.rec[[1]] = tryCatch(igraph::edge_connectivity(gest), error = function(e) NA)
    #Closeness Centrality
    record$closeness.rec[[1]] = tryCatch(igraph::closeness(gest), error = function(e) NA)
    #Betweenness centrality:
    record$betweenness.rec[[1]] <- tryCatch(igraph::betweenness(gest, directed = F), error = function(e) NA, tol = 1e-07)
    #Eigenvecotr centrality:
    record$eigen.cent.rec[[1]] <- tryCatch(igraph::eigen_centrality(gest, directed = F)$vector, error = function(e) NA, tol = 1e-07)
    
    ####################### SNA metrics:
    
    #Transformation function to change from igraph to sna:
    gest = intergraph::asNetwork(gest)
    #Degree centrality:
    record$degree.rec[[1]] = tryCatch(sna::degree(gest, gmode = "graph"), error = function(e) NA)
    #Gil-Schmidt Power index:
    record$gil.sch.rec[[1]] = tryCatch(sna::gilschmidt(gest, gmode = "graph"), error = function(e) NA)
    #Information Centrality:
    record$info.cent.rec[[1]] = tryCatch(sna::infocent(gest, gmode = "graph"), error = function(e) NA)
    #Load Centrality:
    record$load.rec[[1]] = tryCatch(sna::loadcent(gest, gmode = "graph"), error = function(e) NA)
    #Bonacinch Power Centrality:
    record$bonacich.rec[[1]] = tryCatch(sna::bonpow(gest, gmode = "graph", tol = 1e-07), error = function(e) NA)
  }
  #step 3:
  #Apply initial false positive
  {
    gest = as.matrix.network.adjacency(gest)
    
    f.pos.which = which(gest[upper.tri(gest)] == 0)
    
    f.pos.which.length = length(f.pos.which)
    
    f.pos.arcs = sample(f.pos.which, ceiling(f.pos.rate*f.pos.which.length))
    
    gest[upper.tri(gest)][f.pos.arcs] = 1
    
    gest = as.matrix(forceSymmetric(gest))
    
    gest = as.network.matrix(gest)
  }
  for(i in 2:51){
    #step 4: metrics:                           #
    #Run metrics on the graph and record them:
    #Transformation function to change from sna to igraph:
    {
      gest = intergraph::asIgraph(gest)
      ##################### Igraph metrics:
      #Density:
      record$density.rec[i] = tryCatch(igraph::edge_density(gest), error = function(e) NA)
      #Distance:
      record$distance.rec[[i]] = tryCatch(igraph::distances(gest), error = function(e) NA)
      #Degree distribution:
      record$degree.dist.rec[[i]] = tryCatch(igraph::degree.distribution(gest, mode = "total"), error = function(e) NA)
      #Edge connectivity
      record$edge.con.rec[[i]] = tryCatch(igraph::edge_connectivity(gest), error = function(e) NA)
      #Closeness Centrality
      record$closeness.rec[[i]] = tryCatch(igraph::closeness(gest), error = function(e) NA)
      #Betweenness centrality:
      record$betweenness.rec[[i]] <- tryCatch(igraph::betweenness(gest, directed = F), error = function(e) NA, tol = 1e-07)
      #Eigenvecotr centrality:
      record$eigen.cent.rec[[i]] <- tryCatch(igraph::eigen_centrality(gest, directed = F)$vector, error = function(e) NA, tol = 1e-07)
      ####################### SNA metrics:
      
      #Transformation function to change from igraph to sna:
      gest = intergraph::asNetwork(gest)
      #Degree centrality:
      record$degree.rec[[i]] = tryCatch(sna::degree(gest, gmode = "graph"), error = function(e) NA)
      #Gil-Schmidt Power index:
      record$gil.sch.rec[[i]] = tryCatch(sna::gilschmidt(gest, gmode = "graph"), error = function(e) NA)
      #Information Centrality:
      record$info.cent.rec[[i]] = tryCatch(sna::infocent(gest, gmode = "graph"), error = function(e) NA)
      #Load Centrality:
      record$load.rec[[i]] = tryCatch(sna::loadcent(gest, gmode = "graph"), error = function(e) NA)
      #Bonacinch Power Centrality:
      record$bonacich.rec[[i]] = tryCatch(sna::bonpow(gest, gmode = "graph", tol = 1e-07), error = function(e) NA)
    }
    {
      #Step 5: only false negatives               #
      gest = as.matrix.network.adjacency(gest)
      
      f.neg.which = which(gest[upper.tri(gest)] == 1)
      
      f.neg.which.length = length(f.neg.which)
      
      f.neg.arcs = sample(f.neg.which, ceiling(0.01*f.neg.which.length))
      
      gest[upper.tri(gest)][f.neg.arcs] = 0
      
      gest = as.matrix(forceSymmetric(gest))
      
      gest = as.network.matrix(gest)
    }
  }
  save(record, file = paste("OG", data.set.name, f.pos.rate, 0.01, sep = "_"))
}

mcmapply(FUN = indiviual.networks, fresh[,1], fresh[,2], mc.cores = 1)
