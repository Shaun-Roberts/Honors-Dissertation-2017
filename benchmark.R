#benchmarking, I totatly did this ages ago. Honest
library(microbenchmark)
library(sna)
library(igraph)
library(ergm)
library(intergraph)
library(parallel)

sizes = c(30, 100, 250, 500, 750, 1000, 2500)
edgeprobs = 0.2


time.check = function(sizes, edgeprob){
  set.seed(648)
  times = list(sna.distance <- vector(),
               sna.density <- vector(),
               sna.close <- vector(),
               sna.between <- vector(),
               sna.eigen <- vector(),
               sna.bon <- vector(),
               igraph.distance <- vector(),
               igraph.density <- vector(),
               igraph.close <- vector(),
               igraph.between <- vector(),
               igraph.eigen <- vector(),
               igraph.bon <- vector()
  )
  for(j in 1:length(edgeprobs)){
    for(i in 1:length(sizes)){
      size = sizes[i]
      edgeprob = edgeprobs[j]
      graph = rgraph(size, tprob = edgeprob,  mode = "graph")
      graph = as.network.matrix(graph)
      #SNA
      #Distance:
      times$sna.distance[[i]] <- summary(microbenchmark(tryCatch(sna::geodist(graph), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Density:
      times$sna.density[[i]] <- summary(microbenchmark(tryCatch(sna::network.density(graph), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Closeness Centrality:
      times$sna.close[[i]] <- summary(microbenchmark(tryCatch(sna::closeness(graph, gmode = "graph"), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Betweenness Centrality:
      times$sna.between[[i]] <- summary(microbenchmark(tryCatch(sna::betweenness(graph, gmode = "graph"), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Eigenvector centrality:
      times$sna.eigen[[i]] <- summary(microbenchmark(tryCatch(sna::evcent(graph, gmode = "graph"), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Bonacinch Power Centrality:
      times$sna.bon[[i]] <- summary(microbenchmark(tryCatch(sna::bonpow(graph, gmode = "graph", tol = 1e-07), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      
      #Igraph
      graph = intergraph::asIgraph(graph)
      #Distance:
      times$igraph.distance[[i]] <- summary(microbenchmark(tryCatch(igraph::distances(graph), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Density:
      times$igraph.density[[i]] <- summary(microbenchmark(tryCatch(igraph::edge.density(graph), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Closeness centrality
      times$igraph.close[[i]] <- summary(microbenchmark(tryCatch(igraph::closeness(graph), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Betweenness centrality:
      times$igraph.between[[i]] <- summary(microbenchmark(tryCatch(igraph::betweenness(graph, directed = F), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Eigenvector centrality:
      times$igraph.eigen[[i]] <- summary(microbenchmark(tryCatch(igraph::eigen_centrality(graph, directed = F), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      #Bonacinch Power Centrality:
      times$igraph.bon[[i]] <- summary(microbenchmark(tryCatch(igraph::power_centrality(graph), error = function(e) NA, tol = 1e-07), times = 10, unit = "s"))$min
      print(sizes[i])
    }
    save(times, file = paste("benchmark",edgeprob, sep = ""))
  }
}

time.check(sizes, edgeprobs)


install.packages("microbenchmark")
