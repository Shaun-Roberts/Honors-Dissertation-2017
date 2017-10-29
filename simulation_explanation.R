library("igraph")
library("Matrix")
set.seed(4)
ud = rgraph(6, tprob = 0.5, mode = "graph")
ud = as.network.matrix(ud, directed = F)
ud = intergraph::asIgraph(ud)
lay = layout_in_circle(ud, order = c(4,3,2,1,6,5))
V(ud)$label.cex <- c(2.5)
pdf("simulation1.pdf")
plot(ud, layout = lay, edge.color = "black", 
     vertex.color = "lemonchiffon", vertex.label.color = "black", 
     vertex.size = 40)
dev.off()
ud = intergraph::asNetwork(ud)
#We record the metrics at this point
f.pos.rate = 0.1

ud = as.matrix.network.adjacency(ud)
  
f.pos.which = which(ud[upper.tri(ud)] == 0)
  
f.pos.which.length = length(f.pos.which)
  
f.pos.arcs = sample(f.pos.which, ceiling(f.pos.rate*f.pos.which.length))

ud[upper.tri(ud)][f.pos.arcs] = 1
ud = as.matrix(forceSymmetric(ud))

ud = as.network.matrix(ud, directed = F)
ud = intergraph::asIgraph(ud)
V(ud)$label.cex <- c(2.5)
pdf("simulation2.pdf")
plot(ud, layout = lay, edge.color = "black", 
     vertex.color = "lemonchiffon", vertex.label.color = "black", 
     vertex.size = 40)
dev.off()
ud = intergraph::asNetwork(ud)
#we record the metrics at this point.
ud = as.matrix.network.adjacency(ud)
f.neg.which = which(ud[upper.tri(ud)] == 1)
f.neg.which.length = length(f.neg.which)
f.neg.arcs = sample(f.neg.which, ceiling(0.01*f.neg.which.length))
us = ud
ud[upper.tri(ud)][f.neg.arcs] = 0
ud = as.matrix(forceSymmetric(ud))
ud = as.network.matrix(ud, directed = F)
ud = intergraph::asIgraph(ud)
V(ud)$label.cex <- c(2.5)
pdf("simulation3.pdf")
plot(ud, layout = lay, edge.color = "black", 
     vertex.color = "lemonchiffon", vertex.label.color = "black", 
     vertex.size = 40)
dev.off()
ud = intergraph::asNetwork(ud)
#We record the metrics at this point
ud = as.matrix.network.adjacency(ud)
f.neg.which = which(ud[upper.tri(ud)] == 1)
f.neg.which.length = length(f.neg.which)
f.neg.arcs = sample(f.neg.which, ceiling(0.01*f.neg.which.length))
us = ud
ud[upper.tri(ud)][f.neg.arcs] = 0
ud = as.matrix(forceSymmetric(ud))
ud = as.network.matrix(ud, directed = F)
ud = intergraph::asIgraph(ud)
V(ud)$label.cex <- c(2.5)
pdf("simulation4.pdf")
plot(ud, layout = lay, edge.color = "black", 
     vertex.color = "lemonchiffon", vertex.label.color = "black", 
     vertex.size = 40)
dev.off()
