library(igraph)
library(xtable)
a <- matrix(rep(0, 13^2), nrow = 13)
a[1:5,6] = 1
a[6,7]=1
a[7,8] = 1
a[9:13,8] = 1
aa <- graph_from_adjacency_matrix(a, mode = c("undirected"), diag = F)
l2 = matrix(c(0,4,
              1,4,
              2,4,
              3,4,
              4,4,
              2,3,
              2,2,
              2,1,
              0,0,
              1,0,
              2,0,
              3,0,
              4,0), nrow = 13, byrow = T)
V(aa)$label.cex <- c(2.5)
pdf("Centralities_Graph.pdf")
plot(aa,
     vertex.label = LETTERS[1:13],
     edge.color = "black", 
     vertex.color = "grey", 
     vertex.label.color = "black",
     vertex.shape = "rectangle", 
     edge.width = 1.2, 
     vertex.size = 20,
     vertex.size2 = 20,
     layout = l2)
dev.off()
gest = aa
sub = c(1,6,7)
mat = matrix(ncol = 3, nrow = 8)
gest = intergraph::asIgraph(gest)
mat[2,] = igraph::closeness(gest)[sub]
mat[3,] = igraph::betweenness(gest, directed = F)[sub]
mat[4,] = igraph::eigen_centrality(gest, directed = F)$vector[sub]
gest = intergraph::asNetwork(gest)
mat[1,] = sna::degree(gest, gmode = "graph")[sub]
mat[5,] = sna::gilschmidt(gest, gmode = "graph")[sub]
mat[6,] = sna::infocent(gest, gmode = "graph")[sub]
mat[7,] = sna::loadcent(gest, gmode = "graph")[sub]
mat[8,] = sna::bonpow(gest, gmode = "graph", tol = 1e-07)[sub]
row.names(mat) = c("Degree", "Closeness", "Betweenness", "Eigenvector", "Gil-Schmidt Power", "Information", "Load", "Bonicich")
mat
xtable(mat, digits = 2)
