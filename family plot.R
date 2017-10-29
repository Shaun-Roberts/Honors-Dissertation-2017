library(igraph)
n = 9
a <- matrix(rep(0, n^2), nrow = n)
a[1,2:9] <- 1
aa <- graph_from_adjacency_matrix(a, mode = c("undirected"), diag = F)
l = layout_as_star(aa, center = V(aa)[1])
pdf("star_graph.pdf")
plot(aa,vertex.label = LETTERS[1:9], 
     edge.color = "black", vertex.color = "grey", vertex.label.color = "black",
     edge.width = 1.2, layout = l)
dev.off()

a <- matrix(rep(0, 16), nrow = 4)
a[1, 2:4] <- 1
a[2, 4] <- 1
aa <- graph_from_adjacency_matrix(a, mode = c("undirected"), diag = F)
pdf("friends.pdf")
V(aa)$label.cex <- c(2.5)
plot(aa, vertex.label = c("Grace", "Robert", "Alice", "Steve"),
     edge.color = "black", 
     vertex.color = "grey", 
     vertex.shape = "rectangle", 
     vertex.label.color = "black",
     vertex.size = 70,
     vertex.size2 = 25)
dev.off()
