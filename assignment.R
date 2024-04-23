##############################Question 1: 
###################### (a)
# 1-(a) (a)	Can you express the graph in Figure 1 as a bipartite graph? 
# That is divide the nodes into 2 groups such that all edges are between the two groups. 
A = matrix(nrow = 15, ncol = 15, data = 0)
rownames(A) = colnames(A) = c("x1", "x2", "x3", "x4", "x5", "x6", "x7",
                              "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15")
A[1,2] = A[1,3] = A[2,4] = A[2,5] = A[4,8] = A[4,9] = A[5,10] = A[5,11] = 1
A[3,6] = A[3,7] = A[6,12] = A[6,13] = A[7,14] = A[7,15] = 1
A = A + t(A)
A

library(igraph)
graph = graph_from_adjacency_matrix(A, mode = "undirected", weighted = TRUE)
plot(graph, layout = layout.circle, vertex.color = "lightblue", vertex.size = 30, 
     vertex.label.cex = 1.2, vertex.label.dist = 0.5, edge.width = 1.5,
     main = "Network Graph")

B = A%*%A
B