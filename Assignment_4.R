##############################Question 4

set.seed(5225)
A2=matrix(nrow=90,ncol=90,data=0)
x2=c(rep(1,times=30),rep(2,times=30),rep(3,times=30))
for(i in 1:89) for(j in (i+1):90){
  if(x2[i]==x2[j]) A2[i,j]=(runif(1)<0.5)+0
  if(x2[i]!=x2[j]) A2[i,j]=(runif(1)<0.1)+0
}

A2=A2+t(A2)
library(ergm)
A2.network=as.network(A2,directed=FALSE)
A2.network%v%"xval"=x2
A2.network

############################# (a)
library(igraph)
graph = graph_from_adjacency_matrix(A2, mode = "undirected")
#plot(graph, vertex.color = "lightblue", vertex.size = 2, 
#    vertex.label.cex = 1.2, vertex.label.dist = 0.5, edge.width = 1.5,
#   main = "Network Graph")

transitivity_value = transitivity(graph)
print(transitivity_value)

density_value = edge_density(graph)
print(paste("Density:", density_value))

# Method 2: alculate directly
# n = nrow(A2)
# num_edges = sum(A2) / 2
# total_possible_edges = n * (n - 1) / 2
# density = num_edges / total_possible_edges
# print(density)

############################# (b)
num_simulations = 999
n = nrow(A2)
generate_random_graph = function(A2) {
  degree_sequence = degree(graph_from_adjacency_matrix(A2, mode = "undirected"))
  random_graph = sample_degseq(degree_sequence, method = "vl")
  return(random_graph)
}
num_greater_transitivity = 0
for (i in 1:num_simulations) {
  random_graph = generate_random_graph(A2)
  random_transitivity = transitivity(random_graph)
  if (random_transitivity >= transitivity_value) {
    num_greater_transitivity = num_greater_transitivity + 1
  }
}
monte_carlo_p_value = num_greater_transitivity / num_simulations
monte_carlo_p_value


######### (c)
A2.indep=ergm(A2.network ~ edges + match("xval"))
summary(A2.indep)

######### (d)
A2.ergm = ergm(A2.network ~ edges + gwesp(1.1, fixed = TRUE) + gwdsp(2, fixed = TRUE))
summary(A2.ergm)
#A2.ergm=ergm(A2.network~edges+kstar(2)+degree(1))
#summary(A2.ergm)
#mcmc.diagnostics(A2.ergm)

######### (e)
summary(A2.network~edges+gwesp(1.1,fixed=TRUE)+gwdegree(2,fixed=TRUE)+triangle+kstar(2))
N=90*89/2
exp(1.1)*(1-(1-exp(-1.1))^88)*N
exp(2)*(1-(1-exp(-2))^89)*90
alpha=exp(2)*(1-(1-exp(-2))^c(1:6))
alpha
inc=alpha[2:6]-alpha[1:5]
inc
A2.dep=formula(A2.network~edges+gwesp(1.1,fixed=TRUE)+gwdegree(2,fixed=TRUE)+nodematch("xval"))
summary(A2.dep)
set.seed(2)
laz.ergm=ergm(A2.dep)
summary(laz.ergm)
