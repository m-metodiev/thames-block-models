library(ggraph)
library(ggplot2)
g <- make_ring(10)
g$X = as_adjacency_matrix(g)
g <- add_edges(g, c(
   1,3,  3,5,  5,7,  7,9,  9,10, 10,1
))

g <- add_edges(g, c(
   2,6, 2,8, 2,10
))
#
g$X = as_adjacency_matrix(g)
g$X[g$X==2] = 1

g$X[1,9] = 1
g$X[9,1] = 1

g$X[2,6] = 0
g$X[6,2] = 0

g$X[2,8] = 0
g$X[8,2] = 0

g$X[2,10] = 0
g$X[10,2] = 0

g$X[8,6] = 1
g$X[6,8] = 1

g$X[6,4] = 1
g$X[4,6] = 1

g$X[4,2] = 1
g$X[2,4] = 1

g$X[2,10] = 1
g$X[10,2] = 1

g$X[10,8] = 1
g$X[8,10] = 1

g$X[2,7] = 1
g$X[7,2] = 1

g$X[4,9] = 1
g$X[9,4] = 1

plot_graph = ggraph(graph_from_adjacency_matrix(g$X)) + geom_edge_link() + geom_node_point(size=5)
ggsave("atelier/specificdataset.pdf",
       plot_graph,width=6,height=3,device="pdf")
