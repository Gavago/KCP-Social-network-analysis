# Playing with various network options from igraph datacamp tutorial

# for sig test Randomized graphs w erdos renyi
load("data/sna graphs - weighted measures, name & sna measures as vector attributes, index as edge weight.Rdata", verbose = T)

# proper randomization for evaluating change integration with time?
# sna data frame with individual nodes randomized within network type network sexes and year. is same as -->
# annual graphs with node labels randomized
# calc sna centralities n trans per node per year
# each of these data frame outputs is 1 randomized progression per individual

g <- gdf_gm_sex_comb$graph_w_sna[[1]]


# peak inside tings - handy igraph functions -----
edge_attr(g) %>% names()
vertex_attr(g)
V(g1)
E(g1)
gorder(g1)
gsize(g1)


# Global ------
# part 3 in curley igraph DC
# a - global randos holding size and density constant ----
# erdos renyi creates random network, useful for evaluating significance of total network property
ran_graph <- function(g) {
  ran_graph <- erdos.renyi.game(n = gorder(g), p.or.m = edge_density(g), type = "gnp")
  return(ran_graph)
}
erdos.renyi.game(n = gorder(g), p.or.m = edge_density(g), type = "gnp")
ran_graph(g)
sna_measures_undir(ran_graph(g), output = "dataframe") # doesn't work bc vars like year etc unnavailable

sna_measures_undir

# b - triangles ------
# counting triangles of node isn't quite same thing as local transitivity, bc node A could have more partners forming triangles than node B
# but A could have lower proportion of total partners forming cliques than B (and so lower local trans than B)
g <- gdf_gm_sex_comb$graph_w_sna[[1]]
count_triangles(g, vids = vertex.attributes(g)$name)

# c - see largest cliques in network ---------
lc <- largest.cliques(g)
lc

gs1 <- subgraph(g, lc[[1]])
gs2 <- subgraph(g, lc[[2]])


par(mfrow = c(1,2))
plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 2",
     layout = layout.circle(gs2))
     

# d - assortativity --------

# who is attracted to whom?
# pos values means attraction, neg values mean avoidance..

assortativity(g, vertex_attr(g)$deg)
assortativity(g, vertex_attr(g)$ec)
assortativity(g, vertex_attr(g)$sex)
#no strong attractions! maybe rank would do it

# e - reciprocity ----------
# proportion of 
# must be directed... here of course is 1 bc graph undirected
reciprocity(g)


# randomizing vertex names -----
# this maintains network size, density, and individual tendencies for connection
# works just the same as swapping names in final sna dataframe.... so best to just do what already doing.


g1 <- g
vertex_attr(g1)$name <- vertex_attr(g)$name %>% sample()
vertex_attr(g1)$bt

g_weights <- edge_attr(g1)[[1]]
betweenness(g1, weights = g_weights)


