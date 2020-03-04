

# Function 1 -- undirected centrality measures function -----

# sna measures undirected is a function that takes a list column data frame with graph objects for sex-specific dyads by year
# and calculates individual's network positions for betweenness, ec, degree, and transitivity
# the output of the function can be a data frame of observations of individual network score by year, or a sociogram ("graph")
# that produces a visualization of the network, with the sna measure defining the size of the vertex 

#tester
#g <- graph_from_data_frame(d = g_data_gm_sex_sep$data[[1]], directed = FALSE)



sna_measures_undir <- function(g,year = NULL, network_sex = NULL, bt_weight = TRUE, ec_weight = TRUE, deg_weight = TRUE, trans_weight = TRUE, output = c("graph", "data.frame")){ # c("male", "female", "any_combination")
  #lapply(list("tidyverse","igraph"), require, character.only = TRUE)
  require(tidyverse)
  require(igraph)
  
  if( grepl("gm", edge_attr(g) %>% names())){
    behavior <- "total_grooming"  
  }
  if( grepl("prox", edge_attr(g) %>% names())){
    behavior <- "prox"
  }
  
  #creates vector for removing edges = 0
  remove <- edge_attr(g)[[1]] == 0 
  #removes
  g <- delete_edges(g, E(g)[remove]) 
  
  #creates edge weight vector
  g_weights <- edge_attr(g)[[1]]
  
  #betweeness
  if(bt_weight == TRUE){
    gb <- betweenness(g, directed = FALSE, normalized = FALSE, weights = g_weights)
  }
  if(bt_weight == FALSE){
    gb <- betweenness(g, directed = FALSE, normalized = FALSE, weights = NULL)
  }
  
  #eigenvector centrality
  if(ec_weight == TRUE){
  ge <- eigen_centrality(g, weights = g_weights)$vector
  }
  if(ec_weight == FALSE){
  ge <- eigen_centrality(g)$vector
  }
  
  #degree and strength (weighted degree)
  if(deg_weight == TRUE){
  gd <- strength(g) }
  
  if(deg_weight == FALSE){
  gd <- degree(g) }
  
  #local transitivity
  if(trans_weight == TRUE){
    gt <- transitivity(g, vids = vertex_attr(g)[[1]] , type = "local", weights = g_weights) 
  }
  if(trans_weight == FALSE){
    gt <- transitivity(g, vids = vertex_attr(g)[[1]] , type = "local")
  }
  
  gt <- ifelse(is.nan(gt), 0, gt)
  
  if(output == "graph"){
    # have to reinclude "name" or that attr overwritten
    vertex_attr(g) <- list(name = vertex_attr(g)$name, bt = gb, ec = ge, deg = gd, trans = gt)
    return(g)
  }
  if(output == "data.frame"){
    
    df <- data.frame(chimp_id = names(gb), year, network_sex, behavior, network_type = "undirected", bt = gb, ec = ge, deg = gd, trans = gt, stringsAsFactors = FALSE)
    #? remove year & dyad sex, see if in final can just remove list cols "data" and "graph" and then unnest the df that remains
    return(df)
  }
}

#test
#g <- x$graph[[1]]
#sna_measures_undir(g, output = "data.frame")


# Function 2 -- plotting -------

# the plot graph function takes the graph/sociogram output from the sna_measures_undir function and creates
# a sociogram, arguments year, dyads, size centrality supplied manually, & behavior taken from graph object itself

#maybe change where function gets these arguments - maybe instead supply df, c



plot_graph <- function(g, year, dyads, behavior = c("total_grooming", "prox"),
                       size_centrality = c("ec", "bt", "deg", "trans"),
                       layout = c("fr","circle","nicely"), scale_edge_weight = 5, scale_vertex_size = 1) {
  lapply(list("tidyverse","igraph"), require, character.only = TRUE)
  
  
  #set vertex size
  if(size_centrality == "ec" & scale_vertex_size == 1){
    scale_vertex_size <- 10
  }
  
  if(size_centrality == "bt" & dyads == "any_combo"){
    scale_vertex_size <- 0.1
  }
  
  if(size_centrality == "bt" & dyads != "any_combo"){
    scale_vertex_size <- 0.3
  }
  
  #vertex size
  size <- g %>% vertex_attr() %>% .[[size_centrality]]*scale_vertex_size
 
  #set edge weight
  w <- g %>% edge_attr() %>% .[[1]]/scale_edge_weight 
  
 # extract behavior for plot title
  if( grepl("gm", edge_attr(gdf$graph[[1]]) %>% names())){
    behavior <- "total_grooming"  
  }
  if( grepl("prox", edge_attr(gdf$graph[[1]]) %>% names())){
    behavior <- "prox"
  }
  
  #set up layout setting
  if(layout == "fr"){
    layout_setting <- layout_with_fr(g)
  }
  if(layout == "circle"){
    layout_setting <- layout_in_circle(g)
  }
  if(layout == "nicely"){
    layout_setting <- layout_nicely(g)
  }
  
  #plot sociogram
  pg <- plot(g, 
             vertex.label.color = "black", 
             vertex.label = vertex_attr(g, "names"), 
             vertex.size = size,
             edge.color = "blue", 
             edge.width = w, 
             layout = layout_setting,
             main = paste(dyads, behavior, year, size_centrality, sep = " "))
  print(pg)
  
}

#save(sna_measures_undir, plot_graph, file = "functions/functions - SNA measures and graph plotting.Rdata")
