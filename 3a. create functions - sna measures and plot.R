# Function 1 -- undirected centrality measures function -----

# sna measures undirected is a function that takes a list column data frame with graph objects for sex-specific dyads by year
# and calculates individual's network positions for betweenness, ec, degree, and transitivity
# the output of the function can be a data frame of observations of individual network score by year, or a sociogram ("graph")
# that produces a visualization of the network, with the sna measure defining the size of the vertex 

sna_measures_undir <- function(g,year = NULL, network_sex = NULL, output = c("graph", "data.frame")){ # c("male", "female", "any_combination")
  lapply(list("tidyverse","igraph"), require, character.only = TRUE)
  
  
  if( grepl("gm", edge_attr(g) %>% names())){
    behavior <- "total_grooming"  
  }
  if( grepl("prox", edge_attr(g) %>% names())){
    behavior <- "prox"
  }
  
  remove <- edge_attr(g)[[1]] == 0 # create vector for removing edges = 0
  g <- delete_edges(g, E(g)[remove]) # remove
  
  gb <- betweenness(g, directed = F, normalized = F)
  ge <- eigen_centrality(g)$vector
  gd <- degree(g)
  gt <- transitivity(g, vids = vertex_attr(g)[[1]] , type = "local") #local transitivity
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
    scale_vertex_size <- 20
  }
  size <- g %>% vertex_attr() %>% .[[size_centrality]]*scale_vertex_size
  #set edge weight
  w <- g %>% edge_attr() %>% .[[1]]/scale_edge_weight 
  
  #set up components of figure title
  if("dyad_sex" != "any_combination"){
    dyads <- gdf$dyad_sex[[i]]
  } else {
    dyads <- "sexes combined"
  }
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
