library(tidyverse)
library(purrr)
library(igraph)
library(viridis)
select <- dplyr::select


# options for shiny plots
# sexes
# year(s)
# weighted, yes/no
# filter edge level
# graph layout


# Graph function: steps each year:
#  extract data subset and create graph
#  SNA measures  
#  plot - weight edges, size node by sna measure

# Function 1 -- undirected centrality measures function -----

# sna measures undirected is a function that takes a list column data frame with graph objects for sex-specific dyads by year
# and calculates individual's network positions for betweenness, ec, degree, and transitivity
# the output of the function can be a data frame of observations of individual network score by year, or a sociogram ("graph")
# that produces a visualization of the network, with the sna measure defining the size of the vertex 

sna_measures_undir <- function(g,year = NULL, network_sex = NULL, output = c("graph", "data.frame")){ # c("male", "female", "any_combination")
  
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
    
    df <- data.frame(chimp_id = names(gb), year, network_sex, behavior, network_type = "undirected", bt = gb, ec = ge, deg = gd, trans = gt)
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

# 1. Calculate SNA measures -----

# -- 1a. Transform list column data into igraphs for each either year or sex-year ------
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)
load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)
# see data where "data" column is a list of dataframes of dyadic association indices from either that year (sex_comb for sexes combined)
# of for dyads of a given sex (sex_sep for sexes separated) in a given year

names(g_data_gm_sex_sep)
names(g_data_gm_sex_comb)


gdf_gm_sex_sep <- g_data_gm_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

x <- gdf_gm_sex_sep %>%
filter(year == 2017 & dyad_sex == "male") %>%
mutate(sna_measures = map(graph, sna_measures_undir, year = year, network_sex = dyad_sex, output = "data.frame"))
x$sna_measures

gdf_gm_sex_comb <- g_data_gm_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

  
gdf_prox_sex_sep <- g_data_prox_sex_sep %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

gdf_prox_sex_comb <- g_data_prox_sex_comb %>%
  #list cols graph
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

gdf_gm_sex_sep$graph_w_sna[[12]] %>% vertex_attr()

#save(gdf_gm_sex_comb, gdf_prox_sex_comb, gdf_gm_sex_sep, gdf_prox_sex_sep , file = "data/sna graphs -  name & sna measures as vector attributes, index as edge weight.Rdata")


# 2. SNA measure data frames ----
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)
load("data/graph dataframes with sna measures ready for plot and analysis.Rdata", verbose = T)
load("data/attribute data alone.Rdata", verbose = T)

#create master data frame with all individual sna measures by year

#list of list columns (!)
graph_list <- list(gdf_gm_sex_sep, gdf_gm_sex_comb, gdf_prox_sex_sep, gdf_prox_sex_comb) 

all_sna_measure_df_list <- vector("list", length(graph_list))

for(j in 1:length(graph_list)){
  gdf <- graph_list[[j]] 
  n <- nrow(gdf)
  measures_list <- vector("list", length = n)
  
  for(i in seq(n)) {
    g <- gdf$graph[[i]]
    y <- gdf$year[[i]]
    s <- gdf$dyad_sex[[i]]
    measures_list[[i]] <- sna_measures_undir(g, year = y, network_sex = s, output = "data.frame")
  }
  df <- do.call("rbind", measures_list)
  all_sna_measure_df_list[[j]] <- df
}

all_sna_measure_df <- do.call("rbind", all_sna_measure_df_list) %>%
  left_join(attr, by = "chimp_id") %>%
  select(-date_of_birth_corrected) %>%
  add_age(dyad = FALSE)
  

#save(all_sna_measure_df, file = "data/sna dataframe - individual sna measure for each year, network sex, & behavior.Rdata")

# 3. Create pdf sociograms from igraphs in list column  ----
load("data/sna graphs -  name & sna measures as vector attributes, index as edge weight.Rdata", verbose = TRUE)
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)


#problem here is that pdf function comes before calling the graph requires object


# outside of loop, set graph dataframe to visualize / save
gdf <- gdf_gm_sex_sep
gdf <- gdf_gm_sex_comb
gdf <- gdf_prox_sex_sep
gdf <- gdf_prox_sex_comb


#list of list column dfs
#for each df, go through each year and print

# socs <- gdf_prox_sex_comb %>%
#   mutate( behavior = ifelse( grepl("gm", names(.data$data[[1]][,3])), "total_grooming", "prox")) %>%
#    map(graph_w_sna, plot_graph, year = year, behavior = behavior, layout = "nicely", size_centrality = "ec")


#each element of this list is list col data of given behavior and dyad sexes for each year of focal data
gm_prox_graph_list<- list(gdf_gm_sex_sep, gdf_gm_sex_comb, gdf_prox_sex_sep, gdf_prox_sex_comb)


#settings for title and plot graph function, ec bt trans deg
size_centrality_x <- "bt"
layout_setting <- "nicely"


for (i in seq(length(gm_prox_list))){ # for each graph type
  
  gdf <- gm_prox_graph_list[[i]] # choose graph df (w list col of annual graphs)
  behavior = ifelse( grepl("gm", names(gdf$data[[1]][,3])), "total_grooming", "prox") #determine behavior type
  subfolder = ifelse( "any_combo" %in% gdf$dyad_sex, "MF_combined/", "MM_FF_separate/") #and save location

for (j in seq(nrow(gdf))) { #for each sex-year graph

    #and plot sociogram
  title <- paste( gdf$dyad_sex[[j]], gdf$year[[j]], behavior, size_centrality_x, sep = "_")
  pdf(paste0("sociograms/",subfolder, title, ".pdf"))
  
  gdf$graph_w_sna[[j]] %>% plot_graph(., behavior = behavior, dyads = gdf$dyad_sex[[j]],
                                      year = gdf$year[[j]], size_centrality = size_centrality_x, layout = layout_setting,
                                      scale_edge_weight = 1, scale_vertex_size = 1)
  dev.off()
  
  }
}


# peak inside all tings - handy igraph functions -----
g1 <- gdf$graph_w_sna[[1]]
edge_attr(g1) %>% names()
vertex_attr(g1)
V(g1)
E(g1)
gorder(g1)
gsize(g1)





# g yard ----


# adding attributes piecemeal ------

# undirected gm_gmd network, year X, male
names(male_gmgmdi)
years <- unique(male_gmgmdi$year)
years

sub <- male_gmgmdi %>%
  filter(year == 2009)

# prep
mpairs <- sub %>%
  select(ID1, ID2) %>%
  as.matrix()
weights <- sub %>%
  pull(gmgmdi)


g <- graph.edgelist(mpairs, directed = F)
g <- set_edge_attr(g, "gm_time", value = weights) #set_vertex_attr


g <- grg.game(100, 0.2)
g

# gave up on making sna measure a color
V(g1)$color <- vertex_attr(g1)$bt
V(g1)$color <- magma()


years <- unique(df$year)
sexes <- c("M", "F", "both")

i <- 1

for (i in (seq(sexes))){
  
  annual_graphs <- vector("list", length = length(years))
  
  for (j in seq(years)){
    
    yr <- years[j]
    
    if (sexes[i] == "M" | sexes[i] == "F"){
      g  <- df %>%
        filter(sexA == sexes[i] & sexB == sexes[i]) %>%
        filter(year == yr) %>%
        select(ID1,ID2, gmgmdi)
    }
    if (sex == "both")  {
      g <- df %>% 
        filter(year == year) %>%
        select(ID1,ID2, gmgmdi)
    }
    
    annual_graphs[[j]] <- 
  }
}


# labels
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x))) }

lab.locs <- radian.rescale(x=1:gorder(g1), direction= -1, start=0)

plot(g1, vertex.label.color = "black",
     vertex.label.dist = 1,
     edge.color = "blue", edge.width = w, layout = layout_in_circle(g))
