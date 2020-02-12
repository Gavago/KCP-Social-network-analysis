library(tidyverse)
library(purrr)
library(igraph)
library(viridis)
select <- dplyr::select
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)

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


# 1. Calculate SNA measures -----

# -- 1a. Transform list column data into igraphs for each either year or sex-year ------
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

#test df option, doesn't work w map
# x <- gdf_gm_sex_sep %>%
# filter(year == 2017 & dyad_sex == "male") %>%
# mutate(sna_measures = map(graph, sna_measures_undir, year = year, network_sex = dyad_sex, output = "data.frame"))
# x$sna_measures

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
load("functions/functions - data preparation.Rdata", verbose = T)
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)
load("data/sna graphs -  name & sna measures as vector attributes, index as edge weight.Rdata", verbose = T)
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
  left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
  add_age(dyad = FALSE)

all_sna_measure_df

#save(all_sna_measure_df, file = "data/sna dataframe - individual sna measure for each year, network sex, & behavior.Rdata")

# 3. Create pdf sociograms from igraphs in list column  ----
load("data/sna graphs -  name & sna measures as vector attributes, index as edge weight.Rdata", verbose = TRUE)
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)


#list of list column dfs
#for each df, go through each year and print


#each element of this list is list col data of given behavior and dyad sexes for each year of focal data
gm_prox_graph_list<- list(gdf_gm_sex_sep, gdf_gm_sex_comb, gdf_prox_sex_sep, gdf_prox_sex_comb)


#settings for title and plot graph function, ec bt trans deg
size_centrality_x <- "ec"
layout_setting <- "nicely"


for (i in seq(length(gm_prox_graph_list))){ # for each graph type
  
  gdf <- gm_prox_graph_list[[i]] # choose graph df (w list col of annual graphs)
  behavior = ifelse( grepl("gm", names(gdf$data[[1]][,3])), "total_grooming", "prox") #determine behavior type
  sex = ifelse( "any_combo" %in% gdf$dyad_sex, "Sexes_combined", "Sexes_separate")
  #subfolder = ifelse( "any_combo" %in% gdf$dyad_sex, "MF_combined/", "MM_FF_separate/") #and save location
  
  title <- paste(sex, behavior, size_centrality_x, sep = "_")
  pdf(paste0("sociograms/", title, ".pdf"))
  
for (j in seq(nrow(gdf))) { #for each sex-year graph

    #and plot sociogram
  #plot_title <- paste( gdf$dyad_sex[[j]], gdf$year[[j]], behavior, size_centrality_x, sep = "_")
  
  gdf$graph_w_sna[[j]] %>% plot_graph(., behavior = behavior, dyads = gdf$dyad_sex[[j]],
                                      year = gdf$year[[j]], size_centrality = size_centrality_x, layout = layout_setting,
                                      scale_edge_weight = 1, scale_vertex_size = 1)
  }
  dev.off()
}


# peak inside tings - handy igraph functions -----
g1 <- gdf$graph_w_sna[[1]]
edge_attr(g1) %>% names()
vertex_attr(g1)
V(g1)
E(g1)
gorder(g1)
gsize(g1)





# g yard ----

# map for ploting fail
# socs <- gdf_prox_sex_comb %>%
#   mutate( behavior = ifelse( grepl("gm", names(.data$data[[1]][,3])), "total_grooming", "prox")) %>%
#    map(graph_w_sna, plot_graph, year = year, behavior = behavior, layout = "nicely", size_centrality = "ec")




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
