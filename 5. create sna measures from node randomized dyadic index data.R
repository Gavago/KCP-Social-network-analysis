library(tidyverse)
library(igraph)

# within years, randomly sample and replace nodes of networks
# create 1000 randomized versions of 2009 - 2017 networks
# save to extract network measures on
# turn network measures into distrubtions to then test for signicance in e.g. integration in a given year,
# changes in integration from year to year, etc.

load("data/attribute data alone.Rdata")
load("data/indices - annual dyadic grooming.Rdata", verbose = T)
load("data/indices - annual dyadic 5m proximity.Rdata", verbose = T)
#load("data/sna graphs -  name & sna measures as vector attributes, index as edge weight.Rdata", verbose = TRUE)
load("functions/functions - data preparation.Rdata", verbose = T)
load("functions/functions - SNA measures and graph plotting.Rdata", verbose = T)


#  Randomized undirected graphs -------
list_ran_sna_measure_df <- vector("list", length = 1000)

# do this 1000 f'ing times

t <- Sys.time()
for(k in seq(list_ran_sna_measure_df)){
  
  
ran_gdf_gmgmd_sex_comb <- total_gm_gmd_index %>%
  #sample/randomize individuals within years
  group_by(year) %>%
  mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
  ungroup() %>%
  # nest dyadic data in list column (o.g. data script 3.3)
  mutate(dyad_sex = "any_combo") %>% #change from sex specific to anything combination
  select(year, dyad_sex, RID1, RID2, gmgmdi) %>%
  nest(data = c(RID1, RID2, gmgmdi)) %>%
  arrange(year) %>%
  # turn list col dyadic data into graphs (o.g. data in script 4.1)
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

ran_gdf_gmgmd_sex_sep <- total_gm_gmd_index %>%
  #sample/randomize individuals within years and sex dyad type
  filter(dyad_sex != "mixed") %>% #keep only sex matching dyads (o.g. filter in script 3.3)
  group_by(year, dyad_sex) %>%
  mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
  ungroup() %>%
  # nest dyadic data in list column (o.g. data script 3.3)
  select(year, dyad_sex, RID1, RID2, gmgmdi) %>%
  nest(data = c(RID1, RID2, gmgmdi)) %>%
  arrange(year) %>%
  # turn list col dyadic data into graphs (o.g. data in script 4.1)
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

ran_gdf_prox_sex_comb <- index_5m %>%
  #sample/randomize individuals within years
  group_by(year) %>%
  mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
  ungroup() %>%
  # nest dyadic data in list column (o.g. data script 3.3)
  mutate(dyad_sex = "any_combo") %>%
  select(year, dyad_sex, RID1, RID2, prox5i) %>%
  nest(data = c(RID1, RID2, prox5i)) %>%
  arrange(year) %>%
  # turn list col dyadic data into graphs (o.g. data in script 4.1)
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  #add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

ran_gdf_prox_sex_sep <- index_5m %>%
  #sample/randomize individuals within years and dyad sexes
  filter(dyad_sex != "mixed") %>% #keep only sex matching dyads (o.g. filter in script 3.3)
  group_by(year) %>%
  mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
  ungroup() %>%
  # nest dyadic data in list column (o.g. data script 3.3)
  select(year, dyad_sex, RID1, RID2, prox5i) %>%
  nest(data = c(RID1, RID2, prox5i)) %>%
  arrange(year) %>%
  # turn list col dyadic data into graphs (o.g. data in script 4.1)
  mutate(graph = map(data, function(x) graph_from_data_frame(d = x, directed = FALSE))) %>%
  # add sna attributes to vertices
  mutate(graph_w_sna = map(graph, sna_measures_undir, network_sex = dyad_sex, output = "graph"))

#create master data frame with all individual sna measures by year

ran_graph_list <- list(ran_gdf_gmgmd_sex_sep, ran_gdf_gmgmd_sex_comb, ran_gdf_prox_sex_sep, ran_gdf_prox_sex_comb) 

ran_sna_measure_df_list <- vector("list", length(ran_graph_list))

for(j in 1:length(ran_graph_list)){
  gdf <- ran_graph_list[[j]] 
  n <- nrow(gdf)
  measures_list <- vector("list", length = n)
  
  for(i in seq(n)) {
    g <- gdf$graph[[i]]
    y <- gdf$year[[i]]
    s <- gdf$dyad_sex[[i]]
    measures_list[[i]] <- sna_measures_undir(g, year = y, network_sex = s, output = "data.frame")
  }
  df <- do.call("rbind", measures_list)
  ran_sna_measure_df_list[[j]] <- df
}

ran_sna_measure_df <- do.call("rbind", ran_sna_measure_df_list) %>%
  left_join(attr %>% select(chimp_id, sex, ends_with("id"), dobc, dfs, dls), by = "chimp_id") %>%
  add_age(dyad = FALSE) %>%
  arrange(year, network_sex, behavior, chimp_id)


list_ran_sna_measure_df[[k]] <- ran_sna_measure_df

}
#Sys.time() - t # takes about 6.9 minutes to create

#save(list_ran_sna_measure_df, file = "data/sna dataframes - measures based on node randomized graphs.Rdata")

# gyard -----------
#check that sampling happens within years
ID12009 <- ran_total_gm_gmd_index %>%
  filter(year == 2009) %>%
  pull(ID1) %>%
  unique()
RID12009 <- ran_total_gm_gmd_index %>%
  filter(year == 2009) %>%
  pull(RID1) %>% sort() %>%
  unique()
ID12009 %in% RID12009

#same w map?
#load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)


RID1 <- sample(total_gm_gmd_index$ID1)
