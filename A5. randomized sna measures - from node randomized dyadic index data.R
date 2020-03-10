library(tidyverse)
library(igraph)
library(lmerTest)
source("functions/functions - age sex modeling.R")
z. <- function(x) scale(x)
# within years, randomly sample and replace nodes of networks
# create 1000 randomized versions of 2009 - 2017 networks
# save to extract network measures on
# turn network measures into distrubtions to then test for signicance in e.g. integration in a given year,
# changes in integration from year to year, etc.



# 1. Node randomized graphs - undirected networks -------------

load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)


set.seed(100)
list_ran_undir_sna_measure_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_w[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, everything())
}  

#save(list_ran_undir_sna_measure_w, file = "data/ran - node (sex age chimp_id) randomized sna measures undirected prox and gmgmd.Rdata")


# ---- groom ran models for mixed net, age-sex coefs -----------

# node ran data
load("data/ran - node (sex age chimp_id) randomized sna measures undirected prox and gmgmd.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_age_b_int <- vector("list", length = 1000)
list_sex_b_int <- vector("list", length = 1000)
list_int_b_int <- vector("list", length = 1000)
  

#loop
t <- Sys.time()
for(i in 1:1000) {

  
ran_df <- list_ran_undir_sna_measure_w[[i]]  
# run models
gm_mixed_w <- ran_df %>%
    age_sex_fun_all(., beh = "total_grooming", net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_w <- ran_df %>%  
  age_sex_fun_all(., beh = "total_grooming", net_sex = "any_combo", sex_age_int = T, summary = T )


# extract coefficient of model
list_age_b[[i]] <- ex_coef(gm_mixed_w, "age")
list_sex_b[[i]] <- ex_coef(gm_mixed_w, "sex")
list_age_b_int[[i]] <- ex_coef(gm_mixed_int_w, "age")
list_sex_b_int[[i]] <- ex_coef(gm_mixed_int_w, "sex")
list_int_b_int[[i]] <- ex_coef(gm_mixed_int_w, "int")
}
Sys.time() - t

gmgmd_age_b <- do.call("rbind", list_age_b) %>% data.frame()
gmgmd_sex_b <- do.call("rbind", list_sex_b) %>% data.frame()
gmgmd_age_int_b <- do.call("rbind", list_age_b_int) %>% data.frame()
gmgmd_sex_int_b <- do.call("rbind", list_sex_b_int) %>% data.frame()
gmgmd_int_int_b <- do.call("rbind", list_int_b_int) %>% data.frame()

#save(gmgmd_age_b, gmgmd_sex_b, gmgmd_age_int_b, gmgmd_sex_int_b, gmgmd_int_int_b, file = "data/random coefs age sex on grooming sna.Rdata")

# ---- prox ran models for mixed net, age-sex coefs ----------

# node ran data
load("data/ran - node (sex age chimp_id) randomized sna measures undirected prox and gmgmd.Rdata", verbose = T)


# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_age_b_int <- vector("list", length = 1000)
list_sex_b_int <- vector("list", length = 1000)
list_int_b_int <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_w[[i]]  
  
  # run models
  prox_mixed_w <- ran_df %>%
    age_sex_fun_all(., beh = "prox", net_sex = "any_combo", sex_age_int = F, summary = T )
  prox_mixed_int_w <- ran_df %>%  
    age_sex_fun_all(., beh = "prox", net_sex = "any_combo", sex_age_int = T, summary = T )
  
  
  # extract coefficient of model
  list_age_b[[i]] <- ex_coef(prox_mixed_w, "age")
  list_sex_b[[i]] <- ex_coef(prox_mixed_w, "sex")
  list_age_b_int[[i]] <- ex_coef(prox_mixed_int_w, "age")
  list_sex_b_int[[i]] <- ex_coef(prox_mixed_int_w, "sex")
  list_int_b_int[[i]] <- ex_coef(prox_mixed_int_w, "int")
}
Sys.time() - t # 20 min!

prox_age_b <- do.call("rbind", list_age_b) %>% data.frame()
prox_sex_b <- do.call("rbind", list_sex_b) %>% data.frame()
prox_age_int_b <- do.call("rbind", list_age_b_int) %>% data.frame()
prox_sex_int_b <- do.call("rbind", list_sex_b_int) %>% data.frame()
prox_int_int_b <- do.call("rbind", list_int_b_int) %>% data.frame()


save(prox_age_b, prox_sex_b, prox_age_int_b, prox_sex_int_b, prox_int_int_b, file = "data/random coefs age sex on prox mixed sex net sna.Rdata")

# ---- groom ran models for same-sex nets, age coef ------

list_age_b_f <- vector("list", length = 1000)
list_age_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_w[[1]]  
  
  # run models
  gmgmd_mixed_w_f <- ran_df %>%
    age_fun_all(., beh = "total_grooming", net_sex = "female", summary = T )
  gmgmd_mixed_w_m<- ran_df %>%  
    age_fun_all(., beh = "total_grooming", net_sex = "male", summary = T )
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(gmgmd_mixed_w_f, "age")
  list_age_b_m[[i]] <- ex_coef(gmgmd_mixed_w_m, "age")
}
Sys.time() - t #12.7 min

gmgmd_age_b_f <- do.call("rbind", list_age_b_f) %>% data.frame()
gmgmd_age_b_m <- do.call("rbind", list_age_b_m) %>% data.frame()

#save(gmgmd_age_b_f, gmgmd_age_b_m, file = "data/random coefs age on gmgmd same sex sna.Rdata")

# ---- prox ran models for same-sex nets, age coef ------
list_age_b_f <- vector("list", length = 1000)
list_age_b_m <- vector("list", length = 1000)

set.seed(100)
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_w[[1]]  
  
  # run models
  prox_mixed_w_f <- ran_df %>%
    age_fun_all(., beh = "prox", net_sex = "female", summary = T )
  prox_mixed_w_m<- ran_df %>%  
    age_fun_all(., beh = "prox", net_sex = "male", summary = T )
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(prox_mixed_w, "age")
  list_age_b_m[[i]] <- ex_coef(prox_mixed_w, "age")
}

Sys.time() - t # 12 min

prox_age_b_f <- do.call("rbind", list_age_b_f) %>% data.frame()
prox_age_b_m <- do.call("rbind", list_age_b_m) %>% data.frame()

#save(prox_age_b_f, prox_age_b_m, file = "data/random coefs age on prox same sex sna.Rdata")








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


#  Randomized undirected graph data -------
load("data/attribute data alone.Rdata")
load("data/indices - annual dyadic grooming.Rdata", verbose = T)
load("data/indices - annual dyadic 5m proximity.Rdata", verbose = T)
source("functions/functions - data preparation.R")
source("functions/functions - sna measures and plot.R")
# XXX Edge permutations ----------------
#steps
# 1 - permute/randomize edges(!) in index data:
# --- adjust what sex combos of dyads are groupings
# --- within years, randomly sample each ID1 and ID2

list_e_ran_sna_measure_df <- vector("list", length = 1000)

# do this 1000 f'ing times
set.seed <- 100

t <- Sys.time()
for(k in seq(list_ran_sna_measure_df)){
  
  
  ran_gdf_gmgmd_sex_comb <- total_gm_gmd_index %>%
    mutate(dyad_sex = "any_combo") %>% #change from sex specific to any combination of sexes (orig in 3.3)
    #sample/randomize individuals within years
    group_by(year) %>% #no grouping by sex, dyad designation is any combination of partner sexes
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    ungroup() %>%
    # nest dyadic data in list column (o.g. data script 3.3)
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
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
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
    mutate(dyad_sex = "any_combo") %>% #change from sex specific to any combination of sexes (orig in 3.3)
    #sample/randomize individuals within years
    group_by(year) %>% #no grouping by sex, dyad designation is any combination of partner sexes
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    ungroup() %>%
    # nest dyadic data in list column (o.g. data script 3.3)
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
    group_by(year, dyad_sex) %>%
    mutate(RID1 = sample(ID1), RID2 = sample(ID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # eep, do this a few times to remove instance of RID1 = RID2
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>%
    mutate(RID2 = ifelse(RID2 == RID1, sample(ID2), RID2)) %>% # opportunity for recursion (if any ID1 & 2 still equal, then sample again)
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
Sys.time() - t # 17.25 now?? bc all sex attribute is in and 09-10 merged? ;takes about 6.9 minutes to create

list_e_ran_sna_measure_df <- list_ran_sna_measure_df


#save(list_e_ran_sna_measure_df, file = "data/sna dataframes - edge randomized graphs.Rdata")

