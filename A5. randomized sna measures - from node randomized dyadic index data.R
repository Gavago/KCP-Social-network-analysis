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


# 1. Node randomized graphs ----
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)


# 1a. undirected networks -------------

# weighted - for model both sexes ----
set.seed(100)
list_ran_undir_sna_measure_both_sex_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, rank, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_both_sex_w[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, everything())
}  

#save(list_ran_undir_sna_measure_both_sex_w, file = "data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata")

# weighted - for model sex sep ----
# undir ran for each sex in mixed sex net, and for same sex, either way sexes are analyzed separately
# and randomizations of relevant variables are done within sex
set.seed(100)
list_ran_undir_sna_measure_sex_sep_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, rank, and prop_cyc randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_sex_sep_w[[i]] <- all_sna_measure_df_w %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_undir_sna_measure_sex_sep_w, file = "data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata")

# unweighted - for model both sexes ---- 
set.seed(100)
list_ran_undir_sna_measure_both_sex_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_both_sex_uw[[i]] <- all_sna_measure_df_uw %>%
    group_by(year, network_sex, behavior) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_undir_sna_measure_both_sex_uw, file = "data/ran3 - both sexes uw - node (sex age rank chimp_id)randomized sna measures undirected prox and gmgmd unweighted.Rdata")

# unweighted - for model sex sep ---- 
set.seed(100)
list_ran_undir_sna_measure_sex_sep_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_undir_sna_measure_sex_sep_uw[[i]] <- all_sna_measure_df_uw %>%
    group_by(year, network_sex, behavior, sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_undir_sna_measure_sex_sep_uw, file = "data/ran4 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata")


# 1b. directed networks ------

# weighted - for model both sexes -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_w[[i]] <- dir_sna_measure_df_w %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, everything())
}  

#save(list_ran_dir_sna_measure_both_sex_w, file = "data/ran5 - both sexes w - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata")

# weighted - for model sex sep -----
set.seed(100)
list_ran_dir_sna_measure_sex_sep_w <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_sex_sep_w[[i]] <- dir_sna_measure_df_w %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_dir_sna_measure_sex_sep_w, file = "data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata")


# unweighted - for model both sexes -----
set.seed(100)
list_ran_dir_sna_measure_both_sex_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_both_sex_uw[[i]] <- dir_sna_measure_df_uw %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), sex = sample(sex)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, everything())
}  

#save(list_ran_dir_sna_measure_both_sex_uw, file = "data/ran7 - both sexes uw - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata")

# unweighted - for model sex sep -----
set.seed(100)
list_ran_dir_sna_measure_sex_sep_uw <- vector("list", length = 1000)
# Undirected prox gm networks, node attributes of id, age, and sex randomized
for(i in 1:1000) {
  
  # randomize nodes
  list_ran_dir_sna_measure_sex_sep_uw[[i]] <- dir_sna_measure_df_uw %>%
    group_by(year, network_sex) %>% #this holds constant number of individs in community and individual social tendencies within that year (e.g. individ A tendency to groom w many partners that year and have high deg)
    mutate(chimp_id = sample(chimp_id), age_mid_year = sample(age_mid_year), avg_rank = sample(avg_rank), prop_cyc = sample(prop_cyc)) %>%
    ungroup() %>%
    select(chimp_id, age_mid_year, sex, avg_rank, prop_cyc, everything())
}  

#save(list_ran_dir_sna_measure_sex_sep_uw, file = "data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata")



# 2. Coefficient extraction ----
# -- mixed sex networks -----
# ---- total grooming ran models for mixed net weighted -----------

# node ran data
load("data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_rank_b <- vector("list", length = 1000)
list_int_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {

  
ran_df <- list_ran_undir_sna_measure_both_sex_w[[i]]  
# run models
both_gmgmd_mixed_w <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "any_combo", subj_sex = "both", sex_age_int = T, summary = T )

# extract coefficient of model
list_age_b[[i]] <- ex_coef(both_gmgmd_mixed_w, "age")
list_sex_b[[i]] <- ex_coef(both_gmgmd_mixed_w, "sex")
list_rank_b[[i]] <- ex_coef(both_gmgmd_mixed_w, "rank")
list_int_b[[i]] <- ex_coef(both_gmgmd_mixed_w, "int")
}
Sys.time() - t #12.5

both_gmgmd_mixed_age_b <- do.call("rbind", list_age_b) %>% data.frame()
both_gmgmd_mixed_sex_b <- do.call("rbind", list_sex_b) %>% data.frame()
both_gmgmd_mixed_rank_b <- do.call("rbind", list_rank_b) %>% data.frame()
both_gmgmd_mixed_age_sex_int_b <- do.call("rbind", list_int_b) %>% data.frame()

# save(both_gmgmd_mixed_age_b,
# both_gmgmd_mixed_sex_b,
# both_gmgmd_mixed_rank_b,
# both_gmgmd_mixed_age_sex_int_b, file = "data/random coefs - mixed both w - undir gmgmd.Rdata")

# ---- total grooming ran models for mixed net - sex sep - weighted
# node ran data
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# storage for betas
f_list_age_b <- vector("list", length = 1000)
f_list_rank_b <- vector("list", length = 1000)
f_list_prop_cyc_b <- vector("list", length = 1000)
m_list_age_b <- vector("list", length = 1000)
m_list_rank_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_w[[i]]  
  # run models
  f_gmgmd_mixed_w <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
  m_gmgmd_mixed_w <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "any_combo", subj_sex = "M", summary = T )
  
  # extract coefficient of model
  f_list_age_b[[i]] <- ex_coef(f_gmgmd_mixed_w, "age")
  f_list_rank_b[[i]] <- ex_coef(f_gmgmd_mixed_w, "rank")
  f_list_prop_cyc_b[[i]] <- ex_coef(f_gmgmd_mixed_w, "prop_cyc")
  
  m_list_age_b[[i]] <- ex_coef(m_gmgmd_mixed_w, "age")
  m_list_rank_b[[i]] <- ex_coef(m_gmgmd_mixed_w, "rank")

}
Sys.time() - t # 14.8 min

f_gmgmd_mixed_age_b <- do.call("rbind", f_list_age_b) %>% data.frame()
f_gmgmd_mixed_rank_b <- do.call("rbind", f_list_rank_b) %>% data.frame()
f_gmgmd_mixed_prop_cyc_b <- do.call("rbind", f_list_prop_cyc_b) %>% data.frame()

m_gmgmd_mixed_age_b <- do.call("rbind", m_list_age_b) %>% data.frame()
m_gmgmd_mixed_rank_b <- do.call("rbind", m_list_rank_b) %>% data.frame()

# save(f_gmgmd_mixed_age_b,
# f_gmgmd_mixed_rank_b,
# f_gmgmd_mixed_prop_cyc_b,
# m_gmgmd_mixed_age_b,
# m_gmgmd_mixed_rank_b,
# file = "data/random coefs - mixed sex sep w - undir gmgmd.Rdata")

# ---- total grooming ran models for mixed net unweighted -----------

# node ran data
load("data/ran3 - both sexes uw - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_rank_b <- vector("list", length = 1000)
list_int_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  
  ran_df <- list_ran_undir_sna_measure_both_sex_uw[[i]]  
  # run models
  both_gmgmd_mixed_uw <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "any_combo", subj_sex = "both", sex_age_int = T, summary = T )
  
  # extract coefficient of model
  list_age_b[[i]] <- ex_coef(both_gmgmd_mixed_uw, "age")
  list_sex_b[[i]] <- ex_coef(both_gmgmd_mixed_uw, "sex")
  list_rank_b[[i]] <- ex_coef(both_gmgmd_mixed_uw, "rank")
  list_int_b[[i]] <- ex_coef(both_gmgmd_mixed_uw, "int")
}
Sys.time() - t #12.8

both_gmgmd_mixed_age_b_uw <- do.call("rbind", list_age_b) %>% data.frame()
both_gmgmd_mixed_sex_b_uw <- do.call("rbind", list_sex_b) %>% data.frame()
both_gmgmd_mixed_rank_b_uw <- do.call("rbind", list_rank_b) %>% data.frame()
both_gmgmd_mixed_age_sex_int_b_uw <- do.call("rbind", list_int_b) %>% data.frame()

# save(both_gmgmd_mixed_age_b_uw,
# both_gmgmd_mixed_sex_b_uw,
# both_gmgmd_mixed_rank_b_uw,
# both_gmgmd_mixed_age_sex_int_b_uw, file = "data/random coefs - mixed both uw - undir gmgmd.Rdata")

# ---- total grooming ran models for mixed net - sex sep - unweighted
# node ran data
load("data/ran4 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata", verbose = T)

# storage for betas
f_list_age_b <- vector("list", length = 1000)
f_list_rank_b <- vector("list", length = 1000)
f_list_prop_cyc_b <- vector("list", length = 1000)
m_list_age_b <- vector("list", length = 1000)
m_list_rank_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_uw[[i]]  
  # run models
  f_gmgmd_mixed_uw <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
  m_gmgmd_mixed_uw <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "any_combo", subj_sex = "M", summary = T )
  
  # extract coefficient of model
  f_list_age_b[[i]] <- ex_coef(f_gmgmd_mixed_uw, "age")
  f_list_rank_b[[i]] <- ex_coef(f_gmgmd_mixed_uw, "rank")
  f_list_prop_cyc_b[[i]] <- ex_coef(f_gmgmd_mixed_uw, "prop_cyc")
  
  m_list_age_b[[i]] <- ex_coef(m_gmgmd_mixed_uw, "age")
  m_list_rank_b[[i]] <- ex_coef(m_gmgmd_mixed_uw, "rank")
  
}
Sys.time() - t #15.6

f_gmgmd_mixed_age_b_uw <- do.call("rbind", f_list_age_b) %>% data.frame()
f_gmgmd_mixed_rank_b_uw <- do.call("rbind", f_list_rank_b) %>% data.frame()
f_gmgmd_mixed_prop_cyc_b_uw <- do.call("rbind", f_list_prop_cyc_b) %>% data.frame()

m_gmgmd_mixed_age_b_uw <- do.call("rbind", m_list_age_b) %>% data.frame()
m_gmgmd_mixed_rank_b_uw <- do.call("rbind", m_list_rank_b) %>% data.frame()

# save(f_gmgmd_mixed_age_b_uw,
# f_gmgmd_mixed_rank_b_uw,
# f_gmgmd_mixed_prop_cyc_b_uw,
# m_gmgmd_mixed_age_b_uw,
# m_gmgmd_mixed_rank_b_uw,
# file = "data/random coefs - mixed sex sep uw - undir gmgmd.Rdata")

# ---- prox ran models for mixed net weighted -----------

# node ran data
load("data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_rank_b <- vector("list", length = 1000)
list_int_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_both_sex_w[[i]]  
  # run models
  both_prox_mixed_w <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "any_combo", subj_sex = "both", sex_age_int = T, summary = T )
  
  # extract coefficient of model
  list_age_b[[i]] <- ex_coef(both_prox_mixed_w, "age")
  list_sex_b[[i]] <- ex_coef(both_prox_mixed_w, "sex")
  list_rank_b[[i]] <- ex_coef(both_prox_mixed_w, "rank")
  list_int_b[[i]] <- ex_coef(both_prox_mixed_w, "int")
}
Sys.time() - t #15.2 min

both_prox_mixed_age_b <- do.call("rbind", list_age_b) %>% data.frame()
both_prox_mixed_sex_b <- do.call("rbind", list_sex_b) %>% data.frame()
both_prox_mixed_rank_b <- do.call("rbind", list_rank_b) %>% data.frame()
both_prox_mixed_age_sex_int_b <- do.call("rbind", list_int_b) %>% data.frame()

# save(both_prox_mixed_age_b,
# both_prox_mixed_sex_b,
# both_prox_mixed_rank_b,
# both_prox_mixed_age_sex_int_b, file = "data/random coefs - mixed both w - undir prox.Rdata")

# ---- prox ran models for mixed net - sex sep - weighted
# node ran data
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# storage for betas
f_list_age_b <- vector("list", length = 1000)
f_list_rank_b <- vector("list", length = 1000)
f_list_prop_cyc_b <- vector("list", length = 1000)
m_list_age_b <- vector("list", length = 1000)
m_list_rank_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_w[[i]]  
  # run models
  f_prox_mixed_w <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "any_combo", subj_sex = "F", summary = T )
  m_prox_mixed_w <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "any_combo", subj_sex = "M", summary = T )
  
  # extract coefficient of model
  f_list_age_b[[i]] <- ex_coef(f_prox_mixed_w, "age")
  f_list_rank_b[[i]] <- ex_coef(f_prox_mixed_w, "rank")
  f_list_prop_cyc_b[[i]] <- ex_coef(f_prox_mixed_w, "prop_cyc")
  
  m_list_age_b[[i]] <- ex_coef(m_prox_mixed_w, "age")
  m_list_rank_b[[i]] <- ex_coef(m_prox_mixed_w, "rank")
  
}
Sys.time() - t # 14.8 min

f_prox_mixed_age_b <- do.call("rbind", f_list_age_b) %>% data.frame()
f_prox_mixed_rank_b <- do.call("rbind", f_list_rank_b) %>% data.frame()
f_prox_mixed_prop_cyc_b <- do.call("rbind", f_list_prop_cyc_b) %>% data.frame()

m_prox_mixed_age_b <- do.call("rbind", m_list_age_b) %>% data.frame()
m_prox_mixed_rank_b <- do.call("rbind", m_list_rank_b) %>% data.frame()

# save(f_prox_mixed_age_b,
# f_prox_mixed_rank_b,
# f_prox_mixed_prop_cyc_b,
# m_prox_mixed_age_b,
# m_prox_mixed_rank_b,
# file = "data/random coefs - mixed sex sep w - undir prox.Rdata")

# ---- prox ran models for mixed net unweighted -----------

# node ran data
load("data/ran3 - both sexes uw - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_rank_b <- vector("list", length = 1000)
list_int_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  
  ran_df <- list_ran_undir_sna_measure_both_sex_uw[[i]]  
  # run models
  both_prox_mixed_uw <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "any_combo", subj_sex = "both", sex_age_int = T, summary = T )
  
  # extract coefficient of model
  list_age_b[[i]] <- ex_coef(both_prox_mixed_uw, "age")
  list_sex_b[[i]] <- ex_coef(both_prox_mixed_uw, "sex")
  list_rank_b[[i]] <- ex_coef(both_prox_mixed_uw, "rank")
  list_int_b[[i]] <- ex_coef(both_prox_mixed_uw, "int")
}
Sys.time() - t

both_prox_mixed_age_b_uw <- do.call("rbind", list_age_b) %>% data.frame()
both_prox_mixed_sex_b_uw <- do.call("rbind", list_sex_b) %>% data.frame()
both_prox_mixed_rank_b_uw <- do.call("rbind", list_rank_b) %>% data.frame()
both_prox_mixed_age_sex_int_b_uw <- do.call("rbind", list_int_b) %>% data.frame()

# save(both_prox_mixed_age_b_uw,
# both_prox_mixed_sex_b_uw,
# both_prox_mixed_rank_b_uw,
# both_prox_mixed_age_sex_int_b_uw, file = "data/random coefs - mixed both uw - undir prox.Rdata")

# ---- prox ran models for mixed net - sex sep - unweighted
# node ran data
load("data/ran4 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata", verbose = T)

# storage for betas
f_list_age_b <- vector("list", length = 1000)
f_list_rank_b <- vector("list", length = 1000)
f_list_prop_cyc_b <- vector("list", length = 1000)
m_list_age_b <- vector("list", length = 1000)
m_list_rank_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_uw[[i]]  
  # run models
  f_prox_mixed_uw <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "any_combo", subj_sex = "F", summary = T )
  m_prox_mixed_uw <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "any_combo", subj_sex = "M", summary = T )
  
  # extract coefficient of model
  f_list_age_b[[i]] <- ex_coef(f_prox_mixed_uw, "age")
  f_list_rank_b[[i]] <- ex_coef(f_prox_mixed_uw, "rank")
  f_list_prop_cyc_b[[i]] <- ex_coef(f_prox_mixed_uw, "prop_cyc")
  
  m_list_age_b[[i]] <- ex_coef(m_prox_mixed_uw, "age")
  m_list_rank_b[[i]] <- ex_coef(m_prox_mixed_uw, "rank")
  
}
Sys.time() - t # 14.8 min

f_prox_mixed_age_b_uw <- do.call("rbind", f_list_age_b) %>% data.frame()
f_prox_mixed_rank_b_uw <- do.call("rbind", f_list_rank_b) %>% data.frame()
f_prox_mixed_prop_cyc_b_uw <- do.call("rbind", f_list_prop_cyc_b) %>% data.frame()

m_prox_mixed_age_b_uw <- do.call("rbind", m_list_age_b) %>% data.frame()
m_prox_mixed_rank_b_uw <- do.call("rbind", m_list_rank_b) %>% data.frame()

# save(f_prox_mixed_age_b_uw,
# f_prox_mixed_rank_b_uw,
# f_prox_mixed_prop_cyc_b_uw,
# m_prox_mixed_age_b_uw,
# m_prox_mixed_rank_b_uw,
# file = "data/random coefs - mixed sex sep uw - undir prox.Rdata")

# ---- directed grooming ran models for mixed net weighted -----------

# node ran data
load("data/ran5 - both sexes w - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_rank_b <- vector("list", length = 1000)
list_int_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  
  ran_df <- list_ran_dir_sna_measure_both_sex_w[[i]]  
  # run models
  both_gm_mixed_w <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "any_combo", subj_sex = "both", sex_age_int = T, summary = T )
  
  # extract coefficient of model
  list_age_b[[i]] <- ex_coef(both_gm_mixed_w, "age")
  list_sex_b[[i]] <- ex_coef(both_gm_mixed_w, "sex")
  list_rank_b[[i]] <- ex_coef(both_gm_mixed_w, "rank")
  list_int_b[[i]] <- ex_coef(both_gm_mixed_w, "int")
}
Sys.time() - t #9.8

both_gm_mixed_age_b <- do.call("rbind", list_age_b) %>% data.frame()
both_gm_mixed_sex_b <- do.call("rbind", list_sex_b) %>% data.frame()
both_gm_mixed_rank_b <- do.call("rbind", list_rank_b) %>% data.frame()
both_gm_mixed_age_sex_int_b <- do.call("rbind", list_int_b) %>% data.frame()

# save(both_gm_mixed_age_b,
# both_gm_mixed_sex_b,
# both_gm_mixed_rank_b,
# both_gm_mixed_age_sex_int_b, file = "data/random coefs - mixed both w - dir gm.Rdata")

# ---- directed grooming ran models for mixed net - sex sep - weighted
# node ran data
load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

# storage for betas
f_list_age_b <- vector("list", length = 1000)
f_list_rank_b <- vector("list", length = 1000)
f_list_prop_cyc_b <- vector("list", length = 1000)
m_list_age_b <- vector("list", length = 1000)
m_list_rank_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_dir_sna_measure_sex_sep_w[[i]]  
  # run models
  f_gm_mixed_w <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
  m_gm_mixed_w <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "any_combo", subj_sex = "M", summary = T )
  
  # extract coefficient of model
  f_list_age_b[[i]] <- ex_coef(f_gm_mixed_w, "age")
  f_list_rank_b[[i]] <- ex_coef(f_gm_mixed_w, "rank")
  f_list_prop_cyc_b[[i]] <- ex_coef(f_gm_mixed_w, "prop_cyc")
  
  m_list_age_b[[i]] <- ex_coef(m_gm_mixed_w, "age")
  m_list_rank_b[[i]] <- ex_coef(m_gm_mixed_w, "rank")
  
}
Sys.time() - t #  min

f_gm_mixed_age_b <- do.call("rbind", f_list_age_b) %>% data.frame()
f_gm_mixed_rank_b <- do.call("rbind", f_list_rank_b) %>% data.frame()
f_gm_mixed_prop_cyc_b <- do.call("rbind", f_list_prop_cyc_b) %>% data.frame()

m_gm_mixed_age_b <- do.call("rbind", m_list_age_b) %>% data.frame()
m_gm_mixed_rank_b <- do.call("rbind", m_list_rank_b) %>% data.frame()

# save(f_gm_mixed_age_b,
# f_gm_mixed_rank_b,
# f_gm_mixed_prop_cyc_b,
# m_gm_mixed_age_b,
# m_gm_mixed_rank_b,
# file = "data/random coefs - mixed sex sep w - dir gm.Rdata")

# ---- directed grooming ran models for mixed net unweighted -----------

# node ran data
load("data/ran7 - both sexes uw - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

# storage for betas
list_age_b <- vector("list", length = 1000)
list_sex_b <- vector("list", length = 1000)
list_rank_b <- vector("list", length = 1000)
list_int_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  
  ran_df <- list_ran_dir_sna_measure_both_sex_uw[[i]]  
  # run models
  both_gm_mixed_uw <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "any_combo", subj_sex = "both", sex_age_int = T, summary = T )
  
  # extract coefficient of model
  list_age_b[[i]] <- ex_coef(both_gm_mixed_uw, "age")
  list_sex_b[[i]] <- ex_coef(both_gm_mixed_uw, "sex")
  list_rank_b[[i]] <- ex_coef(both_gm_mixed_uw, "rank")
  list_int_b[[i]] <- ex_coef(both_gm_mixed_uw, "int")
}
Sys.time() - t #

both_gm_mixed_age_b_uw <- do.call("rbind", list_age_b) %>% data.frame()
both_gm_mixed_sex_b_uw <- do.call("rbind", list_sex_b) %>% data.frame()
both_gm_mixed_rank_b_uw <- do.call("rbind", list_rank_b) %>% data.frame()
both_gm_mixed_age_sex_int_b_uw <- do.call("rbind", list_int_b) %>% data.frame()

# save(both_gm_mixed_age_b_uw,
# both_gm_mixed_sex_b_uw,
# both_gm_mixed_rank_b_uw,
# both_gm_mixed_age_sex_int_b_uw, file = "data/random coefs - mixed both uw - dir gm.Rdata")

# ---- directed grooming ran models for mixed net - sex sep - unweighted
# node ran data

load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)

# storage for betas
f_list_age_b <- vector("list", length = 1000)
f_list_rank_b <- vector("list", length = 1000)
f_list_prop_cyc_b <- vector("list", length = 1000)
m_list_age_b <- vector("list", length = 1000)
m_list_rank_b <- vector("list", length = 1000)

#loop
t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_dir_sna_measure_sex_sep_uw[[i]]  
  # run models
  f_gm_mixed_uw <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
  m_gm_mixed_uw <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "any_combo", subj_sex = "M", summary = T )
  
  # extract coefficient of model
  f_list_age_b[[i]] <- ex_coef(f_gm_mixed_uw, "age")
  f_list_rank_b[[i]] <- ex_coef(f_gm_mixed_uw, "rank")
  f_list_prop_cyc_b[[i]] <- ex_coef(f_gm_mixed_uw, "prop_cyc")
  
  m_list_age_b[[i]] <- ex_coef(m_gm_mixed_uw, "age")
  m_list_rank_b[[i]] <- ex_coef(m_gm_mixed_uw, "rank")
  
}
Sys.time() - t #15.6

f_gm_mixed_age_b_uw <- do.call("rbind", f_list_age_b) %>% data.frame()
f_gm_mixed_rank_b_uw <- do.call("rbind", f_list_rank_b) %>% data.frame()
f_gm_mixed_prop_cyc_b_uw <- do.call("rbind", f_list_prop_cyc_b) %>% data.frame()

m_gm_mixed_age_b_uw <- do.call("rbind", m_list_age_b) %>% data.frame()
m_gm_mixed_rank_b_uw <- do.call("rbind", m_list_rank_b) %>% data.frame()

# save(f_gm_mixed_age_b_uw,
# f_gm_mixed_rank_b_uw,
# f_gm_mixed_prop_cyc_b_uw,
# m_gm_mixed_age_b_uw,
# m_gm_mixed_rank_b_uw,
# file = "data/random coefs - mixed sex sep uw - dir gm.Rdata")

# -- same sex networks -----
# ---- total grooming ran models for same-sex nets weighted ------

load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

list_age_b_f <- vector("list", length = 1000)
list_rank_b_f <- vector("list", length = 1000)
list_prop_cyc_b_f <- vector("list", length = 1000)

list_age_b_m <- vector("list", length = 1000)
list_rank_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_w[[i]]  
  
  # run models
  f_gmgmd_same_w <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "female", summary = T )
  m_gmgmd_same_w <- age_sex_fun_all(ran_df, beh = "total_grooming",  net_sex = "male", summary = T)
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(f_gmgmd_same_w, "age")
  list_rank_b_f[[i]] <- ex_coef(f_gmgmd_same_w, "rank")
  list_prop_cyc_b_f[[i]] <- ex_coef(f_gmgmd_same_w, "prop_cyc")
  
  list_age_b_m[[i]] <- ex_coef(m_gmgmd_same_w, "age")
  list_rank_b_m[[i]] <- ex_coef(m_gmgmd_same_w, "rank")
}
Sys.time() - t #16.7 min

f_gmgmd_same_age_b <- do.call("rbind", list_age_b_f) %>% data.frame()
f_gmgmd_same_rank_b <- do.call("rbind", list_rank_b_f) %>% data.frame()
f_gmgmd_same_prop_cyc_b <- do.call("rbind", list_prop_cyc_b_f) %>% data.frame()

m_gmgmd_same_age_b <- do.call("rbind", list_age_b_m) %>% data.frame()
m_gmgmd_same_rank_b <- do.call("rbind", list_rank_b_m) %>% data.frame()

# save(f_gmgmd_same_age_b,
# f_gmgmd_same_rank_b,
# f_gmgmd_same_prop_cyc_b,
# m_gmgmd_same_age_b,
# m_gmgmd_same_rank_b, file = "data/random coefs - same sex sep w - undir gmgmd.Rdata")

# ---- total grooming ran models for same-sex nets unweighted ------
load("data/ran4 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata", verbose = T)

list_age_b_f <- vector("list", length = 1000)
list_rank_b_f <- vector("list", length = 1000)
list_prop_cyc_b_f <- vector("list", length = 1000)

list_age_b_m <- vector("list", length = 1000)
list_rank_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_uw[[i]]  
  
  # run models
  f_gmgmd_same_uw <- age_sex_fun_all(ran_df, beh = "total_grooming", net_sex = "female", summary = T )
  m_gmgmd_same_uw <- age_sex_fun_all(ran_df, beh = "total_grooming",  net_sex = "male", summary = T)
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(f_gmgmd_same_uw, "age")
  list_rank_b_f[[i]] <- ex_coef(f_gmgmd_same_uw, "rank")
  list_prop_cyc_b_f[[i]] <- ex_coef(f_gmgmd_same_uw, "prop_cyc")
  
  list_age_b_m[[i]] <- ex_coef(m_gmgmd_same_uw, "age")
  list_rank_b_m[[i]] <- ex_coef(m_gmgmd_same_uw, "rank")
}
Sys.time() - t #12.7 min

f_gmgmd_same_age_b_uw <- do.call("rbind", list_age_b_f) %>% data.frame()
f_gmgmd_same_rank_b_uw <- do.call("rbind", list_rank_b_f) %>% data.frame()
f_gmgmd_same_prop_cyc_b_uw <- do.call("rbind", list_prop_cyc_b_f) %>% data.frame()

m_gmgmd_same_age_b_uw <- do.call("rbind", list_age_b_m) %>% data.frame()
m_gmgmd_same_rank_b_uw <- do.call("rbind", list_rank_b_m) %>% data.frame()

# save(f_gmgmd_same_age_b_uw,
# f_gmgmd_same_rank_b_uw,
# f_gmgmd_same_prop_cyc_b_uw,
# m_gmgmd_same_age_b_uw,
# m_gmgmd_same_rank_b_uw, file = "data/random coefs - same sex sep uw - undir gmgmd.Rdata")

# ---- total prox ran models for same-sex nets weighted ------

load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and prox weighted.Rdata", verbose = T)

list_age_b_f <- vector("list", length = 1000)
list_rank_b_f <- vector("list", length = 1000)
list_prop_cyc_b_f <- vector("list", length = 1000)

list_age_b_m <- vector("list", length = 1000)
list_rank_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_w[[i]]  
  
  # run models
  f_prox_same_w <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "female", summary = T )
  m_prox_same_w <- age_sex_fun_all(ran_df, beh = "prox",  net_sex = "male", summary = T)
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(f_prox_same_w, "age")
  list_rank_b_f[[i]] <- ex_coef(f_prox_same_w, "rank")
  list_prop_cyc_b_f[[i]] <- ex_coef(f_prox_same_w, "prop_cyc")
  
  list_age_b_m[[i]] <- ex_coef(m_prox_same_w, "age")
  list_rank_b_m[[i]] <- ex_coef(m_prox_same_w, "rank")
}
Sys.time() - t #18.9

f_prox_same_age_b <- do.call("rbind", list_age_b_f) %>% data.frame()
f_prox_same_rank_b <- do.call("rbind", list_rank_b_f) %>% data.frame()
f_prox_same_prop_cyc_b <- do.call("rbind", list_prop_cyc_b_f) %>% data.frame()

m_prox_same_age_b <- do.call("rbind", list_age_b_m) %>% data.frame()
m_prox_same_rank_b <- do.call("rbind", list_rank_b_m) %>% data.frame()

save(f_prox_same_age_b,
f_prox_same_rank_b,
f_prox_same_prop_cyc_b,
m_prox_same_age_b,
m_prox_same_rank_b, file = "data/random coefs - same sex sep w - undir prox.Rdata")

# ---- total prox ran models for same-sex nets unweighted ------
load("data/ran4 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd unweighted.Rdata", verbose = T)

list_age_b_f <- vector("list", length = 1000)
list_rank_b_f <- vector("list", length = 1000)
list_prop_cyc_b_f <- vector("list", length = 1000)

list_age_b_m <- vector("list", length = 1000)
list_rank_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_undir_sna_measure_sex_sep_uw[[i]]  
  
  # run models
  f_prox_same_uw <- age_sex_fun_all(ran_df, beh = "prox", net_sex = "female", summary = T )
  m_prox_same_uw <- age_sex_fun_all(ran_df, beh = "prox",  net_sex = "male", summary = T)
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(f_prox_same_uw, "age")
  list_rank_b_f[[i]] <- ex_coef(f_prox_same_uw, "rank")
  list_prop_cyc_b_f[[i]] <- ex_coef(f_prox_same_uw, "prop_cyc")
  
  list_age_b_m[[i]] <- ex_coef(m_prox_same_uw, "age")
  list_rank_b_m[[i]] <- ex_coef(m_prox_same_uw, "rank")
}
Sys.time() - t #12.7 min

f_prox_same_age_b_uw <- do.call("rbind", list_age_b_f) %>% data.frame()
f_prox_same_rank_b_uw <- do.call("rbind", list_rank_b_f) %>% data.frame()
f_prox_same_prop_cyc_b_uw <- do.call("rbind", list_prop_cyc_b_f) %>% data.frame()

m_prox_same_age_b_uw <- do.call("rbind", list_age_b_m) %>% data.frame()
m_prox_same_rank_b_uw <- do.call("rbind", list_rank_b_m) %>% data.frame()

# save(f_prox_same_age_b_uw,
# f_prox_same_rank_b_uw,
# f_prox_same_prop_cyc_b_uw,
# m_prox_same_age_b_uw,
# m_prox_same_rank_b_uw, file = "data/random coefs - same sex sep uw - undir prox.Rdata")

# ---- directed grooming ran models for same sex nets weighted -----
load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

list_age_b_f <- vector("list", length = 1000)
list_rank_b_f <- vector("list", length = 1000)
list_prop_cyc_b_f <- vector("list", length = 1000)

list_age_b_m <- vector("list", length = 1000)
list_rank_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_dir_sna_measure_sex_sep_w[[i]]  
  
  # run models
  f_gm_same_w <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "female", summary = T )
  m_gm_same_w <- age_sex_fun_all(ran_df, beh = "grooming",  net_sex = "male", summary = T)
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(f_gm_same_w, "age")
  list_rank_b_f[[i]] <- ex_coef(f_gm_same_w, "rank")
  list_prop_cyc_b_f[[i]] <- ex_coef(f_gm_same_w, "prop_cyc")
  
  list_age_b_m[[i]] <- ex_coef(m_gm_same_w, "age")
  list_rank_b_m[[i]] <- ex_coef(m_gm_same_w, "rank")
}
Sys.time() - t #12.7 min

f_gm_same_age_b <- do.call("rbind", list_age_b_f) %>% data.frame()
f_gm_same_rank_b <- do.call("rbind", list_rank_b_f) %>% data.frame()
f_gm_same_prop_cyc_b <- do.call("rbind", list_prop_cyc_b_f) %>% data.frame()

m_gm_same_age_b <- do.call("rbind", list_age_b_m) %>% data.frame()
m_gm_same_rank_b <- do.call("rbind", list_rank_b_m) %>% data.frame()

# save(f_gm_same_age_b,
# f_gm_same_rank_b,
# f_gm_same_prop_cyc_b,
# m_gm_same_age_b,
# m_gm_same_rank_b, file = "data/random coefs - same sex sep w - dir gm.Rdata")

# ---- directed grooming ran models for same sex nets unweighted -----
load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)
list_age_b_f <- vector("list", length = 1000)
list_rank_b_f <- vector("list", length = 1000)
list_prop_cyc_b_f <- vector("list", length = 1000)

list_age_b_m <- vector("list", length = 1000)
list_rank_b_m <- vector("list", length = 1000)

t <- Sys.time()
for(i in 1:1000) {
  
  ran_df <- list_ran_dir_sna_measure_sex_sep_uw[[i]]  
  
  # run models
  f_gm_same_uw <- age_sex_fun_all(ran_df, beh = "grooming", net_sex = "female", summary = T )
  m_gm_same_uw <- age_sex_fun_all(ran_df, beh = "grooming",  net_sex = "male", summary = T)
  
  # extract coefficient of model
  list_age_b_f[[i]] <- ex_coef(f_gm_same_uw, "age")
  list_rank_b_f[[i]] <- ex_coef(f_gm_same_uw, "rank")
  list_prop_cyc_b_f[[i]] <- ex_coef(f_gm_same_uw, "prop_cyc")
  
  list_age_b_m[[i]] <- ex_coef(m_gm_same_uw, "age")
  list_rank_b_m[[i]] <- ex_coef(m_gm_same_uw, "rank")
}
Sys.time() - t #18.9 min

f_gm_same_age_b_uw <- do.call("rbind", list_age_b_f) %>% data.frame()
f_gm_same_rank_b_uw <- do.call("rbind", list_rank_b_f) %>% data.frame()
f_gm_same_prop_cyc_b_uw <- do.call("rbind", list_prop_cyc_b_f) %>% data.frame()

m_gm_same_age_b_uw <- do.call("rbind", list_age_b_m) %>% data.frame()
m_gm_same_rank_b_uw <- do.call("rbind", list_rank_b_m) %>% data.frame()

# save(f_gm_same_age_b_uw,
# f_gm_same_rank_b_uw,
# f_gm_same_prop_cyc_b_uw,
# m_gm_same_age_b_uw,
# m_gm_same_rank_b_uw, file = "data/random coefs - same sex sep uw - dir gm.Rdata")

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

