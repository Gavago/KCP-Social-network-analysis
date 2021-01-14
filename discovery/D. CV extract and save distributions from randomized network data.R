library(tidyverse)

load("data/sna dataframes - measures based on node randomized graphs.Rdata", verbose = T)

# 1. CVs distributions -------
# individual sna CVs over all years observed
# calculated 1000 times (once for each randomized data set)

# a. randomized cv calc function ----------
extract_rcv <- function(x){
  rcvs <- x %>%
  group_by(chimp_id, network_sex, behavior) %>%
  summarise(cv_bt = sd(bt)/mean(bt)*100, 
         cv_ec = sd(ec)/mean(ec)*100, 
         cv_deg = sd(deg)/mean(deg)*100,
         cv_trans = sd(trans)/mean(trans)*100) %>%
  ungroup() %>%
    select(chimp_id, network_sex, behavior, starts_with("cv"))
  return(rcvs)
  }


# b. get and save CVs by individual network sex and behavior type-----------
ran_cv_dists1 <- lapply(list_ran_sna_measure_df, extract_rcv) %>%
  do.call("rbind", .) 
ran_cv_dists <- ran_cv_dists1 %>%
  arrange(network_sex, behavior) %>%
  filter(!(is.na(cv_bt) & is.na(cv_ec) & is.na(cv_deg) & is.na(cv_trans)))# individuals where all CVs are NA were observed for only 1 year
 
names(ran_cv_dists)
ran_cv_dists_long <- ran_cv_dists %>%
  pivot_longer(cols = starts_with("cv"), names_to = "CV_type", values_to = "CV") %>% 
  select(-chimp_id) %>%
  #nest for joining
  nest(distributions = CV) #not necessary to have chimpid pres except for checking purposes

#save(ran_cv_dists, ran_cv_dists_long, file = "data/randomized distributions of sna CVs.Rdata")

# c. View random distributions -----------

# any combo
ran_cv_dists %>%
  filter(network_sex == "any_combo", behavior == "prox") %>%
  pull(cv_bt) %>%
  hist(main = "random: any_combo prox bt")

ran_cv_dists %>%
  filter(network_sex == "any_combo", behavior == "total_grooming") %>%
  pull(cv_bt) %>%
  hist(main = "random: any_combo grooming bt")

ran_cv_dists %>%
  filter(network_sex == "any_combo", behavior == "prox") %>%
  pull(cv_ec) %>%
  hist(main = "random: any_combo prox ec")

ran_cv_dists %>%
  filter(network_sex == "any_combo", behavior == "total_grooming") %>%
  pull(cv_ec) %>%
  hist(main = "random: any_combo grooming ec")



# d. check dimensions and filters of ran CV file --------------
nrow(ran_cv_dists1) #155000
nrow(ran_cv_dists) #169701, 24042 rows removed

#39 chimps, 5 removed bc pres only 1 year
total_chimps <- ran_cv_dists1 %>% pull(chimp_id) %>% unique()
chimps_oneyear <- ran_cv_dists1 %>%
  filter(is.na(cv_bt) & is.na(cv_ec) & is.na(cv_deg) & is.na(cv_trans)) %>% pull(chimp_id) %>% unique()
chimps_w_cv <- ran_cv_dists %>% pull(chimp_id) %>% unique()

intersect(chimps_w_cv, chimps_oneyear)
length(total_chimps); length(chimps_w_cv) ; length(chimps_oneyear)

# d. play 
load("data/randomized distributions of sna CVs.Rdata", verbose = T)

names(ran_cv_dists)

ran_cv_dists %>%
  filter(network_sex == "any_combo", behavior == "prox") %>%
  pull(cv_bt) %>%
  hist()





View(ran_cv_dists)
hist(ran_cv_dists$cv_bt)
hist(ran_cv_dists$cv_ec)
hist(ran_cv_dists$cv_deg)


# graveyard ------

#mixed sex grooming dists
ms_gmgmd_bt <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_bt)
ms_gmgmd_ec <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_ec)
ms_gmgmd_deg <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_deg)
ms_gmgmd_trans <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_trans)

#female grooming dists
f_gmgmd_bt <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_bt)
f_gmgmd_ec <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_ec)
f_gmgmd_deg <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_deg)
f_gmgmd_trans <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_trans)

#male grooming dists
m_gmgmd_bt <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_bt)
m_gmgmd_ec <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_ec)
m_gmgmd_deg <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_deg)
m_gmgmd_trans <- ran %>% filter(network_sex == "any_combo", behavior == "total_grooming") %>% pull(data) %>% .[[1]] %>% pull(cv_trans)

