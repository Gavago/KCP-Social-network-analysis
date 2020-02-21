library(tidyverse)
library(magrittr)
load("data/sna dataframe - individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/attribute data alone.Rdata", verbose = T)


# how many years  are diff individuals present
# how much do 

# part 1
# description stability and change w age - 
# will require node permutations of networks within each year to get random sna measures pr year, 
# and distribution of age coefficients from randomized networks
# similarly, individual CVs of network measures will be significantly high or low in reference to their CVs calculated from randomized networks

# part 2
# Response: OS status by quarter, 2017 - 2019 (12 points per individual? - average of 3 per quarter?)
# Predictors of OS status:
# (model selection)
# hardship index - years w amputation from snare injury, number of severe injuries?, loss of mother before age x(?)
# average dominance rank during observation * sex
# sex & agee
# stability/variation in sna measures (adjusted for observation time)
# magnitude of various sna variables


# 1. Integration stability, by obs centrality CV vs random
# 1a. calc observed individ CVs by network sex and behavior --------
cv_obs <- all_sna_measure_df %>%
  group_by(chimp_id, network_sex, behavior) %>%
  summarise(cv_bt = sd(bt)/mean(bt)*100, 
            cv_ec = sd(ec)/mean(ec)*100, 
            cv_deg = sd(deg)/mean(deg)*100,
            cv_trans = sd(trans)/mean(trans)*100) %>%
  ungroup() %>%
  select(chimp_id, network_sex, behavior, starts_with("cv")) %>%
  pivot_longer(cols = starts_with("cv"), names_to = "CV_type", values_to = "obs_CV") %>%
  add_years_obs() %>%
  add_individ_attr() %>%
  add_final_age() %>%
  select(-starts_with("immig"))

nrow(cv_obs) # 620 chimp-network cvs

names(cv_obs)

#save(cv_obs, file = "data/observed cvs integration.Rdata")


# 1b. calc sig stability ------
load("data/randomized distributions of sna CVs.Rdata", verbose = T)

# massive case when? to create sig_high (prop dist below) and sig_low (prop dist below)
# or make sep network sex - behavior files 
#agh, GATHER, make one col CV type, then compare to say whether cv for that sna measure is sig stable/wonky

names(cv_obs)

#CV indicates how much integration varies from year to year in particular type of network
#the higher the CV, the lower the stability of integration
#the lower the CV, the higher the stability of integration

# an obs CV that is lower than 95% observed, means sig high stability
hi_stab_fun <- function(x){ 
  prop <- sum(obs_cv < x$CV)/length(x$CV)
  return(prop)
}

# an obs CV that is higher than 95% observed, mean sig low stability
lo_stab_fun <- function(x){
  prop <- sum(obs_cv > x$CV)/length(x$CV)
  return(prop)
}


sig_stability <- cv_obs %>%
  left_join(., ran_cv_dists_long, by = c("network_sex", "behavior", "CV_type")) %>%
  mutate(stab_hi = map(distributions, hi_stab_fun)) %>%
  mutate(stab_lo = map(distributions, lo_stab_fun)) %>%
  unnest(c(stab_hi, stab_lo))
names(sig_stability)


# 1c. explore sig stability -------

load("data/years each subject pres in network data.Rdata", verbose = T)
# size up years pres
years_pres %>% arrange(n) %>% View()

# is sig stability related to number of years seen? yes.
# more variability (higher CV) as individuals are observed more years
# correlation varies by centrality type (cv_type)
# appears that betweenness most likely to fluctuate.
# with > 2 yrs obs, all other centrality cvs stop correlating with yrs obs
# corr disappears for bt when yrs obs > 6 or < 3
# are 12 F and 6 M chimps pres for more than 6 years

sig_stability  %$% 
  cor.test(obs_CV, yrs_obs, method = "spearman")
sig_stability %>% 
  filter(yrs_obs > 3) %$% 
  cor.test(obs_CV, yrs_obs, method = "spearman") 

# cor by cv type
p_val <- function(df){
  a <- df  %$% 
    cor.test(obs_CV, yrs_obs, method = "spearman")
  return(a$p.value)
}
rho <- function(df){
  a <- df  %$% 
    cor.test(obs_CV, yrs_obs, method = "spearman")
  return(a$estimate)
}

# window of years obs where corr bt years obs and cvs present
# group_by doesn't test corr by cv type, so nest...
sig_stability %>%
  filter(yrs_obs > 6) %>% #change here
  select(CV_type, obs_CV, yrs_obs) %>%
  nest(c(obs_CV, yrs_obs)) %>%
  mutate(rho = map(data, rho), p_val = map(data, p_val)) %>%
  unnest(c(rho, p_val))

# how many of each sex observed a given number of years
years_pres %>%
  filter(n > 6) %>%
  add_individ_attr() %>%
  count(sex)


#does sig hi stability occur more often in particular networks? 

# n is x4 bc 4 centralities for each network
# total sex & network specific CVs =
sig_stability %>%
  filter(yrs_obs > 6) %>%
  count(sex, network_sex, behavior)

# which are sig stable
sig_stability %>%
  filter(yrs_obs > 6) %>%
  filter(stab_hi < 0.05) %>%
  count(sex, network_sex, behavior)
#both sexes less stable integration in gm vs prox
#female less stable in same sex prox network than mixed sex
#zero females stable in same sex gm network

#no one has sig lo stability :-/
sig_stability %>%
  filter(stab_lo < 0.05)

# on individual level, in which networks do individual chimps have a higher proportion of centralities
# that are stable...
sig_stability %>%
  filter(yrs_obs > 6) %>%
  #number of networks a chimp is observed in
  add_count(chimp_id) %>%
  #
  filter(stab_hi < 0.05) %>%
  #proportion of networks a chimp is significantly stable
  group_by(chimp_id) %>%
  mutate(n_sig = n(), prop_cvs_sig_stable = n_sig/n) %>%
  ungroup() %>%
  #distill df to one obs per chimp, his or her proportion of observed networks in which its stable
  distinct(chimp_id, .keep_all = T) %>%
  filter(prop_cvs_sig_stable >= .75) %>%
  filter(sex == "F")
  count(sex)

sig_stability %>%
  distinct(chimp_id, .keep_all = T) %>%
  count(sex)
# all study males are stable in 75% of the networks and integrations measures
# they are measured in. but only 1 female is so stable. (all females are stable in at least 50% tho)










#gyard ----
filt_cv <- function(data, net_sex, cv, beh) {
  val <- data %>%
    filter(network_sex == net_sex, CV_type == cv, behavior == beh) %>% 
    pull(value)
  return(val)
}
