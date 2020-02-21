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


# 1. Calc observed individ CVs by network sex and behavior
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


# test for sig stability ------
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


#does sig hi stability occur more often in particular networks?
sig_stability %>%
  count(sex, network_sex, behavior) %>%
  filter(stab_hi < 0.05) %>%
  group_by(sex, network_sex, behavior) %>%
  mutate()
# where does non sig stability occur?
sig_stability %>%
  filter(stab_hi > 0.05) %>%
  count(network_sex, behavior)

#no one has sig lo stability :-/
sig_stability %>%
  filter(stab_lo < 0.05)

#ok, who at least is stable more often than others
sig_stability %>%
  add_count(chimp_id) %>%
  filter(stab_hi < 0.05) %>%
  group_by(chimp_id) %>%
  mutate(n_sig = n(), prop_sig = n_sig/n) %>%
  ungroup() %>%
  filter()
  



#gyard ----
filt_cv <- function(data, net_sex, cv, beh) {
  val <- data %>%
    filter(network_sex == net_sex, CV_type == cv, behavior == beh) %>% 
    pull(value)
  return(val)
}
