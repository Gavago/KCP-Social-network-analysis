library(tidyverse)
library(magrittr)
library(rptR)

# undirected
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
# directed
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
source("functions/functions - age sex modeling.R")


#add number of individuals in network each year as predictor ------------

sna_w <- all_sna_measure_df_w %>%
  group_by(behavior, year, network_sex) %>%
  mutate(net_size = n()) %>%
  ungroup()

sna_uw <- all_sna_measure_df_uw %>%
  group_by(behavior, year, network_sex) %>%
  mutate(net_size = n()) %>%
  ungroup()

dir_sna_w <- dir_sna_measure_df_w %>%
  group_by(behavior, year, network_sex) %>%
  mutate(net_size = n()) %>%
  ungroup()

dir_sna_uw <- dir_sna_measure_df_uw %>%
  group_by(behavior, year, network_sex) %>%
  mutate(net_size = n()) %>%
  ungroup()


# repeatability -------

names(sna_w)

sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% hist(main = "bt")
sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% hist(main = "ec")
sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% hist(main = "deg w")
sna_uw %>% filter(behavior == "prox") %>% pull(bt) %>% hist(main = "deg_uw")
sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% hist(main = "trans")

hist(dir_sna_w$bt)
hist(dir_sna_w$ec)
hist(dir_sna_w$deg)
hist(sna_uw$deg)
hist(sna_w$trans)

# GMGMD 5 sna x 2 sex -----
rgmgmdf_bt <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(bt ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmgmdm_bt <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(bt ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmgmdf_bt) #
print(rgmgmdm_bt) #

rgmgmdf_ec <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(ec ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmgmdm_ec <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(ec ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmgmdf_ec) # wow
print(rgmgmdm_ec) # and wow

rgmgmdf_deg <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(deg ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmgmdm_deg <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(deg ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmgmdf_deg) # wow
print(rgmgmdm_deg)

rgmgmdf_deg_uw <- sna_uw %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(deg ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmgmdm_deg_uw <- sna_uw %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(deg ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmgmdf_deg_uw) # wow wow
print(rgmgmdm_deg_uw) # wow

rgmgmdf_trans <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(trans ~ prop_cyc + avg_rank + net_size +(1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmgmdm_trans <- sna_w %>% filter(behavior == "total_grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(trans ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmgmdf_trans) #
print(rgmgmdm_trans) #

# DIR GM 7 sna x 2 sex ------
rgmf_bt <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(bt ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmm_bt <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(bt ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmf_bt) 
print(rgmm_bt) 

rgmf_deg_in <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(deg_in ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmm_deg_in <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(deg_in ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmf_deg_in) 
print(rgmm_deg_in)

rgmf_deg_out <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(deg_out ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmm_deg_out <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(deg_out ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmf_deg_out) 
print(rgmm_deg_out)

rgmf_deg_in_uw <- dir_sna_uw %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(deg_in ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmm_deg_in_uw <- dir_sna_uw %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(deg_in ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmf_deg_in_uw) 
print(rgmm_deg_in_uw) 

rgmf_deg_out_uw <- dir_sna_uw %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(deg_out ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmm_deg_out_uw <- dir_sna_uw %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(deg_out ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmf_deg_out_uw) 
print(rgmm_deg_out_uw) 

rgmf_trans <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "F") %>%
  rpt(trans ~ prop_cyc + avg_rank + net_size +(1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rgmm_trans <- dir_sna_w %>% filter(behavior == "grooming", network_sex == "any_combo", sex == "M") %>%
  rpt(trans ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rgmf_trans) 
print(rgmm_trans) 


# PROX 5 sna x 2 sex -----
# much lower repeatability in prox than gmgmd


rpf_bt <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "F") %>%
  rpt(bt ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rpm_bt <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "M") %>%
  rpt(bt ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rpf_bt)
print(rpm_bt)

rpf_ec <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "F") %>%
  rpt(ec ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rpm_ec <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "M") %>%
  rpt(ec ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rpf_ec) #
print(rpm_ec)

rpf_deg <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "F") %>%
  rpt(deg ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rpm_deg <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "M") %>%
  rpt(deg ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rpf_deg) #
print(rpm_deg) 

rpf_deg_uw <- sna_uw %>% filter(behavior == "prox", network_sex == "any_combo", sex == "F") %>%
  rpt(deg ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rpm_deg_uw <- sna_uw %>% filter(behavior == "prox", network_sex == "any_combo", sex == "M") %>%
  rpt(deg ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rpf_deg_uw) #
print(rpm_deg_uw)

rpf_trans <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "F") %>%
  rpt(trans ~ prop_cyc + avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
rpm_trans <- sna_w %>% filter(behavior == "prox", network_sex == "any_combo", sex == "M") %>%
  rpt(trans ~ avg_rank + net_size + (1|chimp_id), grname = "chimp_id", nboot = 1000, npermut = 1000, data = ., datatype = "Gaussian")
print(rpf_trans)
print(rpm_trans)

# make rpt list -----
rpt_models_fun <- function(x) inherits(get(x),"rpt") & !inherits(get(x), 'data.frame')

#rm(list = rpt_list_names[grepl("pmf|pmm", rpt_list_names)])

rpt_list_names <- Filter(rpt_models_fun, ls())
length(rpt_list_names)
rpt_list <- lapply(rpt_list_names, get)
names(rpt_list) <- rpt_list_names


# save models
#save(rpt_list, rpt_list_names, file = "data/repeatability - models list.Rdata")
#load("data/repeatability - models.Rdata")


# create Rpt table -----
extract_rpt <- function(rpt_obj, list = TRUE){
  
  if(list == FALSE){
    name <- deparse(substitute(rpt_obj))  
  }
  if(list == TRUE){
    name <- rpt_name
  }
  
  R <- rpt_obj$R %>% rename(R = "chimp_id")
  CI <- rpt_obj$CI_emp
  if(all(CI > 0)){ sig = "*"} else { sig = ""}
  
  
  df <- data.frame(model = name, R_value = R, CI = CI, sig)
  return(df)
  
}

#storage
rpt_summ <- vector("list", length = length(rpt_list))

start <- Sys.time()
for(i in seq(rpt_list)){
  rpt_name <- names(rpt_list[i])
  rpt_obj <- rpt_list[[i]]
  rpt_summ[[i]] <- extract_rpt(rpt_obj)
}
Sys.time() - start


rpt_sig <- do.call("rbind", rpt_summ) %>% 
  mutate_if(is.numeric, round,2) %>%
  mutate( behavior = case_when(
    grepl("rgmgmd", model) ~ 'Total grooming',
    grepl("rgm[^g]", model) ~ 'Grooming',
    grepl("rp", model) ~ "Prox")) %>%
  mutate( sex = case_when(
    grepl("f_", model) ~ "Female",
    grepl("m_", model) ~ "Male")) %>%
  mutate(sna_measure = case_when(
    grepl("bt", model) ~ "bt",
    grepl("ec", model) ~ "ec",
    grepl("deg$", model) ~ "w deg",
    grepl("deg_uw", model) ~ "uw deg",
    grepl("deg_in$", model) ~ "w deg in",
    grepl("deg_out$", model) ~ "w deg out",
    grepl("deg_in_uw$", model) ~ "uw deg in",
    grepl("deg_out_uw$", model) ~ "uw deg out",
    grepl("trans", model) ~ "trans")) %>%
  arrange(factor(behavior, levels = c("Total grooming", "Grooming", "Prox")), 
          sex, 
          factor(sna_measure, levels = c("bt", "ec", "w deg", "uw deg", "w deg in", "w deg out", "uw deg in", "uw deg out", "trans"))) %>%
  select(behavior, sex, sna_measure, R, CI.2.5., CI.97.5., sig)
  
rpt_sig

rpt_sig[duplicated(rpt_sig[,c("behavior","sex")]), c("behavior","sex")] <- ""

#write.table(rpt_sig, "results/repeatability table.txt", quote = FALSE, sep = "/", row.names = FALSE)
 
# graveyard -----
# 1. CV Stability of integration
# ---- obs - individ CVs by network sex and behavior --------
cv_obs <- sna_df %>%
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


# ---- sig - compare obs to ran ------
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


# ---- explore sig stability -------

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












fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

Performance <- function(x) {                       
  fct_case_when(                                         
    is.na(x) ~ NA_character_,                          
    x > 80   ~ 'Excellent',                            
    x > 50   ~ 'Good',                                 
    TRUE     ~ 'Fail'                                  
  )
}  



rpt_list, rpt_list_names, 

rgmgmdf_bt <- rgmf_bt
rgmgmdm_bt <- rgmm_bt
rgmf_ec
rgmm_ec
rgmf_deg
rgmm_deg
rgmf_deg_uw
rgmm_deg_uw
rgmf_trans
rgmm_trans
# rpmf_bt, rpmm_bt, rpmf_ec, rpmm_ec, rpmf_deg, rpmm_deg,  rpmf_deg_uw, rpmm_deg_uw, rpmf_trans, rpmm_trans