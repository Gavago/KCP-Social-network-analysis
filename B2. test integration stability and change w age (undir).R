library(tidyverse)
library(magrittr)
library(lmerTest)
library(gridExtra)
library(grid)

load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/attribute data alone.Rdata", verbose = T)
source("functions/functions - age sex modeling.R")
z. <- function(x) scale(x)

#load("data/sna dataframe - individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)

sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw

#sna_df <- all_sna_measure_df

# deg and trans not weighting...
all(sna_w$bt == sna_uw$bt)
all(sna_w$ec == sna_uw$ec)
all(sna_w$deg == sna_uw$deg)
all(sna_w$trans == sna_uw$trans)


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
# sex & age
# stability/variation in sna measures (adjusted for observation time)
# magnitude of various sna variables


# 1. Stability of integration
# ---- obs -individ CVs by network sex and behavior --------
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











# 2. Age-sex effects on integration in undirected mixed sex networks (groom and prox) -----
# ----- obs - total grooming (mixed sex) ----

# BT EC weighted 
gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                net_sex = "any_combo", sex_age_int = F, summary = T )
gmgmd_mixed_int_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                net_sex = "any_combo", sex_age_int = T, summary = T )

gmgmd_mixed_w #males higher ec bt deg
gmgmd_mixed_int_w # deg int sig

# BT EC unweighted
gmgmd_mixed_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                              net_sex = "any_combo", sex_age_int = F, summary = T )
gmgmd_mixed_int_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                  net_sex = "any_combo", sex_age_int = T, summary = T )
gmgmd_mixed_uw # males higher bt, ec, and deg
gmgmd_mixed_int_uw # no int sig

# old chimps

gmgmd_mixed_w_old <- sna_w %>% 
  filter(age_mid_year > 32) %>%
           age_sex_fun_all(., beh = "total_grooming", 
                              net_sex = "any_combo", sex_age_int = F, summary = T )
gmgmd_mixed_int_w_old <- sna_w %>% 
  filter(age_mid_year > 32) %>%
  age_sex_fun_all(., beh = "total_grooming", 
                                  net_sex = "any_combo", sex_age_int = T, summary = T )

gmgmd_mixed_w_old # deg dec w age
gmgmd_mixed_int_w_old


# ----- obs - prox (mixed sex) -----

#weighted bt ec
prox_mixed_w <- age_sex_fun_all(sna_w, beh = "prox", 
                            net_sex = "any_combo", sex_age_int = F, summary = T )
prox_mixed_int_w <- age_sex_fun_all(sna_w, beh = "prox", 
                                net_sex = "any_combo", sex_age_int = T, summary = T )
prox_mixed_w # males higher ec and deg than females, lower bt
prox_mixed_int_w # no sig int

#unweighted
prox_mixed_uw <- age_sex_fun_all(sna_uw, beh = "prox", 
                                net_sex = "any_combo", sex_age_int = F, summary = T )
prox_mixed_int_uw <- age_sex_fun_all(sna_uw, beh = "prox", 
                                    net_sex = "any_combo", sex_age_int = T, summary = T )
prox_mixed_uw # no effects age or sex
prox_mixed_int_uw # no sig int


# save(gmgmd_mixed_w, gmgmd_mixed_int_w,
#      gmgmd_mixed_uw, gmgmd_mixed_int_uw,
#      prox_mixed_w, prox_mixed_int_w,
#      prox_mixed_uw, prox_mixed_int_uw, file = "data/models - summaries of age sex effects in mixed networks.Rdata")


# ----- sig - total grooming (mixed sex) -----
load("data/random coefs age sex on gmgmd mixed sex net sna.Rdata", verbose = T) # all weighted bt and ec
load("data/models - summaries of age sex effects in mixed networks.Rdata", verbose = T)

# e.g. gmgmd_age_b
names(gmgmd_sex_b)
# coefficients of age effects on a given sna measure in a model w only age-sex main effects

# Age effects on integration in mixed sex grooming networks (no age sex interaction)
# the observed coefficient is higher than what proportion of random coefs
sum(coef(gmgmd_mixed_w$bt)[2,1] >  gmgmd_age_b$bt_age, na.rm = T) / 1000 # b of -.14 is almost sig low, is higher than very few
sum(coef(gmgmd_mixed_w$ec)[2,1] > gmgmd_age_b$ec_age, na.rm = T) / 1000 # b of .19 is sig high, increase ec w age
sum(coef(gmgmd_mixed_w$deg)[2,1] > gmgmd_age_b$deg_age, na.rm = T) / 1000 # b of -.11 is sig low, dec deg w age
sum(coef(gmgmd_mixed_w$trans)[2,1] > gmgmd_age_b$trans_age, na.rm = T) / 1000 # sig increase

# Sex effects on integration in mixed sex grooming networks (no age sex interaction)
sum(coef(gmgmd_mixed_w$bt)[3,1] >  gmgmd_sex_b$bt_sex, na.rm = T) / 1000 # b bt .79 is sig high, males more between than females
sum(coef(gmgmd_mixed_w$ec)[3,1] > gmgmd_sex_b$ec_sex, na.rm = T) / 1000 # b ec sig high, males more ec than females
sum(coef(gmgmd_mixed_w$deg)[3,1] > gmgmd_sex_b$deg_sex, na.rm = T) / 1000 # b deg sig high, males more deg than females
sum(coef(gmgmd_mixed_w$trans)[3,1] > gmgmd_sex_b$trans_sex, na.rm = T) / 1000 #


# Age by sex in interaction models alone
sum(coef(gmgmd_mixed_int_w$bt)[4,1] >  gmgmd_int_int_b$bt_int_int, na.rm = T) / 1000 # b -0.55 sig lo, males decrease in betweenness w age while fem don't
sum(coef(gmgmd_mixed_int_w$ec)[4,1] > gmgmd_int_int_b$ec_int_int, na.rm = T) / 1000 # b no sig diff
sum(coef(gmgmd_mixed_int_w$deg)[4,1] > gmgmd_int_int_b$deg_int_int, na.rm = T) / 1000 # b 0.45 sig high, males increase deg w age, fem decrease
sum(coef(gmgmd_mixed_int_w$trans)[4,1] > gmgmd_int_int_b$trans_int_int, na.rm = T) / 1000 # b 0.25 sig high, males increase trans w age f dec 

# ----- sig - prox (mixed sex) -----
load("data/random coefs age sex on prox sna.Rdata", verbose = T)
load("data/models - summaries of age sex effects in mixed networks.Rdata", verbose = T)

# Age effects on integration in mixed sex prox networks (no age sex interaction)
sum(coef(prox_mixed_w$bt)[2,1] >  prox_age_b$bt_age, na.rm = T) / 1000 #
sum(coef(prox_mixed_w$ec)[2,1] > prox_age_b$ec_age, na.rm = T) / 1000 # b of .12 is sig high, increase ec w age
sum(coef(prox_mixed_w$deg)[2,1] > prox_age_b$deg_age, na.rm = T) / 1000 #
sum(coef(prox_mixed_w$trans)[2,1] > prox_age_b$trans_age, na.rm = T) / 1000 #

# Sex effects on integration in mixed sex grooming networks (no age sex interaction)
sum(coef(prox_mixed_w$bt)[3,1] >  prox_sex_b$bt_sex, na.rm = T) / 1000 # b bt is sig lo, males less bt than females...
sum(coef(prox_mixed_w$ec)[3,1] > prox_sex_b$ec_sex, na.rm = T) / 1000 # b ec sig high, males more ec than females
sum(coef(prox_mixed_w$deg)[3,1] > prox_sex_b$deg_sex, na.rm = T) / 1000 # b deg sig high, males more deg than females
sum(coef(prox_mixed_w$trans)[3,1] > prox_sex_b$trans_sex, na.rm = T) / 1000 #


# Age by sex in interaction models alone
sum(coef(prox_mixed_int_w$bt)[4,1] >  prox_int_int_b$bt_int_int, na.rm = T) / 1000 #
sum(coef(prox_mixed_int_w$ec)[4,1] > prox_int_int_b$ec_int_int, na.rm = T) / 1000 # b sig hi, males increasing EC w age in prox, females no. (w loess can see that peak is late 20's for M)
sum(coef(prox_mixed_int_w$deg)[4,1] > prox_int_int_b$deg_int_int, na.rm = T) / 1000 #
sum(coef(prox_mixed_int_w$trans)[4,1] > prox_int_int_b$trans_int_int, na.rm = T) / 1000 #




# 3. Age effects on integration in same-sex networks (groom and prox) ------
# ----- obs - prox and grooming (same sex) -----

# all chimps
gm_same_w_f <- age_fun_all(sna_w, beh = "total_grooming", net_sex = "female", summary = T)
gm_same_w_m <- age_fun_all(sna_w, beh = "total_grooming", net_sex = "male", summary = T)


prox_same_w_f <- age_fun_all(sna_w, beh = "prox", net_sex = "female", summary = T ) 
prox_same_w_m <- age_fun_all(sna_w, beh = "prox", net_sex = "male", summary = T ) 


gm_same_w_f # ec and deg dec sharply w age
gm_same_w_m # ec and deg inc w age, bt no converge

prox_same_w_f # no age changes
prox_same_w_m # ec inc w age

# chimps > 30 yo

gm_same_w_f_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "total_grooming", net_sex = "female", summary = T)
gm_same_w_m_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "total_grooming", net_sex = "male", summary = T)


prox_same_w_f_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "female", summary = T ) 
prox_same_w_m_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "male", summary = T ) 

gm_same_w_f_old #trans dec fem older than 32, bt doesn't converge
gm_same_w_m_old # ec dec w age, v close to sig

prox_same_w_f_old # inc trans
prox_same_w_m_old # no age effects

#save(gm_same_w_f, gm_same_w_m, prox_same_w_f, prox_same_w_m, file = "data/models - summaries of age effects in same sex undirected networks.Rdata")
#save(gm_same_w_f_old, gm_same_w_m_old, prox_same_w_f_old, prox_same_w_m_old, file = "data/models - summaries of age effects in same sex undirected networks: chimps > 30 yo.Rdata")



# ----- sig - total grooming (same sex)----
load("data/models - summaries of age effects in same sex undirected networks.Rdata", verbose = T)
load("data/models - summaries of age effects in same sex undirected networks: chimps > 30 yo.Rdata", verbose = T)

load("data/random coefs age on gmgmd same sex sna.Rdata", verbose = T)
load("data/random coefs age on prox same sex sna.Rdata", verbose = T)



sum(coef(gm_same_w_f$bt)[2,1] >  gmgmd_age_b_f$bt_age, na.rm = T) / 1000 # no convergence
sum(coef(gm_same_w_f$ec)[2,1] > gmgmd_age_b_f$ec_age, na.rm = T) / 1000 # b -0.59 sig dec ec w age
sum(coef(gm_same_w_f$deg)[2,1] > gmgmd_age_b_f$deg_age, na.rm = T) / 1000 # b of -0.69 is sig low, dec deg w age
sum(coef(gm_same_w_f$trans)[2,1] > gmgmd_age_b_f$trans_age, na.rm = T) / 1000 #

sum(coef(gm_same_w_m$bt)[2,1] >  gmgmd_age_b_m$bt_age, na.rm = T) / 1000 # no convergence
sum(coef(gm_same_w_m$ec)[2,1] > gmgmd_age_b_m$ec_age, na.rm = T) / 1000 # b 0.3 sig inc ec w age
sum(coef(gm_same_w_m$deg)[2,1] > gmgmd_age_b_m$deg_age, na.rm = T) / 1000 # b 0.32 sig inc deg w age
sum(coef(gm_same_w_m$trans)[2,1] > gmgmd_age_b_m$trans_age, na.rm = T) / 1000 #



# ----- sig - prox (same sex) -----

# Age effects on integration in mixed sex prox networks (no age sex interaction)
sum(coef(prox_same_w_f$bt)[2,1] >  prox_age_b_f$bt_age, na.rm = T) / 1000 # 
sum(coef(prox_same_w_f$ec)[2,1] > prox_age_b_f$ec_age, na.rm = T) / 1000 #
sum(coef(prox_same_w_f$deg)[2,1] > prox_age_b_f$deg_age, na.rm = T) / 1000 #
sum(coef(prox_same_w_f$trans)[2,1] > prox_age_b_f$trans_age, na.rm = T) / 1000 #

sum(coef(prox_same_w_m$bt)[2,1] >  prox_age_b_m$bt_age, na.rm = T) / 1000 # no converge
sum(coef(prox_same_w_m$ec)[2,1] > prox_age_b_m$ec_age, na.rm = T) / 1000 # b of .21 sig hi, ec inc w age
sum(coef(prox_same_w_m$deg)[2,1] > prox_age_b_m$deg_age, na.rm = T) / 1000 #
sum(coef(prox_same_w_m$trans)[2,1] > prox_age_b_m$trans_age, na.rm = T) / 1000 #

# 4. Visualization----------
# -- viz - mixed sex gm age sex ------

# grooming
# think we'll find all of these are sig relative to randomization - yup.

sg1 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # sig
  labs( x = "Age (years)", y = "", title = "Betweenness") +
  annotate("text",x = 35, y = 115, label = "*", size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #annotate("text",x = 52, y = 50, label = "M > F", size = 4)  
sg1

sg2 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") +
  labs( x = "Age (years)", y = "",title = "Eigenvector centrality") +
  annotate("text",x = 52, y = 1, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #annotate("text",x = 52, y = 0.25, label = "M > F", size = 4)  
sg2

sg3 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig
  labs( x = "Age (years)", y = "", title = "Weighted degree") +
  annotate("text",x = 35, y = 65, label = "*", size = 12) +
  annotate("text",x = 50, y = 75, label = "** Age changes in \n same sex networks, too", size = 3)+
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #annotate("text",x = 52, y = 10, label = "M > F", size = 4)  
sg3

sg4 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  #geom_point() +
  geom_smooth( method = "lm") + #sig
  labs( x = "Age (years)", y = "", title = "Weighted transitivity") +
  annotate("text",x = 35, y = .8, label = "*", size = 12) +
  annotate("text",x = 50, y = .9, label = "** Age changes in \n same sex networks, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #no main effect diff M F
sg4

sg1; sg2; sg3; sg4

pdf("results/results viz - age sex changes in grooming integration.pdf", height = 16, width = 16)
grid.arrange(grobs = list(sg1, sg2, sg3, sg4), nrow = 2, top = textGrob("Age changes in grooming integration, mixed sex networks"), gp = gpar(fontize = 32))
dev.off()

sna_uw %>%
  filter(chimp_id == "QT", network_sex == "female", behavior =="total_grooming") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_point() +
  geom_line()
  geom_smooth( method = "lm")


# -- viz - mixed sex prox age sex ------

sp1 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  #geom_point() +
  geom_smooth( method = "lm") +
  labs(x = "Age (years)", y = "" ,title = "Betweenness") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp1
  
sp2 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # sig M F same sex
  labs( x = "Age (years)", y = "", title = "Eigenvector centrality") +
  annotate("text",x = 35, y = 0.9, label = "*", size = 12) +
  annotate("text",x = 50, y = 1, label = "** Age change in \n male network, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp2

sp3 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig mixed, sig M F same sex
  labs( x = "Age (years)", y = "", title = "Weighted degree") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp3

sp4 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  #geom_point() +
  geom_smooth( method = "lm") +
  labs( x = "Age (years)", y = "", title = "Weighted transitivity") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp4

pdf("results/results viz - age sex changes in proximity integration.pdf", height = 16, width = 16)
grid.arrange(grobs = list(sp1, sp2, sp3, sp4), nrow = 2, top = textGrob("Age changes in proximity integration, mixed sex network"), gp = gpar(fontize = 32))
dev.off()


#gyard ----
filt_cv <- function(data, net_sex, cv, beh) {
  val <- data %>%
    filter(network_sex == net_sex, CV_type == cv, behavior == beh) %>% 
    pull(value)
  return(val)
}

# Mixed network GMGMD results - PRE 09-10 merge
# bt gm: inc w age, higher for males; pos int - if male slope age more pos than females
# ec gm: no effect age, higher for males; no int
# deg gm: dec age, higher for males; no int
# trans gm: no effects; no int
# Mixed network PROX results - not informative - PRE 09-10 merge
# bt prox: no age effect, males higher bt than; no int
# ec prox: no effects; no int
# deg prox: no effects; no int
# trans prox: no effects; no int


# Same sex GMGMD networks - PRE 09-10 merge
# bt gm: bt dec w age in same sex networks, not diff by sex.
# ec gm: ec changes w age in both networks, dec fem and inc male
# deg gm: same as ec
# trans gm: is different bt networks, but no age effects.
# Same sex PROX networks - PRE 09-10 merge, not very informative.... -
# bt prox: no and int doesn't converge....
# ec prox: no, no int
# deg prox: lower among males - just a product of network size, no int
# trans prox: no effects, no int

