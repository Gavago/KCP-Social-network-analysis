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


dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw

# 2. Age-sex effects on integration in undirected mixed sex networks (groom and prox) -----
# ----- obs - total grooming (mixed sex) ----

# BT EC weighted 
gm_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                              net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                  net_sex = "any_combo", sex_age_int = T, summary = T )

gm_mixed_w #males higher ec bt deg
gm_mixed_int_w # deg int sig

# BT EC unweighted
gm_mixed_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                               net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                   net_sex = "any_combo", sex_age_int = T, summary = T )
gm_mixed_uw # males higher bt, ec, and deg
gm_mixed_int_uw # no int sig

# old chimps

gm_mixed_w_old <- sna_w %>% 
  filter(age_mid_year > 32) %>%
  age_sex_fun_all(., beh = "total_grooming", 
                  net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_w_old <- sna_w %>% 
  filter(age_mid_year > 32) %>%
  age_sex_fun_all(., beh = "total_grooming", 
                  net_sex = "any_combo", sex_age_int = T, summary = T )

gm_mixed_w_old # deg dec w age
gm_mixed_int_w_old


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


# save(gm_mixed_w, gm_mixed_int_w, 
#      gm_mixed_uw, gm_mixed_int_uw,
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
sum(coef(gm_mixed_w$bt)[2,1] >  gmgmd_age_b$bt_age, na.rm = T) / 1000 # b of -.14 is almost sig low, is higher than very few
sum(coef(gm_mixed_w$ec)[2,1] > gmgmd_age_b$ec_age, na.rm = T) / 1000 # b of .19 is sig high, increase ec w age
sum(coef(gm_mixed_w$deg)[2,1] > gmgmd_age_b$deg_age, na.rm = T) / 1000 # b of -.11 is sig low, dec deg w age
sum(coef(gm_mixed_w$trans)[2,1] > gmgmd_age_b$trans_age, na.rm = T) / 1000 # sig increase

# Sex effects on integration in mixed sex grooming networks (no age sex interaction)
sum(coef(gm_mixed_w$bt)[3,1] >  gmgmd_sex_b$bt_sex, na.rm = T) / 1000 # b bt .79 is sig high, males more between than females
sum(coef(gm_mixed_w$ec)[3,1] > gmgmd_sex_b$ec_sex, na.rm = T) / 1000 # b ec sig high, males more ec than females
sum(coef(gm_mixed_w$deg)[3,1] > gmgmd_sex_b$deg_sex, na.rm = T) / 1000 # b deg sig high, males more deg than females
sum(coef(gm_mixed_w$trans)[3,1] > gmgmd_sex_b$trans_sex, na.rm = T) / 1000 #


# Age by sex in interaction models alone
sum(coef(gm_mixed_int_w$bt)[4,1] >  gmgmd_int_int_b$bt_int_int, na.rm = T) / 1000 # b -0.55 sig lo, males decrease in betweenness w age while fem don't
sum(coef(gm_mixed_int_w$ec)[4,1] > gmgmd_int_int_b$ec_int_int, na.rm = T) / 1000 # b no sig diff
sum(coef(gm_mixed_int_w$deg)[4,1] > gmgmd_int_int_b$deg_int_int, na.rm = T) / 1000 # b 0.45 sig high, males increase deg w age, fem decrease
sum(coef(gm_mixed_int_w$trans)[4,1] > gmgmd_int_int_b$trans_int_int, na.rm = T) / 1000 # b 0.25 sig high, males increase trans w age f dec 

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
