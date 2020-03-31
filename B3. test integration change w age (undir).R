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

# Response: OS status by quarter, 2017 - 2019 (12 points per individual? - average of 3 per quarter?)
# Predictors of OS status:
# (model selection)
# hardship index - years w amputation from snare injury, number of severe injuries?, loss of mother before age x(?)
# average dominance rank during observation * sex
# sex & age
# stability/variation in sna measures (adjusted for observation time)
# magnitude of various sna variables


# 1. Age-sex effects on integration in undirected mixed sex networks (groom and prox) -----
# ----- obs - total grooming (mixed sex) ----

# weighted 
both_gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                   net_sex = "any_combo", subj_sex = "both",
                                   sex_age_int = T, summary = T )
f_gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                             net_sex = "any_combo", subj_sex = "F", summary = T )
m_gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                             net_sex = "any_combo", subj_sex = "M", summary = T )


# unweighted
both_gmgmd_mixed_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                      net_sex = "any_combo", subj_sex = "both",
                                      sex_age_int = T, summary = T )
f_gmgmd_mixed_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                   net_sex = "any_combo", subj_sex = "F", summary = T )
m_gmgmd_mixed_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                   net_sex = "any_combo", subj_sex = "M", summary = T )

# old chimps
both_gmgmd_mixed_w_old <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                      net_sex = "any_combo", subj_sex = "both", 
                                      sex_age_int = T,summary = T )
f_gmgmd_mixed_w_old <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                   net_sex = "any_combo", subj_sex = "F", summary = T )
m_gmgmd_mixed_w_old <- age_sex_fun_all(sna_w, beh = "total_grooming", 
                                   net_sex = "any_combo", subj_sex = "M", summary = T )

both_gmgmd_mixed_uw_old <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                          net_sex = "any_combo", subj_sex = "both", 
                                          sex_age_int = T,summary = T )
f_gmgmd_mixed_uw_old <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                       net_sex = "any_combo", subj_sex = "F", summary = T )
m_gmgmd_mixed_uw_old <- age_sex_fun_all(sna_uw, beh = "total_grooming", 
                                       net_sex = "any_combo", subj_sex = "M", summary = T )


both_gmgmd_mixed_w # strong age sex for deg, rank inc ec
f_gmgmd_mixed_w #deg dec w age
m_gmgmd_mixed_w

both_gmgmd_mixed_uw
f_gmgmd_mixed_uw
m_gmgmd_mixed_uw

both_gmgmd_mixed_w_old 
f_gmgmd_mixed_w_old
m_gmgmd_mixed_w_old 

both_gmgmd_mixed_uw_old 
f_gmgmd_mixed_uw_old
m_gmgmd_mixed_uw_old 


# ----- obs - prox (mixed sex) -----

#weighted
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

# single sex mixed
# weighted # only 47 rows, not enough data...?
f_prox_mixed_w <- sna_w %>% filter(sex == "F") %>% age_fun_all(., beh = "prox", 
                                net_sex = "any_combo", summary = T )
prox_mixed_w # males higher ec and deg than females, lower bt

#unweighted
prox_mixed_uw <- sna_w %>% filter(sex == "F") %>% age_fun_all(., beh = "prox", 
                                                              net_sex = "any_combo", summary = T )
prox_mixed_uw # no effects age or sex



# save(gmgmd_mixed_w, gmgmd_mixed_int_w,
#      gmgmd_mixed_uw, gmgmd_mixed_int_uw,
#      prox_mixed_w, prox_mixed_int_w,
#      prox_mixed_uw, prox_mixed_int_uw, file = "data/models - summaries of age sex effects in undirected mixed networks.Rdata")

#save(gmgmd_mixed_w_old, gmgmd_mixed_uw_old, gmgmd_mixed_int_w_old, gmgmd_mixed_int_uw_old, file =
#       "data/models - summaries of age sex effects of older individuals.Rdata")


# ----- sig w - total grooming (mixed sex - weighted) -----
load("data/random coefs age sex on gmgmd mixed sex net sna weighted.Rdata", verbose = T) # all weighted
load("data/models - summaries of age sex effects in undirected mixed networks.Rdata", verbose = T)

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

# ----- sig uw - total grooming (mixed sex - unweighted) -----
load("data/random coefs age sex on gmgmd mixed sex net sna unweighted.Rdata", verbose = T) # all weighted
load("data/models - summaries of age sex effects in undirected mixed networks.Rdata", verbose = T)

# e.g. gmgmd_age_b
names(gmgmd_sex_b_uw)
# coefficients of age effects on a given sna measure in a model w only age-sex main effects

# Age effects on integration in mixed sex grooming networks (no age sex interaction)
# the observed coefficient is higher than what proportion of random coefs
sum(coef(gmgmd_mixed_uw$bt)[2,1] >  gmgmd_age_b_uw$bt_age, na.rm = T) / 1000 # b of -0.26 sig lo
sum(coef(gmgmd_mixed_uw$ec)[2,1] > gmgmd_age_b_uw$ec_age, na.rm = T) / 1000
sum(coef(gmgmd_mixed_uw$deg)[2,1] > gmgmd_age_b_uw$deg_age, na.rm = T) / 1000 # b of -0.14 sig lo
sum(coef(gmgmd_mixed_uw$trans)[2,1] > gmgmd_age_b_uw$trans_age, na.rm = T) / 1000 # b of 0.06 sig hi

# Sex effects on integration in mixed sex grooming networks (no age sex interaction)
sum(coef(gmgmd_mixed_uw$bt)[3,1] >  gmgmd_sex_b_uw$bt_sex, na.rm = T) / 1000 # b of 1.8 sig hi
sum(coef(gmgmd_mixed_uw$ec)[3,1] > gmgmd_sex_b_uw$ec_sex, na.rm = T) / 1000 # b of 0.84 sig hi
sum(coef(gmgmd_mixed_uw$deg)[3,1] > gmgmd_sex_b_uw$deg_sex, na.rm = T) / 1000 # b of 0.953 sig hi
sum(coef(gmgmd_mixed_uw$trans)[3,1] > gmgmd_sex_b_uw$trans_sex, na.rm = T) / 1000 


# Age by sex in interaction models alone
sum(coef(gmgmd_mixed_int_uw$bt)[4,1] >  gmgmd_int_int_b_uw$bt_int_int, na.rm = T) / 1000
sum(coef(gmgmd_mixed_int_uw$ec)[4,1] > gmgmd_int_int_b_uw$ec_int_int, na.rm = T) / 1000 
sum(coef(gmgmd_mixed_int_uw$deg)[4,1] > gmgmd_int_int_b_uw$deg_int_int, na.rm = T) / 1000 
sum(coef(gmgmd_mixed_int_uw$trans)[4,1] > gmgmd_int_int_b_uw$trans_int_int, na.rm = T) / 1000 # b of 0.23 sig hi

# ----- sig w - prox (mixed sex - weighted) -----
load("data/random coefs age sex on prox sna weighted.Rdata", verbose = T)
load("data/models - summaries of age sex effects in undirected mixed networks.Rdata", verbose = T)

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




# ----- sig uw - prox (mixed sex - unweighted) -----
load("data/random coefs age sex on prox mixed sex net sna unweighted.Rdata", verbose = T)
load("data/models - summaries of age sex effects in undirected mixed networks.Rdata", verbose = T)

# Age effects on integration in mixed sex prox networks (no age sex interaction)
sum(coef(prox_mixed_uw$bt)[2,1] >  prox_age_b_uw$bt_age, na.rm = T) / 1000 #
sum(coef(prox_mixed_uw$ec)[2,1] > prox_age_b_uw$ec_age, na.rm = T) / 1000 # 
sum(coef(prox_mixed_uw$deg)[2,1] > prox_age_b_uw$deg_age, na.rm = T) / 1000 #
sum(coef(prox_mixed_uw$trans)[2,1] > prox_age_b_uw$trans_age, na.rm = T) / 1000 #

# Sex effects on integration in mixed sex grooming networks (no age sex interaction)
sum(coef(prox_mixed_uw$bt)[3,1] >  prox_sex_b_uw$bt_sex, na.rm = T) / 1000 # 
sum(coef(prox_mixed_uw$ec)[3,1] > prox_sex_b_uw$ec_sex, na.rm = T) / 1000 # 
sum(coef(prox_mixed_uw$deg)[3,1] > prox_sex_b_uw$deg_sex, na.rm = T) / 1000 # 
sum(coef(prox_mixed_uw$trans)[3,1] > prox_sex_b_uw$trans_sex, na.rm = T) / 1000 #


# Age by sex in interaction models alone
sum(coef(prox_mixed_int_uw$bt)[4,1] >  prox_int_int_b_uw$bt_int_int, na.rm = T) / 1000 #
sum(coef(prox_mixed_int_uw$ec)[4,1] > prox_int_int_b_uw$ec_int_int, na.rm = T) / 1000 # 
sum(coef(prox_mixed_int_uw$deg)[4,1] > prox_int_int_b_uw$deg_int_int, na.rm = T) / 1000 #
sum(coef(prox_mixed_int_uw$trans)[4,1] > prox_int_int_b_uw$trans_int_int, na.rm = T) / 1000 #


# 2. Age effects on integration in same-sex networks (groom and prox) ------
# ----- obs - prox and grooming (same sex) -----

# all chimps
gmgmd_same_w_f <- age_fun_all(sna_w, beh = "total_grooming", net_sex = "female", summary = T)
gmgmd_same_w_m <- age_fun_all(sna_w, beh = "total_grooming", net_sex = "male", summary = T)
gmgmd_same_uw_f <- age_fun_all(sna_uw, beh = "total_grooming", net_sex = "female", summary = T)
gmgmd_same_uw_m <- age_fun_all(sna_uw, beh = "total_grooming", net_sex = "male", summary = T)

prox_same_w_f <- age_fun_all(sna_w, beh = "prox", net_sex = "female", summary = T ) 
prox_same_w_m <- age_fun_all(sna_w, beh = "prox", net_sex = "male", summary = T ) 
prox_same_uw_f <- age_fun_all(sna_uw, beh = "prox", net_sex = "female", summary = T ) 
prox_same_uw_m <- age_fun_all(sna_uw, beh = "prox", net_sex = "male", summary = T ) 

gmgmd_same_w_f # ec and deg dec sharply w age
gmgmd_same_w_m # ec and deg inc w age, bt no converge
gmgmd_same_uw_f # deg dec ec dec w age, bt almost
gmgmd_same_uw_m #


prox_same_w_f #
prox_same_w_m # ec inc w age, bt no converge
prox_same_uw_f # trans inc
prox_same_uw_m #

# chimps > 30 yo

gmgmd_same_w_f_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "total_grooming", net_sex = "female", summary = T)
gmgmd_same_w_m_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "total_grooming", net_sex = "male", summary = T)
gmgmd_same_uw_f_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "total_grooming", net_sex = "female", summary = T)
gmgmd_same_uw_m_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "total_grooming", net_sex = "male", summary = T)


prox_same_w_f_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "female", summary = T ) 
prox_same_w_m_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "male", summary = T ) 
prox_same_uw_f_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "female", summary = T ) 
prox_same_uw_m_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "male", summary = T ) 


gmgmd_same_w_f_old # trans dec fem older than 32, bt doesn't converge
gmgmd_same_w_m_old # ec dec w age close to sig
gmgmd_same_uw_f_old #
gmgmd_same_uw_m_old # 


prox_same_w_f_old # inc trans
prox_same_w_m_old # no age effects
prox_same_uw_f_old # inc trans
prox_same_uw_m_old # no age effects


#save(gmgmd_same_w_f, gmgmd_same_w_m, prox_same_w_f, prox_same_w_m,
#     gmgmd_same_uw_f, gmgmd_same_uw_m, prox_same_uw_f, prox_same_uw_m, 
#     file = "data/models - summaries of age effects in same sex undirected networks.Rdata")

#save(gmgmd_same_w_f_old, gmgmd_same_w_m_old, prox_same_w_f_old, prox_same_w_m_old,
# gmgmd_same_uw_f_old, gmgmd_same_uw_m_old, prox_same_uw_f_old, prox_same_uw_m_old,
# file = "data/models - summaries of age effects in same sex undirected networks: chimps > 30 yo.Rdata")



# ----- sig - total grooming (same sex - weighted)----
load("data/models - summaries of age effects in same sex undirected networks.Rdata", verbose = T)
load("data/models - summaries of age effects in same sex undirected networks: chimps > 30 yo.Rdata", verbose = T)

load("data/random coefs age on gmgmd same sex net sna weighted.Rdata", verbose = T)
load("data/random coefs age on prox same sex net sna weighted.Rdata", verbose = T)

sum(coef(gmgmd_same_w_f$bt)[2,1] >  gmgmd_age_b_f$bt_age, na.rm = T) / 1000 # no convergence
sum(coef(gmgmd_same_w_f$ec)[2,1] > gmgmd_age_b_f$ec_age, na.rm = T) / 1000 # b -0.59 sig dec ec w age
sum(coef(gmgmd_same_w_f$deg)[2,1] > gmgmd_age_b_f$deg_age, na.rm = T) / 1000 # b of -0.69 is sig low, dec deg w age
sum(coef(gmgmd_same_w_f$trans)[2,1] > gmgmd_age_b_f$trans_age, na.rm = T) / 1000 #

sum(coef(gmgmd_same_w_m$bt)[2,1] >  gmgmd_age_b_m$bt_age, na.rm = T) / 1000 # no convergence
sum(coef(gmgmd_same_w_m$ec)[2,1] > gmgmd_age_b_m$ec_age, na.rm = T) / 1000 # b 0.3 sig inc ec w age
sum(coef(gmgmd_same_w_m$deg)[2,1] > gmgmd_age_b_m$deg_age, na.rm = T) / 1000 # b 0.32 sig inc deg w age
sum(coef(gmgmd_same_w_m$trans)[2,1] > gmgmd_age_b_m$trans_age, na.rm = T) / 1000 #



# ----- sig - total grooming (same sex - unweighted)----
load("data/models - summaries of age effects in same sex undirected networks.Rdata", verbose = T)
load("data/models - summaries of age sex effects of older individuals.Rdata", verbose = T)

load("data/random coefs age on gmgmd same sex net sna unweighted.Rdata", verbose = T)
load("data/random coefs age on prox same sex net sna unweighted.Rdata", verbose = T)



sum(coef(gmgmd_same_uw_f$bt)[2,1] >  gmgmd_age_b_f_uw$bt_age, na.rm = T) / 1000 # sig low
sum(coef(gmgmd_same_uw_f$ec)[2,1] > gmgmd_age_b_f_uw$ec_age, na.rm = T) / 1000 # sig lo
sum(coef(gmgmd_same_uw_f$deg)[2,1] > gmgmd_age_b_f_uw$deg_age, na.rm = T) / 1000 #sig low
sum(coef(gmgmd_same_uw_f$trans)[2,1] > gmgmd_age_b_f_uw$trans_age, na.rm = T) / 1000 #

sum(coef(gmgmd_same_uw_m$bt)[2,1] >  gmgmd_age_b_m_uw$bt_age, na.rm = T) / 1000 #
sum(coef(gmgmd_same_uw_m$ec)[2,1] > gmgmd_age_b_m_uw$ec_age, na.rm = T) / 1000 # sig hi
sum(coef(gmgmd_same_uw_m$deg)[2,1] > gmgmd_age_b_m_uw$deg_age, na.rm = T) / 1000 # sig hi
sum(coef(gmgmd_same_uw_m$trans)[2,1] > gmgmd_age_b_m_uw$trans_age, na.rm = T) / 1000 #


# ----- sig - prox (same sex - weighted) -----

# Age effects on integration in mixed sex prox networks (no age sex interaction)
sum(coef(prox_same_w_f$bt)[2,1] >  prox_age_b_f$bt_age, na.rm = T) / 1000 # 
sum(coef(prox_same_w_f$ec)[2,1] > prox_age_b_f$ec_age, na.rm = T) / 1000 #
sum(coef(prox_same_w_f$deg)[2,1] > prox_age_b_f$deg_age, na.rm = T) / 1000 #
sum(coef(prox_same_w_f$trans)[2,1] > prox_age_b_f$trans_age, na.rm = T) / 1000 #

sum(coef(prox_same_w_m$bt)[2,1] >  prox_age_b_m$bt_age, na.rm = T) / 1000 # no converge
sum(coef(prox_same_w_m$ec)[2,1] > prox_age_b_m$ec_age, na.rm = T) / 1000 # b of .21 sig hi, ec inc w age
sum(coef(prox_same_w_m$deg)[2,1] > prox_age_b_m$deg_age, na.rm = T) / 1000 #
sum(coef(prox_same_w_m$trans)[2,1] > prox_age_b_m$trans_age, na.rm = T) / 1000 #

# ----- sig - prox (same sex - unweighted) -----

# Age effects on integration in mixed sex prox networks (no age sex interaction)
sum(coef(prox_same_uw_f$bt)[2,1] >  prox_age_b_f_uw$bt_age, na.rm = T) / 1000 # 
sum(coef(prox_same_uw_f$ec)[2,1] > prox_age_b_f_uw$ec_age, na.rm = T) / 1000 # sig hi
sum(coef(prox_same_uw_f$deg)[2,1] > prox_age_b_f_uw$deg_age, na.rm = T) / 1000 # sig hi
sum(coef(prox_same_uw_f$trans)[2,1] > prox_age_b_f_uw$trans_age, na.rm = T) / 1000 # sig hi

sum(coef(prox_same_uw_m$bt)[2,1] >  prox_age_b_m_uw$bt_age, na.rm = T) / 1000 #
sum(coef(prox_same_uw_m$ec)[2,1] > prox_age_b_m_uw$ec_age, na.rm = T) / 1000 #
sum(coef(prox_same_uw_m$deg)[2,1] > prox_age_b_m_uw$deg_age, na.rm = T) / 1000 #
sum(coef(prox_same_uw_m$trans)[2,1] > prox_age_b_m_uw$trans_age, na.rm = T) / 1000 #

# 3. Visualization----------
# -- viz - mixed sex gm age sex ------

# grooming
# think we'll find all of these are sig relative to randomization - yup.

sg1 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # sig
  labs( x = "Age (years)", y = "", title = "Grooming Betweenness") +
  annotate("text",x = 35, y = 115, label = "*", size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #annotate("text",x = 52, y = 50, label = "M > F", size = 4)  
sg1

sg2 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") +
  labs( x = "Age (years)", y = "",title = "Grooming Eigenvector centrality") +
  annotate("text",x = 52, y = 1, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #annotate("text",x = 52, y = 0.25, label = "M > F", size = 4)  
sg2

sg3 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig
  labs( x = "Age (years)", y = "", title = "Grooming Weighted degree") +
  annotate("text",x = 35, y = 65, label = "*", size = 12) +
  annotate("text",x = 50, y = 75, label = "** Age changes in \n same sex networks, too", size = 3)+
  theme(plot.title = element_text(hjust = 0.5, size = 14))
  #annotate("text",x = 52, y = 10, label = "M > F", size = 4)  
sg3

sg4 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig
  labs( x = "Age (years)", y = "", title = "Grooming Weighted transitivity") +
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
  geom_point() +
  geom_smooth( method = "lm") +
  labs(x = "Age (years)", y = "" ,title = "Prox Betweenness") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp1
  
sp2 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # sig M F same sex
  labs( x = "Age (years)", y = "", title = "Prox Eigenvector centrality") +
  annotate("text",x = 35, y = 0.9, label = "*", size = 12) +
  annotate("text",x = 50, y = 1, label = "** Age change in \n male network, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp2

sp3 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig mixed, sig M F same sex
  labs( x = "Age (years)", y = "", title = "Prox Weighted degree") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp3

sp4 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") +
  labs( x = "Age (years)", y = "", title = "Prox Weighted transitivity") +
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




