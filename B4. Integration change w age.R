library(tidyverse)
library(magrittr)
library(lmerTest)
library(gridExtra)
library(grid)

# undirected
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
# directed
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)


load("data/attribute data alone.Rdata", verbose = T)
source("functions/functions - age sex modeling.R")
z. <- function(x) scale(x)

#load("data/sna dataframe - individual sna measure for each year, network sex, & behavior.Rdata", verbose = T)

sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw


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
both_gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", net_sex = "any_combo", subj_sex = "both",
                                   sex_age_int = T, summary = T )
f_gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
m_gmgmd_mixed_w <- age_sex_fun_all(sna_w, beh = "total_grooming", net_sex = "any_combo", subj_sex = "M", summary = T )


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

# weighted 
both_prox_mixed_w <- age_sex_fun_all(sna_w, beh = "prox", 
                                      net_sex = "any_combo", subj_sex = "both",
                                      sex_age_int = T, summary = T )
f_prox_mixed_w <- age_sex_fun_all(sna_w, beh = "prox", 
                                   net_sex = "any_combo", subj_sex = "F", summary = T )
m_prox_mixed_w <- age_sex_fun_all(sna_w, beh = "prox", 
                                   net_sex = "any_combo", subj_sex = "M", summary = T )

# unweighted
both_prox_mixed_uw <- age_sex_fun_all(sna_uw, beh = "prox", 
                                       net_sex = "any_combo", subj_sex = "both",
                                       sex_age_int = T, summary = T )
f_prox_mixed_uw <- age_sex_fun_all(sna_uw, beh = "prox", 
                                    net_sex = "any_combo", subj_sex = "F", summary = T )
m_prox_mixed_uw <- age_sex_fun_all(sna_uw, beh = "prox", 
                                    net_sex = "any_combo", subj_sex = "M", summary = T )

# old chimps
both_prox_mixed_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "prox", 
                                          net_sex = "any_combo", subj_sex = "both", 
                                          sex_age_int = T,summary = T )
f_prox_mixed_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "prox", 
                                       net_sex = "any_combo", subj_sex = "F", summary = T )
m_prox_mixed_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "prox", 
                                       net_sex = "any_combo", subj_sex = "M", summary = T )

both_prox_mixed_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "prox", 
                                           net_sex = "any_combo", subj_sex = "both", 
                                           sex_age_int = T,summary = T )
f_prox_mixed_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "prox", 
                                        net_sex = "any_combo", subj_sex = "F", summary = T )
m_prox_mixed_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "prox", 
                                        net_sex = "any_combo", subj_sex = "M", summary = T )

# ----- obs - directed grooming (mixed sex) ----
both_gm_mixed_w <- age_sex_fun_all(dir_sna_w, beh = "grooming", net_sex = "any_combo", sex_age_int = T, summary = T )
f_gm_mixed_w <- age_sex_fun_all(dir_sna_w, beh = "grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
m_gm_mixed_w <- age_sex_fun_all(dir_sna_w, beh = "grooming", net_sex = "any_combo", subj_sex = "M", summary = T )

both_gm_mixed_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming", net_sex = "any_combo", sex_age_int = T, summary = T )
f_gm_mixed_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming", net_sex = "any_combo", subj_sex = "F", summary = T )
m_gm_mixed_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming", net_sex = "any_combo", subj_sex = "M", summary = T )

# many 0's for females in deg out
peep_dataset(dir_sna_w, beh = "grooming", net_sex = "any_combo", subj_sex = "F")
peep_dataset(dir_sna_w, beh = "grooming", net_sex = "any_combo", subj_sex = "M")

#f_gm_mixed_w <- age_sex_fun_single(sna_measure = "deg_out", dir_sna_w, beh = "grooming", net_sex = "any_combo", sex_age_int = F, subj_sex = "F")
#f_gm_mixed_uw <- age_sex_fun_single(sna_measure = "deg_out", dir_sna_uw, beh = "grooming", net_sex = "any_combo", sex_age_int = F, subj_sex = "F")

# old chimps

both_gm_mixed_w_old <- dir_sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "grooming", net_sex = "any_combo", sex_age_int = T, summary = T )
f_gm_mixed_w_old <- dir_sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "grooming", net_sex = "any_combo", subj_sex = "F",  summary = T )
m_gm_mixed_w_old <- dir_sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "grooming", net_sex = "any_combo", subj_sex = "M",  summary = T )

both_gm_mixed_uw_old <- dir_sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "grooming", net_sex = "any_combo", sex_age_int = T, summary = T )
f_gm_mixed_uw_old <- dir_sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "grooming", net_sex = "any_combo", subj_sex = "F",  summary = T )
m_gm_mixed_uw_old <- dir_sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(., beh = "grooming", net_sex = "any_combo", subj_sex = "M",  summary = T )

# ----- save obs models ------

# *8* primary models here, with others adding potential side analyses
# e.g. unweighted undir degree with age
# and patterns in older chimps i.e. > 30 yrs old

# save(both_gmgmd_mixed_w,#*
#     f_gmgmd_mixed_w,    #*
#     m_gmgmd_mixed_w,    #*
#     both_gmgmd_mixed_uw,
#     f_gmgmd_mixed_uw,
#     m_gmgmd_mixed_uw,
#     both_prox_mixed_w,  #*
#     f_prox_mixed_w,     #*
#     m_prox_mixed_w,     #*
#     both_prox_mixed_uw,
#     f_prox_mixed_uw,
#     m_prox_mixed_uw,
#     both_gm_mixed_w,
#     f_gm_mixed_w, #*
#     m_gm_mixed_w, #8
#     both_gm_mixed_uw,
#     f_gm_mixed_uw,
#     m_gm_mixed_uw,
#     file = "data/models - mixed sex networks.Rdata")

# save(both_gmgmd_mixed_w_old,
#     f_gmgmd_mixed_w_old,
#     m_gmgmd_mixed_w_old,
#     both_gmgmd_mixed_uw_old,
#     f_gmgmd_mixed_uw_old,
#     m_gmgmd_mixed_uw_old,
#     both_prox_mixed_w_old,
#     f_prox_mixed_w_old,
#     m_prox_mixed_w_old,
#     both_prox_mixed_uw_old,
#     f_prox_mixed_uw_old,
#     m_prox_mixed_uw_old, both_gm_mixed_w_old,
#     f_gm_mixed_w_old,
#     m_gm_mixed_w_old,
#     both_gm_mixed_uw_old,
#     f_gm_mixed_uw_old,
#     m_gm_mixed_uw_old,
# file = "data/models - mixed sex networks - older.Rdata")


# 2. Age effects on integration in same-sex networks (groom and prox) ------

# ----- obs - total grooming (same sex) -----
#weighted
f_gmgmd_same_w <- age_sex_fun_all(sna_w, beh = "total_grooming", net_sex = "female", summary = T )
m_gmgmd_same_w <- age_sex_fun_all(sna_w, beh = "total_grooming",  net_sex = "male", summary = T)
#unweighted
f_gmgmd_same_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming",net_sex = "female", summary = T ) # not converging
m_gmgmd_same_uw <- age_sex_fun_all(sna_uw, beh = "total_grooming", net_sex = "male", summary = T)
# chimps > 30 yo
f_gmgmd_same_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "total_grooming", net_sex = "female", summary = T )
m_gmgmd_same_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "total_grooming", net_sex = "male", summary = T )
f_gmgmd_same_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "total_grooming", net_sex = "female", summary = T )
m_gmgmd_same_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "total_grooming", net_sex = "male", summary = T )


#peep_dataset(sna_uw, beh = "total_grooming", net_sex = "female")
# ----- obs - prox (same sex) -----
#weighted
f_prox_same_w <- age_sex_fun_all(sna_w, beh = "prox", net_sex = "female", summary = T )
m_prox_same_w <- age_sex_fun_all(sna_w, beh = "prox", net_sex = "male", summary = T)
#unweighted
f_prox_same_uw <- age_sex_fun_all(sna_uw, beh = "prox", net_sex = "female", summary = T )
m_prox_same_uw <- age_sex_fun_all(sna_uw, beh = "prox", net_sex = "male", summary = T)
# chimps > 30 yo
f_prox_same_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "prox", net_sex = "female", summary = T )
m_prox_same_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "prox", net_sex = "male", summary = T )
f_prox_same_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "prox", net_sex = "female", summary = T )
m_prox_same_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "prox", net_sex = "male", summary = T )

#looooads of female-years with 0 bt within same sex network
peep_dataset(sna_w, beh = "prox", net_sex = "female")

# ----- obs - grooming (same sex) -----
#weighted
f_gm_same_w <- age_sex_fun_all(dir_sna_w, beh = "grooming", net_sex = "female", summary = T )
m_gm_same_w <- age_sex_fun_all(dir_sna_w, beh = "grooming",  net_sex = "male", summary = T)
#unweighted
f_gm_same_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming",net_sex = "female", summary = T ) # not converging
m_gm_same_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming", net_sex = "male", summary = T)
# chimps > 30 yo
f_gm_same_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "grooming", net_sex = "female", summary = T )
m_gm_same_w_old <- sna_w %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "grooming", net_sex = "male", summary = T )
f_gm_same_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "grooming", net_sex = "female", summary = T )
m_gm_same_uw_old <- sna_uw %>% filter(age_mid_year > 30) %>% age_sex_fun_all(sna_uw, beh = "grooming", net_sex = "male", summary = T )





# ----- save obs models -----
# save(f_gmgmd_same_w,
#     m_gmgmd_same_w,
#     f_prox_same_w,
#     m_prox_same_w,
#     f_gmgmd_same_uw,
#     m_gmgmd_same_uw,
#     f_prox_same_uw,
#     m_prox_same_uw,
#     f_gm_same_w,
#     m_gm_same_w,
#     f_gm_same_uw,
#     m_gm_same_uw,
#     file = "data/models - same sex networks.Rdata")

# save(f_gmgmd_same_w_old,
#     m_gmgmd_same_w_old,
#     f_prox_same_w_old,
#     m_prox_same_w_old,
#     f_gmgmd_same_uw_old,
#     m_gmgmd_same_uw_old,
#     f_prox_same_uw_old,
#     m_prox_same_uw_old,
#     f_gm_same_w_old,
#     m_gm_same_w_old,
#     f_gm_same_uw_old,
#     m_gm_same_uw_old,
# file = "data/models - same sex networks - older.Rdata")



# Non linear age effects ----

#fem deg in gm uw - yes quad
f_gm_mixed_uw_l  <- age_sex_fun_single(data = dir_sna_uw, sna_measure = "deg_in", beh = "grooming", net_sex = "any_combo", subj_sex = "F", summary = F)
f_gm_mixed_uw_nl <- age_sex_fun_single(data = dir_sna_uw, sna_measure = "deg_in", beh = "grooming", net_sex = "any_combo", subj_sex = "F", quadratic = TRUE, summary = F)
AIC(f_gm_mixed_uw_l, f_gm_mixed_uw_nl)
f_gm_mixed_deg_in_uw <- summary(f_gm_mixed_uw_nl)


#male deg out gm w and uw - no quad for either 
m_gm_mixed_w_l <- age_sex_fun_single(data = dir_sna_w, sna_measure = "deg_out", beh = "grooming", net_sex = "any_combo", subj_sex = "M", quadratic = F, summary = F)
m_gm_mixed_w_nl <- age_sex_fun_single(data = dir_sna_w, sna_measure = "deg_out",  beh = "grooming", net_sex = "any_combo", subj_sex = "M", quadratic = T, summary = F)

AIC(m_gm_mixed_w_l,m_gm_mixed_w_nl)

m_gm_mixed_uw_l <- age_sex_fun_single(data = dir_sna_uw, sna_measure = "deg_out", beh = "grooming", net_sex = "any_combo", subj_sex = "M", quadratic = F, summary = F)
m_gm_mixed_uw_nl <- age_sex_fun_single(data = dir_sna_uw, sna_measure = "deg_out",  beh = "grooming", net_sex = "any_combo", subj_sex = "M", quadratic = T, summary = F)

AIC(m_gm_mixed_uw_l,m_gm_mixed_uw_nl)

#male and fem deg prox - no quad for either
m_prox_mixed_w_l <- age_sex_fun_single(data = sna_w, sna_measure = "deg", beh = "prox", net_sex = "any_combo", subj_sex = "M", quadratic = F, summary = F)
m_prox_mixed_w_nl <- age_sex_fun_single(data = sna_w, sna_measure = "deg", beh = "prox", net_sex = "any_combo", subj_sex = "M", quadratic = T, summary = F)
AIC(m_prox_mixed_w_l, m_prox_mixed_w_nl)

f_prox_mixed_w_l <- age_sex_fun_single(data = sna_w, sna_measure = "deg", beh = "prox", net_sex = "any_combo", subj_sex = "F", quadratic = F, summary = F)
f_prox_mixed_w_nl <- age_sex_fun_single(data = sna_w, sna_measure = "deg", beh = "prox", net_sex = "any_combo", subj_sex = "F", quadratic = T, summary = F)
AIC(f_prox_mixed_w_l, f_prox_mixed_w_nl)

#fem trans gmgmd - yes quad
f_gmgmd_mixed_w_l <- age_sex_fun_single(data = sna_w, sna_measure = "trans", beh = "total_grooming", net_sex = "any_combo", subj_sex = "F", quadratic = F, summary = F)
f_gmgmd_mixed_w_nl <- age_sex_fun_single(data = sna_w, sna_measure = "trans", beh = "total_grooming", net_sex = "any_combo", subj_sex = "F", quadratic = T, summary = F)
AIC(f_gmgmd_mixed_w_l, f_gmgmd_mixed_w_nl)
f_gmgmd_mixed_trans_w <- summary(f_gmgmd_mixed_w_nl)

#male trans gmgmd - yes quad
m_gmgmd_mixed_w_l <- age_sex_fun_single(data = sna_w, sna_measure = "ec", beh = "total_grooming", net_sex = "any_combo", subj_sex = "M", quadratic = F, summary = F)
m_gmgmd_mixed_w_nl <- age_sex_fun_single(data = sna_w, sna_measure = "ec", beh = "total_grooming", net_sex = "any_combo", subj_sex = "M", quadratic = T, summary = F)
AIC(m_gmgmd_mixed_w_l, m_gmgmd_mixed_w_nl)
m_gmgmd_mixed_ec_w <- summary(m_gmgmd_mixed_w_nl)

#male and fem ec prox - quad for females
m_prox_mixed_w_l <- age_sex_fun_single(data = sna_w, sna_measure = "ec", beh = "prox", net_sex = "any_combo", subj_sex = "M", quadratic = F, summary = F)
m_prox_mixed_w_nl <- age_sex_fun_single(data = sna_w, sna_measure = "ec", beh = "prox", net_sex = "any_combo", subj_sex = "M", quadratic = T, summary = F)
AIC(m_prox_mixed_w_l, m_prox_mixed_w_nl)

f_prox_mixed_w_l <- age_sex_fun_single(data = sna_w, sna_measure = "ec", beh = "prox", net_sex = "any_combo", subj_sex = "F", quadratic = F, summary = F)
f_prox_mixed_w_nl <- age_sex_fun_single(data = sna_w, sna_measure = "ec", beh = "prox", net_sex = "any_combo", subj_sex = "F", quadratic = T, summary = F)
AIC(f_prox_mixed_w_l, f_prox_mixed_w_nl)
f_prox_mixed_ec_w <- summary(f_prox_mixed_w_nl)


# save observed non-linear effects
# save(f_gm_mixed_deg_in_uw, f_gmgmd_mixed_trans_w, m_gmgmd_mixed_ec_w, f_prox_mixed_ec_w, file = "data/models - non linear age relationships.Rdata")

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




