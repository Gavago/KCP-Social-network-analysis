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


dir_sna_w <- dir_sna_measure_df_w
#dir_sna_uw <- dir_sna_measure_df_uw

# 1. Age-sex effects on integration in directed mixed sex networks (groom and prox) -----
# ----- obs - directed grooming (mixed sex) ----

 
gm_mixed_w <- age_sex_fun_all(dir_sna_w, beh = "grooming", 
                              net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_w <- age_sex_fun_all(dir_sna_w, beh = "grooming", 
                                  net_sex = "any_combo", sex_age_int = T, summary = T )

gm_mixed_w #males higher deg in and out
gm_mixed_int_w # no age changes


gm_mixed_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming", 
                               net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_uw <- age_sex_fun_all(dir_sna_uw, beh = "grooming", 
                                   net_sex = "any_combo", sex_age_int = T, summary = T )
gm_mixed_uw #
gm_mixed_int_uw #

# old chimps

gm_mixed_w_old <- dir_sna_w %>% 
  filter(age_mid_year > 30) %>%
  age_sex_fun_all(., beh = "grooming", 
                  net_sex = "any_combo", sex_age_int = F, summary = T )
gm_mixed_int_w_old <- dir_sna_w %>% 
  filter(age_mid_year > 30) %>%
  age_sex_fun_all(., beh = "grooming", 
                  net_sex = "any_combo", sex_age_int = T, summary = T )

gm_mixed_w_old # deg dec w age
gm_mixed_int_w_old


#save(gm_mixed_w, gm_mixed_int_w, gm_mixed_uw, gm_mixed_int_uw, file = "data/models - summaries of age sex effects in directed gm mixed networks.Rdata")


# ----- sig - directed grooming (mixed sex) -----
load("data/models - summaries of age sex effects in directed gm mixed networks.Rdata", verbose = T) # all weighted bt and ec
load("data/random coefs age sex on dir grooming mixed sex net sna.Rdata", verbose = T)

# e.g. gmgmd_age_b
names(gmgmd_sex_b)
# coefficients of age effects on a given sna measure in a model w only age-sex main effects

# Age effects on integration in mixed sex grooming networks (no age sex interaction)
# the observed coefficient is higher than what proportion of random coefs
sum(coef(gm_mixed_w$bt)[2,1] >  gm_age_b$bt_age, na.rm = T) / 1000 # no converge
sum(coef(gm_mixed_w$ec)[2,1] > gm_age_b$ec_age, na.rm = T) / 1000 # no converge
sum(coef(gm_mixed_w$deg_in)[2,1] > gm_age_b$deg_in_age, na.rm = T) / 1000 # 
sum(coef(gm_mixed_w$deg_out)[2,1] > gm_age_b$deg_out_age, na.rm = T) / 1000 #

# Sex effects on integration in mixed sex grooming networks (no age sex interaction)
sum(coef(gm_mixed_w$bt)[3,1] >  gm_sex_b$bt_sex, na.rm = T) / 1000 #
sum(coef(gm_mixed_w$ec)[3,1] > gm_sex_b$ec_sex, na.rm = T) / 1000 #
sum(coef(gm_mixed_w$deg_in)[3,1] > gm_sex_b$deg_in_sex, na.rm = T) / 1000 #
sum(coef(gm_mixed_w$deg_out)[3,1] > gm_sex_b$deg_out_sex, na.rm = T) / 1000 #


# Age by sex in interaction models alone
sum(coef(gm_mixed_int_w$bt)[4,1] >  gm_int_int_b$bt_int_int, na.rm = T) / 1000 #
sum(coef(gm_mixed_int_w$ec)[4,1] > gm_int_int_b$ec_int_int, na.rm = T) / 1000 #
sum(coef(gm_mixed_int_w$deg_in)[4,1] > gm_int_int_b$deg_in_int_int, na.rm = T) / 1000 # b 0.5 sig high, males increase deg in w age, fem decrease
sum(coef(gm_mixed_int_w$deg_out)[4,1] > gm_int_int_b$deg_out_int_int, na.rm = T) / 1000 # b 0.38 sig high, males increase deg out w age f dec 

# 2. Age effects on integration in same-sex networks (groom and prox) ------
# ----- obs - prox and grooming (same sex) -----

# all chimps
gm_same_w_f <- age_fun_all(dir_sna_w, beh = "grooming", net_sex = "female", summary = T)
gm_same_w_m <- age_fun_all(dir_sna_w, beh = "grooming", net_sex = "male", summary = T)
#gm_same_uw_f <- age_fun_all(dir_sna_uw, beh = "grooming", net_sex = "female", summary = T) #intereseting, weighting picks up the pattern
#gm_same_uw_m <- age_fun_all(dir_sna_uw, beh = "grooming", net_sex = "male", summary = T) 


prox_same_w_f <- age_fun_all(dir_sna_uw, beh = "prox", net_sex = "female", summary = T ) 
prox_same_w_m <- age_fun_all(dir_sna_w, beh = "prox", net_sex = "male", summary = T ) 


gm_same_w_f # F deg out dec w age
gm_same_w_m # M deg in inc w age

prox_same_w_f # no age changes
prox_same_w_m # ec inc w age

# chimps > 30 yo

gm_same_w_f_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "grooming", net_sex = "female", summary = T)
gm_same_w_m_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "grooming", net_sex = "male", summary = T)

prox_same_w_f_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "female", summary = T ) 
prox_same_w_m_old <- sna_w %>% filter(age_mid_year > 30) %>% age_fun_all(., beh = "prox", net_sex = "male", summary = T ) 

gm_same_w_f_old #trans dec fem older than 32, bt doesn't converge
gm_same_w_m_old # ec dec w age, v close to sig

prox_same_w_f_old # inc trans
prox_same_w_m_old # no age effects

#save(gm_same_w_f, gm_same_w_m, prox_same_w_f, prox_same_w_m, file = "data/models - summaries of age effects in same sex dir groom networks.Rdata")
#save(gm_same_w_f_old, gm_same_w_m_old, prox_same_w_f_old, prox_same_w_m_old, file = "data/models - summaries of age effects in same sex dir groom networks: chimps > 30 yo.Rdata")

#save(gm_same_w_f, gm_same_w_m, prox_same_w_f, prox_same_w_m, file = "data/models - summaries of age effects in same sex dir groom networks.Rdata")

# ----- sig - directed grooming (same sex) weighted ----
load("data/models - summaries of age effects in same sex dir groom networks.Rdata", verbose = T) #
load("data/random coefs age on gm same sex net sna.Rdata", verbose = T)

#sum(coef(gm_same_w_f$bt)[2,1] >  gm_age_b_f$bt_age, na.rm = T) / 1000 # no convergence
#sum(coef(gm_same_w_f$ec)[2,1] > gm_age_b_f$ec_age, na.rm = T) / 1000 # no convergence
sum(coef(gm_same_w_f$deg_in)[2,1] > gm_age_b_f$deg_in_age, na.rm = T) / 1000 # b -0.36 almost sig low
sum(coef(gm_same_w_f$deg_out)[2,1] > gm_age_b_f$deg_out_age, na.rm = T) / 1000 # b -2.69 sig low, perfect, females are *choosing* essentially to be less integrated with age, and males are doing the opposite in that they're just more atractive w age

#sum(coef(gm_same_w_m$bt)[2,1] >  gm_age_b_m$bt_age, na.rm = T) / 1000 #
#sum(coef(gm_same_w_m$ec)[2,1] > gm_age_b_m$ec_age, na.rm = T) / 1000 # b 0.33 sig high
sum(coef(gm_same_w_m$deg_in)[2,1] > gm_age_b_m$deg_in_age, na.rm = T) / 1000 # b 0.42 sig inc deg w age # this is hot. this is the increase in prestige I predicted.
sum(coef(gm_same_w_m$deg_out)[2,1] > gm_age_b_m$deg_out_age, na.rm = T) / 1000 # not sig, males increase ec w age bc receiving more gm, not giving

# 3. Vizualization ------


dir_sna_w %>%
  ggplot(aes(x = age_mid_year, y = deg_out, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +  #sig age sex effect mixed net, sig decrease among female net
  labs( x = "Age (years)", y = "", title = "Grooming given: Weighted degree") +
  annotate("text",x = 35, y = 35, label = "*", size = 12) +
  annotate("text",x = 50, y = 40, label = "** Age change in \n female network, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
#females give less gm w age

dir_sna_w %>%
  ggplot(aes(x = age_mid_year, y = deg_in, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ylim(0, 42) +
  labs( x = "Age (years)", y = "", title = "Grooming received: Weighted degree") +
  annotate("text",x = 35, y = 35, label = "*", size = 12) +
  annotate("text",x = 50, y = 40, label = "** Age change in \n male network, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
# males give more gm w age