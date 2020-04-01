library(tidyverse)

# 1. Age-sex effects on integration in undirected mixed sex networks (groom and prox)-----
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
# ----- sig - total grooming (same sex - weighted)----
load("data/models - summaries of age effects in same sex undirected networks.Rdata", verbose = T)
load("data/models - summaries of age effects in same sex undirected networks: chimps > 30 yo.Rdata", verbose = T)

load("data/random coefs age on gmgmd same sex net sna weighted.Rdata", verbose = T)
load("data/random coefs age on prox same sex net sna weighted.Rdata", verbose = T)

sum(coef(gmgmd_same_w$bt)[2,1] >  gmgmd_age_b_f$bt_age, na.rm = T) / 1000 # no convergence
sum(coef(gmgmd_same_w$ec)[2,1] > gmgmd_age_b_f$ec_age, na.rm = T) / 1000 # b -0.59 sig dec ec w age
sum(coef(gmgmd_same_w$deg)[2,1] > gmgmd_age_b_f$deg_age, na.rm = T) / 1000 # b of -0.69 is sig low, dec deg w age
sum(coef(gmgmd_same_w$trans)[2,1] > gmgmd_age_b_f$trans_age, na.rm = T) / 1000 #

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

