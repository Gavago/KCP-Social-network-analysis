library(mgcViz)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)

# SNA measures

#load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
#load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)

source("data/data sets for gams.R") # same sex dfs alredy here
sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw


# f_same_sna_w <- all_sna_measure_df_w %>%
#   filter(network_type == "female")
# m_same_sna_w <- all_sna_measure_df_w %>%
#   filter(network_type == "male")
# f_same_sna_uw <- all_sna_measure_df_uw %>%
#   filter(network_type == "female")
# m_same_sna_uw <- all_sna_measure_df_uw %>%
#   filter(network_type == "male")
# f_same_dir_sna_w <- dir_sna_measure_df_w %>%
#   filter(network_type == "female")
# f_same_dir_sna_w <- dir_sna_measure_df_w %>%
#   filter(network_type == "female")
# m_same_dir_sna_uw <- dir_sna_measure_df_uw %>%
#   filter(network_type == "male")


# Rank w age ----
rank_by_age <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, avg_rank, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5 , alpha = 0.5) +
  labs(x = "Age (years)", y = "Average annual rank", family = "Georgia") +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = .8) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = .8) +
  theme_bw() +
  theme(axis.title.y = element_text(family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none") 
rank_by_age

ggsave(filename = "results/visualization/GAM figures/Rank by age.png", plot = rank_by_age, height = 4, width = 4)


# GAM models -----
load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/models gam - mixed sex grooming and total grooming with and without rank - sex specific for viz.Rdata", verbose = T)
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)

load("data/models gam - mixed sex prox with and without rank.Rdata", verbose = T)
load("data/models gam - same sex prox with and without rank.Rdata", verbose = T)




# GROOM MIXED -----
#-msgH1. -------

#      In-Degree mixed sex gm ----

# ggplot matches gam well, female and male pattern w R signif in mixed

sig_label <- expression(F["ir"]^"*" *" "* M["ir"]^"*")

plot(dig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dig)[1]) # no change with and without rank
plot(mdig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdig)[1])  #looks positive when males alone, neg in interaction?
plot(fdig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdig)[1]) 

plot(digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(digr)[1]) 
plot(mdigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdigr)[1]) #male should have early peak
plot(fdigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdigr)[1]) 

# fem_teen <- dir_sna_uw %>%
#  filter(age_mid_year > 15 & age_mid_year < 20) %>%
#  filter(network_sex == "any_combo" & sex == "F") %>%
#   mutate(id_year = paste(chimp_id, year, sep = "-"))


v_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 3, alpha = 0.3) + # shape = 1
  #geom_smooth(method = "gam", formula = y ~ s(x, bs ="cs", k = 5)) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2) +
  labs( x = "", y = "", title = "In-Degree") + #Age (years)
  #annotate("text",x = 35, y = 14, label = sig_label, size = 5, family = "Georgia") +
  #geom_label_repel(data = fem_teen, aes(x = age_mid_year, y = deg_in, label = id_year)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_in_deg

#ggsave(filename = "results/visualization/females with high in-degree in late teens.png", plot = v_in_deg, width = 6, height = 6)

#      Out-Degree mixed sex gm ----


# ggplot matches gam, M sig NR and R

sig_label <- expression(M["nr,ir"]^"*")

plot(dog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dog)[1])
plot(mdog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdog)[1])
plot(fdog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdog)[1]) 

plot(dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dogr)[1]) 
plot(mdogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdogr)[1]) 
plot(fdogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdogr)[1]) 

v_out_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 3, alpha = 0.3) +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2) + 
  labs( x = "Age (years)", y = "", title = "Out-Degree") +
  #annotate("text",x = 35, y = 20, label = sig_label, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_out_deg

# msgH1b. 
#      In-Strength mixed sex gm -----

# ggplot matches gam well , no sig pattern in mixed sex sig

plot(sig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sig)[1]) #no change - strength-in goes up w age for males w and without rank
plot(msig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msig)[1]) # difference in scale
plot(fsig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsig)[1])

plot(sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sigr)[1])
plot(msigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msigr)[1]) # m alone is decrease but flat in interaction
plot(fsigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsigr)[1])

v_in_strength <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 3, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "lm", size = 2) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2) +
  #geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  labs(x = "", y = "", title = "In-Strength") + #Age (years)
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_in_strength

#      Out-Strength mixed sex gm -----
# ggplot matches gam well, no sig pattern in mixed sex sog

plot(sog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sog)[1]) #no change in direction or sig of age on strength-out in males
plot(msog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msog)[1]) 
plot(fsog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsog)[1])

plot(sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sogr)[1]) 
plot(msogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msog)[1]) # plot doesn't look the same as in ggplot w gam
plot(fsogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsog)[1])

v_out_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 3, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2) +
  labs( x = "Age (years)", y = "", title = "Out-Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12) )
v_out_strength



#-msgH2 ------
#     BT mixed sex gm -----

# ggplot matches gam well, no sig patterns in mixed sex bt

plot(btg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btg)[1]) # without rank, female parabolic but non sig, male linear decrease sig
plot(mbtg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mbtg)[1])
plot(fbtg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fbtg)[1])

plot(btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btgr)[1]) # with rank, both linear decrease (rank inc bt for both sexes) and male still sig
plot(mbtgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mbtgr)[1])
plot(fbtgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fbtgr)[1])

v_bt_gmgmd <- sna_w %>%
  filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 3, alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 2) +
  labs( x = "", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_bt_gmgmd

#     TRANS mixed sex gm -----
# ggplot matches gam plot well, M and F trans change w age NR and R

sig_label <- expression(F["nr,ir"]^"*" *" "* M["nr,ir"]^"*")

plot(trg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trg)[1]) # female sig parabolic in both, male no more sig increase after adding rank, can see from concurvity check that male rank predicts transitivity
plot(mtrg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mtrg)[1]) 
plot(ftrg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ftrg)[1]) 

plot(trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trgr)[1])  #male transitivity was linear in interaction
plot(mtrgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mtrgr)[1]) # difference in pattern is just matter of scale
plot(ftrgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ftrgr)[1]) 

#  fem_twenties <- sna_w %>%
#    filter(age_mid_year > 20 & age_mid_year < 30) %>%
#    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
#    mutate(id_year = paste(chimp_id, year, sep = "-"))
# all_f <- sna_w %>%
#   filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
#   mutate(id_year = paste(chimp_id, year, sep = "-"))


v_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 3 , alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 2) + 
  labs(x = "", y = "", title = "Local Transitivity") +
  #annotate("text",x = 35, y = 0.25, label = sig_label, size = 5, family = "Georgia") +
  #geom_label_repel(data = fem_twenties, aes(x = age_mid_year, y = trans, label = id_year)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_trans_gmgmd

#ggsave(filename = "results/visualization/females with high trans 20s.png", plot = v_in_deg, width = 6, height = 6)



#-msgH3. EC mixed sex gm ------

# ggplot matches gam where rank is included, when rank parcelled out, marginal effect of age shows rise and plateau and not a peak and dip
# M pattern sig NR and R
sig_label <- expression(M["nr,ir"]^"*")

plot(ecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecg)[1]) #
plot(mecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mecg)[1]) # hm, maybe male pattern is preserved in interaction model, just diminished bc on female scale!
plot(fecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fecg)[1]) #

plot(ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecgr)[1]) #that's different from linear model. see male ec w age is linear
plot(mecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mecgr)[1])
plot(fecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fecgr)[1])

v_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, ec, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 3, alpha = 0.3) +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8) + # separate lines mimic plot gam better, but looks like it doesn't fit points...
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2) +
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm", size = 2) +
  labs( x = "Age (years)", y = "",title = "Eigenvector Centrality") +
  #annotate("text",x = 35, y = .65, label = sig_label, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_ec_gmgmd








# GROOM SAME -----
#-ssgH1. ------
#      In-Degree same sex gm ----

# ggplot matches gam well

sig_label_m <- expression(M["nr,ir"]^"*")

plot(f_dig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_dig)[1]) 
plot(m_dig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_dig)[1]) 

plot(f_digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_digr)[1]) 
plot(m_digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_digr)[1]) 


v_in_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 3 , alpha = 0.5, shape = 1, color = "red") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2, linetype = 1, color = "red") +
  labs( x = "", y = "", title = "In-Degree") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_in_deg_fem

v_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 3, alpha = 0.5, shape = 2, color = "blue") + # shape = 1
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 2, linetype = 2) +
  labs( x = "", y = "", title = "In-Degree") +
  #annotate("text",x = 35, y = 2.5, label = sig_label_m, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_in_deg_mal

#      Out-Degree same sex gm ------

# ggplot matches ok

plot(f_dog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_dog)[1])
plot(m_dog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_dog)[1])

plot(f_dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_dogr)[1]) 
plot(m_dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_dogr)[1]) 

v_out_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 3, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 1, color = "red") + 
  labs( x = "", y = "", title = "Out-Degree") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_out_deg_fem

v_out_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 3, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 2) + 
  labs( x = "", y = "", title = "Out-Degree") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_out_deg_mal

#      In-Strength same sex gm -----

plot(m_sig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sig)[1]) # difference in scale
plot(f_sig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sig)[1])

plot(m_sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sigr)[1]) # m alone is decrease but flat in interaction
plot(f_sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sigr)[1])

v_in_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 3,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 2) +
  labs(x = "", y = "", title = "In-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_in_strength_fem

v_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 3, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 2) +
  labs(x = "", y = "", title = "In-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_in_strength_mal

#      Out-Strength same sex gm -----
# ggplot matches gam well, no sig pattern in mixed sex sog

sig_label_f <- expression(F["nr"]^"*")

plot(f_sog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sog)[1])
plot(m_sog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sog)[1])

plot(f_sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sogr)[1]) 
plot(m_sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sogr)[1]) 

v_out_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 3, shape = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "red", size =2) +
  labs(x = "Age (years)", y = "", title = "Out-Strength") +
  theme_bw() +
  #annotate("text",x = 35, y = 2.5, label = sig_label_f, size = 5, family = "Georgia") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_out_strength_fem

v_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 3, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 2) +
  labs(x = "Age (years)", y = "", title = "Out-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_out_strength_mal

#-ssgH2. -----------
#     BT same sex gm -------

#matches gam plot ok

plot(f_btg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_btg)[1])
plot(m_btg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_btg)[1])

plot(f_btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_btgr)[1]) 
plot(m_btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_btgr)[1]) 

v_bt_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 3,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 1, color = "red") + 
  labs( x = "", y = "", title = "Betweenness") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_bt_gmgmd_w_fem

v_bt_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 3, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 2) + 
  labs( x = "", y = "", title = "Betweenness") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_bt_gmgmd_w_mal

#     TRANS same sex gm -----
plot(f_trg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_trg)[1])
plot(m_trg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_trg)[1])

plot(f_trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_trgr)[1]) 
plot(m_trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_trgr)[1]) 

v_trans_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 3,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 1, color = "red") + 
  labs( x = "", y = "", title = "Local Transitivity") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_trans_gmgmd_w_fem

v_trans_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 3, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 2) + 
  labs( x = "", y = "", title = "Local Transitivity") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_trans_gmgmd_w_mal

#-ssgH3. EC Same sex gm -----

# matches gam well
sig_label_f <- expression(F["nr"]^"*")

sig_label_m <- expression(M["nr,ir"]^"*")


plot(f_ecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_ecg)[1])
plot(m_ecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_ecg)[1])
plot(f_ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_ecgr)[1])
plot(m_ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_ecgr)[1])

v_ec_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 3,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "lm", size = 2, color = "red") + # separate lines mimic plot gam better, but looks like it doesn't fit points...
  #geom_smooth(data = sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  #geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm") +
  labs( x = "Age (years)", y = "",title = "Eigenvector Centrality") +
  #annotate("text",x = 35, y = .8, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_ec_gmgmd_fem

v_ec_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 3, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 2, linetype = 2) + # separate lines mimic plot gam better, but looks like it doesn't fit points...
  #geom_smooth(data = sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  #geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm") +
  labs( x = "Age (years)", y = "",title = "Eigenvector Centrality") +
  #annotate("text",x = 37, y = .35, label = sig_label_m, size = 5, family = "Georgia") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "none",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
v_ec_gmgmd_mal



# PROX



# PROX MIXED ------
#-mspH1. Strength mixed sex prox ----
plot(sp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sp)[1])
plot(spr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sogr)[1]) 

v_deg_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, deg, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Age (years)", y = "", title = "Strength") + #tag = "A)",
  theme_bw() +
  theme(plot.tag = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_prox


#-mspH2. BT & TRANS mixed sex prox ----

plot(btp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btp)[1])
plot(btpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btpr)[1]) 

v_bt_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, bt, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(x = "Age (years)", y = "", title = "Betweenness") + #tag = "C)",
  theme_bw() +
  theme(plot.tag = element_text(size = 14),plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_bt_prox

plot(trp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trp)[1])
plot(trpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trpr)[1]) 


v_trans_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, trans, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "lm") + # formula = y ~ s(x, bs = "cs")
  labs( x = "Age (years)", y = "", title = "Local Transitivity") + #tag = "B)",
  theme_bw() +
  theme(plot.tag = element_text(size = 14), plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_trans_prox


#-mspH3. EC mixed sex prox ----
plot(ecp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecp)[1]) #
plot(ecpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecpr)[1]) # really interesting that the rel between male age and ec becomes pos and liear when controlling for 

sig_label <- expression(F["nr"]^"*" *" "* M["nr,ir"]^"*")

v_ec_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, ec, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Age (years)", y = "", title = "Eigenvector Centrality") + #tag = "D)",
  #annotate("text",x = 35, y = .6, label = sig_label, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.tag = element_text(size = 14),plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_ec_prox




# PROX SAME --------
#-sspH1. Strength same sex prox ----
#sig_label_m <- expression(M["nr"]^"*")

plot(f_sp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sp)[1])
plot(f_spr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_spr)[1]) 

plot(m_sp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sp)[1])
plot(m_spr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_spr)[1]) 

v_deg_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red") +
  labs( x = "Age (years)", y = "", title = "Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_prox_fem

v_deg_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Age (years)", y = "", title = "Strength") +
  #annotate("text",x = 35, y = 12.5, label = sig_label_m, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_prox_mal


#-sspH2. ----
#     BT same sex prox -------

plot(f_btp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_btp)[1])
plot(m_btp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_btp)[1])

plot(f_btpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_btpr)[1]) 
plot(m_btpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_btpr)[1]) 

v_bt_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 1.5, alpha = 0.5, color = "red") + 
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red") + 
  labs( x = "Age (years)", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_bt_prox_fem

v_bt_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 1.5, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Age (years)", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_bt_prox_mal

#     TRANS same sex prox -----
plot(f_trp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_trp)[1])
plot(m_trp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_trp)[1])

plot(f_trpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_trpr)[1]) 
plot(m_trpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_trpr)[1]) 

v_trans_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 1.5, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red") + 
  labs( x = "Age (years)", y = "", title = "Local Transitivity") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_trans_prox_fem

v_trans_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 1.5, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Age (years)", y = "", title = "Local Transitivity") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_trans_prox_mal

#-sspH3. EC same sex prox ----
sig_label_m <- expression(M["nr,ir"]^"*")

plot(f_ecp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_ecp)[1]) #
plot(f_ecpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_ecpr)[1])

plot(m_ecp, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_ecp)[1]) #
plot(m_ecpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_ecpr)[1])

v_ec_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 1, color = "red") +
  labs( x = "Age (years)", y = "",title = "Eigenvector Centrality") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_ec_prox_fem

v_ec_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) +
  labs( x = "Age (years)", y = "",title = "Eigenvector Centrality") +
  #annotate("text",x = 35, y = 0.6, label = sig_label_m, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_ec_prox_mal



# Save ----


# grooming mixed ----
dev.off()
g <- arrangeGrob(v_in_deg, v_in_strength, 
                 v_out_deg, v_out_strength,
                 nrow = 2, top = textGrob("Directed Grooming (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 18)))
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H1 Grooming mixed directed degree.png", plot = g, height = 6, width = 6)

dev.off()
g <- arrangeGrob(v_trans_gmgmd,v_bt_gmgmd,
                 nrow = 1, top = textGrob("Total Grooming (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 18)))
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H2 Grooming mixed bt trans.png", plot = g, height = 3.25, width = 6)

dev.off()
g <- arrangeGrob(v_ec_gmgmd, nrow = 1) # top = textGrob("Total Grooming (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 18))
grid.draw(g)
v_ec_gmgmd
ggsave(filename = "results/visualization/GAM figures/H3 Grooming mixed eigenvector centrality.png", plot = g, height = 3, width = 3)


#   grooming same ----

#      degree strength ----
dev.off()
g <- arrangeGrob(v_in_deg_fem, v_in_deg_mal, v_in_strength_fem, v_in_strength_mal,
                 v_out_deg_fem, v_out_deg_mal, v_out_strength_fem, v_out_strength_mal, nrow = 4, top = textGrob("Directed Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 18))) #str in out
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H1 Grooming same sex in- out- degree and strength.png", plot = g, height = 12, width = 6)

#      bt trans -----
dev.off()

g <- arrangeGrob( v_trans_gmgmd_w_fem, v_trans_gmgmd_w_mal, v_bt_gmgmd_w_fem, v_bt_gmgmd_w_mal, nrow = 2,top = textGrob("Total Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 18))) # deg in out

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H2 Grooming same sex bt trans.png", plot = g, height = 6, width = 6)



#      ec ------
dev.off()

g <- arrangeGrob(v_ec_gmgmd_fem, v_ec_gmgmd_mal, nrow = 1) # top = textGrob("Total Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16))
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H3 Grooming same eigenvector centrality.png", plot = g, height = 3, width = 6)


#   prox mixed -----
dev.off()
g <- arrangeGrob(v_deg_prox,  v_trans_prox, v_bt_prox, v_ec_prox, nrow = 2, top = textGrob("Proximity (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 18)))

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H1- 3 Proximity .png", plot = g, height = 6, width = 6)


#   prox same ----
dev.off()
g <- arrangeGrob(v_deg_prox_fem, v_deg_prox_mal,
                 v_trans_prox_fem, v_trans_prox_mal,
                 v_bt_prox_fem, v_bt_prox_mal, 
                 v_ec_prox_fem, v_ec_prox_mal, nrow = 4,
                 top = textGrob("Proximity (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H1-3 Prox same.png", plot = g, height = 10, width = 5)



# gyard -----
# annotate("text",x = 36, y = 38, label = "*", size = 12, family = "Georgia") +
#   annotate("text",x = 32, y = 35, label = "M*", size = 5, family = "Georgia") +
#   annotate("text",x = 40, y = 35, label = "F*", size = 5, family = "Georgia") +
#legend.title = element_text(family = "Georgia"), legend.text = element_text(family = "Georgia"))


# b <- getViz(dig)
# c <- smooth(sm(b,2))
# b + l_fitLine(colour = sex) + l_rug(mapping = aes(x= age_mid_year, y= deg_in), alpha = 0.8) +
#   l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
#   l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
# b

dev.off()
g <- arrangeGrob(v_in_deg_fem, v_out_deg_fem,v_in_deg_mal, v_out_deg_mal, nrow = 2,top = textGrob("Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16))) # deg in out

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H1a Grooming same sex degree.png", plot = g, height = 6, width = 5)

dev.off()
g <- arrangeGrob(v_in_strength_fem, v_out_strength_fem, v_in_strength_mal, v_out_strength_mal, nrow = 2, top = textGrob("Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16))) #str in out
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/H1b Grooming same sex strength.png", plot = g, height = 6, width = 5)

