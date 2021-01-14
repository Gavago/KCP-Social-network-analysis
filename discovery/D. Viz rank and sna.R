library(mgcViz)
library(tidyverse)
library(grid)
library(gridExtra)

# SNA measures
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)

sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw

source("data/data sets for gams.R") # good for same sex dfs

# GAM models -----
load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/models gam - mixed sex grooming and total grooming with and without rank - sex specific for viz.Rdata", verbose = T)
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)

load("data/models gam - mixed sex prox with and without rank.Rdata", verbose = T)
load("data/models gam - same sex prox with and without rank.Rdata", verbose = T)




# plot.gam and then immediately ggplot


# H1 ------
# msgH1. -------
#      In-Degree mixed sex gm ----

plot(digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(digr)[1]) 
plot(mdigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdigr)[1]) #male should have early peak
plot(fdigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdigr)[1]) 

rank_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5 , alpha = 0.5) + # shape = 1
  #geom_smooth(method = "gam", formula = y ~ s(x, bs ="cs", k = 5)) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = .8) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = .8) +
  #annotate("text", x = 0.5, y = 15, label = "F*, M*", size = 5, family = "Georgia") +
  labs( x = "", y = "", title = "In-Degree") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_in_deg

#      Out-Degree mixed sex gm ----

plot(dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dogr)[1]) 
plot(mdogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdogr)[1]) 
plot(fdogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdogr)[1]) 

rank_deg_out_gm_uw <- dir_sna_uw %>% # male rank doesn't look quite same as dogr plot
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_out, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8) + 
  labs( x = "Avg annual rank", y = "", title = "Out-Degree") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_deg_out_gm_uw

# msgH1b. 
#      In-Strength mixed sex gm -----

plot(sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sigr)[1])
plot(msigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msigr)[1]) # m alone is decrease but flat in interaction
plot(fsigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsigr)[1])

rank_in_strength <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "lm") +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  #geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  labs(x = "", y = "", title = "In-Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_in_strength

#      Out-Strength mixed sex gm -----

plot(sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sogr)[1]) 
plot(msogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msog)[1]) # plot doesn't look the same as in ggplot w gam
plot(fsogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsog)[1])

rank_out_strength <- dir_sna_w %>% #doesn't look same as sogr
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_out, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  labs( x = "Avg annual rank", y = "", title = "Out-Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_out_strength

# ssgH1. -------
#      In-Degree same sex gm ----

plot(f_digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_digr)[1]) 
plot(m_digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_digr)[1]) 


rank_in_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 0.8, linetype = 1, color = "red") +
  labs( x = "", y = "", title = "In-Degree") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_in_deg_fem

rank_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2, color = "blue") + # shape = 1
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 0.8, linetype = 2) +
  labs( x = "", y = "", title = "In-Degree") +
  #annotate("text",x = 0.5, y = 2.5, label = sig_label_m, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_in_deg_mal

#      Out-Degree same sex gm ------

plot(f_dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_dogr)[1]) 
plot(m_dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_dogr)[1]) 

rank_out_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 1, color = "red") + 
  labs( x = "", y = "", title = "Out-Degree") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_out_deg_fem

rank_out_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "", y = "", title = "Out-Degree") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_out_deg_mal

#      In-Strength same sex gm -----

plot(m_sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sigr)[1]) # m alone is decrease but flat in interaction
plot(f_sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sigr)[1])

rank_in_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 1, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 0.8) +
  labs(x = "", y = "", title = "In-Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_in_strength_fem

rank_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 2,  color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) +
  labs(x = "", y = "", title = "In-Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_in_strength_mal

#      Out-Strength same sex gm -----

plot(f_sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_sogr)[1]) 
plot(m_sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_sogr)[1]) 

rank_out_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 1, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 0.8) + # + method = "gam", formula = y ~ s(x, bs = "cs")) + #
  labs(x = "Avg annual rank", y = "", title = "Out-Strength") +
  theme_bw() +
  #annotate("text",x = 0.5, y = 4, label = "*", size = 10, family = "Georgia") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_out_strength_fem



rank_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 0.8) +
  labs(x = "Avg annual rank", y = "", title = "Out-Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_out_strength_mal

# mspH1. Strength mixed sex prox ----

plot(spr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sogr)[1]) 

rank_strength_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, deg, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Avg annual rank", y = "", title = "Strength") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_strength_prox


# sspH1. Strength same sex prox ----

plot(f_spr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_spr)[1]) 
plot(m_spr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_spr)[1]) 

rank_strength_prox_fem <- f_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 1, color = "red") +
  labs( x = "Avg annual rank", y = "", title = "Strength") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2, color = "red") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_strength_prox_fem

rank_strength_prox_mal <- m_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Avg annual rank", y = "", title = "Strength") +
  #annotate("text",x = 35, y = 2.5, label = sig_label_m, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_strength_prox_mal


# -----
# -----
# H2 ----
# msgH2 ------
#     BT mixed sex gm -----

plot(btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btgr)[1]) # with rank, both linear decrease (rank inc bt for both sexes) and male still sig
plot(mbtgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mbtgr)[1])
plot(fbtgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fbtgr)[1])

rank_bt_gmgmd <- sna_w %>%
  filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, bt, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = .8) +
  labs( x = "Avg annual rank", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_bt_gmgmd

#     TRANS mixed sex gm -----

plot(trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trgr)[1])  #male transitivity was linear in interaction
plot(mtrgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mtrgr)[1]) # difference in pattern is just matter of scale
plot(ftrgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ftrgr)[1]) 


rank_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, trans, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5 , alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8) + 
  labs(x = "Avg annual rank", y = "", title = "Local Transitivity") +
  annotate("text",x = .5, y = 1, label = "F*", size = 6, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_trans_gmgmd

# ssgH2. 
#     BT same sex gm -------

plot(f_btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_btgr)[1]) 
plot(m_btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_btgr)[1]) 

rank_bt_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1.5, alpha = 0.3, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 1, color = "red") + 
  labs( x = "Avg annual rank", y = "", title = "Betweenness") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_bt_gmgmd_fem

rank_bt_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1.5, shape = 2, alpha = 0.3, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Avg annual rank", y = "", title = "Betweenness") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_bt_gmgmd_mal

#     TRANS same sex gm -----

plot(f_trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_trgr)[1]) 
plot(m_trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_trgr)[1]) 

rank_trans_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1.5, alpha = 0.3, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 1, color = "red") + 
  labs( x = "Avg annual rank", y = "", title = "Local Transitivity") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_trans_gmgmd_fem

rank_trans_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1.5, shape = 2, alpha = 0.3, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Avg annual rank", y = "", title = "Local Transitivity") +
  #annotate("text",x = 35, y = 20, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_trans_gmgmd_mal

# mspH2. BT & TRANS mixed sex prox ----

plot(btpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btpr)[1]) 

rank_bt_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, bt, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Avg annual rank", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_bt_prox


plot(trpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trpr)[1]) 


rank_trans_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, trans, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Avg annual rank", y = "", title = "Local Transitivity") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_trans_prox


# sspH2. ----
#     BT same sex gm -------

plot(f_btpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_btpr)[1]) 
plot(m_btpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_btpr)[1]) 

rank_bt_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1.5, alpha = 0.3, color = "red") + 
  labs( x = "Avg annual rank", y = "", title = "Betweenness") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2, color = "red") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_bt_prox_fem

rank_bt_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1.5, shape = 2, alpha = 0.3, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  labs( x = "Avg annual rank", y = "", title = "Betweenness") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_bt_prox_mal

#     TRANS same sex prox -----

plot(f_trpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_trpr)[1]) 
plot(m_trpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_trpr)[1]) 

rank_trans_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1.5, alpha = 0.3, color = "red") +
  labs( x = "Avg annual rank", y = "", title = "Local Transitivity") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2, color = "red") + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_trans_prox_fem

rank_trans_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1.5, shape = 2, alpha = 0.3, color = "blue") +
  labs( x = "Avg annual rank", y = "", title = "Local Transitivity") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_trans_prox_mal

# H3 ------------
# msgH3. EC mixed sex gm ------

plot(ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecgr)[1]) #that's different from linear model. see male ec w age is linear
plot(mecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mecgr)[1])
plot(fecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fecgr)[1])

rank_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, ec, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  #geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8) + # separate lines mimic plot gam better, but looks like it doesn't fit points...
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm") +
  labs( x = "Avg annual rank", y = "",title = "Eigenvector Centrality") +
  #annotate("text",x = 0.5, y = 1, label = "M*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_ec_gmgmd


# ssgH3. EC Same sex gm -----

plot(f_ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_ecgr)[1])
plot(m_ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_ecgr)[1])

rank_ec_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1.5, alpha = 0.3, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 0.8, color = "red") + 
  labs( x = "Avg annual rank", y = "",title = "Eigenvector Centrality") +
  #annotate("text",x = 35, y = .8, label = sig_label_f, size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_ec_gmgmd_fem

rank_ec_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) +
  labs( x = "Avg annual rank", y = "",title = "Eigenvector Centrality") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_ec_gmgmd_mal


# mspH3. EC mixed sex prox ----

plot(ecpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecpr)[1]) # really interesting that the rel between male age and ec becomes pos and liear when controlling for 

rank_ec_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, ec, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Avg annual rank", y = "", title = "Eigenvector Centrality") +
  annotate("text",x = 0.5, y = 1, label = "M*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_ec_prox

# sspH3. EC same sex prox ----

plot(f_ecpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(f_ecpr)[1])
plot(m_ecpr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(m_ecpr)[1])

rank_ec_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 1, color = "red") +
  labs( x = "Avg annual rank", y = "",title = "Eigenvector Centrality") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2, color = "red") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_ec_prox_fem

rank_ec_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1.5, alpha = 0.3, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) +
  labs( x = "Avg annual rank", y = "",title = "Eigenvector Centrality") +
  annotate("text",x = .5, y = 1, label = "M*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
rank_ec_prox_mal

# Save ----

#   grooming mixed ----
dev.off()
g <- arrangeGrob(rank_in_deg, rank_in_strength, 
                 rank_deg_out_gm_uw, rank_out_strength,
                 nrow = 2, top = textGrob("Directed Grooming (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H1 Grooming mixed directed degree by rank.png", plot = g, height = 6, width = 6)

dev.off()
g <- arrangeGrob(rank_trans_gmgmd,rank_bt_gmgmd,
                 nrow = 1, top = textGrob("Total Grooming (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H2 Grooming mixed bt trans by rank.png", plot = g, height = 5, width = 9)

dev.off()
g <- arrangeGrob(rank_ec_gmgmd,
                 nrow = 1, top = textGrob("Total Grooming (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H3 Grooming mixed eigenvector centrality by rank.png", plot = g, height = 5, width = 8)

#   prox mixed -----
dev.off()
g <- arrangeGrob(rank_strength_prox, rank_bt_prox, rank_trans_prox, rank_ec_prox, nrow = 2, top = textGrob("Proximity (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H1-3 Proximity by rank.png", plot = g, height = 6, width = 5)

#   grooming same ----

#      degree strength ----
dev.off()
g <- arrangeGrob(rank_in_deg_fem, rank_in_deg_mal, rank_in_strength_fem, rank_in_strength_mal,
                 rank_out_deg_fem, rank_out_deg_mal, rank_out_strength_fem, rank_out_strength_mal, nrow = 4, top = textGrob("Directed Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16))) #str in out
grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H1 Grooming same sex degree by rank.png", plot = g, height = 8, width = 4)


#      bt trans -----
dev.off()

g <- arrangeGrob(rank_trans_gmgmd_fem, rank_bt_gmgmd_fem,rank_trans_gmgmd_mal, rank_bt_gmgmd_mal,  nrow = 2,top = textGrob("Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16))) # deg in out

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H2 Grooming same sex bt trans by rank.png", plot = g, height = 6, width = 5)


#      ec ------
dev.off()

g <- arrangeGrob(rank_ec_gmgmd_fem, rank_ec_gmgmd_mal, nrow = 1,top = textGrob("Grooming (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16))) # deg in out

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H3 Grooming same eigenvector centrality by rank.png", plot = g, height = 6, width = 8)


#   prox same ----
dev.off()
g <- arrangeGrob(rank_strength_prox_fem, rank_strength_prox_mal,
                 rank_bt_prox_fem, rank_bt_prox_mal, # fem bt and trans and ec, male trans no smooth, not able to model
                 rank_trans_prox_fem, rank_trans_prox_mal,
                 rank_ec_prox_fem, rank_ec_prox_mal, nrow = 4,
                 top = textGrob("Proximity (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H1-3 Prox same by rank.png", plot = g, height = 12, width = 6)

### Supp - EC prox mixed alone -----
dev.off()
g <- arrangeGrob(rank_ec_prox, nrow = 1, top = textGrob("Proximity (mixed sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H3 Proximity by rank mixed sex.png", plot = g, height = 5, width = 5)


### Supp - EC same sex prox male alone ----
dev.off()
g <- arrangeGrob(rank_ec_prox_mal, nrow = 1,
                 top = textGrob("Proximity (same sex)", gp=gpar(fontfamily = "Georgia", fontsize = 16)))

grid.draw(g)
ggsave(filename = "results/visualization/GAM figures/sna by rank/H3 Prox same by rank prox same sex.png", plot = g, height = 5, width = 5)





# gyard -----
