
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)

source("data/data sets for gams.R") # same sex dfs alredy here
sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw


# GROOM MIXED -----
#-msgH1. -------

#      In-Degree mixed sex gm ----


v_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  labs( x = "", y = "") + #In-Degree
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) #plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"),


#      Out-Degree mixed sex gm ----

v_out_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1) + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))

# msgH1b. 
#      In-Strength mixed sex gm -----

v_in_strength <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "lm", size = 1) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  labs(x = "", y = "") + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#      Out-Strength mixed sex gm -----


v_out_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia") )


#-msgH2 ------
#     BT mixed sex gm -----

v_bt_gmgmd <- sna_w %>%
  filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1) +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#     TRANS mixed sex gm -----
# ggplot matches gam plot well, M and F trans change w age NR and R

v_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1 , alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1) + 
  labs(x = "", y = "") +
  theme_bw() +
  theme( legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#-msgH3. EC mixed sex gm ------

v_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, ec, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm", size = 1) +
  labs( x = "Age (years)", y = "") +
  theme_bw() +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


# GROOM SAME -----
#-ssgH1. ------
#      In-Degree same sex gm ----

v_in_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 1 , alpha = 0.5, shape = 1, color = "red") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1, linetype = 1, color = "red") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9))

v_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") + # shape = 1
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1, linetype = 2) +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))


#      Out-Degree same sex gm ------

v_out_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 1, color = "red") + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9))


v_out_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57)) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9))


#      In-Strength same sex gm -----

v_in_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 1) +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,17))


v_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_in)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 1) +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57)) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,17))


#      Out-Strength same sex gm -----

v_out_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 1, shape = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 1) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,15))


v_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 1) +
  theme_bw()  +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57)) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,15))


#-ssgH2. -----------
#     BT same sex gm -------

v_bt_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 1, color = "red") + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 45, 10), limits = c(0,50))



v_bt_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57)) +
  scale_y_continuous(breaks = seq(0, 45, 10), limits = c(0,50))


#     TRANS same sex gm -----

v_trans_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 1, color = "red") + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


v_trans_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))


#-ssgH3. EC Same sex gm -----

v_ec_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "lm", size = 1, color = "red") + 
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0,1))

v_ec_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) +
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))

# save grooming panel -------
grobs <- list(v_in_deg,  v_in_deg_mal, v_in_deg_fem,
              v_out_deg, v_out_deg_mal, v_out_deg_fem,
              v_in_strength, v_in_strength_mal, v_in_strength_fem,
              v_out_strength, v_out_strength_mal,v_out_strength_fem,
              v_trans_gmgmd, v_trans_gmgmd_w_mal,  v_trans_gmgmd_w_fem,
              v_bt_gmgmd, v_bt_gmgmd_w_mal, v_bt_gmgmd_w_fem, 
              v_ec_gmgmd, v_ec_gmgmd_mal, v_ec_gmgmd_fem)

sna <- tableGrob(c( "In-Degree", "Out-Degree", "In-Strength",
                   "Out-Strength", "Local \nTransitivity", 
                   "Betweenness", "Eigenvector \nCentrality"), 
                 theme = ttheme_minimal(base_size = 14, base_family = "Georgia"))

grooming <- tableGrob(t(c("", "Grooming", "")), theme = ttheme_minimal(base_family = "Georgia", base_size = 17), rows = "")
title <- tableGrob(t(c("Mixed-sex", "All Male", "All Female")), theme = ttheme_minimal(base_family = "Georgia", base_size = 14), rows = "")

grob_bind <- rbind(title, 
                    cbind(sna, arrangeGrob(grobs = grobs,  nrow = 7, ncol = 3),
                          size = "last"), size = "last")

grob_bind2 <- rbind( grooming, rbind(title, 
                   cbind(sna, arrangeGrob(grobs = grobs,  nrow = 7, ncol = 3),
                         size = "last"), size = "last"), size = "last")

dev.off()
grid.draw(grob_bind2)

#saving manually at 800 width and 1200 height
#ggsave(filename = "results/visualization/GAM figures/Grooming panel.png", plot = g, height = 15, width = 7)




# GRAVEYARD -----

