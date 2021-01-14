
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


rv_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  labs( x = "", y = "") + #In-Degree
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) + #plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"),
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

#      Out-Degree mixed sex gm ----

rv_out_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_out, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1) + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

# msgH1b. 
#      In-Strength mixed sex gm -----

rv_in_strength <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_in, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "lm", size = 1) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  labs(x = "", y = "") + 
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Strength mixed sex gm -----


rv_out_strength <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, deg_out, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "F"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia") ) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-msgH2 ------
#     BT mixed sex gm -----

rv_bt_gmgmd <- sna_w %>%
  filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, bt, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1) +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#     TRANS mixed sex gm -----
# ggplot matches gam plot well, M and F trans change w age NR and R

rv_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(avg_rank, trans, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1 , alpha = 0.3) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), size = 1) + 
  labs(x = "", y = "") +
  theme_bw() +
  theme( legend.position = "none",
         axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
         plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-msgH3. EC mixed sex gm ------

rv_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, ec, color = sex, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm", size = 1) +
  labs( x = "Dominance rank", y = "") +
  theme_bw() +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


# GROOM SAME -----
#-ssgH1. ------
#      In-Degree same sex gm ----

rv_in_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1 , alpha = 0.5, shape = 1, color = "red") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1, linetype = 1, color = "red") +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

rv_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") + # shape = 1
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1, linetype = 2) +
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Degree same sex gm ------

rv_out_deg_fem <- f_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 1, color = "red") + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_out_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,2,4,6,8), limits = c(0,9)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      In-Strength same sex gm -----

rv_in_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 1) +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,17))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_in)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 1) +
  labs(x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,17))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#      Out-Strength same sex gm -----

rv_out_strength_fem <- f_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1, shape = 1, alpha = 0.5, color = "red") +
  geom_smooth(method = "lm", color = "red", size = 1) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,15)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(avg_rank, deg_out)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), linetype = 2, size = 1) +
  theme_bw()  +
  labs(x = "", y = "") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = c(0,5,10,15), limits = c(0,15)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-ssgH2. -----------
#     BT same sex gm -------

rv_bt_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 1, color = "red") + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 45, 10), limits = c(0,50)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))



rv_bt_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0, 45, 10), limits = c(0,50)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#     TRANS same sex gm -----

rv_trans_gmgmd_w_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 1, color = "red") + 
  labs( x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


rv_trans_gmgmd_w_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) + 
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-ssgH3. EC Same sex gm -----

rv_ec_gmgmd_fem <- f_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1,  shape = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "lm", size = 1, color = "red") + 
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.25), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

rv_ec_gmgmd_mal <- m_same_sna_w %>%
  filter(behavior == "total_grooming") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 1, linetype = 2) +
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

# save grooming panel -------
grobs <- list(rv_in_deg,  rv_in_deg_mal, rv_in_deg_fem,
              rv_out_deg, rv_out_deg_mal, rv_out_deg_fem,
              rv_in_strength, rv_in_strength_mal, rv_in_strength_fem,
              rv_out_strength, rv_out_strength_mal,rv_out_strength_fem,
              rv_trans_gmgmd, rv_trans_gmgmd_w_mal,  rv_trans_gmgmd_w_fem,
              rv_bt_gmgmd, rv_bt_gmgmd_w_mal, rv_bt_gmgmd_w_fem, 
              rv_ec_gmgmd, rv_ec_gmgmd_mal, rv_ec_gmgmd_fem)

sna <- tableGrob(c( "In-Degree", "Out-Degree", "In-Strength",
                    "Out-Strength", "Local \nTransitivity", 
                    "Betweenness", "Eigenvector \nCentrality"), 
                 theme = ttheme_minimal(base_size = 14, base_family = "Georgia"))

grooming <- tableGrob(t(c("", "Grooming by rank", "")), theme = ttheme_minimal(base_family = "Georgia", base_size = 17), rows = "")
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
#ggsave(filename = "results/visualization/GAM figures/Grooming by rank panel.png", plot = g, height = 15, width = 7)




# GRAVEYARD -----

