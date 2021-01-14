library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)

source("data/data sets for gams.R") # same sex dfs alredy here
sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw



# PROX MIXED ------
#-mspH1. Strength mixed sex prox ----

vr_deg_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, deg, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))



#-mspH2. BT & TRANS mixed sex prox ----

vr_bt_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, bt, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,125,25))+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

vr_trans_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, trans, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-mspH3. EC mixed sex prox ----

vr_ec_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(avg_rank, ec, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "", y = "") +
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))




# PROX SAME --------
#-sspH1. Strength same sex prox ----
#sig_label_m <- expression(M["nr"]^"*")

vr_deg_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, deg)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,250, 50), limits = c(0,250)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

vr_deg_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, deg)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2)  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,250,50), limits = c(0,250)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))


#-sspH2. ----
#     BT same sex prox -------

vr_bt_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

vr_bt_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, bt)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2)  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,50, 10), limits = c(0,50)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

#     TRANS same sex prox -----

vr_trans_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1.1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

vr_trans_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, trans)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2)  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1.1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

#-sspH3. EC same sex prox ----

vr_ec_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red") +
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

vr_ec_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(avg_rank, ec)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) +
  labs( x = "Dominance rank", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2))

### Save panel ---

grobs_prox <- list(vr_deg_prox,  vr_deg_prox_mal, vr_deg_prox_fem,
                   vr_trans_prox, vr_trans_prox_mal,  vr_trans_prox_fem,
                   vr_bt_prox, vr_bt_prox_mal, vr_bt_prox_fem, 
                   vr_ec_prox, vr_ec_prox_mal, vr_ec_prox_fem)

sna <- tableGrob(c( "Strength", "Local \nTransitivity", 
                    "Betweenness", "Eigenvector \nCentrality"), 
                 theme = ttheme_minimal(base_size = 14, base_family = "Georgia"))

prox <- tableGrob(t(c("", "Proximity by rank", "")), theme = ttheme_minimal(base_family = "Georgia", base_size = 17), rows = "")
title <- tableGrob(t(c("Mixed-sex", "All Male", "All Female")), theme = ttheme_minimal(base_family = "Georgia", base_size = 14), rows = "")


grob_bind_prox <- rbind(prox, rbind(title, 
                                    cbind(sna, arrangeGrob(grobs = grobs_prox,  nrow = 4, ncol = 3),
                                          size = "last"), size = "last"), size = "last")

dev.off()
grid.draw(grob_bind_prox)

#save manually 900 by 900