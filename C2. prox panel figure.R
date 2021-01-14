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

v_deg_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, deg, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))


#-mspH2. BT & TRANS mixed sex prox ----

v_bt_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, bt, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"))  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,125,25))

v_trans_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, trans, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "lm")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1))


#-mspH3. EC mixed sex prox ----

v_ec_prox <- sna_w %>%
  filter(network_sex == "any_combo" & behavior  == "prox") %>%
  ggplot(aes(age_mid_year, ec, color = sex, label = chimp_id, shape = sex, linetype = sex)) +
  geom_jitter(size = 1, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "", y = "") +
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1))




# PROX SAME --------
#-sspH1. Strength same sex prox ----
#sig_label_m <- expression(M["nr"]^"*")

v_deg_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,250, 50), limits = c(0,250))

v_deg_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2)  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))  +
  scale_y_continuous(breaks = seq(0,250,50), limits = c(0,250))


#-sspH2. ----
#     BT same sex prox -------

v_bt_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 1, alpha = 0.5, color = "red") + 
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))

v_bt_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, bt)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2)  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))  +
  scale_y_continuous(breaks = seq(0,50, 10), limits = c(0,50))

#     TRANS same sex prox -----

v_trans_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 1, alpha = 0.5, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red")  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt"))  +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1.1))

v_trans_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, trans)) +
  geom_jitter(size = 1, shape = 2, alpha = 0.5, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2)  +
  labs( x = "", y = "") +
  theme_bw()  +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))  +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1.1))

#-sspH3. EC same sex prox ----

v_ec_prox_fem <- f_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 1, color = "red") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, color = "red") +
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1))

v_ec_prox_mal <- m_same_sna_w %>%
  filter(behavior == "prox") %>%
  ggplot(aes(age_mid_year, ec)) +
  geom_jitter(size = 1, alpha = 0.5, shape = 2, color = "blue") +
  geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs"), size = 0.8, linetype = 2) +
  labs( x = "Age (years)", y = "") +
  theme_bw()  +
  theme(axis.title.x = element_text(family = "Georgia", size = 12), legend.position = "none",
        axis.text.x = element_text(size = 11, family = "Georgia"), axis.text.y = element_text(size = 10, family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) +
  scale_y_continuous(breaks = seq(0,1,.2), limits = c(0,1)) +
  scale_x_continuous(breaks = c(10,20,30,40,50), limits = c(12,57))

### Save panel ---

grobs_prox <- list(v_deg_prox,  v_deg_prox_mal, v_deg_prox_fem,
              v_trans_prox, v_trans_prox_mal,  v_trans_prox_fem,
              v_bt_prox, v_bt_prox_mal, v_bt_prox_fem, 
              v_ec_prox, v_ec_prox_mal, v_ec_prox_fem)

sna <- tableGrob(c( "Strength", "Local \nTransitivity", 
                    "Betweenness", "Eigenvector \nCentrality"), 
                 theme = ttheme_minimal(base_size = 14, base_family = "Georgia"))

prox <- tableGrob(t(c("", "Proximity", "")), theme = ttheme_minimal(base_family = "Georgia", base_size = 17), rows = "")
title <- tableGrob(t(c("Mixed-sex", "All Male", "All Female")), theme = ttheme_minimal(base_family = "Georgia", base_size = 14), rows = "")


grob_bind_prox <- rbind(prox, rbind(title, 
                                     cbind(sna, arrangeGrob(grobs = grobs_prox,  nrow = 4, ncol = 3),
                                           size = "last"), size = "last"), size = "last")

dev.off()
grid.draw(grob_bind_prox)
