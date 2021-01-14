
library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
library(mgcViz)

source("data/data sets for gams.R") # same sex dfs alredy here
sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw



load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/models gam - mixed sex grooming and total grooming with and without rank - sex specific for viz.Rdata", verbose = T)
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)

load("data/models gam - mixed sex prox with and without rank.Rdata", verbose = T)
load("data/models gam - same sex prox with and without rank.Rdata", verbose = T)


load("data/models - estrus female in mixed sex net.R", verbose = T)

# sig interactiosn for time swollen are in strength and ec prox
plot.gam(ii_sig_f_estr, pages = 1)
plot.gam(ii_ecp_f_estr, pages = 1)

# GROOM MIXED -----
#-msgH1. -------

#      In-Degree mixed sex gm ----

vc_in_deg <- dir_sna_uw %>%
  filter(network_sex == "any_combo" & sex == "female") %>%
  ggplot(aes(prop_cyc, deg_in)) +
  geom_jitter(size = 1, alpha = 0.3) +
  geom_smooth(data = dir_sna_w %>% filter(sex == "M"), method = "gam", formula = y ~ s(x, bs = "cs", k = 5), size = 1) +
  labs( x = "", y = "") + #In-Degree
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10,family = "Georgia"), axis.text.y = element_text(size = 10,family = "Georgia"),
        plot.margin = margin(1,1,1,1, unit = "pt")) #plot.title = element_text(hjust = 0.5, size = 12, family = "Georgia"),
vc_in_deg

#      Out-Degree mixed sex gm ----

vc_out_deg <- dir_sna_uw %>%
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

vc_in_strength <- dir_sna_w %>%
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


vc_out_strength <- dir_sna_w %>%
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

vc_bt_gmgmd <- sna_w %>%
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

vc_trans_gmgmd <- sna_w %>%
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

vc_ec_gmgmd <- sna_w %>%
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


# save grooming panel -------
grobs <- list(vc_in_deg,  vc_in_deg_mal, vc_in_deg_fem,
              vc_out_deg, vc_out_deg_mal, vc_out_deg_fem,
              vc_in_strength, vc_in_strength_mal, vc_in_strength_fem,
              vc_out_strength, vc_out_strength_mal,vc_out_strength_fem,
              vc_trans_gmgmd, vc_trans_gmgmd_w_mal,  vc_trans_gmgmd_w_fem,
              vc_bt_gmgmd, vc_bt_gmgmd_w_mal, vc_bt_gmgmd_w_fem, 
              vc_ec_gmgmd, vc_ec_gmgmd_mal, vc_ec_gmgmd_fem)

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

