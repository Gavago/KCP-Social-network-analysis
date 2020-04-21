
library(tidyverse)
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
# directed
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
source("functions/functions - age sex modeling.R")
source("functions/functions - table age sex results.R")

sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw



# 3. Visualization----------
#### H1 - attraction isolation - w and uw degree -------
# - gm ---- 
# -- deg in ----

# weighted
v_deg_in_gm_w <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, label = chimp_id)) + #
  geom_point() +
  geom_smooth( method = "lm") + #no non linear
  labs( x = "Age (years)", y = "", title = "W Deg In") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  geom_text()
v_deg_in_gm_w


# unweighted
v_deg_in_gm_uw <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + #males linear, female non-lin
  labs( x = "Age (years)", y = "", title = "UW Deg In") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_deg_in_gm_uw

# -- deg out -----

# weighted
v_deg_out_gm_w <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id)) +
  geom_point() +
  geom_smooth( method = "loess") + #male non linear, female linear
  labs( x = "Age (years)", y = "", title = "W Deg Out") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14)) +
  geom_text()
v_deg_out_gm_w

# unweighted
v_deg_out_gm_uw <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + #male non linear female linear
  labs( x = "Age (years)", y = "", title = "UW Deg Out") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_deg_out_gm_uw

# - prox ----
# -- deg ----

v_deg_prox_w <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + #loess no clearer
  labs( x = "Age (years)", y = "", title = "W Degree Prox") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_deg_prox_w


#### H2 - bridge vs cluster - bt and trans -------
# - gmgmd ----
# --- bt----
v_bt_gm <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + # no clear curves loess
  labs( x = "Age (years)", y = "", title = "W Grooming Betweenness") +
  annotate("text",x = 35, y = 115, label = "*", size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_bt_gm

# --- trans ----
v_trans_gm <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + #extreme curve for F w loess
  labs( x = "Age (years)", y = "", title = "Grooming Weighted transitivity") +
  annotate("text",x = 35, y = 0.9, label = "*", size = 12) +
  annotate("text",x = 50, y = 1, label = "** Age changes in \n same sex networks, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_trans_gm

# - prox -----
# --- bt -----
v_bt_prox <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # no clear pattern w loess
  labs(x = "Age (years)", y = "" ,title = "Prox Betweenness") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_bt_prox
# --- trans ------
v_trans_prox <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # no diff w loess, almost no variation in prox trans
  labs( x = "Age (years)", y = "", title = "Prox Weighted transitivity") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_trans_prox

#### H3 - embededness - ec --------

#  - gmgmd -----
v_ec_gm <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + # clear > 35 yr drop in males
  labs( x = "Age (years)", y = "",title = "Grooming Eigenvector centrality") +
  annotate("text",x = 52, y = 1, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_ec_gm

#ATTEMPT TO PLOT Marginal relationship of EC and age - see if any affect of age left after rank accounted for


# - prox -----
v_ec_prox <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") + #see non linear pattern, > 30 male and female decline
  labs( x = "Age (years)", y = "", title = "Prox Eigenvector centrality") +
  annotate("text",x = 35, y = 0.9, label = "*", size = 12) +
  annotate("text",x = 50, y = 1, label = "** Age change in \n male network, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
v_ec_prox

# save vis ----
# ------- H1 ----
#pdf("results/results viz - age sex changes in grooming integration.pdf", height = 16, width = 16)
#grid.arrange(grobs = list(sg1, sg2, sg3, sg4), nrow = 2, top = textGrob("Age changes in grooming integration, mixed sex networks"), gp = gpar(fontize = 32))
#dev.off()
# ------- H2 ----
# ------- H3 ----
pdf("results/results viz - age sex changes in proximity integration.pdf", height = 16, width = 16)
grid.arrange(grobs = list(sp1, sp2, sp3, sp4), nrow = 2, top = textGrob("Age changes in proximity integration, mixed sex network"), gp = gpar(fontize = 32))
dev.off()


# explore particular individuals -----
sna_uw %>%
  filter(chimp_id == "QT", network_sex == "female", behavior =="total_grooming") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_point() +
  geom_smooth(method = "lm")

