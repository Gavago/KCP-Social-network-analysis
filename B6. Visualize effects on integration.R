
library(tidyverse)
library(extrafont)
library(gridExtra)
library(grid)
#loadfonts(device = "win")


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

load("data/sig tests - non linear models.Rdata", verbose = T)
# 4 models for lines to be drawn non -linearly, add separate lm and loess lines per ggplot
unique(non_linear_raw$mod_name)



# Visualization----------

#### Mixed sex networks -----------
#### H1 - attraction isolation - w and uw degree -------
# - gm ---- 
dir_deg_tab

# -- deg in ----


# weighted
v_deg_in_gm_w <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, label = chimp_id)) + #
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "lm", size = .5) + #no non linear
  labs(x = "Age (years)", y = "", title = "Strength In") +
  annotate("text",x = 36, y = 38, label = "*", size = 12, family = "Georgia") +
  annotate("text",x = 32, y = 35, label = "M*", size = 5, family = "Georgia") +
  annotate("text",x = 40, y = 35, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
  #geom_text() 
v_deg_in_gm_w

# Viz Does not show male dec w age - this is weird, what's going on. Possibly bc not controlling for rank...
# Plot marginal relationship?

# unweighted
v_deg_in_gm_uw <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  #geom_smooth(method = "loess") +
  geom_smooth(data = dir_sna_uw %>% filter(sex == "M"), method = "lm", size = .5) +
  geom_smooth(data = dir_sna_uw %>% filter(sex == "F"), method = "loess", size = .5) +
  geom_smooth(data = dir_sna_uw %>% filter(sex == "F"), method = "lm", formula = y ~ x + I(x^2), linetype = 2, size = .5) + #males linear, female non-lin
  labs( x = "Age (years)", y = "", title = "Degree In") +
  annotate("text",x = 32, y = 14, label = "M*", size = 5, family = "Georgia") +
  annotate("text",x = 40, y = 14, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_in_gm_uw

# -- deg out -----

# weighted
v_deg_out_gm_w <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "lm") + # both linear - male maybe non linear, female linear
  labs( x = "Age (years)", y = "", title = "Strength Out") +
  annotate("text",x = 32, y = 35, label = "M*", size = 5, family = "Georgia") +
  annotate("text",x = 40, y = 35, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
  #geom_text()
v_deg_out_gm_w

# unweighted
v_deg_out_gm_uw <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "lm") + # both linear - male maybe non linear female linear
  labs( x = "Age (years)", y = "", title = "Degree Out") +
  annotate("text",x = 32, y = 20, label = "M*", size = 5, family = "Georgia") +
  annotate("text",x = 40, y = 20, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
        #legend.title = element_text(family = "Georgia"), legend.text = element_text(family = "Georgia"))
v_deg_out_gm_uw


g <- arrangeGrob(v_deg_in_gm_uw, v_deg_in_gm_w, 
                 v_deg_out_gm_uw, v_deg_out_gm_w, 
                 nrow = 2, top = textGrob("Directed Grooming", gp=gpar(fontfamily = "Georgia", fontsize = 18)))
grid.draw(g)
ggsave(filename = "results/visualization/directed degree.png", plot = g, height = 9, width = 8)


# - prox ----
# -- deg ----

v_deg_prox_w <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "lm") + #loess no clearer
  labs( x = "Age (years)", y = "", title = "Strength Proximity") +
  annotate("text",x = 35, y = 4, label = "*", size = 12, family = "Georgia") +
  annotate("text",x = 35, y = 3.5, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_prox_w

#uw deg not interesting bc number of partners so saturated over a year

ggsave(filename = "results/visualization/Strength prox.png", width = 4, height = 4)

#### H2 - bridge vs cluster - bt and trans -------
# - gmgmd ----
# --- bt----
v_bt_gmgmd <- sna_w %>%
  filter(bt < 125) %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "lm", size = .5) + # no clear curves loess
  labs( x = "Age (years)", y = "", title = "Total grooming") +
  annotate("text",x = 36, y = 95, label = "M*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_bt_gmgmd

# --- trans ----
v_trans_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_jitter( shape = 1, size = 1.5) +
  #geom_smooth(method = "loess") + #extreme curve for F w loess
  geom_smooth(data = sna_uw %>% filter(sex == "M"), method = "lm", size = .5) +
  geom_smooth(data = sna_uw %>% filter(sex == "F"), method = "loess", size = .5) +
  geom_smooth(data = sna_uw %>% filter(sex == "F"), method = "lm", formula = y ~ x + I(x^2), linetype = 2, size = .5) +
  labs( x = "Age (years)", y = "", title = "Total Grooming") +
  annotate("text",x = 36, y = .95, label = "*", size = 12, family = "Georgia") +
  annotate("text",x = 32, y = .9, label = "M*", size = 5, family = "Georgia") +
  annotate("text",x = 40, y = .9, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
  
v_trans_gmgmd

# - prox -----
# --- bt -----
v_bt_prox <- sna_w %>%
  filter(bt < 100) %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "lm") + # no clear pattern w loess
  labs(x = "Age (years)", y = "" ,title = "Proximity") +
  annotate("text",x = 36, y = 85, label = "M*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_bt_prox

# --- trans ------
v_trans_prox <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "lm") + # no diff w loess, almost no variation in prox trans
  labs( x = "Age (years)", y = "", title = "Proximity") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none") #legend.text = element_text(family = "Georgia"), legend.title = element_text(family = "Georgia")
v_trans_prox

g <- arrangeGrob(v_bt_gmgmd, v_bt_prox,
                 nrow = 2, top = textGrob("Betweenness", gp=gpar(fontfamily = "Georgia", fontsize = 18)))
grid.draw(g)
ggsave(filename = "results/visualization/Betweenness.png", plot = g, height = 9, width = 4)

g <- arrangeGrob(v_trans_gmgmd, v_trans_prox,
                 nrow = 2, top = textGrob("Local Transitivity", gp=gpar(fontfamily = "Georgia", fontsize = 18)))
grid.draw(g)
ggsave(filename = "results/visualization/Transitivity.png", plot = g, height = 9, width = 4)


#### H3 - embededness - ec --------

#  - gmgmd -----
v_ec_gmgmd <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  #geom_smooth( method = "loess") + # clear > 35 yr drop in males
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm", size = .5) +
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "loess", size = .5) +
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "lm", formula = y ~ x + I(x^2), linetype = 2, size = .5) +
  labs( x = "Age (years)", y = "",title = "Total Grooming") +
  annotate("text",x = 35, y = .95, label = "M*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_ec_gmgmd

#?ATTEMPT TO PLOT Marginal relationship of EC and age - see if any affect of age left after rank accounted for

# - prox -----
v_ec_prox <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  #geom_smooth( method = "loess") + #see non linear pattern, > 30 male and female decline
  geom_smooth(data = sna_w %>% filter(sex == "M"), method = "lm", size = .5) +
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "loess", size = .5) +
  geom_smooth(data = sna_w %>% filter(sex == "F"), method = "lm", formula = y ~ x + I(x^2), linetype = 2, size = .5) +
  labs( x = "Age (years)", y = "", title = "Proximity") +
  annotate("text",x = 35, y = .95, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), 
        axis.title.x = element_text(family = "Georgia"), legend.position = "none") #, legend.text = element_text(family = "Georgia"),legend.title = element_text(family = "Georgia")
v_ec_prox

g <- arrangeGrob(v_ec_gmgmd, v_ec_prox,
                 nrow = 1, top = textGrob("Eigenvector Centrality", gp=gpar(fontfamily = "Georgia", fontsize = 18)))
grid.draw(g)
ggsave(filename = "results/visualization/Eigenvector centrality.png", plot = g, height = 5, width = 8)



# explore particular individuals -----
sna_uw %>%
  filter(chimp_id == "QT", network_sex == "female", behavior =="total_grooming") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_point() +
  geom_smooth(method = "lm")



# saving arranged plots
pdf("results/visualization/directed degree.pdf", height = 9, width = 8)
grid.arrange(v_deg_in_gm_w, v_deg_in_gm_uw,
             v_deg_out_gm_w, v_deg_out_gm_uw,
             nrow = 2, top = textGrob("Directed Grooming",
                                      gp=gpar(fontfamily = "Georgia", fontsize = 18)))
dev.off()

####
# Same sex - evaluate non-linear
# H1 -------
# -- deg in ----


# weighted
dir_sna_w %>%
  filter(network_sex == "female") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, label = chimp_id)) + #
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "loess", size = .5) #no non linear
dir_sna_w %>%
  filter(network_sex == "male") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex, label = chimp_id)) + #
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "loess", size = .5) #some non linear, but is increase and plateau not parabolic

# unweighted
dir_sna_uw %>%
  filter(network_sex == "female") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "loess", size = .5) # no non lin

dir_sna_uw %>%
  filter(network_sex == "male") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "loess", size = .5) # yes test for non lin

# -- deg out -----

# weighted
dir_sna_w %>%
  filter(network_sex == "female") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # no non lin

dir_sna_w %>%
  filter(network_sex == "male") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # very non lin


# unweighted
dir_sna_uw %>%
  filter(network_sex == "female") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") #no non lin
  
dir_sna_uw %>%
  filter(network_sex == "male") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") #non lin

# - prox ----
# -- deg ----

sna_w %>%
  filter(behavior == "prox" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # no non lin

sna_w %>%
  filter(behavior == "prox" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # non lin


# H2 -------
# - gmgmd ----
# --- bt----
sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess", size = .5) # no non lin

sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess", size = .5) # no non lin

# --- trans ----
sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_jitter( shape = 1, size = 1.5) +
  geom_smooth(method = "loess") # no non lin

sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_jitter( shape = 1, size = 1.5) +
  geom_smooth(method = "loess") #no non lin

# - prox -----
# --- bt -----
sna_w %>%
  filter(bt < 100) %>%
  filter(behavior == "prox" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "loess") # try non lin, strange pattern w loess, dip down then up and plateau

sna_w %>%
  filter(bt < 100) %>%
  filter(behavior == "prox" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "loess") # dip down and plateau at zero

# --- trans ------
sna_w %>%
  filter(behavior == "prox" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # no clear pattern, early hump then linear rise

sna_w %>%
  filter(behavior == "prox" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # total saturation of trans at all ages

# H3 --------
#  - gmgmd -----
sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") #no clear non lin pattern, but hump again early life

sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") #clear non lin

# - prox -----
sna_w %>%
  filter(behavior == "prox" & network_sex == "female") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") # same as gmgmd: no clear non lin pattern, but hump again early life

sna_w %>%
  filter(behavior == "prox" & network_sex == "male") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth( method = "loess") #non lin, stark rise and plateau


