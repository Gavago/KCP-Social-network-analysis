library(mgcViz)
library(tidyverse)

# SNA measures
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)

sna_w <- all_sna_measure_df_w
sna_uw <- all_sna_measure_df_uw
dir_sna_w <- dir_sna_measure_df_w
dir_sna_uw <- dir_sna_measure_df_uw

# GAM models
load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)

b <- getViz(dig)
c <- smooth(sm(b,2))
b + l_fitLine(colour = sex) + l_rug(mapping = aes(x= age_mid_year, y= deg_in), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
b


# plot.gam and then immediately ggplot
# 1a. DEGREE IN & OUT ----

plot(dig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dig)[1]) # no change with and without rank
plot(mdig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdig)[1])  #looks positive when males alone, neg in interaction?
plot(fdig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdig)[1]) 

v_deg_in_gm_uw <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs( x = "Age (years)", y = "", title = "Degree In") +
  annotate("text",x = 32, y = 14, label = "M*", size = 5, family = "Georgia") +
  annotate("text",x = 40, y = 14, label = "F*", size = 5, family = "Georgia") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_in_gm_uw

plot(digr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(digr)[1]) 
plot(mdigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdigr)[1]) # male look pos now
plot(fdigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdigr)[1]) 

plot(dog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dog)[1]) # no change
plot(mdog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdog)[1]) # estimates are above 0 now? shape simlar...
plot(fdog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdog)[1]) 

v_deg_out_gm_uw <- dir_sna_uw %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  #geom_smooth( method = "gam", formula = y ~ s(x, bs = "cs")) + # both linear - male maybe non linear female linear
  labs( x = "Age (years)", y = "", title = "Degree Out") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_out_gm_uw

plot(dogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(dogr)[1]) 
plot(mdogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mdogr)[1]) # male look pos now
plot(fdogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fdogr)[1]) 

# 1b. STRENGTH IN AND OUT -----

plot(sig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sig)[1]) #no change - strength-in goes up w age for males w and without rank
plot(msig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msig)[1]) # difference in scale
plot(fsig, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsig)[1])

plot(sigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sigr)[1])
plot(msigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msigr)[1]) # m alone is decrease but flat in interaction
plot(fsigr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsigr)[1])

v_deg_in_gm_w <- dir_sna_w %>%
  filter(deg_in < 50) %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_in, color = sex)) +
  geom_jitter(shape = 1, size = 1.5) +
  #geom_smooth(method = "lm") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) + # TOTES DIFFERENT FROM GAM MODEL
  labs(x = "Age (years)", y = "", title = "Strength In") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_in_gm_w


plot(sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sog)[1]) #no change in direction or sig of age on strength-out in males
plot(msog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msog)[1]) 
plot(fsog, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsog)[1])

v_deg_out_gm_w <- dir_sna_w %>%
  filter(network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg_out, color = sex, label = chimp_id)) +
  geom_jitter(shape = 1, size = 1.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs" , k = 5)) + #DIFFERENT FROM GAM model
  labs( x = "Age (years)", y = "", title = "Strength Out") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, family = "Georgia"), axis.title.x = element_text(family = "Georgia"), legend.position = "none")
v_deg_out_gm_w

plot(sogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(sogr)[1]) 
plot(msogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(msog)[1]) # plot doesn't look the same as in ggplot w gam
plot(fsogr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fsog)[1])




# 2. BT & TRANS -----

plot(btg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btg)[1]) # without rank, female parabolic but non sig, male linear decrease sig
plot(mbtg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mbtg)[1])
plot(fbtg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fbtg)[1])

plot(btgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(btgr)[1]) # with rank, both linear decrease (rank inc bt for both sexes) and male still sig
plot(mbtgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mbtgr)[1])
plot(fbtgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fbtgr)[1])


plot(trg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trg)[1]) # female sig parabolic in both, male no more sig increase after adding rank, can see from concurvity check that male rank predicts transitivity
plot(mtrg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mtrg)[1]) 
plot(ftrg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ftrg)[1]) 

plot(trgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(trgr)[1])  #male transitivity was linear in interaction
plot(mtrgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mtrgr)[1]) # difference in pattern is just matter of scale
plot(ftrgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ftrgr)[1])  # 


# 3. EC ------
plot(ecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecg)[1]) #
plot(mecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mecg)[1]) # hm, maybe male pattern is preserved in interaction model, just diminished bc on female scale!
plot(fecg, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fecg)[1]) #

plot(ecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(ecgr)[1]) #that's different from linear model. see male ec w age is linear
plot(mecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(mecgr)[1])
plot(fecgr, pages = 1, seWithMean = TRUE, shade = TRUE, shift = coef(fecgr)[1])



# gyard -----
# annotate("text",x = 36, y = 38, label = "*", size = 12, family = "Georgia") +
#   annotate("text",x = 32, y = 35, label = "M*", size = 5, family = "Georgia") +
#   annotate("text",x = 40, y = 35, label = "F*", size = 5, family = "Georgia") +
#legend.title = element_text(family = "Georgia"), legend.text = element_text(family = "Georgia"))