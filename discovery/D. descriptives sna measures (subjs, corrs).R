library(tidyverse)
library(magrittr)
library(fitdistrplus)
library(psych) #for KMO
library(paran) #parallel analysis
library(devtools)
source("~/Dropbox/2. R projects/ggbiplot function.R")
source("~/Dropbox/2. R projects/ggscreeplot function.R")

load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/attribute data alone.Rdata", verbose = T)
select <- dplyr::select

sna_df <- all_sna_measure_df_w
#sna_df <- all_sna_measure_df_uw
#all_sna_measure_df_w$trans == all_sna_measure_df_uw$trans


# a. descriptives - subjects ----
names(all_sna_measure_df)
unique(all_sna_measure_df$network_sex)
unique(all_sna_measure_df$behavior)

# relevant ages
final_ages <- attr %>%
  transmute(chimp_id = chimp_id, age_mid_2018 = as.numeric(as.Date("2018-06-01") - dobc)/365.25, age_last_seen = as.numeric(dls - dobc)/365.25)
final_ages

# mean n sd of sna measures by what sexes are included in the network
all_sna_measure_df %>%
  filter(behavior == "total_grooming") %>%
  filter(year != 2009) %>%
  group_by(network_sex) %>%
  summarise_at(vars(bt, ec, deg, trans), .funs = list(mean = mean, sd = sd))

# how many years individs present in focal data  
years_pres <- all_sna_measure_df %>%
  filter(behavior == "total_grooming") %>%
  filter(year != 2009) %>%
  filter(network_sex == "any_combo") %>%
  group_by(chimp_id) %>%
  tally() %>%
  ungroup()

years_pres

#save(years_pres, file = "data/years each subject pres in network data.Rdata")

#identify priority individuals for OS - those who are oldest in the middle of sample collection 
#and simultaneously have been focal followed longest.
priorities_alive_2018 <- years_pres %>%
  left_join(final_ages, by = "chimp_id") %>%
  filter(is.na(age_last_seen)) %>%
  arrange(desc(n), desc(age_mid_2018)) %>%
  filter(n == 8) %>%
  pull(chimp_id)
priorities_alive_2018


years_pres %>%
  left_join(final_ages, by = "chimp_id") %>%
  filter(!is.na(age_last_seen)) %>%
  arrange(desc(n), desc(age_last_seen)) %>%
  filter(age_last_seen > 40) %>%
  pull(chimp_id)

priorities %>%
  filter(n == 8) %>%
  summarise( mean = mean(age_mid_2018), sd = sd(age_mid_2018), min = )


# b. distributions & averages -- indices -----
annual_avg_gmgmd_mixed <- g_data_gmgmd_sex_comb %>%
  unnest(c(data)) %>%
  group_by(year) %>%
  summarize(mean_gm = mean(gmgmdi), sd_gm = sd(gmgmdi)) %>%
  ungroup()
annual_avg_prox_mixed <- g_data_prox_sex_comb %>%
  unnest(c(data)) %>%
  group_by(year) %>%
  summarize(mean_prox = mean(prox5i), sd_prox = sd(prox5i)) %>%
  ungroup()

annual_avg_gmgmd_sep <- g_data_gmgmd_sex_sep %>%
  unnest(c(data)) %>%
  group_by(year, dyad_sex) %>%
  summarize(mean_gmgmd = mean(gmgmdi), sd_gmgmd = sd(gmgmdi)) %>%
  arrange(dyad_sex, year) %>%
  ungroup()
annual_avg_prox_sep <- g_data_prox_sex_sep %>%
  unnest(c(data)) %>%
  group_by(dyad_sex, year) %>%
  summarize(mean_prox = mean(prox5i), sd_prox = sd(prox5i)) %>%
  ungroup()


# coments annual prox indices: comments after ";" are pre 09-10 merge
annual_avg_prox_mixed # no patt; starts hi ends lower
annual_avg_prox_sep # no patt; is trend for M, F hold steady at low prox indices (relative to M)

# comments annual grooming indices
annual_avg_gmgmd_mixed # why 2015 so low ; no patt; super hi in 2009 wtf - prob just obs conspicuous socializing individuals,steadily lower w time w smaller variation
annual_avg_gmgmd_sep # why 2015 so low ; no patt; females decline to lowest val...tho males start higher and decline earlier and more steeply, whats w M 2015?

# c. distributions & averages -- sna measures -----
sna_df %>%
  filter(network_sex == "female") %>%
  filter(behavior == "prox") %$%
  descdist(bt)
#bt & ec are pretty gamma-fied in any network,
#maybe good to model trans as beta bc bt 0 -- 1

#mean sd
overall_sna_avg <- sna_df %>% # overall
  group_by(network_sex, behavior) %>%
  summarise_at(vars(bt, ec, deg, trans), list(mean = mean, sd = sd)) %>%
  select(network_sex, behavior, starts_with("bt"), starts_with("ec"), starts_with("deg"), starts_with("trans")) %>%
  arrange(behavior)
overall_sna_avg

annual_sna_avg <- sna_df %>% # for every year
  group_by(network_sex, behavior, year) %>%
  summarise_at(vars(bt, ec, deg, trans), list(mean = mean, sd = sd)) %>%
  ungroup() %>%
  select(year, network_sex, behavior, starts_with("bt"), starts_with("ec"), starts_with("deg"), starts_with("trans"))
  View(annual_sna_avg, title = "sna avg sd by year") # useful to look at in combo w sociogram
annual_sna_avg


# save(overall_sna_avg, annual_sna_avg,
#     annual_avg_gmgmd_mixed, annual_avg_prox_mixed,
#     annual_avg_gmgmd_sep, annual_avg_prox_sep,
#     file = "data/average social measures for exploration (sna weighted and 09-10 merged).Rdata")


#overall_sna_avg_uw <- overall_sna_avg
#annual_sna_avg_uw <- annual_sna_avg
#save(overall_sna_avg_uw, annual_sna_avg_uw, file = "data/average social measures for exploration (sna unweighted and 09-10 merged).Rdata")

# ---- comments on averages --------

load("data/average social measures for exploration (sna weighted and 09-10 merged).Rdata", verbose = T)
#load("data/average social measures for exploration (sna unweighted and 09-10 merged).Rdata", verbose = T)

overall_sna_avg
overall_sna_avg_uw

annual_sna_avg %>% View()
annual_sna_avg_uw

# --- comments ---
# overall avg:
# all groom networks are sparser than prox (low avg. DEG) --
# tho males have lower degree in prox network than females (it seems).
# have much higher degree in gm network.
# BT: see higher avg bt in gm vs prox networks for both sexes, think that sparseness opens opportunity for bt
# EC: no big diffs bt gm and prox except generally higher avg values for prox.
# TRANS: trans always higher in prox networks bc are denser.
#
# annual avgs: see variation in sna measures year to year (y2y)
# -- Mixed
# prox - BT: why goes down as years advance?, DEG steadily increases (after is tiny as shit in 2009 - weird),
# BT DEG prob related; EC hold steady w smaaaall variance, TRANS steadily kinda low
# gm - BT and DEG avg varies a lot from y2y. EC and TRANS steadily low again.
# -- Female
# prox - BT lots of var y2y
# gm - BT same, EC and DEG super low and not much var y2y, TRANS 0 often
# -- Male
# prox - BT is zero and EC is 1 almost every year bc network is so saturated
# gm - all sna measures vary quite beautifully and irregularly from y2y


#ranges
sna_df %>%
  group_by(network_sex, behavior) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(max = max, min = min)) %>%
  select(network_sex, behavior, starts_with("bt"), starts_with("ec"), starts_with("deg"), starts_with("trans"))

# prop 0's sna measures in mixed sex gm
sna_df %>%
  filter(network_sex == "any_combo", behavior == "total_grooming") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))

# prop 0's sna measures in same sex gm
sna_df %>%
  filter(network_sex %in% c("male", "female"), behavior == "total_grooming") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))
  #wow k, a lot of females have 0 betweenness and transitivity in same sex gm networks. hence same sex gm models don't work

# prop 0's sna measures in mixed sex prox
sna_df %>%
  filter(network_sex == "any_combo", behavior == "prox") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))

# prop 0's sna measures in same sex prox
sna_df %>%
  filter(network_sex %in% c("male", "female"), behavior == "prox") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))
  #bc prox network is so saturated, you have a lot of males w zero betweenness in prox network, same sex prox models don't work in B2


# d. Look at very simple correlations w age within networks (no RE for individual...) -----

unique(sna_df$network_sex)
unique(sna_df$network_type)

# female
sna_df %>%
  filter(network_sex == "female", behavior == "total_grooming") %$% 
  cor.test(trans, age_mid_year) # strong neg ec, bt, deg

sna_df %>%
  filter(network_sex == "female", behavior == "prox") %$% 
  cor.test(trans, age_mid_year) #nada

# male
sna_df %>%
  filter(network_sex == "male", behavior == "total_grooming") %$% 
  cor.test(deg, age_mid_year) # pos ec

sna_df %>%
  filter(network_sex == "male", behavior == "prox") %$% 
  cor.test(trans, age_mid_year) #nada

# all
#group this by male and female within full network
sna_df %>%
  filter(network_sex == "any_combo", behavior == "total_grooming") %$% 
  cor.test(trans, age_mid_year) #nada

sna_df %>%
  filter(network_sex == "any_combo", behavior == "prox") %$% 
  cor.test(trans, age_mid_year) #nada 

# e. basic correlations between network measures ----
names(df)

df <- all_sna_measure_df
df <- all_sna_measure_df %>%
  filter(network_sex == "female", behavior == "total_grooming") 
df <- all_sna_measure_df %>%
  filter(network_sex == "male", behavior == "total_grooming") 
df <- all_sna_measure_df %>%
  filter(network_sex == "female", behavior == "prox") 
df <- all_sna_measure_df %>%
  filter(network_sex == "male", behavior == "prox") 


# total
cors <- df %>%
  select(bt, ec, deg, trans) %>%
  cor(., method = "spearman" , use = "pairwise.complete.obs") %>%
  round(., 2)
p_cors <- df %>%
  select(bt, ec, deg, trans) %>%
  cor_pmat(., method = "spearman") %>%
  round(.,2)
1 * (p_cors < 0.05)
1 * (cors > 0 & p_cors < 0.05)

cors
p_cors
#in every network, all measures are positively and significantly correlated except for trans and bt 8|


# f. TIME SEX SPENDS GM or IN PROX OF SAME VS OPPOSITE SEX?

