library(tidyverse)
library(magrittr)
library(fitdistrplus)
library(psych) #for KMO
library(paran) #parallel analysis
load("data/sna dataframe - individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/attribute data alone.Rdata", verbose = T)


# how many years  are diff individuals present
# how much do 

# i. descriptives - subjects ----
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


# ii. distributions - sna measures -----
all_sna_measure_df %>%
  filter(network_sex == "female") %>%
  filter(behavior == "prox") %$%
  descdist(deg)
#bt & ec are pretty gamma-fied in any network,
#maybe good to model trans as beta bc bt 0 -- 1

# prop 0's sna measures in mixed sex gm
all_sna_measure_df %>%
  filter(network_sex == "any_combo", behavior == "total_grooming") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))

# prop 0's sna measures in same sex gm
all_sna_measure_df %>%
  filter(network_sex %in% c("male", "female"), behavior == "total_grooming") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))
  #wow k, a lot of females have 0 betweenness and transitivity in same sex gm networks. hence same sex gm models don't work

# prop 0's sna measures in mixed sex prox
all_sna_measure_df %>%
  filter(network_sex == "any_combo", behavior == "prox") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))

# prop 0's sna measures in same sex prox
all_sna_measure_df %>%
  filter(network_sex %in% c("male", "female"), behavior == "prox") %>%
  group_by(sex) %>%
  summarise_at(vars(bt, ec, deg, trans), funs(N = n(), zeros = sum(. == 0))) %>%
  ungroup() %>%
  mutate(prop_z_bt = bt_zeros/bt_N, prop_z_ec = ec_zeros/ec_N, prop_z_deg = deg_zeros/deg_N, prop_z_trans = trans_zeros/trans_N) %>%
  select(sex, starts_with("prop"))
  #bc prox network is so saturated, you have a lot of males w zero betweenness in prox network, same sex prox models don't work in B2


# iii. Look at very simple correlations w age within networks (no RE for individual...) -----

unique(all_sna_measure_df$network_sex)
unique(all_sna_measure_df$network_type)

# female
all_sna_measure_df %>%
  filter(network_sex == "female", behavior == "total_grooming") %$% 
  cor.test(trans, age_mid_year) # strong neg ec, bt, deg

all_sna_measure_df %>%
  filter(network_sex == "female", behavior == "prox") %$% 
  cor.test(trans, age_mid_year) #nada

# male
all_sna_measure_df %>%
  filter(network_sex == "male", behavior == "total_grooming") %$% 
  cor.test(deg, age_mid_year) # pos ec

all_sna_measure_df %>%
  filter(network_sex == "male", behavior == "prox") %$% 
  cor.test(trans, age_mid_year) #nada

# all
#group this by male and female within full network
all_sna_measure_df %>%
  filter(network_sex == "any_combo", behavior == "total_grooming") %$% 
  cor.test(trans, age_mid_year) #nada

all_sna_measure_df %>%
  filter(network_sex == "any_combo", behavior == "prox") %$% 
  cor.test(trans, age_mid_year) #nada 

# iii. basic correlations between network measures ----
names(all_sna_measure_df)

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


# iv. PCA how do measures load together or apart? ------

# both sexes
sna_comp <- all_sna_measure_df %>% # keeping all the various networks, mixed and sex specific
  filter(complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans)
# females in mixed network
sna_comp_fem_mixed <- all_sna_measure_df %>%
  filter(sex == "F", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans)
# females in same sex
sna_comp_fem_same <- all_sna_measure_df %>%
  filter(sex == "F", network_sex == "female", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans)
# males in mixed network
sna_comp_male_mixed <- all_sna_measure_df %>%
  filter(sex == "M", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans)
# males in mixed network
sna_comp_male_same <- all_sna_measure_df %>%
  filter(sex == "M", network_sex == "male", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans)



nrow(sna_comp) #879
nrow(sna_comp_fem_mixed) # 266
nrow(sna_comp_fem_same) #265

nrow(sna_comp_male_mixed) # 174
nrow(sna_comp_male_same) # 174

df <- sna_comp

df <- sna_comp_fem_mixed
df <- sna_comp_fem_same

df <- sna_comp_male_mixed
df<- sna_comp_male_same 


# bartlett's and KMO
bart<-function(dat){ #dat is your raw data
  R<-cor(dat)
  p<-ncol(dat)
  n<-nrow(dat)
  chi2<- -((n-1)-((2*p)+5)/6 ) * log(det(R)) #this is the formula
  df<-(p*(p-1)/2)
  crit<-qchisq(.95,df) #critical value
  p<-pchisq(chi2,df,lower.tail=F) #pvalue
  cat("Bartlett's test of sphericity: X2(",
      df,")=",chi2,", p=",
      round(p,3),sep="" )   
}

bart(df)
KMO(df) # KMO is higher for F in same sex network, KMO not as high for males

pc <- prcomp(scale(df)) # should scale


pc$rotation # var loadings
# with both sexes: PC1 kinda captures traditional "integration", i.e. connections of connections - 
# ec deg trans high pos load and bt no
# PC2 captures inverse relationship between betweenness and local clustering, 
# high neg load bt and medium pos loading trans.
# loadings diff when just considering certain sexes within mixed network?
# looking at males in mixed sex network - PC1 captures inverse bt and trans. PC2 is centrality, 
# high neg loading of bt ec and deg, but males in all male network, they follow above pattern too, PC1
# bt low and ec deg trans high pos, PC2, bt high neg and trans on flip side.
# looking at females - PC1 and PC2 follow the above, females probably contribute lots to that signal,
# bc they have the majority of observations in the dataset. is same in same-sex female networks.
# Should not use 1 size fits all for PCs, or shouldn't use PCs at all...
# maybe need to focus on different kinds of networks and centrality within them based on specific
# questions/hypotheses.
# bc relationship bt sna measures (i.e. patterns of correlations and loadings) is different for 
# males in mixed vs same sex network, maybe don't use PCA to reduce dimensionality of integration.


pc$sd^2 # kaiser rule, eigenvalue > 1
summary(pc) #cum value
plot(pc) #screeplot
paran(sna_comp, iterations = 5000, centile = 0, quietly = FALSE, #paran says 2
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

biplot(pc, choices = c(1,2))

all_sna_measure_df %>%
  select(chimp_id, sex, year, age_mid_year) %>%
  cbind(., predict(pc)[,1:2]) 


# gyard ------
a <- sna_comp_fem_mixed %>% pull(chimp_id) %>% unique() %>% sort()
b <- sna_comp_fem_same %>% pull(chimp_id) %>% unique() %>% sort()
a[!(a %in% b)]


# for some reason, GS does not appear in all female gm network
# maybe she's not in annual possible dyads - nope! and that means she's seen to gm w males in 2011 but never females
nrow(sna_comp_fem_mixed) # has one more line than 
nrow(sna_comp_fem_same)
# that's bc GS
sna_comp_fem_same %>%
  filter(chimp_id == "GS")
sna_comp_fem_mixed %>%
  filter(chimp_id == "GS")

#check number of chimp obs per year, 
all_sna_measure_df %>%
  filter(chimp_id == "GS")

#of whole data set GS 2011 is only one missing from either obs or poss dyads. good catch.
all_sna_measure_df %>% 
  group_by(chimp_id, year) %>%
  tally() %>%
  filter(n < 4)


load("data/annual possible focal dyads.Rdata", verbose = T)
undir_annual_dyads %>%
  filter(ID1 == "GS" | ID2 == "GS")