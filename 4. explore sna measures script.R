library(tidyverse)
library(magrittr)
load("all sna measures for behavior and dyad sex types.Rdata", verbose = T)
load("attribute data alone.Rdata", verbose = T)


# how many years  are diff individuals present
# how much do 

# part 1
# description stability and change w age - 
# will require node permutations of networks within each year to get random sna measures pr year, 
# and distribution of age coefficients from randomized networks
# similarly, individual CVs of network measures will be significantly high or low in reference to their CVs calculated from randomized networks

# part 2
# Response: OS status by quarter, 2017 - 2019 (12 points per individual? - average of 3 per quarter?)
# Predictors of OS status:
# (model selection)
# hardship index - years w amputation from snare injury, number of severe injuries?, loss of mother before age x(?)
# average dominance rank during observation * sex
# sex & agee
# stability/variation in sna measures (adjusted for observation time)
# magnitude of various sna variables


# Explore sna ----
names(all_sna_measure_df)
unique(all_sna_measure_df$network_sex)
unique(all_sna_measure_df$behavior)

# relevant ages
attr %<>%
  mutate(age_mid_2018 = (as.Date("2018-06-01") - dobc)/365.25, age_disappeared = (date_last_seen - dobc)/365.25)
str(attr)

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
  filter(network_sex == "any_combination") %>%
  group_by(chimp_id) %>%
  tally()

#identify priority individuals for OS - those who are oldest in the middle of sample collection 
#and simultaneously have been focal followed longest.
years_pres %>%
  left_join(attr, by = "chimp_id") %>%
  filter(is.na(age_disappeared)) %>%
  arrange(desc(n), desc(age_mid_2018)) %>%
  View()

