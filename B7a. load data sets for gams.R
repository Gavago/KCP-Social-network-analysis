library(tidyverse)

# weighted
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
# unweighted
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)



# data sets ------

#undirected weighted - mixed and same
mixed_sna_w <- all_sna_measure_df_w  %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001, ec = ec + 0.0001) %>%
  filter(network_sex == "any_combo")
f_mixed_sna_w <- all_sna_measure_df_w  %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001, ec = ec + 0.0001) %>%
  filter(network_sex == "any_combo", sex == "F")
m_mixed_sna_w <- all_sna_measure_df_w  %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001, ec = ec + 0.0001) %>%
  filter(network_sex == "any_combo", sex == "M")
f_same_sna_w <- all_sna_measure_df_w  %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001, ec = ec + 0.0001) %>%
  filter(network_sex == "female")
m_same_sna_w <- all_sna_measure_df_w  %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001, ec = ec + 0.0001) %>%
  filter(network_sex == "male") 
#sna_uw <- all_sna_measure_df_uw  %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001, ec = ec + 0.0001) # never use undirected unweighted now

#directed 
mixed_dir_sna_w <- dir_sna_measure_df_w %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "any_combo")
mixed_dir_sna_uw <- dir_sna_measure_df_uw %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "any_combo")

f_mixed_dir_sna_w <- dir_sna_measure_df_w %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "any_combo", sex == "F")
f_mixed_dir_sna_uw <- dir_sna_measure_df_uw %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "any_combo", sex == "F")
f_same_dir_sna_w <- dir_sna_measure_df_w %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "female")
f_same_dir_sna_uw <- dir_sna_measure_df_uw %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "female")


m_mixed_dir_sna_w <- dir_sna_measure_df_w %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "any_combo", sex == "M")
m_mixed_dir_sna_uw <- dir_sna_measure_df_uw %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "any_combo", sex == "M")
m_same_dir_sna_w <- dir_sna_measure_df_w %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "male")
m_same_dir_sna_uw <- dir_sna_measure_df_uw %>% mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001) %>%
  filter(network_sex == "male")
