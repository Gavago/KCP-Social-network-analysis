# testing permutation gam sig check

library(tidyverse)
library(mgcv)

# A. -- load node-randomized datasets ----
# undir w
load("data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)

# dir gm w
load("data/ran5 - both sexes w - node (sex age rank chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)
load("data/ran6 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures directed gm weighted.Rdata", verbose = T)

# dir gm uw
load("data/ran7 - both sexes uw - node (sex age rank chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)
load("data/ran8 - sex sep uw - node (age rank prop_cyc chimp_id) randomized sna measures directed gm unweighted.Rdata", verbose = T)


# B. Grooming mixed sex networks --------
#   1. Random F stat extraction -----
#     H1 - mixed sex Strength and degree in/out directed grooming --- mixed sex ----

#Degree & strength in - storage of Fs from randomized data models
sF_ran_dig <- vector("list", length = 1000)
sF_ran_digr <- vector("list", length = 1000)
sF_ran_sig <- vector("list", length = 1000)
sF_ran_sigr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data_w <- list_ran_dir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_both_sex_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_dig <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_digr <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_sig <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  ran_sigr <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  
  
  #extract F stat like this
  sF_ran_dig[[i]] <- summary(ran_dig) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_digr[[i]] <- summary(ran_digr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  sF_ran_sig[[i]] <- summary(ran_sig) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sigr[[i]] <- summary(ran_sigr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
}
Sys.time() - t # 17.5 min

F_ran_dig <- do.call("rbind", sF_ran_dig)
F_ran_digr <- do.call("rbind", sF_ran_digr)
F_ran_sig <- do.call("rbind", sF_ran_sig)
F_ran_sigr <- do.call("rbind", sF_ran_sigr)

# Strenght and degree out
sF_ran_dog <- vector("list", length = 1000)
sF_ran_dogr <- vector("list", length = 1000)
sF_ran_sog <- vector("list", length = 1000)
sF_ran_sogr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data_w <- list_ran_dir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_both_sex_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_dog <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_dogr <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_uw, family = gaussian(link = "log"), method = "REML")
  ran_sog <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  ran_sogr <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data_w, family = gaussian(link = "log"), method = "REML")
  
  
  #extract F stat like this
  sF_ran_dog[[i]] <- summary(ran_dog) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_dogr[[i]] <- summary(ran_dogr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sog[[i]] <- summary(ran_sog) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_sogr[[i]] <- summary(ran_sogr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
}
Sys.time() - t #  min 13.9

F_ran_dog <- do.call("rbind", sF_ran_dog)
F_ran_dogr <- do.call("rbind", sF_ran_dogr)
F_ran_sog <- do.call("rbind", sF_ran_sog)
F_ran_sogr <- do.call("rbind", sF_ran_sogr)


#     H2 - mixed sex Betweenness and transitivity undirected grooming --- mixed sex ----
#storage of Fs from randomized data models
sF_ran_btg <- vector("list", length = 1000)
sF_ran_btgr <- vector("list", length = 1000)
sF_ran_trg <- vector("list", length = 1000)
sF_ran_trgr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)

  ran_btg <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_btgr <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trg <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trgr <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  
  
  #extract F stat like this
  sF_ran_btg[[i]] <- summary(ran_btg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_btgr[[i]] <- summary(ran_btgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_trg[[i]] <- summary(ran_trg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_trgr[[i]] <- summary(ran_trgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
}
Sys.time() - t #22.9 min

F_ran_btg <- do.call("rbind", sF_ran_btg)
F_ran_btgr <- do.call("rbind", sF_ran_btgr)
F_ran_trg <- do.call("rbind", sF_ran_trg)
F_ran_trgr <- do.call("rbind", sF_ran_trgr)


#     H3 - mixed sex EC ----- 
#storage of Fs from randomized data models
sF_ran_ecg <- vector("list", length = 1000)
sF_ran_ecgr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecg <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_ecgr <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")

  #extract F stat like this
  sF_ran_ecg[[i]] <- summary(ran_ecg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_ecgr[[i]] <- summary(ran_ecgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
}
Sys.time() - t #18.9 min

F_ran_ecg <- do.call("rbind", sF_ran_ecg)
F_ran_ecgr <- do.call("rbind", sF_ran_ecgr)

#     saving ran coefs from each Hypos -----
save(F_ran_dig, F_ran_digr, F_ran_dog, F_ran_dogr, 
     F_ran_sig, F_ran_sigr, F_ran_sog, F_ran_sogr,
     F_ran_btg, F_ran_btgr, F_ran_trg, F_ran_trgr, F_ran_ecg, F_ran_ecgr, file = "data/ran coefs gams - directed and total grooming mixed sex.Rdata")

#   2. Evaluating sig groomnig mixed - comparing observed to random --------

load("data/models gam - mixed sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/ran coefs gams - directed and total grooming mixed sex.Rdata", verbose = T)
source("functions/functions - test sig gamm.R")
source("functions/functions - prettify results table.R")

F_ran_ecgr %>% filter(rowname == "s(age_mid_year):sexM") %>% pull(F) %>% hist()

a <- test_sig_gamm_mixed(response = "degree in", behavior = "grooming", mod = dig, modr = digr, F_ran = F_ran_dig, F_ran_r = F_ran_digr) %>% prettify_table()
b <- test_sig_gamm_mixed(response = "degree out", behavior = "grooming", mod = dog, modr = dogr, F_ran = F_ran_dog, F_ran_r = F_ran_dogr) %>% prettify_table()

c <- test_sig_gamm_mixed(response = "strength in", behavior = "grooming", mod = sig, modr = sigr, F_ran = F_ran_sig, F_ran_r = F_ran_sigr) %>% prettify_table()
d <- test_sig_gamm_mixed(response = "strength out", behavior = "grooming", mod = sog, modr = sogr, F_ran = F_ran_sog, F_ran_r = F_ran_sogr) %>% prettify_table()

e <- test_sig_gamm_mixed(response = "betweenness", behavior = "grooming", mod = btg, modr = btgr, F_ran = F_ran_btg, F_ran_r = F_ran_btgr) %>% prettify_table()
f <- test_sig_gamm_mixed(response = "local transitivty", behavior = "grooming", mod = trg, modr = trgr, F_ran = F_ran_trg, F_ran_r = F_ran_trgr) %>% prettify_table()

g <- test_sig_gamm_mixed(response = "eigenvector centrality", behavior = "grooming", mod = ecg, modr = ecgr, F_ran = F_ran_ecg, F_ran_r = F_ran_ecgr) %>% prettify_table()

#same sex tables
ms_gamH1 <- do.call("rbind", list(a,b,c,d))
ms_gamH2 <- do.call("rbind", list(e,f))
ms_gamH3 <- g


write.table(ms_gamH1, file = "results/tables/H1. GAM same sex deg strength in out.txt", quote = FALSE, row.names = FALSE)
write.table(ms_gamH2, file = "results/tables/H2. GAM same sex bt trans.txt", quote = FALSE, row.names = FALSE)
write.table(ms_gamH3, file = "results/tables/H3. GAM same sex ec.txt", quote = FALSE, row.names = FALSE)


# C. Grooming same sex networks ------
#   3. Random F stat extraction -----
#     H1 - same sex Strength and degree in/out directed grooming --- mixed sex ----

#Degree & strength in - storage of Fs from randomized data models
sF_ran_fsdig <- vector("list", length = 1000)
sF_ran_fsdigr <- vector("list", length = 1000)
sF_ran_fssig <- vector("list", length = 1000)
sF_ran_fssigr <- vector("list", length = 1000)

sF_ran_msdig <- vector("list", length = 1000)
sF_ran_msdigr <- vector("list", length = 1000)
sF_ran_mssig <- vector("list", length = 1000)
sF_ran_mssigr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_fsdig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsdigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fssig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fssigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  
  ran_msdig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msdigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
  
  #extract F stat like this
  sF_ran_fsdig[[i]] <- summary(ran_fsdig) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsdigr[[i]] <- summary(ran_fsdigr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fssig[[i]] <- summary(ran_fssig) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fssigr[[i]] <- summary(ran_fssigr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msdig[[i]] <- summary(ran_msdig) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msdigr[[i]] <- summary(ran_msdigr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssig[[i]] <- summary(ran_mssig) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssigr[[i]] <- summary(ran_mssigr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  
}
Sys.time() - t # 15.5 min

F_ran_fsdig <- do.call("rbind", sF_ran_fsdig)
F_ran_fsdigr <- do.call("rbind", sF_ran_fsdigr)
F_ran_fssig <- do.call("rbind", sF_ran_fssig)
F_ran_fssigr <- do.call("rbind", sF_ran_fssigr)

F_ran_msdig <- do.call("rbind", sF_ran_msdig)
F_ran_msdigr <- do.call("rbind", sF_ran_msdigr)
F_ran_mssig <- do.call("rbind", sF_ran_mssig)
F_ran_mssigr <- do.call("rbind", sF_ran_mssigr)


#Degree & strength out - storage of Fs from randomized data models
sF_ran_fsdog <- vector("list", length = 1000)
sF_ran_fsdogr <- vector("list", length = 1000)
sF_ran_fssog <- vector("list", length = 1000)
sF_ran_fssogr <- vector("list", length = 1000)

sF_ran_msdog <- vector("list", length = 1000)
sF_ran_msdogr <- vector("list", length = 1000)
sF_ran_mssog <- vector("list", length = 1000)
sF_ran_mssogr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data_w <- list_ran_dir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  data_uw <- list_ran_dir_sna_measure_sex_sep_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_fsdog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsdogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fssog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = Gamma(link = "log"), method = "REML")
  ran_fssogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "F"), family = Gamma(link = "log"), method = "REML")
  
  ran_msdog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msdogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mssogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data_w %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
  
  #extract F stat like this
  sF_ran_fsdog[[i]] <- summary(ran_fsdog) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsdogr[[i]] <- summary(ran_fsdogr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fssog[[i]] <- summary(ran_fssog) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fssogr[[i]] <- summary(ran_fssogr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msdog[[i]] <- summary(ran_msdog) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msdogr[[i]] <- summary(ran_msdogr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssog[[i]] <- summary(ran_mssog) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssogr[[i]] <- summary(ran_mssogr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  
}
Sys.time() - t # 20.2 min

F_ran_fsdog <- do.call("rbind", sF_ran_fsdog)
F_ran_fsdogr <- do.call("rbind", sF_ran_fsdogr)
F_ran_fssog <- do.call("rbind", sF_ran_fssog)
F_ran_fssogr <- do.call("rbind", sF_ran_fssogr)

F_ran_msdog <- do.call("rbind", sF_ran_msdog)
F_ran_msdogr <- do.call("rbind", sF_ran_msdogr)
F_ran_mssog <- do.call("rbind", sF_ran_mssog)
F_ran_mssogr <- do.call("rbind", sF_ran_mssogr)


#     H2 - same sex Betweenness and transitivity undirected grooming --- mixed sex ----
#storage of Fs from randomized data models

sF_ran_fsbtg <- vector("list", length = 1000)
sF_ran_fsbtgr <- vector("list", length = 1000)
sF_ran_fstrg <- vector("list", length = 1000)
sF_ran_fstrgr <- vector("list", length = 1000)

sF_ran_msbtg <- vector("list", length = 1000)
sF_ran_msbtgr <- vector("list", length = 1000)
sF_ran_mstrg <- vector("list", length = 1000)
sF_ran_mstrgr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_fsbtg <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsbtgr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fstrg <- gam(trans ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fstrgr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  
  ran_msbtg <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msbtgr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mstrg <- gam(trans ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mstrgr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
  
  #extract F stat like this
  sF_ran_fsbtg[[i]] <- summary(ran_fsbtg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsbtgr[[i]] <- summary(ran_fsbtgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fstrg[[i]] <- summary(ran_fstrg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fstrgr[[i]] <- summary(ran_fstrgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msbtg[[i]] <- summary(ran_msbtg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msbtgr[[i]] <- summary(ran_msbtgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mstrg[[i]] <- summary(ran_mstrg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mstrgr[[i]] <- summary(ran_mstrgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  
}
Sys.time() - t # 11.4 min

F_ran_fsbtg <- do.call("rbind", sF_ran_fsbtg)
F_ran_fsbtgr <- do.call("rbind", sF_ran_fsbtgr)
F_ran_fstrg <- do.call("rbind", sF_ran_fstrg)
F_ran_fstrgr <- do.call("rbind", sF_ran_fstrgr)

F_ran_msbtg <- do.call("rbind", sF_ran_msbtg)
F_ran_msbtgr <- do.call("rbind", sF_ran_msbtgr)
F_ran_mstrg <- do.call("rbind", sF_ran_mstrg)
F_ran_mstrgr <- do.call("rbind", sF_ran_mstrgr)


#     H3 - same sex EC ----- 
#Degree & strength in - storage of Fs from randomized data models
sF_ran_fsecg <- vector("list", length = 1000)
sF_ran_fsecgr <- vector("list", length = 1000)
sF_ran_msecg <- vector("list", length = 1000)
sF_ran_msecgr <- vector("list", length = 1000)


t <- Sys.time()
for(i in 1:1000){
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fsecg <- gam(ec ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsecgr <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_msecg <- gam(ec ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msecgr <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")

  #extract F stat like this
  sF_ran_fsecg[[i]] <- summary(ran_fsecg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsecgr[[i]] <- summary(ran_fsecgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msecg[[i]] <- summary(ran_msecg) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msecgr[[i]] <- summary(ran_msecgr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  
}
Sys.time() - t #  6.9 min

F_ran_fsecg <- do.call("rbind", sF_ran_fsecg)
F_ran_fsecgr <- do.call("rbind", sF_ran_fsecgr)
F_ran_msecg <- do.call("rbind", sF_ran_msecg)
F_ran_msecgr <- do.call("rbind", sF_ran_msecgr)



#     saving same sex ran coefs from each Hypos -----
save(F_ran_fsdig, F_ran_fsdigr, F_ran_fsdog, F_ran_fsdogr, 
     F_ran_msdig, F_ran_msdigr, F_ran_msdog, F_ran_msdogr, 
     F_ran_fssig, F_ran_fssigr, F_ran_fssog, F_ran_fssogr,
     F_ran_mssig, F_ran_mssigr, F_ran_mssog, F_ran_mssogr,
     F_ran_fsbtg, F_ran_fsbtgr, F_ran_fstrg, F_ran_fstrgr, 
     F_ran_msbtg, F_ran_msbtgr, F_ran_mstrg, F_ran_mstrgr, 
     F_ran_fsecg, F_ran_fsecgr, F_ran_msecg, F_ran_msecgr, file = "data/ran coefs gams - directed and total grooming same sex.Rdata")



#   4. Evaluating sig groomnig mixed - comparing observed to random --------

load("data/models gam - same sex grooming and total grooming with and without rank.Rdata", verbose = T)
load("data/ran coefs gams - directed and total grooming same sex.Rdata", verbose = T)
source("functions/functions - test sig gamm.R") # add sex of network and behavior to sig table function, also create new funcction for same sex nets

a <- test_sig_gamm_same(response = "degree in", behavior = "grooming", net_sex = "same",
                   fmod = f_dig, fmodr = f_digr, mmod = m_dig, mmodr = m_digr,
                   F_ranf = F_ran_fsdig, F_ranf_r = F_ran_fsdigr, 
                   F_ranm = F_ran_msdig, F_ranm_r = F_ran_msdigr) %>% prettify_table()

b <- test_sig_gamm_same(response = "degree out", behavior = "grooming", net_sex = "same",
                   fmod = f_dog, fmodr = f_digr, mmod = m_dog, mmodr = m_dogr,
                   F_ranf = F_ran_fsdog, F_ranf_r = F_ran_fsdogr, 
                   F_ranm = F_ran_msdog, F_ranm_r = F_ran_msdogr) %>% prettify_table()

c <- test_sig_gamm_same(response = "strength in", behavior = "grooming", net_sex = "same",
                        fmod = f_sig, fmodr = f_sigr, mmod = m_sig, mmodr = m_sigr,
                        F_ranf = F_ran_fssig, F_ranf_r = F_ran_fssigr, 
                        F_ranm = F_ran_mssig, F_ranm_r = F_ran_mssigr) %>% prettify_table()

d <- test_sig_gamm_same(response = "strength out", behavior = "grooming", net_sex = "same",
                        fmod = f_sog, fmodr = f_sigr, mmod = m_sog, mmodr = m_sogr,
                        F_ranf = F_ran_fssog, F_ranf_r = F_ran_fssogr, 
                        F_ranm = F_ran_mssog, F_ranm_r = F_ran_mssogr) %>% prettify_table()

e <- test_sig_gamm_same(response = "betweeness", behavior = "grooming", net_sex = "same",
                   fmod = f_btg, fmodr = f_btgr, mmod = m_dog, mmodr = m_btgr,
                   F_ranf = F_ran_fsbtg, F_ranf_r = F_ran_fsbtgr, 
                   F_ranm = F_ran_msbtg, F_ranm_r = F_ran_msbtgr) %>% prettify_table()

f <- test_sig_gamm_same(response = "local transitivity", behavior = "grooming", net_sex = "same",
                   fmod = f_trg, fmodr = f_trgr, mmod = m_trg, mmodr = m_trgr,
                   F_ranf = F_ran_fstrg, F_ranf_r = F_ran_fstrgr, 
                   F_ranm = F_ran_mstrg, F_ranm_r = F_ran_mstrgr) %>% prettify_table()

g <- test_sig_gamm_same(response = "eigenvector centrality", behavior = "grooming", net_sex = "same",
                   fmod = f_ecg, fmodr = f_ecgr, mmod = m_ecg, mmodr = m_ecgr,
                   F_ranf = F_ran_fsecg, F_ranf_r = F_ran_fsecgr, 
                   F_ranm = F_ran_msecg, F_ranm_r = F_ran_msecgr) %>% prettify_table()


#same sex tables
ss_gamH1 <- do.call("rbind", list(a,b,c,d))
ss_gamH2 <- do.call("rbind", list(e, f))
ss_gamH3 <- g


# write.table(ss_gamH1, file = "results/tables/H1. GAM same sex deg strength in out.txt", quote = F, row.names = F)
# write.table(ss_gamH2, file = "results/tables/H2. GAM same sex bt trans.txt", quote = F, row.names = F)
# write.table(ss_gamH3, file = "results/tables/H3. GAM same sex ec.txt", quote = F, row.names = F)


# gyard ---------------

# see if gam does differently than glmms when assessing sig for isolated male age and degree in ----

xo <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML") %>%
  summary() %>% 
  .$s.table %>% data.frame() %>% pull(F) 
xo

test <- vector("list", 1000)

for(i in 1:1000){

  data_uw <- list_ran_dir_sna_measure_both_sex_uw[[i]] %>%
    filter(network_sex == "any_combo") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001, deg_out = deg_out + 0.0001)
  
  ran_digr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data_uw %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
  #extract F stat like this
  test[[i]] <- summary(ran_digr) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

}

test <- do.call("rbind", test)

xr <- test %>% filter(rowname == "s(age_mid_year)") %>% pull(F)

sum(xo[1] > xr)/1000


