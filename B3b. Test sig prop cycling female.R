library(tidyverse)
library(mgcv)
library(progress)

# load data sets
load("data/ran9 - estrus dir uw - node (age rank prop_cyc chimp_id) randomized sna measures directed unweighted.Rdata", verbose = T)
load("data/ran10 - estrus dir w - node (age rank prop_cyc chimp_id) randomized sna measures directed weighted.Rdata", verbose = T)
load("data/ran11 - estrus undir w - node (age rank prop_cyc chimp_id) randomized sna measures undirected weighted.Rdata", verbose = T)

# 1. Run ran mods ----

## Grooming -----

# In Degree -----
#ii_dig_f_estr <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_dig_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_dig_list[[i]] <- gam(deg_in ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 9.5


# Out Degree --------
#ii_dog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

ran_dog_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_uw[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_dog_list[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #24 min


# In Strength -----
#ii_sig_f_estr <- gam(deg_in ~ s(age_mid_year) + s(prop_cyc) + ti(age_mid_year, prop_cyc) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sig_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_in = deg_in + 0.0001)
  
  ran_sig_list[[i]] <- gam(deg_in ~ s(age_mid_year, k = 5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t #

# Out Strength -----
#ii_sog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(avg_rank) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sog_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_dir_w[[i]] %>%
    filter(network_sex == "any_combo" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg_out = deg_out + 0.0001)
  
  ran_sog_list[[i]] <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = Gamma(link = "log"), method = "REML")
  
}
Sys.time() - t #

#save(ran_dig_list, ran_dog_list, ran_sig_list, ran_sog_list, file = "data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength.Rdata")

# ------ extract Fs -----
load.("data/models gam - ran - H1 f estrus mixed sex grooming in out degree n strength.Rdata", verbose = T)

sF_ran_dig <- vector("list", length = 1000)
sF_ran_dog <- vector("list", length = 1000)
sF_ran_sig <- vector("list", length = 1000)
sF_ran_sog <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_dig[[i]] <- summary(ran_dig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_dig_list[[i]])$r.sq, 2))
  
  sF_ran_dog[[i]] <- summary(ran_dog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_dog_list[[i]])$r.sq,2))
  
  sF_ran_sig[[i]] <- summary(ran_sig_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_sig_list[[i]])$r.sq, 2))
  
  sF_ran_sog[[i]] <- summary(ran_sog_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_sog_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 1.5 min

F_ran_dig_estr <- do.call("rbind", sF_ran_dig)
F_ran_dog_estr <- do.call("rbind", sF_ran_dog)
F_ran_sig_estr <- do.call("rbind", sF_ran_sig)
F_ran_sog_estr <- do.call("rbind", sF_ran_sog)

# Trans -----
#ii_trg_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")

ran_trg_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trg_list[[i]] <- gam(trans ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #



# Betweenness -----
#ii_btg_f_estr <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")

ran_btg_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btg_list[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #

any(is.null(ran_btg_list))

save(ran_trg_list, ran_btg_list, file = "data/models gam - ran - H2 f estrus mixed sex grooming trans bt.Rdata")

# ------ extract Fs -----
load("data/models gam - ran - H2 f estrus mixed sex grooming trans bt.Rdata", verbose = T)

sF_ran_trg <- vector("list", length = 1000)
sF_ran_btg <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trg[[i]] <- summary(ran_trg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trg_list[[i]])$r.sq, 2))
  
  sF_ran_btg[[i]] <- summary(ran_btg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btg_list[[i]])$r.sq,2))
  
}
Sys.time() - t # 1.5 min

F_ran_trg_estr <- do.call("rbind", sF_ran_trg)
F_ran_btg_estr <- do.call("rbind", sF_ran_btg)


# EC -----
ran_ecg_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "total_grooming" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecg_list[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

}
Sys.time() - t # 7 min

#save(ran_ecg_list, file = "data/models gam - ran - H3 f estrus mixed sex grooming ec.Rdata")
# ---- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex grooming ec.Rdata", verbose = T)

sF_ran_ecg <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecg[[i]] <- summary(ran_ecg_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecg_list[[i]])$r.sq, 2))

}
Sys.time() - t # 1.5 min

F_ran_ecg_estr <- do.call("rbind", sF_ran_ecg)


## Proximity ----
# Strength ----
#ii_sog_f_estr <- gam(deg_out ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = Gamma(link = "log"), method = "REML")

ran_sp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo", behavior == "prox", sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)
  
  ran_sp_list[[i]] <- gam(deg ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #5.4


#save(ran_sp_list, file = "data/models gam - ran - H1 f estrus mixed sex prox strength.Rdata")
# ----- extract Fs ------
load("data/models gam - ran - H1 f estrus mixed sex prox strength.Rdata", verbose = T)
sF_ran_sp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_sp[[i]] <- summary(ran_sp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_sp_list[[i]])$r.sq, 2))
  
}
Sys.time() - t 

F_ran_sp_estr <- do.call("rbind", sF_ran_sp)



# Trans -----
#ii_trg_f_estr <- gam(trans ~ s(age_mid_year, k = 3) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")

ran_trp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), trans = trans + 0.0001)
  
  ran_trp_list[[i]] <- gam(trans ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #4.4



# Betweenness -----
ran_btp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001)
  
  ran_btp_list[[i]] <- gam(bt ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #24 min

#save(ran_trp_list, ran_btp_list, file = "data/models gam - ran - H2 f estrus mixed sex prox trans bt.Rdata")
#----- extract Fs ------
load("data/models gam - ran - H2 f estrus mixed sex prox trans bt.Rdata", verbose = T)

sF_ran_trp <- vector("list", length = 1000)
sF_ran_btp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_trp[[i]] <- summary(ran_trp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_trp_list[[i]])$r.sq, 2))
  
  sF_ran_btp[[i]] <- summary(ran_btp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)  %>% data.frame(., Rs = round(summary(ran_btp_list[[i]])$r.sq,2))
  
}
Sys.time() - t # 1.5 min

F_ran_trp_estr <- do.call("rbind", sF_ran_trp)
F_ran_btp_estr <- do.call("rbind", sF_ran_btp)


# EC -----
ran_ecp_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_f_estrus_undir_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox" & sex == "F") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecp_list[[i]] <- gam(ec ~ s(age_mid_year, k =5) + s(prop_cyc, k = 5) + ti(age_mid_year, prop_cyc, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
  
} #6.7
Sys.time() - t 

#save(ran_ecp_list, file = "data/models gam - ran - H3 f estrus mixed sex prox ec.Rdata")

# ----- extract Fs -----
load("data/models gam - ran - H3 f estrus mixed sex prox ec.Rdata", verbose = T)

sF_ran_ecp <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)

t <- Sys.time()

for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecp[[i]] <- summary(ran_ecp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F) %>% data.frame(., Rs = round(summary(ran_ecp_list[[i]])$r.sq, 2))
  
}
Sys.time() - t # 18

F_ran_ecp_estr <- do.call("rbind", sF_ran_ecp)



#####  save ran Fs & R ----
# save(F_ran_dig_estr,  F_ran_dog_estr,
#      F_ran_sig_estr, F_ran_sog_estr,
#      F_ran_btg_estr,  F_ran_trg_estr, F_ran_ecg_estr, file = "data/ran Fs gams - f estrus directed and total grooming mixed sex.Rdata")

# save(F_ran_sp_estr,
#      F_ran_btp_estr,  F_ran_trp_estr, F_ran_ecp_estr, file = "data/ran Fs gams - f estrus prox mixed sex.Rdata")


# 2. Compare for sig ----------
source("functions/functions - test sig gamm.R")
load("data/ran Fs gams - f estrus directed and total grooming mixed sex.Rdata", verbose = T)
load( "data/ran Fs gams - f estrus prox mixed sex.Rdata", verbose = T)

# load obs models
load("data/models - estrus female in mixed sex net.R", verbose = T)


a <- test_sig_gamm_f_estrus(response = "In-Degree", behavior = "Grooming", 
                         mod = ii_dig_f_estr, F_ran = F_ran_dig_estr) 

b <- test_sig_gamm_f_estrus(response = "Out-Degree", behavior = "Grooming",
                         mod = ii_dog_f_estr, F_ran = F_ran_dog_estr) 

c <- test_sig_gamm_f_estrus(response = "In-Strength", behavior = "Grooming",
                         mod = ii_sig_f_estr, F_ran = F_ran_sig_estr) 

d <- test_sig_gamm_f_estrus(response = "Out-Strength", behavior = "Grooming",
                         mod = ii_sog_f_estr, F_ran = F_ran_sog_estr) 

e <- test_sig_gamm_f_estrus(response = "Betweenness", behavior = "Grooming", 
                         mod = ii_btg_f_estr, F_ran = F_ran_btg_estr)

f <- test_sig_gamm_f_estrus(response = "Local Transitivty", behavior = "Grooming",
                         mod = ii_trg_f_estr, F_ran = F_ran_trg_estr)

g <- test_sig_gamm_f_estrus(response = "Eigenvector Centrality", behavior = "Grooming",
                         mod = ii_ecg_f_estr, F_ran = F_ran_ecg_estr)


f_mixed_g_estr_tab <- rbind(a,b,c,d,e,f,g)


h <- test_sig_gamm_f_estrus(response = "Out-Strength", behavior = "Proximity",
                            mod = ii_sp_f_estr, F_ran = F_ran_sp_estr) 

i <- test_sig_gamm_f_estrus(response = "Betweenness", behavior = "Proximity", 
                            mod = ii_btp_f_estr, F_ran = F_ran_btp_estr)

j <- test_sig_gamm_f_estrus(response = "Local Transitivty", behavior = "Proximity",
                            mod = ii_trp_f_estr, F_ran = F_ran_trp_estr)

k <- test_sig_gamm_f_estrus(response = "Eigenvector Centrality", behavior = "Proximity",
                            mod = ii_ecp_f_estr, F_ran = F_ran_ecp_estr)

f_mixed_p_estr_tab <- rbind(h,i,j,k)

#write.table(f_mixed_g_estr_tab, file = "results/tables/GAMs/female estrus grooming.txt", sep = "/", row.names = F, quote = F)
write.table(f_mixed_p_estr_tab, file = "results/tables/GAMs/female estrus proximity.txt", sep = "/", row.names = F, quote = F)
