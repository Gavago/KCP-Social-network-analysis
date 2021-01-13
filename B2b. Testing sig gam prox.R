library(tidyverse)
library(mgcv)
library(progress)
select <- dplyr::select

# A. -- load node-randomized datasets ----
# undir w
load("data/ran1 - both sexes w - node (sex age rank chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)
load("data/ran2 - sex sep w - node (age rank prop_cyc chimp_id) randomized sna measures undirected prox and gmgmd weighted.Rdata", verbose = T)


# B. Prox mixed sex networks --------
#   1. Random F stat extraction -----
#     H1 - mixed sex Strength prox ----
#       - run ran mods -----

ran_sp_list <- vector("list", length = 1000)
ran_spr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)

  ran_sp_list[[i]] <- gam(deg ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_spr_list[[i]] <- gam(deg ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")

}
Sys.time() - t # 13.2 min

#save(ran_sp_list, ran_spr_list, file = "data/models gam - ran - H1 mixed sex prox random strength.Rdata") 

#       - extract Fs, beta and R -----
load("data/models gam - ran - H1 mixed sex prox random strength.Rdata", verbose = T)

sF_ran_sp <- vector("list", length = 1000)
sF_ran_spr <- vector("list", length = 1000)

sbetaR_ran_sp <- vector("list", length = 1000)
sbetaR_ran_spr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat
  sF_ran_sp[[i]] <- summary(ran_sp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_spr[[i]] <- summary(ran_spr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)

  # extract beta 
  sbetaR_ran_sp[[i]] <- summary(ran_sp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_sp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_spr[[i]] <- summary(ran_spr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_spr_list[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t # 40.3 sec

F_ran_sp <- do.call("rbind", sF_ran_sp)
F_ran_spr <- do.call("rbind", sF_ran_spr)

betaR_ran_sp <- do.call("rbind", sbetaR_ran_sp)
betaR_ran_spr <- do.call("rbind", sbetaR_ran_spr)

#     H2 - mixed sex Betweenness and transitivity prox --- mixed sex ----
#       - run ran mods -----
ran_btp_list <- vector("list", length = 1000)
ran_btpr_list <- vector("list", length = 1000)
ran_trp_list <- vector("list", length = 1000)
ran_trpr_list <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_btp_list[[i]] <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_btpr_list[[i]] <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trp_list[[i]] <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_trpr_list[[i]] <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) +  s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # min

#save(ran_btp_list, ran_btpr_list, ran_trp_list, ran_trpr_list, file = "data/models gam - ran - H2 mixed sex prox bt and trans.Rdata")

#       - extract Fs, beta and R ------
load("data/models gam - ran - H2 mixed sex prox bt and trans.Rdata", verbose = T)

sF_ran_btp <- vector("list", length = 1000)
sF_ran_btpr <- vector("list", length = 1000)
sF_ran_trp <- vector("list", length = 1000)
sF_ran_trpr <- vector("list", length = 1000)

sbetaR_ran_btp <- vector("list", length = 1000)
sbetaR_ran_btpr <- vector("list", length = 1000)
sbetaR_ran_trp <- vector("list", length = 1000)
sbetaR_ran_trpr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_btp[[i]] <- summary(ran_btp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_btpr[[i]] <- summary(ran_btpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_trp[[i]] <- summary(ran_trp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_trpr[[i]] <- summary(ran_trpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_btp[[i]] <- summary(ran_btp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_btp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_btpr[[i]] <- summary(ran_btpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_btpr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_trp[[i]] <- summary(ran_trp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_trp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_trpr[[i]] <- summary(ran_trpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_trpr_list[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t #1.4 min

F_ran_btp <- do.call("rbind", sF_ran_btp)
F_ran_btpr <- do.call("rbind", sF_ran_btpr)
F_ran_trp <- do.call("rbind", sF_ran_trp)
F_ran_trpr <- do.call("rbind", sF_ran_trpr)

betaR_ran_btp <- do.call("rbind", sbetaR_ran_btp)
betaR_ran_btpr <- do.call("rbind", sbetaR_ran_btpr)
betaR_ran_trp <- do.call("rbind", sbetaR_ran_trp)
betaR_ran_trpr <- do.call("rbind", sbetaR_ran_trpr)


#     H3 - mixed sex EC prox -----
#       - run ran mods ----
ran_ecp_list <- vector("list", length = 1000)
ran_ecpr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_both_sex_w[[i]] %>%
    filter(network_sex == "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_ecp_list[[i]] <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  ran_ecpr_list[[i]] <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = data, family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t # 11.5 min

#save(ran_ecp_list, ran_ecpr_list, file = "data/models gam - ran - H3 mixed sex prox ec.Rdata")

#       - extract Fs, beta and R -----

load("data/models gam - ran - H3 mixed sex prox ec.Rdata", verbose = T)

sF_ran_ecp <- vector("list", length = 1000)
sF_ran_ecpr <- vector("list", length = 1000)

sbetaR_ran_ecp <- vector("list", length = 1000)
sbetaR_ran_ecpr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_ecp[[i]] <- summary(ran_ecp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_ecpr[[i]] <- summary(ran_ecpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract beta and R
  sbetaR_ran_ecp[[i]] <- summary(ran_ecp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_ecp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_ecpr[[i]] <- summary(ran_ecpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_ecpr_list[[i]])$r.sq) %>% round(., 2)
  
}
Sys.time() - t #43 sec

F_ran_ecp <- do.call("rbind", sF_ran_ecp)
F_ran_ecpr <- do.call("rbind", sF_ran_ecpr)

betaR_ran_ecp <- do.call("rbind", sbetaR_ran_ecp)
betaR_ran_ecpr <- do.call("rbind", sbetaR_ran_ecpr)


#     Saving ran Fs and coefs from each hypo -----

# Fs
save(F_ran_sp, F_ran_spr,
     F_ran_btp, F_ran_btpr, F_ran_trp, F_ran_trpr, F_ran_ecp, F_ran_ecpr, file = "data/ran Fs gams - prox mixed sex.Rdata")

# sex betas and Rs
save(betaR_ran_sp, betaR_ran_spr,
     betaR_ran_btp, betaR_ran_btpr, betaR_ran_trp, betaR_ran_trpr, betaR_ran_ecp, betaR_ran_ecpr, file = "data/ran beta n R gams - prox mixed sex.Rdata")


#   2. Evaluating sig prox mixed - comparing observed to random --------


#11.25.20 CHECK COMPARE INTECEPT SIG IN FUNCTION....

#random values
load("data/ran beta n R gams - prox mixed sex.Rdata", verbose = T)
load("data/ran Fs gams - prox mixed sex.Rdata", verbose = T)

#load observed
load("data/models gam - mixed sex prox with and without rank.Rdata", verbose = T)


source("functions/functions - test sig gamm.R")
source("functions/functions - prettify results table.R")


a <- test_sig_gamm_mixed(response = "Strength", behavior = "Prox", 
                         mod = sp, modr = spr, F_ran = F_ran_sp, F_ran_r = F_ran_spr,
                         beta_ran = betaR_ran_sp, beta_ran_r = betaR_ran_spr) #%>% rank_sep()

b <- test_sig_gamm_mixed(response = "Betweenness", behavior = "Prox", 
                         mod = btp, modr = btpr, F_ran = F_ran_btp, F_ran_r = F_ran_btpr,
                         beta_ran = betaR_ran_btp, beta_ran_r = betaR_ran_btpr)

c <- test_sig_gamm_mixed(response = "Local Transitivity", behavior = "Prox",
                         mod = trp, modr = trpr, F_ran = F_ran_trp, F_ran_r = F_ran_trpr,
                         beta_ran = betaR_ran_trp, beta_ran_r = betaR_ran_trpr)

d <- test_sig_gamm_mixed(response = "Eigenvector Centrality", behavior = "Prox",
                         mod = ecp, modr = ecpr, F_ran = F_ran_ecp, F_ran_r = F_ran_ecpr,
                         beta_ran = betaR_ran_ecp, beta_ran_r = betaR_ran_ecpr)

#mixed sex tables
ms_gamH1p <- a
ms_gamH2p <- do.call("rbind", list(c,b))
ms_gamH3p <- d

# write.table(ms_gamH1p, file = "results/tables/GAMs/H1. GAM mixed sex prox strength.txt", quote = FALSE, row.names = FALSE, sep = "/")
# write.table(ms_gamH2p, file = "results/tables/GAMs/H2. GAM mixed sex prox bt trans.txt", quote = FALSE, row.names = FALSE, sep = "/")
# write.table(ms_gamH3p, file = "results/tables/GAMs/H3. GAM mixed sex prox ec.txt", quote = FALSE, row.names = FALSE, sep = "/")


# ----
# ----
# C. Prox same sex networks ------
#   3. Random F stat extraction -----
#     H1 - Strength prox  ----
#       - run ran mods ------
ran_fssp_list <- vector("list", length = 1000)
ran_fsspr_list <- vector("list", length = 1000)

ran_mssp_list <- vector("list", length = 1000)
ran_msspr_list <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), deg = deg + 0.0001)

  ran_fssp_list[[i]] <- gam(deg ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsspr_list[[i]] <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")

  ran_mssp_list[[i]] <- gam(deg ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msspr_list[[i]] <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
 
}
Sys.time() - t #  6.3 min

#save(ran_fssp_list, ran_fsspr_list, ran_mssp_list, ran_msspr_list,
#      file = "data/models gam - ran - H1 same sex prox random strength.Rdata")

#       - extract Fs, beta and R -------
load("data/models gam - ran - H1 same sex prox random strength.Rdata", verbose = T)

sF_ran_fssp <- vector("list", length = 1000)
sF_ran_fsspr <- vector("list", length = 1000)
sF_ran_mssp <- vector("list", length = 1000)
sF_ran_msspr <- vector("list", length = 1000)

sbetaR_ran_fssp <- vector("list", length = 1000)
sbetaR_ran_fsspr <- vector("list", length = 1000)
sbetaR_ran_mssp <- vector("list", length = 1000)
sbetaR_ran_msspr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_fssp[[i]] <- summary(ran_fssp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsspr[[i]] <- summary(ran_fsspr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mssp[[i]] <- summary(ran_mssp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msspr[[i]] <- summary(ran_msspr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract betas and Rs
  
  sbetaR_ran_fssp[[i]] <- summary(ran_fssp_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_fsspr[[i]] <- summary(ran_fsspr_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_mssp[[i]] <- summary(ran_mssp_list[[i]])$r.sq %>% round(., 2)
  
  sbetaR_ran_msspr[[i]] <- summary(ran_msspr_list[[i]])$r.sq %>% round(., 2)
  
}
Sys.time() - t # 44 sec

F_ran_fssp <- do.call("rbind", sF_ran_fssp)
F_ran_fsspr <- do.call("rbind", sF_ran_fsspr)

F_ran_mssp <- do.call("rbind", sF_ran_mssp)
F_ran_msspr <- do.call("rbind", sF_ran_msspr)

betaR_ran_fssp <- do.call("rbind", sbetaR_ran_fssp)
betaR_ran_fsspr <- do.call("rbind", sbetaR_ran_fsspr)

betaR_ran_mssp <- do.call("rbind", sbetaR_ran_mssp)
betaR_ran_msspr <- do.call("rbind", sbetaR_ran_msspr)

# save(ran_fssp_list, ran_fsspr_list,
#      ran_mssp_list, ran_msspr_list,
#      file = "data/models gam - ran - H1 same sex prox random deg and strength out.Rdata")


#     H2 - same sex Betweenness and transitivity prox ----
#       - run ran mods -----
ran_fsbtp_list <- vector("list", length = 1000)
ran_fsbtpr_list <- vector("list", length = 1000)
ran_fstrp_list <- vector("list", length = 1000)
ran_fstrpr_list <- vector("list", length = 1000)

ran_msbtp_list <- vector("list", length = 1000)
ran_msbtpr_list <- vector("list", length = 1000)
ran_mstrp_list <- vector("list", length = 1000)
ran_mstrpr_list <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), bt = bt + 0.0001, trans = trans + 0.0001)
  
  ran_fsbtp_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsbtpr_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fstrp_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fstrpr_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  
  ran_msbtp_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msbtpr_list[[i]] <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mstrp_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_mstrpr_list[[i]] <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #28.9 min

#save(ran_fsbtp_list, ran_fsbtpr_list, ran_fstrp_list, ran_fstrpr_list, 
#     ran_msbtp_list, ran_msbtpr_list, ran_mstrp_list, ran_mstrpr_list, 
#     file = "data/models gam - ran - H2 same sex prox bt and trans.Rdata")

#       - extract Fs, beta and R -----
load("data/models gam - ran - H2 same sex prox bt and trans.Rdata", verbose = T)

sF_ran_fsbtp <- vector("list", length = 1000)
sF_ran_fsbtpr <- vector("list", length = 1000)
sF_ran_fstrp <- vector("list", length = 1000)
sF_ran_fstrpr <- vector("list", length = 1000)

sF_ran_msbtp <- vector("list", length = 1000)
sF_ran_msbtpr <- vector("list", length = 1000)
sF_ran_mstrp <- vector("list", length = 1000)
sF_ran_mstrpr <- vector("list", length = 1000)

sbetaR_ran_fsbtp <- vector("list", length = 1000)
sbetaR_ran_fsbtpr <- vector("list", length = 1000)
sbetaR_ran_fstrp <- vector("list", length = 1000)
sbetaR_ran_fstrpr <- vector("list", length = 1000)

sbetaR_ran_msbtp <- vector("list", length = 1000)
sbetaR_ran_msbtpr <- vector("list", length = 1000)
sbetaR_ran_mstrp <- vector("list", length = 1000)
sbetaR_ran_mstrpr <- vector("list", length = 1000)

pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  
  #extract F stat like this
  sF_ran_fsbtp[[i]] <- summary(ran_fsbtp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsbtpr[[i]] <- summary(ran_fsbtpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fstrp[[i]] <- summary(ran_fstrp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fstrpr[[i]] <- summary(ran_fstrpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msbtp[[i]] <- summary(ran_msbtp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msbtpr[[i]] <- summary(ran_msbtpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mstrp[[i]] <- summary(ran_mstrp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_mstrpr[[i]] <- summary(ran_mstrpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract betas and Rs
  
  sbetaR_ran_fsbtp[[i]] <- summary(ran_fsbtp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_fsbtp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_fsbtpr[[i]] <- summary(ran_fsbtpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_fsbtpr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_fstrp[[i]] <- summary(ran_fstrp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_fstrp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_fstrpr[[i]] <- summary(ran_fstrpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_fstrpr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_msbtp[[i]] <- summary(ran_msbtp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_msbtp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_msbtpr[[i]] <- summary(ran_msbtpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_msbtpr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_mstrp[[i]] <- summary(ran_mstrp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_mstrp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_mstrpr[[i]] <- summary(ran_mstrpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_mstrpr_list[[i]])$r.sq) %>% round(., 2)
  
}

F_ran_fsbtp <- do.call("rbind", sF_ran_fsbtp)
F_ran_fsbtpr <- do.call("rbind", sF_ran_fsbtpr)
F_ran_fstrp <- do.call("rbind", sF_ran_fstrp)
F_ran_fstrpr <- do.call("rbind", sF_ran_fstrpr)

F_ran_msbtp <- do.call("rbind", sF_ran_msbtp)
F_ran_msbtpr <- do.call("rbind", sF_ran_msbtpr)
F_ran_mstrp <- do.call("rbind", sF_ran_mstrp)
F_ran_mstrpr <- do.call("rbind", sF_ran_mstrpr)

betaR_ran_fsbtp <- do.call("rbind", sbetaR_ran_fsbtp)
betaR_ran_fsbtpr <- do.call("rbind", sbetaR_ran_fsbtpr)
betaR_ran_fstrp <- do.call("rbind", sbetaR_ran_fstrp)
betaR_ran_fstrpr <- do.call("rbind", sbetaR_ran_fstrpr)

betaR_ran_msbtp <- do.call("rbind", sbetaR_ran_msbtp)
betaR_ran_msbtpr <- do.call("rbind", sbetaR_ran_msbtpr)
betaR_ran_mstrp <- do.call("rbind", sbetaR_ran_mstrp)
betaR_ran_mstrpr <- do.call("rbind", sbetaR_ran_mstrpr)


#     H3 - same sex EC ----- 
#       - run ran mods -----
ran_fsecp_list <- vector("list", length = 1000)
ran_fsecpr_list <- vector("list", length = 1000)
ran_msecp_list <- vector("list", length = 1000)
ran_msecpr_list <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  data <- list_ran_undir_sna_measure_sex_sep_w[[i]] %>%
    filter(network_sex != "any_combo" & behavior == "prox") %>%
    mutate(sex = factor(sex), chimp_id = factor(chimp_id), ec = ec + 0.0001)
  
  ran_fsecp_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_fsecpr_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "F"), family = gaussian(link = "log"), method = "REML")
  ran_msecp_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  ran_msecpr_list[[i]] <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) +  s(chimp_id, bs = "re"),  data = data %>% filter(sex == "M"), family = gaussian(link = "log"), method = "REML")
  
}
Sys.time() - t #  5.9 min


#save(ran_fsecp_list, ran_fsecpr_list, ran_msecp_list, ran_msecpr_list, file = "data/models gam - ran - H3 same sex grooming ec.Rdata")

#       - extract Fs, beta and R -----
load("data/models gam - ran - H3 same sex grooming ec.Rdata")

sF_ran_fsecp <- vector("list", length = 1000)
sF_ran_fsecpr <- vector("list", length = 1000)
sF_ran_msecp <- vector("list", length = 1000)
sF_ran_msecpr <- vector("list", length = 1000)

sbetaR_ran_fsecp <- vector("list", length = 1000)
sbetaR_ran_fsecpr <- vector("list", length = 1000)
sbetaR_ran_msecp <- vector("list", length = 1000)
sbetaR_ran_msecpr <- vector("list", length = 1000)


pb <- progress_bar$new(format = " running [:bar] :percent eta: :eta",
                       total = 1000, clear = FALSE, width= 60)
t <- Sys.time()
for(i in 1:1000){
  pb$tick()
  Sys.sleep(1/1000)
  
  #extract F stat like this
  sF_ran_fsecp[[i]] <- summary(ran_fsecp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_fsecpr[[i]] <- summary(ran_fsecpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msecp[[i]] <- summary(ran_msecp_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  sF_ran_msecpr[[i]] <- summary(ran_msecpr_list[[i]]) %>% .$s.table %>% data.frame() %>% rownames_to_column() %>% select(rowname,F)
  
  # extract betas and R
  sbetaR_ran_fsecp[[i]] <- summary(ran_fsecp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_fsecp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_fsecpr[[i]] <- summary(ran_fsecpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_fsecpr_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_msecp[[i]] <- summary(ran_msecp_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_msecp_list[[i]])$r.sq) %>% round(., 2)
  
  sbetaR_ran_msecpr[[i]] <- summary(ran_msecpr_list[[i]])$p.table %>% .[rownames(.) == "sexM", colnames(.) %in% c("Estimate", "Std. Error")] %>% c(., Rs =summary(ran_msecpr_list[[i]])$r.sq) %>% round(., 2)
  
}
t - Sys.time() #

F_ran_fsecp <- do.call("rbind", sF_ran_fsecp)
F_ran_fsecpr <- do.call("rbind", sF_ran_fsecpr)
F_ran_msecp <- do.call("rbind", sF_ran_msecp)
F_ran_msecpr <- do.call("rbind", sF_ran_msecpr)

betaR_ran_fsecp <- do.call("rbind", sbetaR_ran_fsecp)
betaR_ran_fsecpr <- do.call("rbind", sbetaR_ran_fsecpr)
betaR_ran_msecp <- do.call("rbind", sbetaR_ran_msecp)
betaR_ran_msecpr <- do.call("rbind", sbetaR_ran_msecpr)

#     Saving same sex ran coefs from each hypo -----
# save(F_ran_fssp, F_ran_fsspr,
#      F_ran_mssp, F_ran_msspr,
#      F_ran_fsbtp, F_ran_fsbtpr, F_ran_fstrp, F_ran_fstrpr,
#      F_ran_msbtp, F_ran_msbtpr, F_ran_mstrp, F_ran_mstrpr,
#      F_ran_fsecp, F_ran_fsecpr, F_ran_msecp, F_ran_msecpr, file = "data/ran Fs gams - directed and total prox same sex.Rdata")
# 
# save(betaR_ran_fssp, betaR_ran_fsspr,
#      betaR_ran_mssp, betaR_ran_msspr,
#      betaR_ran_fsbtp, betaR_ran_fsbtpr, betaR_ran_fstrp, betaR_ran_fstrpr,
#      betaR_ran_msbtp, betaR_ran_msbtpr, betaR_ran_mstrp, betaR_ran_mstrpr,
#      betaR_ran_fsecp, betaR_ran_fsecpr, betaR_ran_msecp, betaR_ran_msecpr, file = "data/ran beta n R gams - directed and total prox same sex.Rdata")
#


#   4. Evaluating sig prox mixed - comparing observed to random --------

#random values
load("data/ran beta n R gams - directed and total prox same sex.Rdata", verbose = T)
load("data/ran Fs gams - directed and total prox same sex.Rdata", verbose = T)

#observed values
load("data/models gam - same sex prox with and without rank.Rdata", verbose = T)
source("functions/functions - test sig gamm.R") # add sex of network and behavior to sig table function, also create new funcction for same sex nets

a <- test_sig_gamm_same(response = "Strength", behavior = "Prox", net_sex = "same",
                        fmod = f_sp, fmodr = f_spr, mmod = m_sp, mmodr = m_spr,
                        F_ranf = F_ran_fssp, F_ranf_r = F_ran_fsspr, 
                        F_ranm = F_ran_mssp, F_ranm_r = F_ran_msspr)

b <- test_sig_gamm_same(response = "Betweeness", behavior = "Prox", net_sex = "same",
                        fmod = f_btp, fmodr = f_btpr, mmod = m_btp, mmodr = m_btpr,
                        F_ranf = F_ran_fsbtp, F_ranf_r = F_ran_fsbtpr, 
                        F_ranm = F_ran_msbtp, F_ranm_r = F_ran_msbtpr)

c <- test_sig_gamm_same(response = "Local Transitivity", behavior = "Prox", net_sex = "same",
                        fmod = f_trp, fmodr = f_trpr, mmod = m_trp, mmodr = m_trpr,
                        F_ranf = F_ran_fstrp, F_ranf_r = F_ran_fstrpr, 
                        F_ranm = F_ran_mstrp, F_ranm_r = F_ran_mstrpr)

d <- test_sig_gamm_same(response = "Eigenvector Centrality", behavior = "Prox", net_sex = "same",
                        fmod = f_ecp, fmodr = f_ecpr, mmod = m_ecp, mmodr = m_ecpr,
                        F_ranf = F_ran_fsecp, F_ranf_r = F_ran_fsecpr, 
                        F_ranm = F_ran_msecp, F_ranm_r = F_ran_msecpr)


#same sex tables
ss_gamH1p <- a
ss_gamH2p <- do.call("rbind", list(c, b))
ss_gamH3p <- d                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   


# write.table(ss_gamH1p, file = "results/tables/GAMs/H1. GAM same sex prox deg strength in out.txt", quote = F, row.names = FALSE, sep = "/")
# write.table(ss_gamH2p, file = "results/tables/GAMs/H2. GAM same sex prox bt trans.txt", quote = F, row.names = FALSE, sep = "/")
# write.table(ss_gamH3p, file = "results/tables/GAMs/H3. GAM same sex prox ec.txt", quote = F, row.names = FALSE, sep = "/")

