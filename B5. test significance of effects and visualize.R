library(tidyverse)
library(lme4)
# undirected
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

# 1. Calc sig all models - for loop ------
#load models

# for each model list
# where mods and predictors vary by
# subj sex, behavior type, net sex, and weighting

# need to compare observed coefficient of predictor variable in randomized model
# use name of model list to determine which randomization is needed

# specify obs mod name
# get criteria of obs mod name to see what ran model is matched
# make list of preds in obs mod to test sig for
# use ex_coef to know observed coef of given pred
# use loop to compare mag of that coef to random


a <- load("data/models - mixed sex networks.Rdata", verbose = T)
b <- load("data/models - same sex networks.Rdata", verbose = T)

all_mod_names <- c(a,b)
length(all_mod_names) # 30

#storage for sig of all models structures (each has 4 versions of results, 1 per sna measure)
sig_mod <- vector("list", length = length(all_mod_names))

for(i in seq(all_mod_names)){
  
  mod_name <- all_mod_names[[i]]
  
  
  # A - create search criteria to match model name with randomized coefficient dataframes -----
  #net sex arg
  if(grepl("mixed", mod_name)){net_sex = "mixed"}
  if(grepl("same", mod_name)){net_sex = "same"}
  
  #sex arg
  if(grepl("^f", mod_name)){ sex = "f"}
  if(grepl("^m", mod_name)){ sex = "m"}
  if(grepl("^both", mod_name)){ sex = "both"}
  
  #beh
  if(grepl("gmgmd", mod_name)){beh = "total_grooming"}
  if(grepl("prox", mod_name)){beh = "prox"}
  if(grepl("gm[^gmd]", mod_name)){beh = "grooming"}  
  
  #weighted
  if(grepl("_w$",mod_name)){weighted = TRUE} else {weighted = FALSE}
  
  
  # B - Search and load appropriate rando coefficient list, w names saved to object "c" ----
  # each c contains 1000 coefficients of a given predictor in relation
  # to a given sna measure, based on node permuted graphs.
  if(net_sex == "mixed"){
    if(sex == "both"){
      #mixed sex net - both sexes in model
      if(beh == "total_grooming"){
        if(weighted == TRUE){
          # ----- w - total grooming (mixed sex - both sexes - weighted)
          c <- load("data/random coefs - mixed both w - undir gmgmd.Rdata", verbose = T)
        }else{
          # ----- uw - total grooming (mixed sex - both sexes - unweighted)
          c <- load("data/random coefs - mixed both uw - undir gmgmd.Rdata", verbose = T)
        }
      }
      if(beh == "prox"){
        if(weighted == TRUE){
          # ----- w - prox (mixed sex - both sexes - weighted)
          c <- load("data/random coefs - mixed both w - undir prox.Rdata", verbose = T) 
        } else {
          # ----- uw - prox (mixed sex - both sexes - unweighted)
          c <- load("data/random coefs - mixed both uw - undir prox.Rdata", verbose = T)
        }
      }
      if(beh == "grooming"){
        if(weighted == TRUE){
          # ----- w - directed grooming (mixed sex - both sexes - weighted)
          c <- load("data/random coefs - mixed both w - dir gm.Rdata", verbose = T)
        } else {
          # ----- uw - directed grooming (mixed sex - both sexes - unweighted)
          c <- load("data/random coefs - mixed both uw - dir gm.Rdata", verbose = T)    
        } 
      }
    }
    if(sex != "both"){
      #mixed sex net - sexes modeled separately
      if(beh == "total_grooming"){
        if(weighted == TRUE){
          # ----- w - total grooming (mixed sex - sex sep - weighted)
          c <- load("data/random coefs - mixed sex sep w - undir gmgmd.Rdata", verbose = T)  
        } else {
          # ----- uw - total grooming (mixed sex - sex sep - unweighted)
          c <- load("data/random coefs - mixed sex sep uw - undir gmgmd.Rdata", verbose = T)  
        }
      }
      if(beh == "prox"){
        if(weighted == TRUE){
          # ----- w - prox (mixed sex - sex sep - weighted)
          c <- load("data/random coefs - mixed sex sep w - undir prox.Rdata", verbose = T) 
        } else {
          # ----- uw - prox (mixed sex - sex sep - unweighted)
          c <- load("data/random coefs - mixed sex sep uw - undir prox.Rdata", verbose = T) 
        }
      }
      if(beh == "grooming"){
        if(weighted == TRUE){
          # ----- w - directed grooming (mixed sex - sex sep - weighted)
          c <- load("data/random coefs - mixed sex sep w - dir gm.Rdata", verbose = T)
        }else{
          # ----- uw - directed grooming (mixed sex - sex sep - unweighted)
          c <- load("data/random coefs - mixed sex sep uw - dir gm.Rdata", verbose = T)  
        }
      }
    }
  }
  
  if(net_sex == "same"){
    if(beh == "total_grooming"){
      if(weighted == TRUE){
        # ----- w - total grooming (mixed sex - sex sep - weighted)
        c <- load("data/random coefs - same sex sep w - undir gmgmd.Rdata", verbose = T)  
      } else {
        # ----- uw - total grooming (mixed sex - sex sep - unweighted)
        c <- load("data/random coefs - same sex sep uw - undir gmgmd.Rdata", verbose = T)  
      }
    }
    if(beh == "prox"){
      if(weighted == TRUE){
        # ----- w - prox (same sex - sex sep - weighted)
        c <- load("data/random coefs - same sex sep w - undir prox.Rdata", verbose = T) 
      } else {
        # ----- uw - prox (same sex - sex sep - unweighted)
        c <- load("data/random coefs - same sex sep uw - undir prox.Rdata", verbose = T) 
      }
    }
    if(beh == "grooming"){
      if(weighted == TRUE){
        # ----- w - directed grooming (same sex - sex sep - weighted)
        c <- load("data/random coefs - same sex sep w - dir gm.Rdata", verbose = T)
      } else {
        # ----- sig uw - directed grooming (same sex - sex sep - unweighted)
        c <- load("data/random coefs - same sex sep uw - dir gm.Rdata", verbose = T)
      }
    }
  }
  
  
  # C - Set up predictors from observed model: ----
  # get(mod name) results in a list of models
  # where model structure and dataset is the same
  # only the response sna variable changes
  # and for each predictor asses its sig across the various sna measures 
  mod_list <- get(mod_name)
  for_preds <- mod_list[lapply(mod_list, function(x) (class(x) == "summary.merMod")) %>% unlist()]
  preds <- 
    coef(for_preds[[1]]) %>% rownames()
  preds <- preds[preds != "(Intercept)"]
  
  
  # storage for sig of predictors
  sig_pred <- vector("list", length = length(preds))
  
  # D - Assess significance of each predictor on integration -----------
  for(j in seq(preds)){
    
    #title the predictor appropriately for extract coef function
    pred = NA #resets for each loop
    if(grepl("year)$", preds[j])){pred = "age"}   
    if(grepl("^sexM$", preds[j])){pred = "sex"}
    if(grepl("rank", preds[j])){pred = "rank"}
    if(grepl("prop_cyc", preds[j])){pred = "prop_cyc"}
    if(grepl(":", preds[j])){pred = "int"}
    
    #get observed coefficients of predictor with different sna responses
    obs_coefs <- ex_coef(mod_list, pred)
    
    #get the relevant permutated coefs for the predictor
    
    if(weighted == TRUE){ends_c <- paste0(pred,"_b$")} else {ends_c <- paste0(pred,"_b_uw$")}
    search_c <- paste(sex, ends_c, sep = ".*")
    name_c <- c[grep(search_c, c)] #finds the exact df
    perm_c <- get(name_c)
    
    #storage for significance of predictor's effect/coef
    sig_coef <- vector("list", length = length(obs_coefs))
    names(sig_coef) <- names(obs_coefs)
    
    # asses the sig of a given predictor across the 4 sna measures  ------
    for (k in seq(obs_coefs)) {
      beta <- obs_coefs[k]
      beta_name <- names(beta)
      if(!is.na(beta)){
        prop_greater <- sum(beta > perm_c[k], na.rm = T)/ sum(!is.na(perm_c[k]))
        if(all(prop_greater >= 0.95 | prop_greater <= 0.05)){sig = "*"} else {sig = ""}  
      }  else { beta = NA ; prop_greater = NA ; sig = NA }
      # mod_name gives the source, beta name gives the specific sna measure of the beta in question - identifies response
      sig_coef[[k]] <- data.frame(mod_name, response = beta_name, pred, sex, beh, net_sex, weighted, beta, prop_greater, sig, stringsAsFactors = F)
      
    }
    sig_pred[[j]] <- do.call("rbind",sig_coef) 
  }
  
  sig_mod[[i]]  <- do.call("rbind", sig_pred)
}

sig_tests <- do.call("rbind", sig_mod)

dim(sig_tests)
#336, 10 hot diggity damn.

same_sex_results <- sig_tests %>%
  filter(grepl("same", mod_name))
nrow(same_sex_results)  #120

mixed_sex_results <- sig_tests %>%
  filter(grepl("mixed", mod_name))
nrow(mixed_sex_results)  #216



sig_tests %>%
  filter(is.na(beta)) %>%
  filter(!(weighted == FALSE & !grepl("deg",response)))
# most of what does not converge is either 1) in directed grooming and is ec, confirms that 
# we don't want to look at grand/indirect integration measures when directed, doesn't
# work mathematically and not very informative in the first place (who cares about the directions of your indirect connectons?)
# or 2) is a product of very little variation in indirect measures of integration when ties aren't weighted
# eg. betweenness in unweighted networks (can see without 2nd filter, bt in both gmgmd mixed uw)
# strange non-ec that doesn't converge is betweenness in f_prox_same, many 0's

#save(sig_tests, same_sex_results, mixed_sex_results, file = "data/sig tests - all models.Rdata")

# 2. Explore sig ------
load("data/sig tests - all models.Rdata", verbose = T)
source("functions/functions - table age sex results.R")

#H1
undir_deg_tab <- table_results(mixed_sex_results, "DEG")
#Hone degree table... Only use directed? refer to hypos

dir_deg_tab <- table_results(mixed_sex_results, "DIR DEG")
# could potentially only use undir degree for prox...

#H2
bt_trans_tab <- table_results(mixed_sex_results, "BT")

#H3
ec_tab <- table_results(mixed_sex_results, "EC")

undir_deg_tab


# write.table(undir_deg_tab, file = "results/tables/H1a. undir deg - age sex effects table.txt", row.names = F, quote = F, sep = "/")
# write.table(dir_deg_tab, file = "results/tables/H1b. dir deg - age sex effects table.txt", row.names = F, quote = F, sep = "/")
# write.table(bt_trans_tab, file = "results/tables/H2. bt trans - age sex effects table.txt", row.names = F, quote = F, sep = "/")
# write.table(ec_tab, file = "results/tables/H3. ec - age sex effects table.txt", row.names = F, quote = F, sep = "/")



#measure
#behavior
#network members

  
# graveyard --------------

# What are age effects on integration? ----


# A. in total grooming -----

# -- 1. both sexes mixed -----
sig_tests %>%
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "both") %>% 
  filter(beh == "total_grooming")
# sig age*sex pos for deg and trans
# rank always pos
# males always higher than females except for trans

# -- 2. females mixed ------
sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "f") %>% 
  filter(beh == "total_grooming")
# deg and trans dec w age
# rank all inc, ec inc w prop cyc

# -- 3. females same sex ----
sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "same", sex == "f") %>% 
  filter(beh == "total_grooming")
# bt and deg dec w age
# bt inc w rank, ec dec w rank

# -- 4 - 5. directed w & uw - females mixed AND SAME sex (DECIDE ON SAME) ----
# just focus on deg in and out, weighted and unweighted
sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "f") %>% 
  filter(beh == "grooming")
# deg in dec w age, deg out no converge 
# ACK, so much doesn't converge for females in mixed directional model
# EC, is normal, but deg out, why no converge??

sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "same", sex == "f") %>% 
  filter(beh == "grooming")
# much better convergence in same sex network
# bt, ec, and deg out dec w age
# deg out inc w rank
# somehow justify exploring same sex networks for directed gm?

peep_dataset(dir_sna_w, net_sex = "any_combo", subj_sex = "F", beh = "grooming")
# double check - rank class should be factor, not == 0

sig_tests %>% 
  filter(weighted == FALSE) %>%
  filter(net_sex == "mixed", sex == "f") %>% 
  filter(beh == "grooming")
# bt deg in n out dec w age, number of gm partners dec w age in F
# ec no converge

sig_tests %>% 
  filter(weighted == FALSE) %>%
  filter(net_sex == "same", sex == "f") %>% 
  filter(beh == "grooming")
# bt deg in n out dec w age, number of gm partners dec w age in F


# -- 6. males mixed ------
sig_tests %>%
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "m") %>% 
  filter(beh == "total_grooming")
# bt dec w age, ec and trans increase w age
# bt inc w rank, no other rank effects

# -- 7. males same ----
sig_tests %>%
  filter(weighted == TRUE) %>%
  filter(net_sex == "same", sex == "m") %>% 
  filter(beh == "total_grooming")
# ec and deg inc w age
# no rank effects

# -- 8- 9. directed w & uw - males mixed sex -----
# just focus on deg in and out, weighted and unweighted
sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "m") %>% 
  filter(beh == "grooming")
# deg in & deg out dec w age (so does bt)
# double checked the specification of this model in B3, looks good!

sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "same", sex == "m") %>% 
  filter(beh == "grooming")
# deg in dec w age

sig_tests %>% 
  filter(weighted == FALSE) %>%
  filter(net_sex == "mixed", sex == "m") %>% 
  filter(beh == "grooming")
# deg in and out inc w age 
# EC does not converge... ah, this has to do with the problem of directed weighted eigenvector stuff,
# don't pay attention to EC in directed nets, would otherwise use pagerank

sig_tests %>% 
  filter(weighted == FALSE) %>%
  filter(net_sex == "same", sex == "m") %>% 
  filter(beh == "grooming")
# deg out alone inc w age
# does this mean that females are more attracted to older males? bc once they are
# removed from network, males don't increase in gm received w age.

# -- follow up ----
# interp initial age*sex interaction in mixed sex model 1:
# in mods 2,3,4 + unweighted 5 - females dec number of gm partners and time spent w them with age
# whereas, in mods 8 - 9, males dec time w gm partners  w age but dec time w them
# leading to overall inc in ec? ec inc both mixed and same sex nets, trans inc mixed and deg same sex

# B. in prox -----
# -- 1. both sexes mixed -----
sig_tests %>%
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "both") %>% 
  filter(beh == "prox")
# sig age*sex pos for deg
# bt dec w rank, ec deg inc w rank

# -- 2. prox w - females mixed ------
sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "f") %>% 
  filter(beh == "prox")
# deg dec w age,
# deg inc w rank, bt inc w prop cyc, deg dec w prop cyc
# WHY EC doesn't converge?

peep_dataset(sna_w, beh = "prox", net_sex = "any_combo", subj_sex = "F")[[2]]
x <- sna_w %>% filter(network_sex == "any_combo", behavior == "prox", sex == "F")

# -- 3. prox w - females same sex ----
sig_tests %>% 
  filter(weighted == TRUE) %>%
  filter(net_sex == "same", sex == "f") %>% 
  filter(beh == "prox")
# ec n deg dec w age
# ec n deg dec w prop cyc
# bt doesn't converge, too many 0 values

peep_dataset(sna_w, beh = "prox", net_sex = "female")[[2]]

# -- 4. prox w - males mixed ------
sig_tests %>%
  filter(weighted == TRUE) %>%
  filter(net_sex == "mixed", sex == "m") %>% 
  filter(beh == "prox")
# bt inc w age
# ec and trans inc w rank

# -- 5. prox w - males same ----
sig_tests %>%
  filter(weighted == TRUE) %>%
  filter(net_sex == "same", sex == "m") %>% 
  filter(beh == "prox")
# bt dec w age, ec n deg inc w age
# same patterns bt, ec, deg w rank

# -- follow up on prox results ----
# using unweighted neworks to understand degree -

# interp initial interaction in mod 1 on degree:
# in mod 2 & 3, fem dec deg w age in both mixed and all female network
# in mod 5, males inc deg (and ec) w age
# is this bc of inc/dec in intensity of interactions or in number of social partners?

# number of spatial associates does dec w age in females, in both mixed and same sex networks
sig_tests %>% 
  filter(weighted == FALSE) %>%
  filter(net_sex == "mixed", sex == "f") %>% 
  filter(beh == "prox")
sig_tests %>% 
  filter(weighted == FALSE) %>%
  filter(net_sex == "same", sex == "f") %>% 
  filter(beh == "prox")

# number of spatial associates doesn't change w age in males, just their time w them
sig_tests %>%
  filter(weighted == FALSE) %>%
  filter(net_sex == "same", sex == "m") %>% 
  filter(beh == "prox")




# comment - strange that males increase in bt w age in mixed sex network, perhaps bc stay in touch w females, who become less integrated w males
# meanwhile they become more integrated in all male network





#graveyard ---

# e.g. gmgmd_age_b
names(gmgmd_sex_b)
# coefficients of age effects on a given sna measure in a model w only age-sex main effects

# Mixed sex net, both sexes, for 4 sna measures, coefs include age, sex, rank, and age*sex
# Mixed sex net, each sex
# female = for 4 sna measures, coefs include age, rank, prop_cyc
# male = for 4 sna measures, coefs include age, rank
sum(coef(gmgmd_mixed_int_w$bt)[4,1] >  gmgmd_int_int_b$bt_int_int, na.rm = T) / 1000 # b -0.55 sig lo, males decrease in betweenness w age while fem don't

# checking filter on mod results -
#number of model results removed bc are unweighted non-degree sna measures 
mixed_sex_results %>%
  filter(weighted == FALSE & !grepl("deg", response)) %>% nrow() # 72
#number of model results removed bc are non-degree measures of directed grooming
mixed_sex_results %>%
  filter(grepl("gm_", mod_name) & !grepl("deg", response)) %>% nrow() # 36
mixed_sex_results %>%
  filter(weighted == FALSE & !grepl("deg", response)) %>%
  filter(grepl("gm_", mod_name) & !grepl("deg", response)) %>% nrow() # 18 of 2nd condition are already in 1st
# filter should subtract 90 from 216 or 126




# total grooming (9 models)
#  - age*sex in mixed (1)
#  - age in m or f mixed (2)
#  - age in same sex (2)
#  - age in directed grooming degree both sexes, weighted and unweighted (4)
# 
# prox (5 models)
# - age*sex in mixed (1)
# - age in m or f mixed (2)
# - age in same sex (2)
