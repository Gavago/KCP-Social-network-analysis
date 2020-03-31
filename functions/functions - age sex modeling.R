# -- age - sex functions -------


# modeling function for single sna measure ------
age_sex_fun_single <- function(data, sna_measure = c("bt", "ec", "deg", "trans", "deg_in", "deg_out"), 
                               beh = c("total_grooming", "prox"), 
                               net_sex = c("any_combo", "female", "male"), 
                               subj_sex = NULL,
                               sex_age_int = FALSE, summary = FALSE){
  
  #scale shorthand function
  z. <- function(x) scale(x)
  #model expressions
  #both sexes 
  if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == TRUE)){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + sex*z.(age_mid_year) + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == FALSE)){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "M" | net_sex == "male")){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + (1|chimp_id))
  }
  if(all(net_sex == "any_combo" & subj_sex == "F" | net_sex == "female")){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + z.(prop_cyc) + (1|chimp_id))
  }
  
  #data
  if(subj_sex == "both"){ subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
  
  d <- data %>%
    filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
  #model with error handling
  mod <- tryCatch({
    mod <- glmer(f, family = Gamma(link = "log") , data = d)
  }, warning = function(w) {
    mod <- "Warning: model does not converge, max|grad|"
  }, error = function(e) {
    mod <- "Error: model does not converge" 
  }) #holy fuck it worked!!!!! I finally figured out tryCatch syntax...8o http://mazamascience.com/WorkingWithData/?p=912
  
  if(all(!inherits(mod, "character") & summary == T)){
    mod <- summary(mod)
  }
  
  
  return(mod)
  
}


#modeling every sna measure -------------
# function filters the network behavior, the network sex, and the sex of network members if a mixed sex network
# then models the relationships between sna measures and network sex and subj sex specific predictor variables

age_sex_fun_all <- function(data, 
                            beh = c("total_grooming", "prox", "grooming"), 
                            net_sex = "any_combo",
                            subj_sex = NULL,
                            sex_age_int = FALSE, summary = TRUE){
  
  #scale shorthand function
  z. <- function(x) scale(x)
  
  #set up loop for modeling each sna measure separately
  if(beh %in% c("total_grooming", "prox")){
    sna_measures <-  c("bt", "ec", "deg", "trans")  
  }
  if(beh %in% "grooming"){
    sna_measures <-  c("bt", "ec", "deg_in", "deg_out")  
  }
  
  
  mods <- vector("list", length = length(sna_measures))
  names(mods) <- sna_measures
  
  #interaction option for model list names
  if(sex_age_int == TRUE){
    names(mods) <- paste0(sna_measures,"_int")
  }
  
  # assign a subj_sex if is NULL
  # subj_sex defaults to both if only a mixed network, net_sex = "any_combo", is specified
  if(net_sex == "any_combo" & is.null(subj_sex)){subj_sex = "both"}
  if(net_sex == "female"){subj_sex <- "F"}
  if(net_sex == "male"){subj_sex <- "M"}
  
  
  #loop
  for (i in seq(sna_measures)){
    
    #model expressions
    #both sexes 
    if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == TRUE)){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + sex*z.(age_mid_year) + (1|chimp_id))
    }
    if(all(net_sex == "any_combo" & subj_sex == "both" & sex_age_int == FALSE)){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + (1|chimp_id))
    }
    if(all(net_sex == "any_combo" & subj_sex == "M" | net_sex == "male")){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + (1|chimp_id))
    }
    if(all(net_sex == "any_combo" & subj_sex == "F" | net_sex == "female")){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + z.(prop_cyc) + (1|chimp_id))
    }

    #data
    # when subj_sex is both, change subj_sex1 filter term to M and F
    if(subj_sex == "both"){subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
    
    d <- data %>%
      filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
    
    #model with error handling
    mod <- tryCatch({
      mod <- glmer(f, family = Gamma(link = "log") , data = d)
    }, warning = function(w) {
      mod <- "Warning: model does not converge, max|grad|"
    }, error = function(e) {
      mod <- "Error: model does not converge" 
            }) #holy fuck it worked!!!!! I finally figured out tryCatch syntax...8o http://mazamascience.com/WorkingWithData/?p=912
    if(all(!inherits(mod, "character") & summary == T)){
      mod <- summary(mod)
    }
    
    #storage
    mods[[i]] <- mod
  }
  
  return(mods)
  
}

#peek in modeled data set ------

peep_dataset <- function(data, 
                         beh = c("total_grooming", "prox", "grooming"), 
                         net_sex = "any_combo",
                        subj_sex = NULL){
  
  # assign a subj_sex if is NULL
  # subj_sex defaults to both if only a mixed network, net_sex = "any_combo", is specified
  if(net_sex == "any_combo" & is.null(subj_sex)){subj_sex = "both"}
  if(net_sex == "female"){subj_sex <- "F"}
  if(net_sex == "male"){subj_sex <- "M"}
  
  #data
  # when subj_sex is both, change subj_sex1 filter term to M and F
  if(subj_sex == "both"){subj_sex1 <- c("M", "F")} else {subj_sex1 <- subj_sex}
  
  dat <- data %>%
    filter(behavior == beh, network_sex == net_sex, sex %in% subj_sex1)
  
  zeros <- dat %>%
    apply(., 2, function(x) sum(x == 0))
  
  if(subj_sex == "both"){
    desc <- dat %>%
      group_by(sex) %>%
      summarise(n_obs = n(), n_chimps = n_distinct(chimp_id))  
  }
  if(subj_sex != "both"){
  desc <- dat %>%
    summarise(n_obs = n(), n_chimps = n_distinct(chimp_id))
  }
  
  df_info <- list(dat, zeros, desc)
  return(df_info)
  
}

#extract coefficient function - mostly for randomizations ------

# ex_coef <- function(m, coef = c("age", "sex")){
#   if( m == "Error: model does not converge"){
#     b <- NA
#   }
#   
#   if(coef == "age"){
#     b <- coef(m)[2,1]
#   }
#   if(coef == "sex"){
#     b <- coef(m)[3,1]
#   }
#   return(b)
# } 
#figure out conditional lapply or just make loop for extracting
# REWRITE FOR VARIOUS MODEL STRUCTURES... 
# mixed both sexes
# mixed female
# mixed male
# same female
# same male

ex_coef <- function(mod_list, coef = c("age", "sex", "int")){
  
  coef_list <- vector("list", length = 4)
  names(coef_list) <- paste(names(mod_list), coef, sep = "_")
  
  for(k in 1:4){
    m <- mod_list[[k]]
    
    if(inherits(m, "summary.merMod")){

        if(coef == "age"){
          b <- coef(m)[2,1]
        }
        if(coef == "sex"){
          b <- coef(m)[3,1]
        }
      if(coef == "int"){
        b <- coef(m)[4,1]
      }
      }
    
    if( is.character(m)){
      b <- NA
    }
    
    coef_list[[k]] <- b
  }
  coefs <- unlist(coef_list)
  return(coefs)
}

