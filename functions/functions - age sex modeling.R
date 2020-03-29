# -- age - sex functions -------


#modeling function for single sna measure
age_sex_fun_single <- function(data, sna_measure = c("bt", "ec", "deg", "trans", "deg_in", "deg_out"), 
                               beh = c("total_grooming", "prox"), 
                               net_sex = c("any_combo", "female", "male"), 
                               sex_age_int = FALSE, summary = FALSE){
  
  #scale shorthand function
  z. <- function(x) scale(x)
  
  #model expression
  if(sex_age_int == TRUE){
    f <- expr(!!sym(sna_measure) + 0.00001 ~ z.(age_mid_year) + sex + sex*z.(age_mid_year) + (1|chimp_id))
  } else {
    f <- expr(!!sym(sna_measure) + 0.00001 ~ z.(age_mid_year) + sex + (1|chimp_id))
  }
  
  #data
  d <- data %>%
    filter(behavior == beh, network_sex %in% net_sex)
  
  #model with error handling
  mod <- tryCatch({glmer(f, family = Gamma(link = "log") , data = d)}, error = function(e) e) 
  
  #summary option
  if(summary == TRUE & !inherits(mod, "error")){
    mod <- summary(mod)
  }
  
  #error reporting (only error known is non-convergence)
  if(inherits(mod, "error")){mod <- "Error: model does not converge"}
  
  return(mod)
  
}


#for all sna measures
age_sex_fun_all <- function(data, 
                            beh = c("total_grooming", "prox", "grooming"), 
                            net_sex = c("any_combo", "female", "male"), 
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
  
  #loop
  for (i in seq(sna_measures)){
    
    #model expressions
    if(sex_age_int == TRUE){
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + sex*z.(age_mid_year) + (1|chimp_id))
    } else {
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + z.(avg_rank) + (1|chimp_id))
    }
    
    #data
    d <- data %>%
      filter(behavior == beh, network_sex %in% net_sex)
    #model with error handling
    mod <- tryCatch({glmer(f, family = Gamma(link = "log") , data = d)}, error = function(e) e) 
    if(summary == TRUE & !inherits(mod, "error")){
      mod <- summary(mod)
    }
    #error message, only known error is non-convergence
    if(inherits(mod, "error")){mod <- "Error: model does not converge"}
    
    #storage
    mods[[i]] <- mod
  }
  
  return(mods)
  
}


#NOTE - could apply function to each sna col for output and workaround from error

# age in same sex networks
age_fun_all <- function(data,
                        beh = c("total_grooming", "prox", "grooming"), 
                        net_sex = c("any_combo", "female", "male"),
                        summary = TRUE){

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

  for (i in seq(sna_measures)){

    #model expression
    f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + z.(avg_rank) + (1|chimp_id))

    #data
    d <- data %>%
      filter(behavior == beh, network_sex %in% net_sex)
    
    #model with error handling
    mod <- tryCatch({glmer(f, family = Gamma(link = "log") , data = d)}, error = function(e) e)
    if(summary == TRUE & !inherits(mod, "error")){
      mod <- summary(mod)
    }
    
    #error message, only known error is non-convergence
    if(inherits(mod, "error")){mod <- "Error: model does not converge"}

    mods[[i]] <- mod
  }

  return(mods)

}

#extract coefficient function - mostly for randomizations
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

ex_coef <- function(mod_list, coef = c("age", "sex", "Iint")){
  
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

