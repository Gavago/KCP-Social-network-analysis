# -- age - sex functions -------


#modeling function for single sna measure
age_sex_fun_single <- function(data, sna_measure = c("bt", "ec", "deg", "trans"), 
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
                            beh = c("total_grooming", "prox"), 
                            net_sex = c("any_combo", "female", "male"), 
                            sex_age_int = FALSE, summary = TRUE){
  
  #scale shorthand function
  z. <- function(x) scale(x)
  
  #set up loop for modeling each sna measure separately
  sna_measures <-  c("bt", "ec", "deg", "trans")
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
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + sex*z.(age_mid_year) + (1|chimp_id))
    } else {
      f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + sex + (1|chimp_id))
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

data<-sna_df


#NOTE - could apply function to each sna col for output and workaround from error

# age in same sex networks
# age_fun_all <- function(data, 
#                         beh = c("total_grooming", "prox"), 
#                         net_sex = c("any_combo", "female", "male"), 
#                         summary = TRUE){
#   
#   sna_measures <-  c("bt", "ec", "deg", "trans")
#   mods <- vector("list", length = length(sna_measures))
#   names(mods) <- sna_measures
#   
#   for (i in seq(sna_measures)){
#     
#     f <- expr(!!sym(sna_measures[[i]]) + 0.00001 ~ z.(age_mid_year) + (1|chimp_id))
#     
#     s <- data %>%
#       filter(behavior == beh, network_sex %in% net_sex) %>%
#       tryCatch({glmer(f, family = Gamma(link = "log") , data = .)}, error = function(e) e) 
#     
#     d <- data %>%
#       filter(behavior == beh, network_sex %in% net_sex)
#     mod <- tryCatch({glmer(f, family = Gamma(link = "log") , data = d)}, error = function(e) e) 
#     if(summary == TRUE & !inherits(mod, "error")){
#       mod <- summary(mod)
#     }
#     if(inherits(mod, "error")){mod <- "Error: model does not converge"}
#     
#     mods[[i]] <- mod
#   }
#   
#   return(mods)
#   
# }