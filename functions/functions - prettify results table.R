

prettify_table <- function(x, full = FALSE, include_intercept = FALSE, include_beta_sex = TRUE, include_R = TRUE) {
  # duplicated mentions of sna measure and behavior
  # dup_sna_beh <- which(duplicated(x[,c("sna_measure", "beh")]))
  # duplicated mentions of sna and beh, given beh is same
  
  dup_beh_sex <- which(duplicated(x[,c("sna_measure", "beh", "sex")]))
  
  if(include_R == TRUE){
  #duplicated model adj R sq
  dup_beh_sex <- which(duplicated(x[,c("sna_measure", "beh", "sex","R")]))
  }
  
  if(include_beta_sex == TRUE){ 
    #duplicated beta sex term
    dup_bsex <- which(duplicated(x[,c("B_sex", "B_prop_greater_than")]))
  }

  x <- x %>%
    mutate_if(is.numeric, round, 2)
  
  if(include_R == TRUE){
  if(include_beta_sex == TRUE){
    x[dup_beh_sex, c("sna_measure","beh","sex", "R")] <- ""  
    x[dup_bsex, c("B_sex", "B_prop_greater_than")] <- ""
    } else { 
      x[dup_beh_sex, c("sna_measure","beh","sex","R")] <- ""  
    }
  } else {
    x[dup_beh_sex, c("sna_measure","beh","sex")] <- "" 
    }
  

  #include/exclude intercept
  if(include_intercept == FALSE) {
    x <- x %>%
      filter(!grepl("intercept", pred) & !grepl("Intercept", pred))
  }
  
  x[is.na(x)] <- ""
  
  return(x)
}
