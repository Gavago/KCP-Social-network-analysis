library(dplyr)

#sorting var as if factor but preserve as character funs
sna_sort <- function(x) factor(x, levels = c( "W Degree", "UW Degree", "W Degree In","UW Degree In", "W Degree Out", "UW Degree Out", "Betweenness", "Local Transitivity", "Eigenvector Centrality"))
beh_sort <- function(x) factor(x, levels = c("Total Grooming", "Grooming" ,"Prox"))

#filter age sex model results
table_results <- function(results, table = c("full", "DEG", "DIR DEG", "BT", "EC", "TRANS")){
  
  x <- results %>%
    mutate(beh = str_to_title(gsub("_", " ", beh))) %>%
    mutate(pred = ifelse(pred == "int", "Age:Sex", str_to_title(pred))) %>%
    mutate(pred = ifelse(pred == "Prop_cyc", "Time swelling", pred)) %>%
    mutate(sex = case_when(
      sex == "f" ~ "Female",
      sex == "m" ~ "Male",
      sex == "both" ~ "Both")) %>%
    mutate(sna_measure = case_when(
      grepl("bt", response) ~ "Betweenness",
      grepl("ec|1", response) ~ "Eigenvectory Centrality",
      grepl("trans", response) ~ "Local Transitivity",
      grepl("deg_in_", response) & grepl("_w$", mod_name) ~ "W Degree In",
      grepl("deg_out_", response) & grepl("_w$", mod_name) ~ "W Degree Out",
      grepl("deg_int|deg_[^i]|deg_[^o]", response) & grepl("_w$", mod_name) ~ "W Degree",
      grepl("deg_in_", response) & grepl("_uw$", mod_name) ~ "UW Degree In",
      grepl("deg_out_", response) & grepl("_uw$", mod_name) ~ "UW Degree Out",
      grepl("deg_int|deg_[^i]|deg_[^o]", response) & grepl("_uw$", mod_name) ~ "UW Degree")) %>%
    #don't want unweighted sna measures that aren't degree
    filter(!(weighted == FALSE & !grepl("deg", response))) %>%
    # don't want sna measures for grooming that aren't degree 
    filter(!(grepl("gm_", mod_name) & !grepl("deg", response))) %>%
    arrange(sna_sort(sna_measure),beh_sort(beh), mod_name) %>%
    mutate_if(is.numeric, round, 2) %>%
    select(sna_measure, beh, sex, pred, beta, prop_greater, sig) # , response, mod_name
  
  if(table == "full"){
    x <- prettify(x)
  }
  
  if(all(table == "DEG")){
    x <- x %>%
      filter(grepl("Deg", sna_measure) & !grepl("In|Out", sna_measure)) %>%
      prettify()
  }
  
  if(all(table == "DIR DEG")){
    x <- x %>%
      filter(grepl("Deg", sna_measure) & grepl("In|Out", sna_measure)) %>%
      prettify()
  }
  
  
  if(all(table == "BT"|table == "TRANS")){
    x <- x %>%
      filter(grepl("Bet|Trans", sna_measure)) %>%
      prettify()
  }
  
  if(table == "EC"){
    x <- x %>%
      filter(grepl("Eigen", sna_measure)) %>%
      prettify()
  }
  
  return(x)
}

prettify <- function(x, full = FALSE){
  # duplicated mentions of sna measure and behavior
  dup_sna_beh <- which(duplicated(x[,c("sna_measure", "beh")]))
  # duplicated mentions of sna and beh, given beh is same
  dup_beh_sex <- which(duplicated(x[,c("sna_measure", "beh", "sex")]))
  
  
  if(full == TRUE){
    x[dup_sna_beh, "sna_measure"] <- ""
    x[dup_beh_sex, c("beh","sex")] <- ""  
  } else { 
    x[dup_beh_sex, "sex"] <- "" 
    x[dup_sna_beh, c("sna_measure", "beh")] <- ""
    
    }
  
  return(x)
}
