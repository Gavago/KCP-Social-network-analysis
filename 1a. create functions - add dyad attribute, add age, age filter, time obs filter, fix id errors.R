


# Create functions - adding sexes & ages to df, apply sex specific filter ages, fix ID errors

# add dyad member sexes and birthdates
add_dyad_attr <- function(df, ID1 = "ID1", ID2 = "ID2", ...){
  load("data/attribute data alone.Rdata")
  
  names(df)[names(df) == ID1] <- "ID1"
  names(df)[names(df) == ID2] <- "ID2"  
  
  #ID1 <- rlang::enquo(ID1) %>% rlang::as_name() # tried these with !! and setNames but can't make them = "chimp_id"
  #ID2 <- rlang::as_name(ID2) %>% rlang::as_name() 
  a <- df %>%
    left_join(., attr %>% select(chimp_id, sex, dobc,...), by = c("ID1" = "chimp_id")) %>%
    left_join(., attr %>% select(chimp_id, sex, dobc,...), by = c("ID2" = "chimp_id")) %>%
    rename_at(vars(contains(".x")), list( ~ sub("\\.x", "_ID1", .))) %>%
    rename_at(vars(contains(".y")), list( ~ sub(".y", "_ID2", .)))
  return(a)
}



#create ages on june 1 of observation year
add_age <- function(df, dyad = TRUE) {
  
  if(dyad == TRUE){
    b <- df %>%
      mutate(mid_year = as.Date(paste(year,"-06-01", sep="")), 
             age_mid_year_ID1 =  as.numeric(mid_year - dobc_ID1)/365.25,
             age_mid_year_ID2 =  as.numeric(mid_year - dobc_ID2)/365.25)
    return(b)  
  }
  
  if(dyad == FALSE){
    b <- df %>%
      mutate(mid_year = as.Date(paste(year,"-06-01", sep="")), 
             age_mid_year =  as.numeric(mid_year - dobc)/365.25)
    return(b)  
  }
  
}  



#sex specific age filter
filter_age <- function(df, Age_F = 12, Age_M = 15) {
  f <- df %>%
    filter( ((sex_ID1 == "F" & age_mid_year_ID1 >= Age_F) & (sex_ID2 == "F" & age_mid_year_ID2 >= Age_F)) | #FF dyad
              ((sex_ID1 == "M" & age_mid_year_ID1 >= Age_M) & (sex_ID2 == "M" & age_mid_year_ID2 >= Age_M)) | #MM dyad
              ((sex_ID1 == "F" & age_mid_year_ID1 >= Age_F) & (sex_ID2 == "M" & age_mid_year_ID2 >= Age_M)) | #FM dyad
              ((sex_ID1 == "M" & age_mid_year_ID1 >= Age_M) & (sex_ID2 == "F" & age_mid_year_ID2 >= Age_F))) # MF dyad
  return(f)
}

#time present filter
filter_age <- function(df, Age_F = 12, Age_M = 15) {
  f <- df %>%
    filter( ((sex_ID1 == "F" & age_mid_year_ID1 >= Age_F) & (sex_ID2 == "F" & age_mid_year_ID2 >= Age_F)) | #FF dyad
              ((sex_ID1 == "M" & age_mid_year_ID1 >= Age_M) & (sex_ID2 == "M" & age_mid_year_ID2 >= Age_M)) | #MM dyad
              ((sex_ID1 == "F" & age_mid_year_ID1 >= Age_F) & (sex_ID2 == "M" & age_mid_year_ID2 >= Age_M)) | #FM dyad
              ((sex_ID1 == "M" & age_mid_year_ID1 >= Age_M) & (sex_ID2 == "F" & age_mid_year_ID2 >= Age_F))) # MF dyad
  return(f)
}


# remove spaces from any IDs
fix_ID_errors <- function(df, ID1 = "ID1", ID2 = "ID2"){
  
  names(df)[names(df) == ID1] <- "ID1"
  names(df)[names(df) == ID2] <- "ID2" 
  
  df_fixed <- df
  
  loc_ID1 <- grepl(" ", df$ID1) #locations of mistyped codes in ID1
  ID1_whack <- df[loc_ID1, "ID1"] 
  ID1_fixed <- gsub(" ", "", ID1_whack)
  df_fixed[loc_ID1, "ID1"] <- ID1_fixed
  
  loc_ID2 <- grepl(" ", df$ID2) #locations of mistyped codes in ID2
  ID2_whack <- df[loc_ID2, "ID2"]
  ID2_fixed <- gsub(" ", "", ID2_whack)
  df_fixed[loc_ID2, "ID2"] <- ID2_fixed
  
  return(df_fixed)
  
}

#save(add_dyad_attr, add_age, filter_age, fix_ID_errors, file = "functions/functions - add dyad attributes, age, filter age, fix ID errors.Rdata")

