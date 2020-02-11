# Create functions - adding sexes & ages to df, apply sex specific filter ages, fix ID errors

# add dyad member sexes and birthdates
add_dyad_attr <- function(df, ID1 = "ID1", ID2 = "ID2", ...){
  load("data/attribute data alone.Rdata")
  
  names(df)[names(df) == ID1] <- "ID1"
  names(df)[names(df) == ID2] <- "ID2"  
  
  #ID1 <- rlang::enquo(ID1) %>% rlang::as_name() # tried these with !! and setNames but can't make them = "chimp_id"
  #ID2 <- rlang::as_name(ID2) %>% rlang::as_name() 
  a <- df %>%
    left_join(., attr %>% select(chimp_id, sex, dobc, dls, starts_with("immigration"), ...), by = c("ID1" = "chimp_id")) %>%
    left_join(., attr %>% select(chimp_id, sex, dobc, dls, starts_with("immigration"),...), by = c("ID2" = "chimp_id")) %>%
    rename_at(vars(contains(".x")), list( ~ sub(".x$", "_ID1", .))) %>%
    rename_at(vars(contains(".y")), list( ~ sub(".y$", "_ID2", .)))
  return(a)
}

#create ages on june 1 of observation year
add_age <- function(df, dyad = TRUE) {
  
  if(dyad == TRUE){
    b <- df %>%
      mutate(mid_year = as.Date(paste0(year,"-06-01")), 
             age_mid_year_ID1 =  as.numeric(mid_year - dobc_ID1)/365.25,
             age_mid_year_ID2 =  as.numeric(mid_year - dobc_ID2)/365.25)
    return(b)  
  }
  
  if(dyad == FALSE){
    b <- df %>%
      mutate(mid_year = as.Date(paste0(year,"-06-01")), 
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

# mark what individuals are present for < X number of weeks in year
#load gm df for testing
#load(data/counts - annual dyadic grooming.Rdata", verbose = T)
#df <- total_gm

mark_short_time_pres <- function(df, year = year, weeks_of_year = 26) {

    t <- df %>%
    #create year start and end
    mutate(year_start = as.Date(paste0(year,"-01-01")),
           year_end = as.Date(paste0(year,"-12-31"))) %>%
    #temporary calc of weeks present from start of year
      mutate(temp_wks_pres_start_ID1 = as.numeric(difftime(dls_ID1, year_start, units = "weeks"))) %>%
      mutate(temp_wks_pres_start_ID2 = as.numeric(difftime(dls_ID2, year_start, units = "weeks"))) %>%
    #if date last seen is NA then present 52 weeks, if dls not na but later than end of year also 52, 
    #else difference bt start and dls  
    mutate(weeks_pres_ID1 = case_when(
      is.na(dls_ID1) ~ 52,
      (!is.na(dls_ID1) & (dls_ID1 > year_end)) ~ 52,
      (!is.na(dls_ID1) & (dls_ID1 < year_end)) ~ temp_weeks_pres_ID1)) %>%
    mutate(weeks_pres_ID2 = case_when(
      is.na(dls_ID2) ~ 52,
      (!is.na(dls_ID2) & (dls_ID2 > year_end)) ~ 52,
      (!is.na(dls_ID2) & (dls_ID2 < year_end)) ~ temp_weeks_pres_ID2)) %>%
    #is individ pres < half of year? then mark for removal
    mutate(short_presence_ID1 = ifelse(weeks_pres_ID1 < weeks_of_year, 1, 0)) %>%
    mutate(short_presence_ID2 = ifelse(weeks_pres_ID2 < weeks_of_year, 1, 0)) %>%
    select(-year_start, -starts_with("temp"))
  
  return(t)
}

# filter immigrants



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

#save(add_dyad_attr, add_age, filter_age, mark_short_time_pres, fix_ID_errors, file = "functions/functions - data preparation.Rdata")
