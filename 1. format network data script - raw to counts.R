#install.packages("tidyverse")
#install.packages("magrittr")
library(tidyverse)
library(magrittr)
library(RODBC)
library(readxl)
ymd <- lubridate::ymd
mdy <- lubridate::mdy
month <- lubridate::month
year <- lubridate::year
ymd_hms <- lubridate::ymd_hms
# Open Database Connectivity (ODBC) is a protocol 
# that you can use to connect a Microsoft Access database to an external data source such as Microsoft SQL Server. 
# help: https://www.statmethods.net/input/dbinterface.html
# https://www.datacamp.com/community/tutorials/importing-data-r-part-two
# define a data source name DSN - check if access is running at 32 or 64 bit to choose correct odbc option in administrative tools
# https://support.office.com/en-us/article/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7


# Timing issues to take care of:
# create new variable for time present or seen during year...?, if individual is not present for >= 6 months in a year, is removed from year
# create date first and last seen lookup table, add this on to all final network dfs


# Rdata files output from this file are
# action lookup.Rdata
# raw aggression.Rdata
# attribute data alone.Rdata
# raw 5 m proximity.Rdata
# grooming raw and dyad attributes.Rdata
# dyadic focal party and total focal counts.Rdata
# annual possible focal dyads.Rdata
# functions - add dyad attributes, age, filter age.Rdataa
# annual dyadic grooming counts.Rdata
# count time in 5m.Rdata
# counts are to pass to indices in 2

## 1. Create functions - adding sexes & ages to df, apply sex specific filter ages, fix ID errors -------

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



## 2. Upload data from access database DSN ####
load("functions/functions - add dyad attributes, age, filter age, fix ID errors.Rdata", verbose = T)

connection <- odbcConnect("Kanyawara social")

#demography data
demox <- sqlFetch(connection, "DEMOGRAPHY")
i <- sapply(demox, is.factor)
demox[i] <- lapply(demox[i], as.character)

#grooming records from focal data
groomingx <- sqlFetch(connection, "FOCAL GROOMING SCANS") %>%
  mutate(year = year(ymd(Date)), month = month(ymd(Date)))
i <- sapply(groomingx, is.factor)
groomingx[i] <- lapply(groomingx[i], as.character)


#5 m records from focal data
focal_5m1 <- sqlFetch(connection, "FOCAL PROXIMITY within 5")
focal_5m1 %<>% mutate_if(is.factor, as.character)

#aggression 
agg <- sqlFetch(connection, "AGGRESSION")

#various action codes, e.g. grooming types
action <- sqlFetch(connection, "ACTION_LOOKUP")
agg_key <- read_xlsx("data/Aggression key.xlsx") %>%
  separate(`Key:`, into = c("code", "meaning"), sep = "=|-") %>%
  filter(!is.na(code))

#save(agg, agg_key, file = "data/raw aggression and agg key.Rdata")
#save(action, file = "data/action lookup.Rdata")



# ----- Format attribute data #####
names(demox)
#demox %>%
#  filter(chimp_name == "Ngamba") %>% View()
# Ngamba the female was first seen in 2004, last seen feb 7 2007, before focal obs started
# her code name "NA" needs correcting only when dealing with scan data from <= 2007


attr <- demox %>%
  filter(kanyawara_member_flag == 1) %>%
  select(chimp_id, sex, date_of_birth_corrected, date_last_seen, mother_id, father_id) %>%
  mutate(dobc = as.Date(date_of_birth_corrected)) %>%
  mutate_if(is.factor, as.character)

#attr %<>% # 1.15.2020
# mutate(dobc = as.Date(dobc), date_last_seen = as.Date(date_last_seen), chimp_id = as.character(chimp_id))

attr[is.na(attr$chimp_id), "chimp_id"] <- "NX" #change ngamba to NX
attr[attr$chimp_id == "NPT", "chimp_id"] <- "NT"

#save(attr, file = "data/attribute data alone.Rdata")

# ----- Format prox in 5 #####
names(focal_5m1)
names(attr)

focal_5m1 %>%
  filter(Exclude == T)

focal_5m_raw <- focal_5m1 %>%
  rename(ID1 = Focal, ID2 = w5) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(year = year(ymd(Date)), month = month(ymd(Date))) %>%
  filter(ID1 != ID2) %>% # removes 50 cases, removes 59 cases
  fix_ID_errors()

focal_5m_raw$ID1[grepl(" ", focal_5m_raw$ID1)] #check, each should be 0
focal_5m_raw$ID2[grepl(" ", focal_5m_raw$ID2)]

nrow(focal_5m_raw) # 177686

#save(attr, focal_5m_raw, file = "data/raw 5 m proximity.Rdata")

# ----- Format grooming and add attribute data ####

# fix a few typos of chimpid names
names(groomingx)

groomingx$Focal[grepl(" ", groomingx$Focal)]
groomingx$Partner_ID[grepl(" ", groomingx$Partner_ID)]
groomingx[groomingx$Focal == "TT ", "Focal"] <- "TT"

groomingx[groomingx$Partner_ID == "NPT", "Partner_ID"] <- "NT"
groomingx[groomingx$Partner_ID == "OP(DEAD)", "Partner_ID"] <- "OP"

names(groomingx)

grooming_raw <- groomingx %>%
  rename(ID1 = Focal, ID2 = Partner_ID) %>% 
  filter(ID2 != "UNK") %>%
  filter(ID1 != ID2)
# to check for appropriate codes in file, can left join w attributes and see what rows attributes are NA...

#save(attr, grooming_raw, file = "data/grooming raw and dyad attributes.Rdata")

<<<<<<< HEAD
# ----- Format and agg and action look up
load("data/raw aggression.Rdata", verbose = T)
load("data/action lookup.Rdata", verbose = T)
=======
## 3. Focal party data and possible dyadsfrom MET (starts 2009) ####
load("functions/functions - add dyad attributes, age, filter age, fix ID errors.Rdata", verbose = T)

foc_part1 <- read.csv(file = "data/d. FOCAL PARTY CORRECTED MET.txt", header = F, stringsAsFactors = F) %>%
  rename(date = "V1", time = "V2", f_obs = "V3", focal = "V4", kpc_obs = "V5", partner = "V6") %>%
  unite("scan_id", date, time, sep = "_") %>%
  separate(scan_id, into = c("date", "trash", "scan_time"), sep = " ", remove = F) %>%
  select(-trash) %>%
  mutate(year = year(mdy(date)), month = month(mdy(date))) 

foc_part1$focal[foc_part1$focal == "NPT"] <- "NT"
foc_part1$partner[foc_part1$partner == "NPT"] <- "NT"

foc_part1[grepl(" ", foc_part1$focal), "focal"]
foc_part1[grepl(" ", foc_part1$partner), "partner"]

foc_part <- fix_ID_errors(foc_part1, ID1 = "focal", ID2 = "partner")

# test fix id errors worked
# foc_part[grepl(" ", foc_part$ID1), "ID1"]
# foc_part[grepl(" ", foc_part$ID2), "ID2"]


# ----- create total AB party -----

#this is a file where you have all focal individuals and who was in their party for every scan taken
# i.e. count of focal A in which B was in party, for time period X
#this is what you use to control for 
#time AB grooming, time AB in 5 meters
#is not used for party association of A and B (that would be somthing like)

head(foc_part)
nrow(foc_part) #1760408

#counts where focal was in same party as partner for a given year
dyad_party <- foc_part %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()

nrow(dyad_party) #12076
head(dyad_party)

total_AB_party <- dyad_party %>%
  full_join(., dyad_party, by = c("ID1" = "ID2", "ID2" = "ID1", "year")) %>%
  rename(n_AB = n.x, n_BA = n.y) %>%
  mutate(n_AB = ifelse(is.na(n_AB), 0, n_AB), n_BA = ifelse(is.na(n_BA), 0, n_BA)) %>%
  mutate(total_AB_party = n_AB + n_BA) %>%
  arrange(ID1, year, ID2)

x <- names(total_AB_party)[grepl("^n_", names(total_AB_party))]
names(total_AB_party)[grepl("^n_", names(total_AB_party))] <- paste(x, "party", sep = "_")


nrow(total_AB_party) #17568 after removing ID typos
head(total_AB_party)

#total_AB_party %>%
#  filter(apply(.,1, function(x) any(is.na(x))))


#total focal obs in given year
total_focal <- foc_part %>%
  group_by(year, ID1) %>%
  tally()
nrow(total_focal) #267 


#save(total_AB_party, total_focal, file = "data/counts - dyadic focal party and total focal.Rdata")

# ----- create possible dyads per year #####
#start using met's file 1/2020

head(foc_part)

#focal_scans_raw[focal_scans_raw$Focal == "TT ", "Focal"] <- "TT"
#focal_scans_raw[focal_scans_raw$Focal == "UM ", "Focal"] <- "UM"

dir_annual_dyads <- foc_part %>%
  distinct(ID2, year) %>% #use ID2 bc is more comprehensive than ID1 (id1 was focals, 2 was partners)
  mutate(ID1 = ID2) %>%
  group_by(year) %>%
  complete(ID2, nesting(ID1)) %>%
  ungroup() %>%
  filter(ID1 != ID2)

nrow(dir_annual_dyads) # using "partner" column is 24036, when using "Focal" column is 7712

abc_ids <- dir_annual_dyads %>%
  select(ID1, ID2) %>%
  apply(., 1, sort) %>% #alphabetize sort in each row
  t(.) %>%
  data.frame(., stringsAsFactors = F) %>%
  rename(ID1 = X1, ID2 = X2) # because undirected

undir_annual_dyads <- dir_annual_dyads %>%
  select(year) %>%
  cbind(abc_ids, .) %>%
  distinct(ID1, ID2, year)

nrow(undir_annual_dyads) # using "partner" column is 12018, when using "Focal" column is 3856

#save(dir_annual_dyads, undir_annual_dyads, file = "data/annual possible focal dyads.Rdata")

## 4. Create dyadic annual grooming counts (starts 2009)  ####
# ----- load grooming data #####

#used grooming table made in Access "FOCAL GROOMING SCANS"
load("data/grooming raw and dyad attributes.Rdata", verbose = T)
load("data/annual possible focal dyads.Rdata", verbose = T)
load("functions/functions - add dyad attributes, age, filter age, fix ID errors.Rdata", verbose = T)


# focal codes for grooming between adults: G (grooming), MG (mutual grooming), BG (being groomed), "BG,F", HGC, HCG, GC (grooming chain, focal grooming ID2 and being groomed by ID3)
# see others in "ACTION LOOKUP" table in Access
all_gm <- c("G", "MG", "BG", "BG,F", "HGC", "HCG")
gmd <- c("BG", "BG,F", "MG") #counting mutual groom towards both giving and receiving grooming indices, not counting twice aka separate instances in total gm
gm <- c("GC", "MG")


# how much MG? explore
grooming_raw %>%
  filter(Activity == "MG") %>%
  nrow() #358, 264
358/nrow(grooming_raw) #4.4% grooming is mutual

nrow(grooming_raw) #8817
nrow(undir_annual_dyads) #12018

# ----- All AB grooming counts, undirected ####

AB <- grooming_raw %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()
names(AB)
nrow(AB) # 1935
#no_sexAB <- AB %>% select(-starts_with("sex"))

total_gm_gmd1 <- AB %>% 
  #join obs where focal A gms with B and focal B gms with A (gms with = gms or is gmd by)
  left_join(., AB, by = c("ID1" = "ID2", "ID2" = "ID1", "year"), all = T) %>%
  rename(n_AB = n.x, n_BA = n.y) %>%
  mutate(n_AB = ifelse(is.na(n_AB), 0, n_AB), n_BA = ifelse(is.na(n_BA), 0, n_BA)) %>%
  mutate(total_AB_gm_gmd = n_AB + n_BA) %>%
  arrange(ID1, year, ID2) %>%
  full_join(undir_annual_dyads, ., by = c("ID1", "ID2", "year")) %>%
  #replace NA in gm time w 0, emerge after add undir dyads, meaning dyad was never seen to groom.
  replace(., is.na(.), 0)
#rename_if(starts_with("n_"), .funs = list(function(x) paste(x, "party", sep = "_")))

x <- names(total_gm_gmd1)[grepl("^n_", names(total_gm_gmd1))]
names(total_gm_gmd1)[grepl("^n_", names(total_gm_gmd1))] <- paste(x, "gmgmd", sep = "_")

nrow(total_gm_gmd1) #now 13003 added poss dyads at early stage and don't age filter
head(total_gm_gmd1)

# readd IDs in alphabetical order to then remove duplicate dyads
IDs <- total_gm_gmd1 %>%
  select(ID1, ID2) %>%
  apply(., 1, sort) %>% #alphabetize sort in each row
  t(.) %>%
  data.frame(., stringsAsFactors = F) %>%
  rename(ID1 = X1, ID2 = X2) # because undirected

total_gm_gmd <- total_gm_gmd1 %>%
  select(-ID1, -ID2) %>%
  cbind(IDs, .) %>%
  distinct(ID1, ID2, year, .keep_all = T) %>% #remove dup dyads 
  add_dyad_attr() %>% # add their attributes, sex, dobc...
  add_age() %>% # and ages
  filter_age()


nrow(total_gm_gmd) #2914, because have fixed IDs and ages are added, therefore more rows kept 1/30/20 -  with total possible dyads



str(total_gm_gmd)
total_gm_gmd %>%
  filter(apply(.,1, function(x) any(is.na(x))))

grooming_raw%>%
  filter(apply(.,1, function(x) any(is.na(x))))


View(total_gm_gmd)

x <- total_gm_gmd1 %>%
  select(-ID1, -ID2) %>%
  cbind(IDs, .) %>%
  distinct(ID1, ID2, year, .keep_all = T) %>% #remove dup dyads 
  add_dyad_attr() %>% # add their attributes, sex, dobc...
  add_age() 

x %>%
  filter_age() %>%
  filter(ID1 == "LK") %>%
  distinct(year)

x %>%
  filter(sex_ID1 == "M" & sex_ID2 == "M") %>%
  nrow()
total_gm_gmd %>%
  filter(sex_ID1 == "M" & sex_ID2 == "M") %>%
  nrow()

x %>%
  filter(sex_ID1 == "F" & sex_ID2 == "F") %>%
  nrow()
total_gm_gmd %>%
  filter(sex_ID1 == "F" & sex_ID2 == "F") %>%
  nrow()


# ----- A groom B counts, directed ####

#obs where A is grooming B
ABgm <- grooming_raw %>%
  filter(Activity %in% gm) %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()

# where B is being groomed by A
BAgmd <- grooming_raw %>%
  filter(Activity %in% gmd) %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()
names(BAgmd)


total_gm1 <- ABgm %>%
  #join obs where focal A gms B and focal B gmd by A.
  full_join(., BAgmd, by = c("ID1" = "ID2", "ID2" = "ID1", "year"))  %>%
  rename(n_ABgm = n.x, n_BAgmd = n.y) %>%
  mutate(n_ABgm = ifelse(is.na(n_ABgm), 0, n_ABgm), n_BAgmd = ifelse(is.na(n_BAgmd), 0, n_BAgmd)) %>%
  mutate(total_AB_gm = n_ABgm + n_BAgmd) %>%
  #here full join with possible annual dyads directed 
  full_join(., dir_annual_dyads, by = c( "ID1", "ID2", "year")) %>%
  #here replace NA in non-grooming dyads
  replace(., is.na(.), 0)

total_gm <- total_gm1 %>% 
  arrange(ID1, year, ID2) %>%
  add_dyad_attr() %>%
  add_age() %>%
  filter_age() 

nrow(total_gm) # 5826, 825 without possibl dyads, 1382 before age filter


# describe non grooming
# can see that 2344 of 4683 non-grooming dyads are same-sex
# most non grooming dyads are FF, next non grooming is F to M, then M to F, then MM
# that is, males are most unlikely to not groom each other.
a <- total_gm %>% 
  filter(total_AB_gm == 0)

nrow(a)  
a %>% filter(sex_ID1 == sex_ID2) %>% nrow()
a %>% filter(sex_ID1 == "M" & sex_ID2 == "M") %>% nrow() # 441 - bonds
a %>% filter(sex_ID1 == "M" & sex_ID2 == "F") %>% nrow() # 1208 - sex
a %>% filter(sex_ID1 == "F" & sex_ID2 == "M") %>% nrow() # 1269 - appeasement? protection?
a %>% filter(sex_ID1 == "F" & sex_ID2 == "F") %>% nrow() # 2024 - family?

total_gm1 %>%
  filter(apply(.,1, function(x) any(is.na(x)))) %>% nrow() #same stats as total_gm1

# ----- A groomed by B counts, directed ####

#obs where A is being groomed by B
ABgmd <- grooming_raw %>%
  filter(Activity %in% gmd) %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()

# where B is grooming A
BAgm <- grooming_raw %>%
  filter(Activity %in% gm) %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()


total_gmd1 <- ABgmd %>%
  #join obs where focal A gmd by B and focal B gms A.
  full_join(., BAgm, by = c("ID1" = "ID2", "ID2" = "ID1", "year"))  %>%
  rename(n_ABgmd = n.x, n_BAgm = n.y) %>%
  mutate(n_ABgmd = ifelse(is.na(n_ABgmd), 0, n_ABgmd), n_BAgm = ifelse(is.na(n_BAgm), 0, n_BAgm)) %>%
  mutate(total_AB_gmd = n_ABgmd + n_BAgm) %>%
  #full join with possible annual dyads directed 
  full_join(., dir_annual_dyads, by = c( "ID1", "ID2", "year")) %>% 
  #here replace NA in non-grooming dyads
  replace(., is.na(.), 0)

total_gmd <- total_gmd1 %>%
  arrange(ID1, year, ID2) %>%
  add_dyad_attr() %>% 
  add_age() %>% 
  filter_age()

head(total_gm)
nrow(total_gmd) #5826 w total possible dyads, 825

total_gmd1 %>%
  filter(apply(.,1, function(x) any(is.na(x)))) %>% nrow() #same stats as total_gm1

#save(total_gm_gmd, total_gm, total_gmd, file = "data/counts - annual dyadic grooming.Rdata")


## 5. Focal 5 meter (where to find 5 m data?) ####
# ----- All AB 5m prox counts ####
load("data/raw 5 m proximity.Rdata", verbose = T) #this is stephs, only goees up to 2016
load("data/annual possible focal dyads.Rdata", verbose = T)

names(attr)
names(focal_5m_raw)
nrow(focal_5m_raw) #177686
str(focal_5m_raw)

total_5m2 <- focal_5m_raw %>% 
  group_by(ID1, ID2, year) %>%
  tally() %>%
  ungroup() 


total_5m1 <- total_5m2 %>%
  full_join(., total_5m2, by = c("ID1" = "ID2", "ID2" = "ID1", "year")) %>%
  rename(n_AB = n.x, n_BA = n.y) %>%
  mutate(n_AB = ifelse(is.na(n_AB), 0, n_AB), n_BA = ifelse(is.na(n_BA), 0, n_BA)) %>%
  mutate(total_5m = n_AB + n_BA)
nrow(total_5m1) #14740, 4254

x <- names(total_5m1)[grepl("^n_", names(total_5m1))]
names(total_5m1)[grepl("^n_", names(total_5m1))] <- paste(x, "prox5", sep = "_")

#RE-ADD ids in alphabetial order
IDs <- total_5m1 %>%
  select(ID1, ID2) %>%
  apply(., 1, sort) %>% #alphabetize sort in each row
  t(.) %>%
  data.frame(., stringsAsFactors = F) %>%
  rename(ID1 = X1, ID2 = X2)
nrow(IDs) #14740, 4254

total_5m <- total_5m1 %>%
  select(-ID1, -ID2) %>%
  cbind(IDs, .) %>%
  full_join(undir_annual_dyads, ., by = c("ID1", "ID2", "year")) %>%
  #here replace NA in non-grooming dyads
  replace(., is.na(.), 0) %>%
  add_dyad_attr() %>%
  add_age() %>%
  filter_age() %>%
  distinct(ID1, ID2, year, .keep_all = T)


names(total_5m)
nrow(total_5m) #2936
head(total_5m)
tail(total_5m)

total_5m %>%
  filter(apply(.,1, function(x) any(is.na(x))))
total_5m %>%
  filter(total_5m == 0) %>% nrow() #521 dyad-years w 0 time in 5d

#save(total_5m, file = "data/counts - time in 5m.Rdata")


# graveyard #####
# SCAN ####

# figuring out how to format scan data into network data,
# i.d. each row represents a date, scan time, 

# Scan summary function
#1 select time frame
#2 create unique scans
#3 for each scan - pull all ids during one scan into two vectors
#4 expand.grid two vectors
#5 alphabetize sort in each row, and take unique combinations & apply unique
#6 tally to see how many times pair observed together in a scan

# summary function - times individuals observed in same party in given time period ####

summary_scan <- function(data, date_start, date_end) {
  
  #1 select time frame ####
  small_sc <- data %>%
    filter(date >= ymd(date_start) & date <= ymd(date_end)) %>%
    mutate(scan_id = as.character(scan_id))
  
  #calculate number of scans of given individual solo
  scans_solo <- small_sc %>%
    group_by(chimp_id) %>%
    tally()
  
  #nrow(small_sc) #909-1172 rows from just 2 days. geez.
  
  #2 create unique scans ####
  uscans <- unique(small_sc$scan_id)
  l_uscans <- length(uscans)
  
  d <- vector("list", length = l_uscans)
  
  #3 for each scan - pull all ids during one scan into two vectors ####
  start <- Sys.time()
  for(j in seq_along(uscans)){
    
    a <- small_sc %>%
      filter(scan_id == uscans[j])
    #scan 9961 in year 2002 - double focal samples from 2 diff observers
    
    chimps <- a$chimp_id
    if (length(chimps) > 1 & chimps[1] != chimps[2]){ # if there's more than one chimp in the scan, and it wasn't a double obs, expand grid
      b <- expand.grid(chimps, chimps) %>% #4 expand.grid two vectors
        filter(Var1 != Var2) %>% #remove pairs w self
        apply(., 1, sort) %>% #5 alphabetize sort in each row, and take unique combinations & apply unique
        t(.) %>%
        unique(.) %>%
        data.frame(.) %>%
        rename(ID_1 = X1, ID_2 = X2)
      
      c <- data.frame(uscans[j], b)
      
    } else {
      #if there's not more than one chimp in the scan
      #then just create a solo scan
      c<- data.frame(uscans[j], ID_1 = chimps[1], ID_2 = NA)  
    }
    
    d[[j]] <- c #store scan df
    
  }
  
  time_expand <- Sys.time() - start #understand how long expand grid takes
  
  start2 <- Sys.time() #understand how long bind takes
  e <- do.call("rbind", d)  # example, all of 2016 scan dyads are 168065 lines
  time_bind <- Sys.time() - start2 
  
  #4 tally to see how many times ID_1 and ID_2 seen in same party ----
  scans_dyad <- e %>% 
    group_by(ID_1, ID_2) %>%
    tally() #example, summary of 2016 is 1708 rows, only 13 are solo scans
  
  g <- list(unique_scans = l_uscans, expanding_time = time_expand, binding_time = time_bind,
            scans_solo = scans_solo, scans_dyad = scans_dyad)
  
  return(g)
}

# test summary function #####

years <- seq(2000, 2018, 1)
year_scans <- vector("list", length(years))


library(tcltk2)
total<-length(years)
pb<-tkProgressBar(title = "there yet?", min=0, max=total, width=300)

time1 <- Sys.time()
for( i in seq_along(years)){
  
  start <- paste(years[i], "-01-01", sep = "")
  end <- paste(years[i], "-12-31", sep = "")
  
  year_scans[[i]] <- summary_scan(scan, date_start = start, date_end = end)
  Sys.sleep(0.1) #loop sleeps briefly
  setTkProgressBar(pb,i, label=paste(round((i/total)*100,0),"% complete")) #update progress
  
}
Sys.time() - time1
close(pb)
proc.time()-pc


year_scans[[18]]


#save(year_scans, file ="annual scan summary data.Rdata")

#look at how zarin calculates time spent in same party in access
#could possibly filter at step 1 to only include relevant IDs

# double observer records ####
# are a ton

a <- read.csv(file = "d. FOCAL PARTY CORRECTED MET.txt", header = F)
foc_part <- a %>%
  rename(date = "V1", time = "V2", f_obs = "V3", focal = "V4", kpc_obs = "V5", partner = "V6") %>%
  unite("scan_id", date, time, sep = "_")
names(foc_part)

#1 look for duplicated scan ids and partners
#2 check if kpc obs different in first v second line

dup_partner <- foc_part %>%
  group_by(scan_id, partner) %>%
  mutate(dup_scan_partner = ifelse(n() > 1, "yes", "no")) %>%
  ungroup() %>%
  filter(dup_scan_partner == "yes") %>% 
  arrange(scan_id, partner)
View(dup_partner)

names(foc_part)
foc_part %>%
  group_by(scan_id, partner, .keep_all = T) %>%
  mutate(dup_partner_obs = ifelse(n_distinct(kpc_obs) > 1, "yes", "no")) %>%
  ungroup() %>%
  filter(dup_partner_obs == "yes") %>%
  arrange(scan_id)
View(diff_kpc_obs)

dup_kpc_obs <- foc_part %>%
  distinct(scan_id, partner, kpc_obs, .keep_all = T) %>%
  group_by(scan_id, partner) %>%
  mutate(dup_partner_obs = ifelse(n() > 1, "yes", "no")) %>%
  ungroup() %>%
  filter(dup_partner_obs == "yes") %>%
  arrange(scan_id)

View(dup_kpc_obs)


#scan id needs to include observer ID in da future
duplicate_scan_obs <- scan %>%
  group_by(scan_id, chimp_id) %>%
  mutate(duplicate = n()) %>%
  filter(duplicate >1) %>% 
  arrange(date, scan_id, chimp_id)
write.csv(duplicate_scan_obs, file = "duplicate observations in chimp observation table.csv")
View(duplicate_scan_obs)

scan %>%
  filter(scan_id =="1998-01-16.1899-12-30 06:15:00")

scan %>%
  filter(chimp_id %in% c("AM", "AR") & date == ymd("2005-04-29")) %>%
  arrange(scan_time)
#filter(scan_id == "2005-04-29.1899-12-30 06:30:00")

names(foc_party)


dup_foc<- foc_party[(foc_party %$% duplicated(date, scan_time, chimp_id)),] 

#pre summary? filter out any rows marked duplicate for chimp_id scan_id then run summary - again.


names(demo)
names(total_gm_gmd_index)
nrow(total_gm_gmd_index)

# original adding sex to gm index df's #####
gm_gmdi_sex <- total_gm_gmd_index %>%
  merge(., sex, by.x = "IDA", by.y = "chimp_id", all.x = T) %>%
  merge(., sex, by.x = "IDB", by.y = "chimp_id", all.x = T) %>%
  rename(sexA = sex.x, sexB = sex.y)
nrow(gm_gmdi_sex) #2968

gmi_sex <- total_gm_index %>%
  merge(., sex, by.x = "IDA", by.y = "chimp_id", all.x = T) %>%
  merge(., sex, by.x = "IDB", by.y = "chimp_id", all.x = T) %>%
  rename(sexA = sex.x, sexB = sex.y)
nrow(gmi_sex) #1382

gmdi_sex <- total_gmd_index %>%
  merge(., sex, by.x = "IDA", by.y = "chimp_id", all.x = T) %>%
  merge(., sex, by.x = "IDB", by.y = "chimp_id", all.x = T) %>%
  rename(sexA = sex.x, sexB = sex.y)
nrow(gmdi_sex)

nrow(total_gm_index)
nrow(total_gmd_index)

# ^^^^ remove this after adding sex at the level of original grooming data
# vvvv then can replace sex df's down there to regular total_gm_gmd_index etc
