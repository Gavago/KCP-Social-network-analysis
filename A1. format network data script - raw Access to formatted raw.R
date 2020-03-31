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
source("functions/functions - data preparation.R")

# Open Database Connectivity (ODBC) is a protocol 
# that you can use to connect a Microsoft Access database to an external data source such as Microsoft SQL Server. 
# help: https://www.statmethods.net/input/dbinterface.html
# https://www.datacamp.com/community/tutorials/importing-data-r-part-two
# define a data source name DSN - check if access is running at 32 or 64 bit to choose correct odbc option in administrative tools
# https://support.office.com/en-us/article/administer-odbc-data-sources-b19f856b-5b9b-48c9-8b93-07484bfab5a7



# Rdata files output from this file are
# action lookup.Rdata
# raw aggression.Rdata
# attribute data alone.Rdata
# raw 5 m proximity.Rdata
# grooming raw and dyad attributes.Rdata
# dyadic focal party and total focal counts.Rdata
# annual possible focal dyads.Rdata
# annual dyadic grooming counts.Rdata
# count time in 5m.Rdata
# counts are to pass to indices in 2


## 1. Upload data from access database DSN ####
connection <- odbcConnect("Kanyawara social")

#demography data
demox <- sqlFetch(connection, "DEMOGRAPHY")
i <- sapply(demox, is.factor)
demox[i] <- lapply(demox[i], as.character)

#grooming records from focal data
groomingx <- sqlFetch(connection, "FOCAL GROOMING SCANS") %>%
  mutate(orig_year = year(ymd(Date)), month = month(ymd(Date)))
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


estrous_raw <- sqlFetch(connection, "ESTROUS_DATES")  %>%
  mutate(orig_year = year(date), chimp_id = as.character(chimp_id)) %>%
  rename(rep_status = `Reproductive Status`)

estrous_raw[is.na(estrous_raw$chimp_id), "chimp_id"] <- "NX"
estrous_raw[estrous_raw$orig_year == 2019, "orig_year"] <- 2009


#save(agg, agg_key, file = "data/raw aggression and agg key.Rdata")
#save(action, file = "data/action lookup.Rdata")
#save(demox, file = "data/raw demography.Rdata")
#save(estrous_raw, file = "data/raw estrous.Rdata")

# ----- Format attribute data #####
load("data/raw demography.Rdata")
names(demox)
#demox %>%
#  filter(chimp_name == "Ngamba") %>% View()
# Ngamba the female was first seen in 2004, last seen feb 7 2007, before focal obs started
# her code name "NA" needs correcting only when dealing with scan data from <= 2007

#df of those that immigrated since 2009, used to mark immigrants in attribute file
immigrants <- demox %>% 
  filter(kanyawara_member_flag == 1) %>%
  #dfs is long after birth, filters out babies that are first seen
  filter((date_of_birth_corrected + lubridate::days(31)) < date_first_seen) %>%
  filter(sex == "F") %>%
  mutate(year_first_seen = lubridate::year(date_first_seen)) %>%
  #only want females after 2009 (some previous are just those already present when obs started)
  filter(year_first_seen >= 2009) %>% 
  select(chimp_id, sex, date_first_seen, year_first_seen) %>%
  rename(immig_date = date_first_seen, immig_year = year_first_seen)
immigrants

attr <- demox %>%
  filter(kanyawara_member_flag == 1) %>%
  select(chimp_id, sex, date_of_birth_corrected, date_last_seen, date_first_seen, mother_id, father_id) %>%
  mutate(dobc = as.Date(date_of_birth_corrected), dls = as.Date(date_last_seen), dfs = as.Date(date_first_seen)) %>%
  mutate(year_last_seen = lubridate::year(dls)) %>%
  left_join(., immigrants, by = c("chimp_id", "sex")) %>%
  mutate_if(is.factor, as.character) %>%
  select(-date_of_birth_corrected, -date_last_seen)

#attr[attr$chimp_id %in% immigrants$chimp_id,]$immigration_date <- immigrants$date_first_seen
#attr[attr$chimp_id %in% immigrants$chimp_id,]$immigration_year <- immigrants$year_first_seen

#attr %<>% # 1.15.2020
# mutate(dobc = as.Date(dobc), date_last_seen = as.Date(date_last_seen), chimp_id = as.character(chimp_id))

attr[is.na(attr$chimp_id), "chimp_id"] <- "NX" #change ngamba to NX
attr[attr$chimp_id == "NPT", "chimp_id"] <- "NT"

#save(attr, file = "data/attribute data alone.Rdata")

# ----- Format estrous #####

load("data/raw estrous.Rdata", verbose = T)
prop_cyc <- estrous_raw %>%
  mutate(year = ifelse(orig_year == 2009, 2010, orig_year)) %>%
  group_by(year, chimp_id) %>%
  summarise(days_cycling = sum(rep_status == "CYCSW"), days_obs = n()) %>%
  mutate(days_cycling = ifelse(is.na(days_cycling), 0, days_cycling)) %>%
  mutate(prop_cyc = days_cycling/days_obs) %>%
  ungroup()
#save(prop_cyc, file = "data/female prop annual cycling.Rdata")

# ----- Format rank data #####
m_ranks <- read.csv("data/MaleRanks09thru17.csv", stringsAsFactors = F)
f_ranks <- read.csv("data/FemaleRanks09thru17.csv", stringsAsFactors = F)

str(m_ranks)

which(is.na(m_ranks$Date))

format(m_ranks$Date)

m_ranks %<>%
  mutate(Date = mdy(Date), orig_year = year(Date)) %>%
  mutate(year = ifelse(orig_year == 2009, 2010, orig_year)) %>%
  group_by(year, ID) %>%
  summarise(avg_rank = mean(Rank.Proportion)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank_class = as.numeric(quantcut(avg_rank, 3))) %>%
  ungroup() %>%
  mutate(rank_class = as.factor(case_when(
    rank_class == 1 ~ "lo",
    rank_class == 2 ~ "med",
    rank_class == 3 ~ "hi"
  )))


f_ranks %<>%
  mutate(Date = mdy(Date), orig_year = year(Date)) %>%
  mutate(year = ifelse(orig_year == 2009, 2010, orig_year)) %>%
  group_by(year, ID) %>%
  summarise(avg_rank = mean(Rank.Proportion)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(rank_class = as.numeric(quantcut(avg_rank, 3))) %>%
  ungroup() %>%
  mutate(rank_class = as.factor(case_when(
    rank_class == 1 ~ "lo",
    rank_class == 2 ~ "med",
    rank_class == 3 ~ "hi"
  )))


ann_ranks <- rbind(m_ranks, f_ranks)


#save(ann_ranks, file = "data/annual average standardized ranks.Rdata")


# ----- Format prox in 5 #####
names(focal_5m1)
names(attr)

focal_5m1 %>%
  filter(Exclude == T)

focal_5m_raw <- focal_5m1 %>%
  rename(ID1 = Focal, ID2 = w5) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(orig_year = year(ymd(Date)), month = month(ymd(Date))) %>%
  mutate(year = case_when(
    orig_year == 2009 ~ 2010,
    TRUE ~ orig_year
  )) %>%  
  filter(ID1 != ID2) %>% # removes 50 cases, removes 59 cases
  fix_ID_errors()

focal_5m_raw$ID1[grepl(" ", focal_5m_raw$ID1)] #check, each should be 0
focal_5m_raw$ID2[grepl(" ", focal_5m_raw$ID2)]

View(focal_5m_raw)

nrow(focal_5m_raw) # 177686

#save(focal_5m_raw, file = "data/raw 5 m proximity.Rdata")

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
  filter(ID1 != ID2) %>%
  mutate(year = case_when(
    orig_year == 2009 ~ 2010,
    TRUE ~ orig_year
  ))
# to check for appropriate codes in file, can left join w attributes and see what rows attributes are NA...

#save(grooming_raw, file = "data/grooming raw.Rdata")

# ----- Format and agg and action look up ------
load("data/raw aggression.Rdata", verbose = T)
load("data/action lookup.Rdata", verbose = T)

## 2. Focal party data and possible dyadsfrom MET (starts 2009) ####

foc_part1 <- read.csv(file = "data/d. FOCAL PARTY CORRECTED MET.txt", header = F, stringsAsFactors = F) %>%
  rename(date = "V1", time = "V2", f_obs = "V3", focal = "V4", kpc_obs = "V5", partner = "V6") %>%
  unite("scan_id", date, time, sep = "_") %>%
  separate(scan_id, into = c("date", "trash", "scan_time"), sep = " ", remove = F) %>%
  select(-trash) %>%
  mutate(orig_year = year(mdy(date)), month = month(mdy(date))) %>%
  mutate(year = case_when(
    orig_year == 2009 ~ 2010,
    TRUE ~ orig_year
  ))

names(foc_part1)

foc_part1$focal[foc_part1$focal == "NPT"] <- "NT"
foc_part1$partner[foc_part1$partner == "NPT"] <- "NT"

foc_part1[grepl(" ", foc_part1$focal), "focal"]
foc_part1[grepl(" ", foc_part1$partner), "partner"]

foc_part <- fix_ID_errors(foc_part1, ID1 = "focal", ID2 = "partner")

foc_part

# test fix id errors worked
# foc_part[grepl(" ", foc_part$ID1), "ID1"]
# foc_part[grepl(" ", foc_part$ID2), "ID2"]


# ----- create total AB party -----

#this is a file where you have all focal individuals and who was in their party for every scan taken
# i.e. count of focal A in which B was in party, for time period X
#this is what you use to control for 
#time AB grooming, time AB in 5 meters

head(foc_part)
nrow(foc_part) #1760408

#counts where focal was in same party as partner for a given year
dyad_party <- foc_part %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()

nrow(dyad_party) #11496 (2009-2010 collapsed), 12076
head(dyad_party)

total_AB_party <- dyad_party %>%
  full_join(., dyad_party, by = c("ID1" = "ID2", "ID2" = "ID1", "year")) %>%
  rename(n_AB = n.x, n_BA = n.y) %>%
  mutate(n_AB = ifelse(is.na(n_AB), 0, n_AB), n_BA = ifelse(is.na(n_BA), 0, n_BA)) %>%
  mutate(total_AB_party = n_AB + n_BA) %>%
  arrange(ID1, year, ID2)

x <- names(total_AB_party)[grepl("^n_", names(total_AB_party))]
names(total_AB_party)[grepl("^n_", names(total_AB_party))] <- paste(x, "party", sep = "_")


nrow(total_AB_party) #16640 2009-10 collapsed, 17568 after removing ID typos
head(total_AB_party)

total_AB_party %>%
  count(year)

#total_AB_party %>%
#  filter(apply(.,1, function(x) any(is.na(x))))

#save(total_AB_party, file = "data/counts - dyadic focal party.Rdata")

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

nrow(dir_annual_dyads) # 22414 2009-10 collapsed; using "partner" column is 24036, when using "Focal" column is 7712

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

nrow(undir_annual_dyads) #11207 2009-10 collapsed, using "partner" column is 12018, when using "Focal" column is 3856

#save(dir_annual_dyads, undir_annual_dyads, file = "data/annual possible focal dyads.Rdata")
#save(foc_part, file = "data/focal party scans formatted.Rdata")

# ----- create total focal obs w possible focal years included ----

#total focal obs in given year
poss_focal <- foc_part %>%
  distinct(ID2, year) %>% #use ID2 bc is more comprehensive than ID1 (id1 was focals, 2 was partners)
  rename(ID1 = ID2)
# is correct to assume that if individual was in party that year 

total_focal <- foc_part %>%
  distinct(year, ID1, scan_id) %>%
  group_by(year, ID1) %>%
  tally() %>%
  ungroup() %>%
  add_individ_attr(., ID1 = "ID1") %>%
  add_age(dyad = FALSE) %>%
  filter_age(dyad = FALSE) %>%
  select(year, ID1, n)


total_poss_focal <- total_focal %>%
  full_join(., poss_focal, by = c("year","ID1")) %>%
  add_individ_attr(., ID1 = "ID1") %>%
  add_age(dyad = FALSE) %>%
  filter_age(dyad = FALSE) %>%
  mark_short_time_pres_individ() %>%
  select(year, ID1, n, sex, dls, weeks_pres, short_presence)


nrow(total_focal) #209 2009-10 collapsed, 224
nrow(total_poss_focal) #215 2009-10 collapsed; 234 aha, not so different

total_poss_focal %>%
  filter(is.na(n)) # that, friend, is fucking beautiful.

#interesting to see that sufficient weeks present is not always indicative of sufficient sampling time.
# all n = NA means that potential focals were never focaled, most individs seem to be newly immigrant or disappearing

total_poss_focal %<>%
  mutate(n = replace_na(n, 0))

#save(total_focal, total_poss_focal, file = "data/total focal and possible focal per year.Rdata")


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

#Failed case when with NA date
#mutate(immigration_date = case_when(
#       chimp_id %in% immigrants$chimp_id ~ immigrants$date_first_seen),
#       TRUE ~ as.Date(NA)) %>% 
#mutate(immigration_date = if_else(chimp_id %in% immigrants$chimp_id, immigrants$date_first_seen, as.Date(NA)),
#immigration_year = if_else(chimp_id %in% immigrants$chimp_id, immigrants$year_first_seen, as.Date(NA))) %>%