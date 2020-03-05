# create count data from formatted raw
library(tidyverse)
library(magrittr)
source("functions/functions - data preparation.R")



## 1. Create dyadic annual grooming counts (starts 2009)  ####
# ----- load grooming data #####

#used grooming table made in Access "FOCAL GROOMING SCANS"
load("data/grooming raw.Rdata", verbose = T)
load("data/annual possible focal dyads.Rdata", verbose = T)

# focal codes for grooming between adults: G (grooming), MG (mutual grooming), BG (being groomed), "BG,F", HGC, HCG, GC (grooming chain, focal grooming ID2 and being groomed by ID3)
# see others in "ACTION LOOKUP" table in Access
all_gm <- c("G", "MG", "BG", "BG,F", "HGC", "HCG")
gmd <- c("BG", "BG,F", "MG") #counting mutual groom towards both giving and receiving grooming indices, not counting twice aka separate instances in total gm
gm <- c("GC", "MG")

# how much mutual grooming? explore
#grooming_raw %>%
#   filter(Activity == "MG") %>%
#   nrow() #358, 264
# 358/nrow(grooming_raw) #4.4% grooming is mutual - am counting as regular gm interaction in undirected

nrow(grooming_raw) #8117
nrow(undir_annual_dyads) #11207, 12018

# ----- All AB grooming counts, undirected ####

AB <- grooming_raw %>%
  group_by(year, ID1, ID2) %>%
  tally() %>%
  ungroup()
names(AB)
nrow(AB) #1863, 1935
#no_sexAB <- AB %>% select(-starts_with("sex"))

total_gm_gmd1 <- AB %>% 
  #join obs where focal A gms with B and focal B gms with A (gms with = gms or is gmd by)
  full_join(., AB, by = c("ID1" = "ID2", "ID2" = "ID1", "year"), all = T) %>%
  rename(n_AB = n.x, n_BA = n.y) %>%
  mutate(n_AB = ifelse(is.na(n_AB), 0, n_AB), n_BA = ifelse(is.na(n_BA), 0, n_BA)) %>%
  mutate(total_AB_gm_gmd = n_AB + n_BA) %>%
  arrange(ID1, year, ID2) %>%
  full_join(undir_annual_dyads, ., by = c("ID1", "ID2", "year")) %>%
  #replace NA in gm time w 0, emerge after add undir dyads, meaning dyad was never seen to groom.
  replace(., is.na(.), 0)

#add behavior to N count col name
x <- names(total_gm_gmd1)[grepl("^n_", names(total_gm_gmd1))]
names(total_gm_gmd1)[grepl("^n_", names(total_gm_gmd1))] <- paste(x, "gmgmd", sep = "_")

nrow(total_gm_gmd1) #12632 09_10 merge, 12780, 13003 added poss dyads at early stage and don't age filter
head(total_gm_gmd1)

# readd IDs in alphabetical order to then remove duplicate dyads
IDs <- total_gm_gmd1 %>%
  select(ID1, ID2) %>%
  apply(., 1, sort) %>% #alphabetize sort in each row
  t(.) %>%
  data.frame(., stringsAsFactors = F) %>%
  rename(ID1 = X1, ID2 = X2) # because undirected

total_gm_gmdx <- total_gm_gmd1 %>%
  select(-ID1, -ID2) %>%
  cbind(IDs, .) %>%
  distinct(ID1, ID2, year, .keep_all = T) %>% #remove dup dyads, revealed after IDs were alphabetized 
  add_dyad_attr() %>%   # add their attributes, sex, dobc...
  add_age() %>% # and ages
  filter_age() %>%
  clean_ghosts()
total_gm_gmd <- total_gm_gmdx %>% #sex specific age filter 
  mark_short_time_pres(filter_n_clean = TRUE) #mark whether individual present in year for < 26 wks, filter & clean vars or not

nrow(total_gm_gmd) #2412, 3163, 2585 filter short obs clean ghosts; 2914, because have fixed IDs and ages are added, therefore more rows kept 1/30/20 -  with total possible dyads

total_gm_gmd %>%
  filter(apply(.,1, function(x) any(is.na(x))))

grooming_raw%>%
  filter(apply(.,1, function(x) any(is.na(x))))


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
  filter_age() %>% 
  mark_short_time_pres(filter_n_clean = TRUE) %>%
  clean_ghosts()

nrow(total_gm) #4824 09_10 merged,  5168 short obs and ghosts removed; 5826, 825 without possibl dyads, 1382 before age filter

# describe non grooming
# can see that 2344 of 4683 non-grooming dyads are same-sex
# most non grooming dyads are FF, next non grooming is F to M, then M to F, then MM
# that is, males are most unlikely to not groom each other.
a <- total_gm %>% 
  filter(total_AB_gm == 0)

nrow(a)  
a %>% filter(sex_ID1 == sex_ID2) %>% nrow() #2000
a %>% filter(sex_ID1 == "M" & sex_ID2 == "M") %>% nrow() # 342 - bonds
a %>% filter(sex_ID1 == "M" & sex_ID2 == "F") %>% nrow() # 975 - sex
a %>% filter(sex_ID1 == "F" & sex_ID2 == "M") %>% nrow() # 1037 - appeasement? protection?
a %>% filter(sex_ID1 == "F" & sex_ID2 == "F") %>% nrow() # 1658 - family?

total_gm %>%
  filter(apply(.,1, function(x) any(is.na(x)))) %>% nrow()

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
  filter_age() %>%
  mark_short_time_pres(filter_n_clean = TRUE) %>%
  clean_ghosts()

names(total_gmd)
nrow(total_gmd) #4824; 6331, 5168 w short obs removed, 5826 w total possible dyads, 825

total_gmd %>%
  filter(apply(.,1, function(x) any(is.na(x)))) %>% nrow() #same stats as total_gm1

#save(total_gm_gmd, total_gm, total_gmd, file = "data/counts - annual dyadic grooming.Rdata")

## 2. Focal 5 meter (where to find 5 m data?) ####
# ----- All AB 5m prox counts ####
load("data/raw 5 m proximity.Rdata", verbose = T) #this is stephs, only goees up to 2016
load("data/annual possible focal dyads.Rdata", verbose = T)


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
nrow(total_5m1) #14058, 14740, 4254

#add behavior to N count col name
x <- names(total_5m1)[grepl("^n_", names(total_5m1))]
names(total_5m1)[grepl("^n_", names(total_5m1))] <- paste(x, "prox5", sep = "_")

#RE-ADD ids in alphabetial order
IDs <- total_5m1 %>%
  select(ID1, ID2) %>%
  apply(., 1, sort) %>% #alphabetize sort in each row
  t(.) %>%
  data.frame(., stringsAsFactors = F) %>%
  rename(ID1 = X1, ID2 = X2)
nrow(IDs) #14058, 14740, 4254

total_5mx <- total_5m1 %>%
  select(-ID1, -ID2) %>%
  cbind(IDs, .) %>%
  distinct(ID1, ID2, year, .keep_all = T) %>% #remove dup dyads, revealed after IDs were alphabetized
  full_join(undir_annual_dyads, ., by = c("ID1", "ID2", "year")) %>%
  #here replace NA in non-grooming dyads
  replace(., is.na(.), 0) %>%
  add_dyad_attr() %>%
  add_age() %>%
  filter_age() %>%
  clean_ghosts()

total_5m <- total_5mx %>%
  mark_short_time_pres(filter_n_clean = TRUE) 

names(total_5m)
nrow(total_5m) #2412 09-10 merged ; 2597 after short pres removed and ghosts; 2936
head(total_5m)
tail(total_5m)

total_5m %>%
  filter(apply(.,1, function(x) any(is.na(x))))
total_5m %>%
  filter(total_5m == 0) %>% nrow() #233, 521 dyad-years w 0 time in 5d

#save(total_5m, file = "data/counts - time in 5m.Rdata")

# 5. view those removed for low time pres & check for ghosts (mis-ID's) -----
#save(total_5mx, total_gm_gmdx, file = "data/counts - gm and prox counts before removing short pres individs.Rdata")
load("data/counts - gm and prox counts before removing short pres individs.Rdata", verbose = T)

total_gm_gmdx %>%
  mark_short_time_pres() %>%
  filter(short_presence_ID1 == 1) %>%
  distinct(ID1, year, dls_ID1, immig_date_ID1, weeks_pres_ID1)

total_gm_gmdx %>%
  mark_short_time_pres() %>% 
  filter(weeks_pres_ID1 < 52) %>%
  distinct(ID1, year, dls_ID1, immig_date_ID1, weeks_pres_ID1, short_presence_ID1) %>%
  arrange(year, short_presence_ID1)

