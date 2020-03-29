library(tidyverse)
library(lubridate)
library(gtools)
select <- dplyr::select
year <- lubridate::year

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
