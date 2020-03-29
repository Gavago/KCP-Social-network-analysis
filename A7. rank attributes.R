library(tidyverse)
library(lubridate)
select <- dplyr::select
year <- lubridate::year

m_ranks <- read.csv("data/MaleRanks09thru17.csv", stringsAsFactors = F)
f_ranks <- read.csv("data/FemaleRanks09thru17.csv", stringsAsFactors = F)

str(m_ranks)

which(is.na(m_ranks$Date))

format(m_ranks$Date)

m_ranks %<>%
  mutate(Date = mdy(Date), year = year(Date)) %>% #as.Date(Date, format = "%m/%d/%y"))
  group_by(year, ID) %>%
  summarise(avg_rank = mean(Rank.Proportion)) %>%
  ungroup()

f_ranks %<>%
  mutate(Date = mdy(Date), year = year(Date)) %>%
  group_by(year, ID) %>%
  summarise(avg_rank = mean(Rank.Proportion)) %>%
  ungroup()

ann_ranks <- rbind(m_ranks, f_ranks)


#save(ann_ranks, file = "data/annual average standardized ranks.Rdata")
