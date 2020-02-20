

test <- data.frame(start_date = as.Date(c("2010-01-21", "2010-05-20", "2016-04-13")), 
                   end_date = as.Date(c("2010-02-14", "2010-06-02", "2016-04-18")), 
                   chimp_id = c("TG", "MX", "AL"))


test %>%
  rowwise() %>%
  mutate(date_seq = list(seq(start_date, end_date, by = "1 day"))) %>%
  select(-ends_with("date")) %>%
  unnest(cols = c(date_seq))
         

