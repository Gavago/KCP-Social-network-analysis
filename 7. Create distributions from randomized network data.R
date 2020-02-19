
load("data/sna dataframes - measures based on node randomized graphs.Rdata", verbose = T)


# CV model
# individual sna CVs over all years observed
# calculated 1000 times (once for each randomized data set)

extract_rcv <- function(x){
  rcvs <- x %>%
  group_by(chimp_id) %>%
  summarise(cv_bt = sd(bt)/mean(bt)*100, 
         cv_ec = sd(ec)/mean(ec)*100, 
         cv_deg = sd(deg)/mean(deg)*100,
         cv_trans = sd(trans)/mean(trans)*100) %>%
  ungroup() %>%
    select(starts_with("cv"))
  return(rcvs)
}


x <- lapply(list_ran_sna_measure_df, extract_rcv)
length(x)


ran_cv_dists <- do.call("rbind", x)
save(ran_cv_dists, file = "data/ randomized distributions of sna CVs.Rdata")

View(ran_cv_dists)
hist(ran_cv_dists$cv_bt)
hist(ran_cv_dists$cv_ec)
hist(ran_cv_dists$cv_deg)
