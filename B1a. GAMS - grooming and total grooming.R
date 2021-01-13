library(mgcv)
source("data/data sets for gams.R")

# A whole new gam world
# with and without attributes (rank)

# Rank by age ------
rank_age <- gam(avg_rank ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = mixed_dir_sna_uw, family = gaussian(), method = "REML")
gam.check(rank_age)
summary(rank_age)

# H1 --------
#   mixed sex - deg in and out (dir uw) ------

dig <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
mdig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = m_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
fdig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

digr <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
mdigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
fdigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

dog <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
mdog <- gam(deg_out ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
fdog <- gam(deg_out ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

dogr <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
mdogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
fdogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_uw, family = gaussian(link = "log"), method = "REML")


summary(dig) # in glmm, one problem in coef estimation for male gm deg in is that SE is so large, here looks like F stat is good size
summary(digr)
summary(mdigr)
summary(fdigr)

summary(dig_f_cyc) 

summary(dog)
summary(dogr)
summary(mdogr)
summary(fdogr)

plot(mdog, pages = 1)
plot(mdogr, pages = 1)

gam.check(dig)

#   mixed sex - strength in and out (dir w) -----

sig <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"), data = mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
msig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
fsig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")

sigr <- gam(deg_in ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
msigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
fsigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")

sog <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
msog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
fsog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")

sogr <- gam(deg_out ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
msogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = m_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")
fsogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_dir_sna_w, family = gaussian(link = "log"), method = "REML")


summary(sig)
summary(sigr)
summary(msigr)
summary(fsigr)


summary(sog)

summary(sogr)
summary(msogr)
summary(fsogr)

plot.gam(msog, pages = 1)
plot.gam(msogr, pages = 1)

vis.gam(dig_f_cyc, vis = c("age_mid_year", "prop_cyc"), plot.type = "persp")

#   same sex - strength and degree in and out  -----

f_dig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
f_digr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
m_dig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
m_digr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

f_dog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
f_dogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
m_dog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")
m_dogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_dir_sna_uw, family = gaussian(link = "log"), method = "REML")

f_sig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
f_sigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
m_sig <- gam(deg_in ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
m_sigr <- gam(deg_in ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")

f_sog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_same_dir_sna_w, family = Gamma(link = "log"), method = "REML") # for an example of output when fitting is terminated w step failure, change error family to gaussian on this and f_sogr models... results in an F of 0.
f_sogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_dir_sna_w, family = Gamma(link = "log"), method = "REML")
m_sog <- gam(deg_out ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")
m_sogr <- gam(deg_out ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_dir_sna_w, family = gaussian(link = "log"), method = "REML")

plot(m_dig, pages = 1)
plot(m_digr, pages = 1)
plot(f_sog, pages = 1)
plot(f_sogr, pages = 1)

# H2 ------
#   mixed sex - bt and trans (undir w) ------

btg <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
mbtg <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
fbtg <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

btgr <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
mbtgr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
fbtgr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

trg <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
mtrg <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ftrg <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

trgr <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
mtrgr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
ftrgr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

summary(btg)
summary(btgr)
summary(mbtgr)
summary(fbtgr)

summary(trg)
summary(trgr)
summary(mtrgr) # another reason why gams might be difficult for sig testing, males alone F stat of age is huge, small in age sex model - but is all relative to permutaton where those are also larger vs smaller
summary(ftrgr)

plot.gam(ftrg, pages = 1)
plot.gam(ftrgr, pages = 1)

#   same sex - bt and trans (undir w) ------
m_btg <- gam(bt ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
m_btgr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

f_btg <- gam(bt ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
f_btgr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

m_trg <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
m_trgr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

f_trg <- gam(trans ~ s(age_mid_year, k = 3)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
f_trgr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

summary(f_trg)
gam.check(f_trg)
m_same_sna_w %>% filter(behavior == "total_grooming") %>% pull(trans) %>% hist()

# H3 -----
#   mixed sex - eigenvector (undir w) ------

### grooming
ecg <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
mecg <- gam(ec ~ s(age_mid_year,k = 5) + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
fecg <- gam(ec ~ s(age_mid_year,k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

ecgr <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
mecgr <- gam(ec ~ s(age_mid_year,k = 5) + s(avg_rank, k = 5)+ s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
fecgr <- gam(ec ~ s(age_mid_year,k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

summary(ecg)
summary(ecgr)
summary(mecgr)
summary(fecgr)

plot.gam(mecg, pages = 1)
plot.gam(mecgr, pages = 1)

#   same sex - ec (undir w) ------
m_ecg <- gam(ec ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
m_ecgr <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

f_ecg <- gam(ec ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")
f_ecgr <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "total_grooming"), family = gaussian(link = "log"), method = "REML")

summary(m_ecg) # age still sig w rank
summary(m_ecgr)

summary(f_ecg) # age no longer sig w rank
summary(f_ecgr)

plot.gam(f_ecg, pages = 1)
plot.gam(f_ecgr, pages = 1)

# Save models -----------

# save(dig, digr, dog, dogr, sig, sigr, sog, sogr, btg, btgr, trg, trgr, ecg, ecgr, file = "data/models gam - mixed sex grooming and total grooming with and without rank.Rdata")

#save(fdig, fdigr, fdog, fdogr, fsig, fsigr, fsog, fsogr, fbtg, fbtgr, ftrg, ftrgr, fecg, fecgr, 
#     mdig, mdigr, mdog, mdogr, msig, msigr, msog, msogr, mbtg, mbtgr, mtrg, mtrgr, mecg, mecgr, 
#     file = "data/models gam - mixed sex grooming and total grooming with and without rank - sex specific for viz.Rdata")

# save(f_dig, f_digr, f_dog, f_dogr,
#      m_dig, m_digr, m_dog, m_dogr,
#      f_sig, f_sigr, f_sog, f_sogr,
#      m_sig, m_sigr, m_sog, m_sogr,
#      f_btg, f_btgr, f_trg, f_trgr,
#      m_btg, m_btgr, m_trg, m_trgr,
#      f_ecg, f_ecgr,
#      m_ecg, m_ecgr, file = "data/models gam - same sex grooming and total grooming with and without rank.Rdata")

# Checking models and concurvity ----
concurvity(digr, full = TRUE)
  concurvity(digr, full = FALSE)
concurvity(dogr, full = TRUE) 
  concurvity(dogr, full = FALSE)
concurvity(sigr, full = TRUE)
  concurvity(sigr, full = FALSE)
concurvity(sogr, full = TRUE) 

concurvity(btgr, full = TRUE)
concurvity(trgr, full = TRUE) # hi numbers for male age
concurvity(trgr, full = FALSE) #output shows the degree to which each variable is predetermined by each other variable, rather than all the other variables. This can be used to pinpoint which variables have a close relationship.
# rank predicts effect of male age...

concurvity(ecgr, full = TRUE)
concurvity(m_ecgr, full = TRUE)
concurvity(f_ecgr, full = TRUE)

# Checking residuals -----
plot_gam_check(dig)
gam.check(digr,pch=19,cex=.3)
gam.check(dog) # v nice
gam.check(dogr)

gam.check(sig)
gam.check(sigr)
gam.check(sog)
gam.check(sogr)

gam.check(btg) # not good
gam.check(btgr)
gam.check(trg)
gam.check(trgr)

gam.check(ecg)
gam.check(ecgr)

gam.check(m_ecg)
gam.check(m_ecgr)
gam.check(f_ecg)
gam.check(f_ecgr)

#already have 14, will then add 28 more for each same sex network? That's 42.
# Then add 8 more for prox, and 16 for same sex networks ? 66 models >:-(
# really - reporting


# gyard -----

g <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank), data = data, family = Gamma(link = "log"), method = "REML")
g <- getViz(g)
plot(g, pages = 1, all.terms = TRUE, shade = TRUE, seWithMean = TRUE, shift = coef(g)[1]) #residuals = TRUE, cex = 1, pch = 1
AIC(g)
coef(g)
summary(g)
plot(g)

data <- sna_w %>%
  mutate(sex = factor(sex))
gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank), data = data, method = "REML")


