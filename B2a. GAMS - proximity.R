library(mgcv)
library(fitdistrplus)
source("data/data sets for gams.R") #runs code

# A whole new gam world
# with and without attributes (rank and prop cycling)

# H1 --------
#   mixed sex - strength (undir w) ------

sp <- gam(deg ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"),  data = mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
msp <- gam(deg ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
fsp <- gam(deg ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"),  data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

spr <- gam(deg ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = Gamma(link = "log"), method = "REML")
mspr <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = Gamma(link = "log"), method = "REML")
fspr <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

summary(sp) 
summary(spr)
summary(mspr)
summary(fspr)

#   same sex - strength  -----
f_sp <- gam(deg ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_spr <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_sp <- gam(deg ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_spr <- gam(deg ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

plot(m_sp, pages = 1)
plot(m_spr, pages = 1)


# H2 ------
#   mixed sex - bt and trans (undir w) ------

# betweenness diagnostics look pretty awful regardless of the assigned error dist
#mixed_sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% hist()
#mixed_sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% descdist()

btp <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
mbtp <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
fbtp <- gam(bt ~ s(age_mid_year, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

btpr <- gam(bt ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
mbtpr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
fbtpr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

#trans proximity is not easily modeled - majority values == 1
#mixed_sna_w %>% filter(behavior == "prox") %>% pull(trans) %>% hist()
#mixed_sna_w %>% filter(behavior == "prox") %>% pull(trans) %>% descdist()
trp <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = Gamma(link = "log"), method = "REML")
mtrp <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ftrp <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

trpr <- gam(trans ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
mtrpr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
ftrpr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

summary(btp)
summary(btpr)
summary(mbtpr)
summary(fbtpr)

summary(trpx)
summary(trpr)
summary(mtrpr) 
summary(ftrpr)


#   same sex - bt and trans (undir w) ------
# even when prox is weighted, proximity networks with either mixed or same sex partners
# do not contain enough variation to model measures like betweenness and transitivity

m_same_sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% hist()
m_same_sna_w %>% filter(behavior == "prox") %>% pull(bt) %>% descdist()
gam.check(m_btp)

m_btp <- gam(bt ~ s(age_mid_year, k = 10)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_btpr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

f_btp <- gam(bt ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_btpr <- gam(bt ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

# again, trans is horrible
m_trp <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_trpr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

f_trp <- gam(trans ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_trpr <- gam(trans ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

plot(m_btp, pages = 1)
plot(m_btpr, pages = 1)

# H3 -----
#   mixed sex - eigenvector (undir w) ------

### prox
ecp <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
mecp <- gam(ec ~ s(age_mid_year,k = 5) + s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
fecp <- gam(ec ~ s(age_mid_year,k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

ecpr <- gam(ec ~ sex + s(age_mid_year, by = sex, k = 5) + s(avg_rank, by = sex, k = 5)  + s(chimp_id, bs = "re"), data = mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
mecpr <- gam(ec ~ s(age_mid_year,k = 5) + s(avg_rank, k = 5)+ s(chimp_id, bs = "re"), data = m_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
fecpr <- gam(ec ~ s(age_mid_year,k = 5) + s(avg_rank, k = 5) + s(chimp_id, bs = "re"), data = f_mixed_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

summary(ecp)
summary(ecpr)
summary(mecpr)
summary(fecpr)

plot(ecp, pages = 1)
plot(ecpr, pages = 1)

#   same sex - ec (undir w) ------
m_ecp <- gam(ec ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
m_ecpr <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = m_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

f_ecp <- gam(ec ~ s(age_mid_year, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")
f_ecpr <- gam(ec ~ s(age_mid_year, k = 5) + s(avg_rank, k = 5)  + s(chimp_id, bs = "re"), data = f_same_sna_w %>% filter(behavior == "prox"), family = gaussian(link = "log"), method = "REML")

summary(m_ecp) # age still sig w rank
summary(m_ecpr)

summary(f_ecp) # age no longer sig w rank
summary(f_ecpr)

plot(m_ecp, pages = 1)
plot(m_ecpr, pages = 1)


# Save models -----------

# save(sp, spr, btp, btpr, trp, trpr, ecp, ecpr, file = "data/models gam - mixed sex prox with and without rank.Rdata")
# 
# save(fsp, fspr, fbtp, fbtpr, ftrp, ftrpr, fecp, fecpr,
#     msp, mspr, msp, mspr, mbtp, mbtpr, mtrp, mtrpr, mecp, mecpr,
#     file = "data/models gam - mixed sex prox with and without rank - sex specific for viz.Rdata")
# 
# save(f_sp, f_spr,
#      m_sp, m_spr,
#      f_btp, f_btpr, f_trp, f_trpr,
#      m_btp, m_btpr, m_trp, m_trpr,
#      f_ecp, f_ecpr,
#      m_ecp, m_ecpr, file = "data/models gam - same sex prox with and without rank.Rdata")
# 

# Checking models and concurvity ----
concurvity(dpr, full = TRUE)
concurvity(btpr, full = TRUE)
concurvity(trpr, full = TRUE) # hi numbers for male age
concurvity(trpr, full = FALSE) #output shows the degree to which each variable is predetermined by each other variable, rather than all the other variables. This can be used to pinpoint which variables have a close relationship.
# rank predicts effect of male age...

concurvity(ecpr, full = TRUE)
concurvity(m_ecpr, full = TRUE)
concurvity(f_ecpr, full = TRUE)

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


