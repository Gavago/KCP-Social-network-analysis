# Deep dive into some of these sna measure dists

source("B7a. load data sets for gams.R")

load("data/indices - annual dyadic grooming.Rdata", verbose = T)
load("data/indices - annual dyadic 5m proximity.Rdata", verbose = T)

# Grooming networks
# find who is the max in each of these and then look at their raw gm count data

# directed
dw <- mixed_dir_sna_w
duw <- mixed_dir_sna_uw

# degree in and out
dw$deg_in %>% hist()
dw$deg_in^(1/2) %>% hist()
dw$deg_in^(1/3) %>% hist()
dw$deg_in %>% log() %>% hist()

dw$deg_out %>% hist()
dw$deg_out^(1/2) %>% hist()
dw$deg_out^(1/3) %>% hist()
dw$deg_out %>% log() %>% hist()


# strength in and out
duw$deg_in %>% hist()
duw$deg_in^(1/2) %>% hist()
duw$deg_in^(1/3) %>% hist()
duw$deg_in %>% log() %>% hist()

duw$deg_out %>% hist()
duw$deg_out^(1/2) %>% hist()
duw$deg_out^(1/3) %>% hist()
duw$deg_out %>% log() %>% hist()


# mixed undir
d <- mixed_sna_w %>% filter(behavior == "total_grooming")
d <- mixed_sna_w %>% filter(behavior == "prox")

#bt
d$bt %>% hist()
d$bt^(1/3) %>% hist()
d$bt %>% log() %>% hist()

#trans
d$trans %>% hist()
d$trans^(1/2) %>% hist()
d$trans^(1/3) %>% hist()
d$trans %>% log() %>% hist()

#ec
d$ec %>% hist()
d$ec^(1/2) %>% hist()
d$ec^(1/3) %>% hist()
d$ec %>% log() %>% hist()

