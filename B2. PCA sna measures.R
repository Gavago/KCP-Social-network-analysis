library(tidyverse)
library(magrittr)
library(fitdistrplus)
library(psych) #for KMO
library(paran) #parallel analysis
library(devtools)
source("~/Dropbox/2. R projects/ggbiplot function.R")
source("~/Dropbox/2. R projects/ggscreeplot function.R")

load("data/list column dyadic data prox & gm by year & dyad-sex year.Rdata", verbose = T)
load("data/sna dataframe - weighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/sna dataframe - unweighted measures, individual sna measure for each year, network sex, & behavior.Rdata", verbose = TRUE)
load("data/attribute data alone.Rdata", verbose = T)
select <- dplyr::select

sna_df <- all_sna_measure_df_w
#sna_df <- all_sna_measure_df_uw



# PCA -- how do measures load together or apart? ------
# --- bartlett's and KMO -----
bart<-function(dat){ #dat is your raw data
  R<-cor(dat)
  p<-ncol(dat)
  n<-nrow(dat)
  chi2<- -((n-1)-((2*p)+5)/6 ) * log(det(R)) #this is the formula
  df<-(p*(p-1)/2)
  crit<-qchisq(.95,df) #critical value
  p<-pchisq(chi2,df,lower.tail=F) #pvalue
  cat("Bartlett's test of sphericity: X2(",
      df,")=",chi2,", p=",
      round(p,3),sep="")   
}

# copy paste subset from below
sna_df %>% # keeping all the various networks, mixed and sex specific
  filter(behavior == "total_grooming", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  bart(.)
sna_df %>% # keeping all the various networks, mixed and sex specific
  filter(behavior == "total_grooming", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  KMO(.) 
# weighted: not very hi at all for any one measure for both sexes, plenty hi for males
# unweighted: KMO is higher for F in same sex network, KMO not as high for males


# -- PCs -------
pc_g <- sna_df %>%
  filter(behavior == "total_grooming", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)

pc_p <- sna_df %>% 
  filter(behavior == "prox", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)

# females in mixed sex gmgmd network
pc_fem_mixed_g <- sna_df %>%
  filter(sex == "F", behavior == "total_grooming", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)
# females in mixed sex prox network
pc_fem_mixed_p <- sna_df %>%
  filter(sex == "F", behavior == "prox", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)
# females in same sex gmgmd
pc_fem_same_g <- sna_df %>%
  filter(sex == "F", behavior == "total_grooming", network_sex == "female", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)
# females in same sex prox
pc_fem_same_p <- sna_df %>%
  filter(sex == "F", behavior == "prox", network_sex == "female", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)

# males in mixed gmgmd network
pc_male_mixed_g <- sna_df %>%
  filter(sex == "M", behavior == "total_grooming", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)
# males in mixed prox network
pc_male_mixed_p <- sna_df %>%
  filter(sex == "M", behavior == "prox", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)

# males in mixed gmgmd network
pc_male_same_g <- sna_df %>%
  filter(sex == "M", behavior == "total_grooming", network_sex == "male", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)
# males in mixed prox network
pc_male_same_p <- sna_df %>%
  filter(sex == "M", behavior == "prox", network_sex == "male", complete.cases(bt, ec, deg, trans)) %>%
  select(bt, ec, deg, trans) %>%
  prcomp(., scale = T, center = T)


# --- interpret var loadings ------
pc$rotation # var loadings
pc_p$rotation
pc_g$rotation #by behavior is different, continue to split by behavior
# prox mixed sex; PC1 bt hi n ec deg lo, trans hi deg n bt lo
# gmgmd mixed sex; PC1 ec deg hi, PC2 bt hi trans lo

# within grooming, PCs are similar for m and f, even in same sex nets
# can just focus interpret mixed sex gm net
# m f pc1 a lil different, bt load w ec deg for f, bt load opposite direction for m
pc_fem_mixed_g$rotation
pc_male_mixed_g$rotation
pc_fem_same_g$rotation 
pc_male_same_g$rotation 

#within prox, PCs again are similar, focus interp on mixed sex prox net
pc_fem_mixed_p$rotation  #ec deg together
pc_male_mixed_p$rotation # EC deg together
pc_fem_same_p$rotation # ec deg together
pc_male_same_p$rotation # ec deg together


pc_p$rotation
# PC1 ec deg together and bt opposite (connected vs between), PC2 trans high (clustered)
pc_g$rotation
# PC1 ec deg trans together (connected and clustered), PC2 bt hi and trans lo (between vs clustered)

# kaiser rule, eigenvalue > 1
# keep first 2 PCs in both
pc_p$sd^2 # 
pc_g$sd^2 # 
summary(pc_p) #cum value
summary(pc_g) #cum value

plot(pc_g) #screeplot
plot(pc_p) 
paran(sna_comp_p, iterations = 5000, centile = 0, quietly = FALSE, #paran says 2
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)
paran(sna_comp_g, iterations = 5000, centile = 0, quietly = FALSE, #paran says 2
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

biplot(pc_p, choices = c(1,2))

all_sna_measure_df %>%
  select(chimp_id, sex, year, age_mid_year) %>%
  cbind(., predict(pc)[,1:2]) 


# --- alternate rotations ---
# basically all the same as previous
principal(df, nfactors = 2, rotate = "varimax")
principal(df, nfactors = 2, rotate = "quartimax")
principal(df, nfactors = 2, rotate = "oblimin")

# --- biplots ------

# labels
pc_p_id_sex <- sna_df %>%
  filter(behavior == "prox", network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  mutate(chimp_id_year = paste(chimp_id, year, sep = "_")) %>%
  select(chimp_id_year, sex) %>% mutate_all(as.factor) %>%
  cbind(., predict(pc_p)[,1:2])
pc_p_fs_id_sex <- sna_df %>%
  filter(behavior == "prox", network_sex == "female", complete.cases(bt, ec, deg, trans)) %>%
  mutate(chimp_id_year = paste(chimp_id, year, sep = "_")) %>%
  select(chimp_id_year, sex) %>% mutate_all(as.factor) %>%
  cbind(., predict(pc_fem_same_p)[,1:2])
pc_p_ms_id_sex <- sna_df %>%
  filter(behavior == "prox", network_sex == "male", complete.cases(bt, ec, deg, trans)) %>%
  mutate(chimp_id_year = paste(chimp_id, year, sep = "_")) %>%
  select(chimp_id_year, sex) %>% mutate_all(as.factor) %>%
  cbind(., predict(pc_male_same_p)[,1:2])

pc_g_id_sex <- sna_df %>%
  filter(behavior == "total_grooming",  network_sex == "any_combo", complete.cases(bt, ec, deg, trans)) %>%
  mutate(chimp_id_year = paste(chimp_id, year, sep = "_")) %>%
  select(chimp_id_year, sex) %>% mutate_all(as.factor) %>%
  cbind(., predict(pc_g)[,1:2])
pc_g_fs_id_sex <- sna_df %>%
  filter(behavior == "total_grooming", network_sex == "female", complete.cases(bt, ec, deg, trans)) %>%
  mutate(chimp_id_year = paste(chimp_id, year, sep = "_")) %>%
  select(chimp_id_year, sex) %>% mutate_all(as.factor) %>%
  cbind(., predict(pc_fem_same_g)[,1:2])
pc_g_ms_id_sex <- sna_df %>%
  filter(behavior == "total_grooming", network_sex == "male", complete.cases(bt, ec, deg, trans)) %>%
  mutate(chimp_id_year = paste(chimp_id, year, sep = "_")) %>%
  select(chimp_id_year, sex) %>% mutate_all(as.factor) %>%
  cbind(., predict(pc_male_same_g)[,1:2])


#plots

#prox mixed
p <- ggbiplot(pc_p, choices= c(1,2),varname.size = 3.5,scale = 0, var.scale = 0, groups = pc_p_id_sex$sex,
              ellipse = T, 
              circle = F, alpha = .2) + theme(legend.direction = 'horizontal', 
                                              legend.position = 'top') +
  labs(title = "Prox network SNA measures") +
  ylim(-7.5, 2.7) +
  annotate("text", x = pc_p_id_sex$PC1, y = pc_p_id_sex$PC2, label = pc_p_id_sex$chimp_id_year)
p
# PC1 connected vs between, PC2 clustered

#gm mixed
g <- ggbiplot(pc_g,choices= c(1,2),varname.size = 3.5,scale = 0, var.scale = 0, groups = pc_g_id_sex$sex,
              ellipse = T, 
              circle = F, alpha = 0.2) + theme(legend.direction = 'horizontal', 
                                               legend.position = 'top') +
  labs(title = "Grooming network SNA measures")  +
  annotate("text", x = pc_g_id_sex$PC1, y = pc_g_id_sex$PC2, label = pc_g_id_sex$chimp_id_year)
g
# PC1 connected and clustered, PC2 between vs clustered


p_fs <- ggbiplot(pc_fem_same_p, choices= c(1,2),varname.size = 3.5,scale = 0, var.scale = 0,
                 ellipse = T, 
                 circle = F, alpha = .2) + theme(legend.direction = 'horizontal', 
                                                 legend.position = 'top') +
  labs(title = "Prox network SNA measures Females") +
  ylim(-7.5, 2.7)  +
  annotate("text", x = pc_g_fs_id_sex$PC1, y = pc_g_fs_id_sex$PC2, label = pc_g_fs_id_sex$chimp_id_year)
p_fs

p_ms <- ggbiplot(pc_male_same_p, choices= c(1,2),varname.size = 3.5,scale = 0, var.scale = 0,
                 ellipse = T, 
                 circle = F, alpha = .2) + theme(legend.direction = 'horizontal', 
                                                 legend.position = 'top') +
  labs(title = "Prox network SNA measures Males") +
  ylim(-7.5, 2.7) +
  annotate("text", x = pc_g_ms_id_sex$PC1, y = pc_g_ms_id_sex$PC2, label = pc_g_ms_id_sex$chimp_id_year)
p_ms
# M F same sex prox has same pattern of loadings

g_fs <- ggbiplot(pc_fem_same_g, choices= c(1,2),varname.size = 3.5,scale = 0, var.scale = 0,
                 ellipse = T, 
                 circle = F, alpha = .2) + theme(legend.direction = 'horizontal', 
                                                 legend.position = 'top') +
  labs(title = "Grooming network SNA measures Females") +
  ylim(-7.5, 2.7)
g_fs

g_ms <- ggbiplot(pc_male_same_g, choices= c(1,2),varname.size = 3.5,scale = 0, var.scale = 0,
                 ellipse = T, 
                 circle = F, alpha = .2) + theme(legend.direction = 'horizontal', 
                                                 legend.position = 'top') +
  labs(title = "Grooming network SNA measures Males") +
  ylim(-7.5, 2.7)
g_ms
# For gm, loadings are similar M v F, except that bt pos correlated w
# deg and ec for females, but neg corr for males.

# gyard ------
a <- sna_comp_fem_mixed %>% pull(chimp_id) %>% unique() %>% sort()
b <- sna_comp_fem_same %>% pull(chimp_id) %>% unique() %>% sort()
a[!(a %in% b)]


# for some reason, GS does not appear in all female gm network
# maybe she's not in annual possible dyads - nope! and that means she's seen to gm w males in 2011 but never females
nrow(sna_comp_fem_mixed) # has one more line than 
nrow(sna_comp_fem_same)
# that's bc GS
sna_comp_fem_same %>%
  filter(chimp_id == "GS")
sna_comp_fem_mixed %>%
  filter(chimp_id == "GS")

#check number of chimp obs per year, 
all_sna_measure_df %>%
  filter(chimp_id == "GS")

#of whole data set GS 2011 is only one missing from either obs or poss dyads. good catch.
all_sna_measure_df %>% 
  group_by(chimp_id, year) %>%
  tally() %>%
  filter(n < 4)


load("data/annual possible focal dyads.Rdata", verbose = T)
undir_annual_dyads %>%
  filter(ID1 == "GS" | ID2 == "GS")


# UNWEIGHTED: with both sexes: PC1 kinda captures traditional "integration", i.e. connections of connections - 
# ec deg trans high pos load and bt no
# PC2 captures inverse relationship between betweenness and local clustering, 
# high neg load bt and medium pos loading trans.
# loadings diff when just considering certain sexes within mixed network?
# looking at males in mixed sex network - PC1 captures inverse bt and trans. PC2 is centrality, 
# high neg loading of bt ec and deg, but males in all male network, they follow above pattern too, PC1
# bt low and ec deg trans high pos, PC2, bt high neg and trans on flip side.
# looking at females - PC1 and PC2 follow the above, females probably contribute lots to that signal,
# bc they have the majority of observations in the dataset. is same in same-sex female networks.
# Should not use 1 size fits all for PCs, or shouldn't use PCs at all...
# maybe need to focus on different kinds of networks and centrality within them based on specific
# questions/hypotheses.
# bc relationship bt sna measures (i.e. patterns of correlations and loadings) is different for 
# males in mixed vs same sex network, maybe don't use PCA to reduce dimensionality of integration.

