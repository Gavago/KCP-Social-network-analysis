# Line plot by individ

#feels like repeatability is a hack
# what we're finding out is if there is sig variation by individual in sociality

# what is difference between significant intercept and repeatability?
# would want to know if individual intercept and slope is sig, no?

#repeatability is difference in average phenotype - aka intercept - does not say anything about individual differences in slope
# (can say we wanted to model individual differences in slope in detail in followup) 
# here just wanted to understand if indivdiual differences in average integration phenotype made up substantial amount of pop variation
# (more in depth understanding about individual social aging trajectories to come)
# if individ diffs only (no pop pattern in aging) then have 
# if pop pattern in aging + individ differences in intercept (then same slopes, diff intercept) 



# FEMALES ------
# Out Strength female MS -----
unique_id <- unique(f_mixed_dir_sna_w$chimp_id)
position <- match(unique_id, f_mixed_dir_sna_w$chimp_id)
first_year <- f_mixed_dir_sna_w %>%
  slice(position)

v_out_strength_fem <- f_mixed_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "Out-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_out, color = chimp_id, label = chimp_id))
v_out_strength_fem


## MALES ------


# In Degree male MS - YES REPEATABLE and SIG AGE EFFECT (After removing rank) ------
unique_id <- unique(m_mixed_dir_sna_uw$chimp_id)
position <- match(unique_id, m_mixed_dir_sna_uw$chimp_id)
first_year <- m_mixed_dir_sna_uw %>%
  slice(position)

v_in_deg_mal <- m_mixed_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "In-Degree") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_in, color = chimp_id, label = chimp_id))
v_in_deg_mal

# Out Degree male MS - YES REPEATABLE and SIG AGE EFFECT (regardless of rank) --------
unique_id <- unique(m_mixed_dir_sna_uw$chimp_id)
position <- match(unique_id, m_mixed_dir_sna_uw$chimp_id)
first_year <- m_mixed_dir_sna_uw %>%
  slice(position)

v_out_deg_mal <- m_mixed_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_out, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "Out-Degree") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_out, color = chimp_id, label = chimp_id))
v_out_deg_mal

 
# In Strength male MS YES REPEATABLE ----------
unique_id <- unique(m_mixed_dir_sna_w$chimp_id)
position <- match(unique_id, m_mixed_dir_sna_w$chimp_id)
first_year <- m_same_dir_sna_w %>%
  slice(position)

v_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_in, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "In-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_in, color = chimp_id, label = chimp_id))
v_in_strength_mal

# Out Strength male MS YES REPEATABLE ----------
unique_id <- unique(m_mixed_dir_sna_w$chimp_id)
position <- match(unique_id, m_mixed_dir_sna_w$chimp_id)
first_year <- m_mixed_dir_sna_w %>%
  slice(position)

v_out_strength_mal <- m_mixed_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "Out-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_out, color = chimp_id, label = chimp_id))
v_out_strength_mal



# In Degree male SS - NOT REPEATABLE ---------
unique_id <- unique(m_same_dir_sna_uw$chimp_id)
position <- match(unique_id, m_same_dir_sna_uw$chimp_id)
first_year <- m_same_dir_sna_uw %>%
  slice(position)

v_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "In-Degree") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_in, color = chimp_id, label = chimp_id))
v_in_deg_mal


# Out Degree male SS - YES REPEATABLE --------
unique_id <- unique(m_same_dir_sna_uw$chimp_id)
position <- match(unique_id, m_same_dir_sna_uw$chimp_id)
first_year <- m_same_dir_sna_uw %>%
  slice(position)

v_in_deg_mal <- m_same_dir_sna_uw %>%
  ggplot(aes(age_mid_year, deg_in, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "In-Degree") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_in, color = chimp_id, label = chimp_id))
v_in_deg_mal
# In Strength male SS YES REPEATABLE ----------
unique_id <- unique(m_same_dir_sna_w$chimp_id)
position <- match(unique_id, m_same_dir_sna_w$chimp_id)
first_year <- m_same_dir_sna_w %>%
  slice(position)

v_in_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_in, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "In-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_in, color = chimp_id, label = chimp_id))
v_in_strength_mal

# Out Strength male SS YES REPEATABLE ----------
unique_id <- unique(m_same_dir_sna_w$chimp_id)
position <- match(unique_id, m_same_dir_sna_w$chimp_id)
first_year <- m_same_dir_sna_w %>%
  slice(position)

v_out_strength_mal <- m_same_dir_sna_w %>%
  ggplot(aes(age_mid_year, deg_out, color = chimp_id)) +
  geom_jitter(size = 1.5, alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", linetype = 2, size = .5, se = FALSE ) + #"gam", formula = y ~ s(x, bs = "cs")
  labs(x = "Age (years)", y = "", title = "Out-Strength") +
  theme_bw()  +
  theme(plot.title = element_text(hjust = 0.5, size = 16, family = "Georgia"), axis.title.x = element_text(family = "Georgia", size = 14), legend.position = "bottom",
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) +
  geom_text_repel(data = first_year, aes(age_mid_year, deg_out, color = chimp_id, label = chimp_id))
v_out_strength_mal


