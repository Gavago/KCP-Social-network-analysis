# 3. Visualization----------
# -- viz - mixed sex gm age sex ------

# grooming
# think we'll find all of these are sig relative to randomization - yup.

sg1 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # sig
  labs( x = "Age (years)", y = "", title = "Grooming Betweenness") +
  annotate("text",x = 35, y = 115, label = "*", size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
#annotate("text",x = 52, y = 50, label = "M > F", size = 4)  
sg1

sg2 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "loess") +
  labs( x = "Age (years)", y = "",title = "Grooming Eigenvector centrality") +
  annotate("text",x = 52, y = 1, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
#annotate("text",x = 52, y = 0.25, label = "M > F", size = 4)  
sg2

sg3 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig
  labs( x = "Age (years)", y = "", title = "Grooming Weighted degree") +
  annotate("text",x = 35, y = 65, label = "*", size = 12) +
  annotate("text",x = 50, y = 75, label = "** Age changes in \n same sex networks, too", size = 3)+
  theme(plot.title = element_text(hjust = 0.5, size = 14))
#annotate("text",x = 52, y = 10, label = "M > F", size = 4)  
sg3

sg4 <- sna_w %>%
  filter(behavior == "total_grooming" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig
  labs( x = "Age (years)", y = "", title = "Grooming Weighted transitivity") +
  annotate("text",x = 35, y = .8, label = "*", size = 12) +
  annotate("text",x = 50, y = .9, label = "** Age changes in \n same sex networks, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
#no main effect diff M F
sg4

sg1; sg2; sg3; sg4

pdf("results/results viz - age sex changes in grooming integration.pdf", height = 16, width = 16)
grid.arrange(grobs = list(sg1, sg2, sg3, sg4), nrow = 2, top = textGrob("Age changes in grooming integration, mixed sex networks"), gp = gpar(fontize = 32))
dev.off()

sna_uw %>%
  filter(chimp_id == "QT", network_sex == "female", behavior =="total_grooming") %>%
  ggplot(aes(age_mid_year, deg)) +
  geom_point() +
  geom_line()
geom_smooth( method = "lm")


# -- viz - mixed sex prox age sex ------

sp1 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, bt, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") +
  labs(x = "Age (years)", y = "" ,title = "Prox Betweenness") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp1

sp2 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, ec, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + # sig M F same sex
  labs( x = "Age (years)", y = "", title = "Prox Eigenvector centrality") +
  annotate("text",x = 35, y = 0.9, label = "*", size = 12) +
  annotate("text",x = 50, y = 1, label = "** Age change in \n male network, too", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp2

sp3 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, deg, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") + #sig mixed, sig M F same sex
  labs( x = "Age (years)", y = "", title = "Prox Weighted degree") +
  annotate("text",x = 50, y = 4, label = "** Age changes in \n same sex networks, alone", size = 3) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp3

sp4 <- sna_w %>%
  filter(behavior == "prox" & network_sex == "any_combo") %>%
  ggplot(aes(age_mid_year, trans, color = sex)) +
  geom_point() +
  geom_smooth( method = "lm") +
  labs( x = "Age (years)", y = "", title = "Prox Weighted transitivity") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))
sp4

pdf("results/results viz - age sex changes in proximity integration.pdf", height = 16, width = 16)
grid.arrange(grobs = list(sp1, sp2, sp3, sp4), nrow = 2, top = textGrob("Age changes in proximity integration, mixed sex network"), gp = gpar(fontize = 32))
dev.off()
