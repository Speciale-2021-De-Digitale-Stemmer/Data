
#-------------------------------------------------------------------------
# PERFORM CONJOINT ANALYSIS
#-------------------------------------------------------------------------

# Read clean conjoint data files
CJ <- readRDS("2_Clean/6_Analysis_II/Final_data/Conjoint_clean.rds")
CJ_long <- readRDS("2_Clean/6_Analysis_II/Final_data/CJ_long.rds")


#-------------------------------------------------------------------------
# DESCRIPTIVES
#-------------------------------------------------------------------------

# How many received localism cue
table(CJ_long$Lokalisme)

# Repstil choice
table(CJ_long$Repstil_choice)

# Localism choice
table(CJ_long$Localism_choice)

# Gender
CJ %>% 
  filter(!is.na(Køn_valg)) %>% 
  group_by(Køn_valg) %>% 
  tally(wt = Weight) %>% 
  mutate(share = n/sum(n))

# Calculate age choice
CJ %>% 
  pivot_longer(cols = c(Alder_A, Alder_B),
               names_to = "Annonce",
               values_to = "Alder_kandidat") %>% # pivot age columns longer
  select(Annonce, Alder_kandidat, Alder_samme, Choice, Weight) %>% # select only variables of interest
  filter(!Alder_samme == "Samme aldre") %>% # filter out tasks with two candidates of same age
  mutate(Alder_valg = case_when(
    Annonce == "Alder_A" & Choice == 1 ~ Alder_kandidat, # get age on the candidate that the respondent has preferred
    Annonce == "Alder_B" & Choice == 2 ~ Alder_kandidat)) %>% 
  filter(!is.na(Alder_valg)) %>% # filter out candidates that were not preferred by respondent
  group_by(Alder_valg) %>% 
  tally(wt = Weight) %>% # get distribution over choice grouped by age of candidate
  mutate(Andel = n / sum(n)) # calculate proportion


#-------------------------------------------------------------------------
# H1B AND H2B AS BAR CHARTS
#-------------------------------------------------------------------------

# Function to calculate standard error for variables
se <- function(x) sqrt(var(x)/length(x))

# Filter out NA's in agree variable
CJ_agree <- CJ %>% 
  filter(!is.na(Agree))

# Also filter out NA in localism variable
CJ_agree_local <- CJ_agree %>% 
  filter(!is.na(Localism_choice),
         Localism_same == "Different")

# H2b
H2b <- CJ_agree %>%
  group_by(Agree, Repstil_fac) %>%
  summarise(count = n()) %>%  # count records by species
  mutate(pct = round(count/sum(count), digits = 3), # find percent of total
         se = se(CJ_agree$Repstil_choice))  # calculate standard error


# Plot it
H2b_plot <- ggplot(H2b, aes(Agree, pct, fill = Repstil_fac)) +
  geom_bar(position = "dodge",
           stat = "identity",
           width = 0.42) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_dodge(width = 0.42),
            vjust = 2.5,
            colour = "white",
            size = 5.0) +
  geom_errorbar(aes(x = Agree, ymin = pct - se, ymax = pct + se),
                width = 0.12, colour = "black", alpha = 0.9, size = 0.7,
                position = position_dodge(width = 0.42), stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = standard.col) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18, color = "black"),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black")) +
  ylim(0, 1)

# H3b
H3b <- CJ_agree_local %>%
  group_by(Agree, Localism_choice) %>%
  summarise(count = n()) %>%  # count records by species
  mutate(pct = round(count/sum(count), digits = 3), # find percent of total
         se = se(CJ_agree_local$Localism_choice),  # calculate standard error
         Localism_choice = factor(Localism_choice))

# Set factor levels
levels(H3b$Localism_choice) = c("Ikke lokalisme", "Lokalisme")

# Plot H3b
H3b_plot <- ggplot(H3b, aes(Agree, pct, fill = Localism_choice)) +
  geom_bar(position = "dodge",
           stat = "identity",
           width = 0.42) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_dodge(width = 0.42),
            vjust = 3,
            colour = "white",
            size = 5.0) +
  geom_errorbar(aes(x = Agree, ymin = pct - se, ymax = pct + se),
                width = 0.12, colour = "black", alpha = 0.9, size = 0.7,
                position = position_dodge(width = 0.42), stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = standard.col) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18, color = "black"),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black")) +
  ylim(0, 1)

# Save plot
pdf("../3_Figurer/71_H2b_soejler.pdf", height = 10, width = 14)
ggarrange(H2b_plot, 
          H3b_plot, 
          nrow = 1,
          ncol = 2,
          widths = c(1, 1))
dev.off()


#-------------------------------------------------------------------------
# TEST OF BOTH H1B AND H2B
#-------------------------------------------------------------------------

# Marginal means 
h1b_by <- cj(data = CJ_long, 
            Selected ~ Strategi_local, 
            by = ~ Agree,
            id = ~ Respondent,
            estimate = "mm", h0 = 0.5) 

# Plot result
plot_h1b_by <- plot(
  h1b_by[h1b_by$Agree == "Ideologisk overensstemmelse", ],
  feature_headers = F,
  xlab = "Sandsynlighed for valg af kandidat",
  vline = 0.5,
  xlim = c(0, 1)) + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        axis.ticks.y = element_blank()) +
  scale_color_manual(values = "black") +
  ggtitle("H1b: Ideologisk overensstemmelse")

plot_h1b_by2 <- plot(
  h1b_by[h1b_by$Agree == "Ideologisk uoverensstemmelse", ],
  feature_headers = F,
  xlab = "Sandsynlighed for valg af kandidat",
  vline = 0.5,
  xlim = c(0, 1)) + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        axis.ticks.y = element_blank()) +
  scale_color_manual(values = "black") +
  ggtitle("H2b: Ideologisk uoverensstemmelse")

# Save plot
pdf("../3_Figurer/71_H2b_samlet.pdf", height = 10, width = 14)
ggarrange(plot_h1b_by, 
          plot_h1b_by2 + rremove("y.text"), 
          nrow = 1,
          ncol = 2,
          widths = c(1.40, 1))
dev.off()


#-------------------------------------------------------------------------
# AMCE FOR BOTH H1B AND H2B
#-------------------------------------------------------------------------

### STRATEGY ###
# Specify function to add linespaces in ggplot
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

# Test hypothesis with AMCE test
amce_H2_3b <- cj(data = CJ_long, 
                 Repstil_choice ~ Agree, 
                 id = ~ Respondent)

# Show p-value for difference
signif(amce_H2_3b$p[2], digits = 2)

# Plot result
plot_amce_H2_3b <- plot(
  amce_H2_3b,
  feature_headers = F,
  xlab = "Sandsynlighed for valg af credit claiming",
  vline = 0,
  xlim = c(-0.5, 0.5)) + 
  theme(legend.position = "none",
        text = element_text(size = 18, color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.1265, 0.5, 0.1, -.45), "cm")) +
  scale_color_manual(values = "black") +
  scale_y_discrete(breaks = unique(amce_H2_3b$level), 
                   labels = addline_format(c("Ideologisk overensstemmelse", 
                                             "Ideologisk uoverensstemmelse")))


### LOCALISM ###
# Test hypothesis with AMCE test
amce_H2_3b_local <- cj(data = CJ_long, 
                       Localism_choice ~ Agree, 
                       id = ~ Respondent)

# Show p-value for difference
signif(amce_H2_3b_local$p[2], digits = 2)

# Plot result
plot_amce_H2_3b_local <- plot(
  amce_H2_3b_local,
  feature_headers = F,
  xlab = "Sandsynlighed for valg af lokalisme",
  vline = 0,
  xlim = c(-0.5, 0.5)) + 
  theme(legend.position = "none",
        text = element_text(size = 18, color = "black"),
        axis.text = element_text(color = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0.1265, 0.5, 0.1, -.45), "cm")) +
  scale_color_manual(values = "black") +
  scale_y_discrete(breaks = unique(amce_H2_3b_local$level), 
                   labels = addline_format(c("Ideologisk overensstemmelse", 
                                             "Ideologisk uoverensstemmelse")))

# Save the plots
pdf("../3_Figurer/71_Hypotese_H2_3B_AMCE.pdf", height = 8, width = 12)
ggarrange(plot_amce_H2_3b, 
          plot_amce_H2_3b_local + rremove("y.text"), 
          nrow = 1,
          ncol = 2,
          widths = c(1.40, 1))
dev.off()



#-------------------------------------------------------------------------
# OPENING UP THE BLOCKS
#-------------------------------------------------------------------------

# Marginal means 
mm_H2_3b_blok <- cj(data = CJ, 
                    Repstil_choice ~ Partivalg, 
                    id = ~ Respondent,
                    estimate = "mm", h0 = 0.5)

# Reoder party names according to estimate size
mm_H2_3b_blok$level <- factor(mm_H2_3b_blok$level, levels = mm_H2_3b_blok$level[order(mm_H2_3b_blok$estimate)])

# Plot result
plot_mm_H2_3b_blok <- plot(
  mm_H2_3b_blok[mm_H2_3b_blok$level %in% c(
    "A: Socialdemokratiet", "B: Radikale", "C: Konservative",
    "D: Nye Borgerlige", "F: Socialistisk Folkeparti",
    "I: Liberal Alliance", "K: Kristendemokraterne",
    "O: Dansk Folkeparti", "V: Venstre",
    "Ø: Enhedslisten", "Å: Alternativet"), ], feature_headers = F,
  xlab = "Sandsynlighed for valg af credit claiming",
  vline = 0.5,
  xlim = c(0.1, 0.9)) + 
  theme(legend.position = "none",
        text = element_text(size = 18, color = "black"),
        axis.text = element_text(color = "black")) +
  scale_color_manual(values = "black")
  
# Save plot
pdf("../3_Figurer/71_H2_3b_party.pdf", height = 7, width = 12)
plot_mm_H2_3b_blok
dev.off()


#-------------------------------------------------------------------------
# ROBUSTNESS TEST WITHOUT CERTAIN PARTIES
#-------------------------------------------------------------------------

# H2b filter
H2b_filter <- CJ_agree %>%
  filter(!Partivalg %in% c("A: Socialdemokratiet", 
                           "O: Dansk Folkeparti", "G: Veganerpartiet")) %>% 
  group_by(Agree, Repstil_fac) %>%
  summarise(count = n()) %>%  # count records by species
  mutate(pct = round(count/sum(count), digits = 3), # find percent of total
         se = se(CJ_agree$Repstil_choice))  # calculate standard error


# Plot it
H2b_filter_plot <- ggplot(H2b_filter, aes(Agree, pct, fill = Repstil_fac)) +
  geom_bar(position = "dodge",
           stat = "identity",
           width = 0.42) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_dodge(width = 0.42),
            vjust = 2.5,
            colour = "white",
            size = 5.0) +
  geom_errorbar(aes(x = Agree, ymin = pct - se, ymax = pct + se),
                width = 0.12, colour = "black", alpha = 0.9, size = 0.7,
                position = position_dodge(width = 0.42), stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = standard.col) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18, color = "black"),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black")) +
  ylim(0, 1)

# H3b filter
H3b_filter <- CJ_agree_local %>%
  filter(!Partivalg %in% c("A: Socialdemokratiet", 
                           "O: Dansk Folkeparti", "G: Veganerpartiet")) %>% 
  group_by(Agree, Localism_choice) %>%
  summarise(count = n()) %>%  # count records by species
  mutate(pct = round(count/sum(count), digits = 3), # find percent of total
         se = se(CJ_agree_local$Localism_choice),  # calculate standard error
         Localism_choice = factor(Localism_choice))

# Set factor levels
levels(H3b_filter$Localism_choice) = c("Ikke lokalisme", "Lokalisme")

# Plot H3b
H3b_filter_plot <- ggplot(H3b_filter, aes(Agree, pct, fill = Localism_choice)) +
  geom_bar(position = "dodge",
           stat = "identity",
           width = 0.42) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_dodge(width = 0.42),
            vjust = 3,
            colour = "white",
            size = 5.0) +
  geom_errorbar(aes(x = Agree, ymin = pct - se, ymax = pct + se),
                width = 0.12, colour = "black", alpha = 0.9, size = 0.7,
                position = position_dodge(width = 0.42), stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = standard.col) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 18, color = "black"),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black")) +
  ylim(0, 1)

# Save plot
pdf("../3_Figurer/71_Hypotese_H2_3B_AMCE_filter.pdf", height = 8, width = 10)
H2b_filter_plot
dev.off()



#-------------------------------------------------------------------------
# TEST WITH ALTERNATIVE IDEOLOGY VARIABLE
#-------------------------------------------------------------------------

# Filter out troublesome parties
CJ_ideology <- CJ %>% 
  mutate(Extreme = factor(ifelse(Ideologi %in% c(0:1, 9:10), 
                                 "Ideologisk yderligtgående", "Ideologisk moderate")))

# Test hypothesis with AMCE test
mm_H2_3b_ideology <- cj(data = CJ_ideology, 
                        Repstil_choice ~ Topic,
                        id = ~ Respondent,
                        by = ~ Ideologi,
                        estimate = "mm", h0 = 0.5) %>% 
  mutate(
    Ideologi = paste(Ideologi, " ", sep = ""),                      
    Ideologi = as.numeric(str_trim(str_extract(Ideologi, ".."))), # creative workaround to remove part of 
    level = factor(level, levels = c("Skat", "Transport", "Uddannelse", "Sundhed")), # character string and convert to numeric
    )  


# Plot MM with new ideology variable
plot_mm_H2_3b_ideology <- plot(
  mm_H2_3b_ideology,
  feature_headers = F,
  xlab = "Sandsynlighed for valg af credit claiming",
  vline = 0.5,
  xlim = c(0, 1)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 16, color = "black"),
        axis.ticks.y = element_blank()) +
  scale_color_gradient(low = "red3", high = "navy",
                       breaks = c(0.0, 10),
                       labels = c("0: Mest venstreorienteret",
                                  "10: Mest højreorienteret"))

# Save the plots
pdf("../3_Figurer/71_Hypotese_H2_3B_MM_ideology.pdf", height = 10, width = 12)
plot_mm_H2_3b_ideology
dev.off()



#-------------------------------------------------------------------------
# OPEN UP FOR TOPICS
#-------------------------------------------------------------------------

# Topic venstreorienteret
mm_topic_blok <- cj(data = CJ,
                    Repstil_choice ~ Topic, 
                    by = ~ Blok,
                    id = ~ Respondent,
                    estimate = "mm", h0 = 0.5)

plot_mm_topic_blok <- plot(mm_topic_blok, 
                           group = "Blok", 
                           vline = 0.5, 
                           xlim = c(0, 1),
                           feature_headers = F,
                           legend_title = "",
                           xlab = "Sandsynlighed for valg af credit claiming") +
  ggplot2::facet_wrap(~level, ncol = 1L,
                      scales = "free_y") +
  scale_color_manual(values = c("red2", "blue"),
                     labels = c("Venstrefløjen (A, B, F, Ø, Å)",
                                "Højrefløjen (C, D, I, K, O, V)")) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "black"),
    plot.margin = unit(c(0.1265, 0.05, 0.1, 0.5), "cm")
  )
  
# Save the plots
pdf("../3_Figurer/71_H2_Topic_Blok.pdf", height = 9, width = 12.5)
plot_mm_topic_blok
dev.off()



#-------------------------------------------------------------------------
# H4B: EFFECT OF LOCALISM
#-------------------------------------------------------------------------

# AMCE test for localism cue
amce_localism <- cj(data = CJ_long,
                    Selected ~ Lokalisme, 
                    id = ~ Respondent,
                    estimate = "amce")

# Plot it
plot_amce_localism <- plot(amce_localism,
                      vline = 0,
                      xlim = c(-0.5, 0.5),
                      feature_headers = F,
                      legend_title = "",
                      xlab = "Sandsynlighed for valg af kandidat") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
    legend.title = element_blank(),
  ) +
  scale_color_manual(values = "black")

# Save plot
pdf("../3_Figurer/72_H4B.pdf", height = 5, width = 9)
plot_amce_localism
dev.off()


#-------------------------------------------------------------------------
# LOCALISM GROUPED BY DIRECTLY AFFECTED CITIZENS
#-------------------------------------------------------------------------

# MM test for localism cue
mm_localism_by <- cj(data = CJ,
                  Localism_choice ~ Localism_same, 
                  id = ~ Respondent,
                  by = ~ By_mentioned,
                  estimate = "mm", h0 = 0.5) %>% 
  mutate(level = recode(level, "Different" = "Lokalisme")) # change label for plotting purposes

# Plot it
plot_mm_localism_by <- plot(
  mm_localism_by[mm_localism_by$level == "Lokalisme" ,],
  feature_headers = F,
  xlab = "Sandsynlighed for valg af annonce med lokalisme",
  vline = 0.5,
  xlim = c(0.2, 0.8)) + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 18, color = "black"),
        axis.text = element_text(color = "black")) +
  scale_color_manual(values = standard.col, 
                     labels = c("By ikke nævnt", "By nævnt"))

# Save plot
pdf("../3_Figurer/72_H4B_city_mentioned.pdf", height = 5, width = 9)
plot_mm_localism_by
dev.off()


#-------------------------------------------------------------------------
# LOCALISM BY IDEOLOGY
#-------------------------------------------------------------------------

# Preference conditioned by localism by ideology
mm_local_rep <- cj(data = CJ,
                   Repstil_choice ~ Agree,
                   by = ~ Type_gather,
                   id = ~ Respondent,
                   estimate = "mm", h0 = 0.5) %>%  
  mutate(level = recode(
    level, 
    "Ideologisk overensstemmelse" = "Ideologisk \noverensstemmelse",
    "Ideologisk uoverensstemmelse" = "Ideologisk \nuoverensstemmelse"))

# Plot
plot_mm_local_rep <- plot(mm_local_rep,
                          group = "Type_gather",
                          vline = 0.5,
                          xlim = c(0, 1),
                          feature_headers = F,
                          legend_title = "Bloktilhørsforhold",
                          xlab = "Sandsynlighed for valg af credit claiming") +
  ggplot2::facet_wrap(~level, ncol = 1L,
                      scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color = "black"),
  ) +
  scale_color_manual(values = standard.col,
                     labels = c("Ingen lokalisme",
                                "Begge lokalisme",
                                "Credit claiming og \nlokalisme",
                                "Politisk positionering og \nlokalisme"))
  
# Test marginal means difference
mm_diff_repstil_local <- mm_diffs(
  data = CJ,
  Repstil_choice ~ Agree, 
  by = ~ Type_gather,
  id = ~ Respondent, 
  alpha = 0.05)

# Save the plots
pdf("../3_Figurer/71_H4_Local_rep.pdf", height = 8, width = 12.5)
plot_mm_local_rep
dev.off()


#-------------------------------------------------------------------------
# VOTER BACKGROUND AND CANDIDATE PREFERENCES (POPULATION)
#-------------------------------------------------------------------------

# Recalculate population into 1.000s
CJ$Indbyggertal_10000 <- CJ$Indbyggertal/10000

# Test preference conditioned by voter place of living
# Linear relationship between population in city and preference
city_pop_rep <- glm(Repstil_choice ~ as.numeric(Indbyggertal_10000) + Alder + Uddannelse, data = CJ, family = "binomial")
city_pop_loc <- glm(Localism_choice ~ as.numeric(Indbyggertal_10000) + Alder + Uddannelse, data = CJ, family = "binomial")

# Table for LaTeX
stargazer(city_pop_rep, city_pop_loc,
          column.labels = c("Credit claiming", "Lokalisme"),
          dep.var.labels.include = FALSE,
          covariate.labels = c(
            "Indbyggertal \ni titusinde", "Alder", "Gymnasial uddannelse",
            "Erhvervsuddannelse", "KVU", "MVU", "LVU"),
          type = "latex", digits = 2)


#-------------------------------------------------------------------------
# VOTER AGE AND CANDIDATE PREFERENCES
#-------------------------------------------------------------------------

# Filter for respondents that experienced difference
# in localism in the two ads
CJ_local <- CJ %>%
  filter(Localism_same == "Different")


# Testing population effect with age as control
city_pop_rep_2 <- glm(Repstil_choice ~ as.numeric(Indbyggertal_10000), data = CJ)
city_pop_rep_2_age <- glm(Repstil_choice ~ as.numeric(Indbyggertal_10000) + Alder_kat_det, data = CJ)

# Testing linear relationship between population and age
age_pop <- lm(as.numeric(Indbyggertal_10000) ~ Alder, data = CJ)
stargazer(age_pop, type = "text")

# Calculate mean population for all different ages
CJ_age_pop <- CJ %>% 
  group_by(Alder) %>% 
  summarise(indbygger = mean(Indbyggertal_10000))

# Plot relationship between age and population in city
CJ_age_pop_plot <- CJ_age_pop %>% 
  ggplot(aes(x = Alder, y = indbygger)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_color_manual(values = standard.col) +
  theme_bw() +
  xlab("Alder") +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        text = element_text(size = 18, color = "black"),
        axis.text = element_text(color = "black")) +
  ylim(0, 100) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 50, label.y = 600,
    p.accuracy = 0.001, r.accuracy = 0.01
)

# Save plot
pdf("../3_Figurer/73_Alder_Population.pdf", height = 9, width = 11)
CJ_age_pop_plot
dev.off()


#-------------------------------------------------------------------------
# VOTER EDUCATION AND CANDIDATE PREFERENCES
#-------------------------------------------------------------------------

amce_udd <- cj(data = CJ,
               Repstil_choice ~ Uddannelse,
               by = ~ Agree,
               id = ~ Respondent,
               estimate = "mm", h0 = 0.5) %>%
  mutate(level = recode(
    level,
    "Folkeskole / Grundskole" = "Grundskole",
    "Gymnasial uddannelse (Student, HF, HH, HTX og lign.)" = "Gymnasial \n uddannelse",
    "Kort videregående uddannelse" = "KVU",
    "Mellemlang videregående uddannelse (Bachelor niveau)" = "MVU",
    "Lang videregående uddannelse (Kandidat niveau)" = "LVU"))

# Plot
plot_amce_udd <- plot(amce_udd,
                      group = "Agree",
                      vline = 0.5,
                      xlim = c(0, 1),
                      feature_headers = F,
                      legend_title = "Bloktilhørsforhold",
                      xlab = "Sandsynlighed for valg af credit claiming") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    ) +
  scale_color_manual(values = standard.col,
                     labels = c("Ideologisk \noverensstemmelse",
                                "Ideologisk \nuoverensstemmelse")) +
  ggtitle("Credit claiming")


# Preference for localism 
amce_udd_local <- cj(data = CJ_local,
                     Localism_choice ~ Uddannelse,
                     by = ~ Agree,
                     id = ~ Respondent,
                     estimate = "mm", h0 = 0.5)

# Plot
plot_amce_udd_local <- plot(amce_udd_local,
                            group = "Agree",
                            vline = 0.5,
                            xlim = c(0, 1),
                            feature_headers = F,
                            legend_title = "Bloktilhørsforhold",
                            xlab = "Sandsynlighed for valg af lokalisme") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 16, color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    legend.text = element_text(size = 16, color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
  ) +
  scale_color_manual(values = standard.col,
                     labels = c("Ideologisk \noverensstemmelse",
                                "Ideologisk \nuoverensstemmelse")) +
  ggtitle("Lokalisme")

# Save the plots
pdf("../3_Figurer/73_Uddannelse.pdf", height = 9, width = 12.5)
ggarrange(plot_amce_udd, 
          plot_amce_udd_local + rremove("y.text"), 
          ncol = 2,
          widths = c(1.3, 1, 1))
dev.off()



### BY TOPIC ###
# Preference conditioned by age and education 
# for credit claiming
amce_udd_age_topic <- cj(data = CJ,
                   Repstil_choice ~ Uddannelse,
                   by = ~ Topic,
                   id = ~ Respondent,
                   estimate = "mm", h0 = 0.5) %>% 
  mutate(level = recode(
    level, 
    "Folkeskole / Grundskole" = "Grundskole",
    "Gymnasial uddannelse (Student, HF, HH, HTX og lign.)" = "Gymnasial \n uddannelse",
    "Kort videregående uddannelse" = "KVU",
    "Mellemlang videregående uddannelse (Bachelor niveau)" = "MVU",
    "Lang videregående uddannelse (Kandidat niveau)" = "LVU"))

# Plot
plot_amce_udd_age_topic <- plot(amce_udd_age_topic,
                          group = "Topic",
                          vline = 0.5,
                          xlim = c(0, 1),
                          feature_headers = F,
                          legend_title = "Bloktilhørsforhold",
                          xlab = "Sandsynlighed for valg af credit claiming") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
  ) +
  scale_color_manual(values = standard.col)

# Save plot
pdf("../3_Figurer/73_Bilag_Uddannelse_Alder_Topic.pdf", height = 9, width = 10)
plot_amce_udd_age_topic
dev.off()



#-------------------------------------------------------------------------
# INSPECT CORRELATION BETWEEN EDUCATION AND POLITICAL INTEREST
#-------------------------------------------------------------------------

# Load data from Danish election data
load("1_Raw/1_Valgdata/Valgundersøgelserne 1971-2015/Valgundersøgelsen 2015 (DDA-31083)/Data/R/data31083.rdata")
spss_data <- read_sav("1_Raw/1_Valgdata/Valgundersøgelserne 1971-2015/Valgundersøgelsen 2015 (DDA-31083)/Data/SPSS/data31083.sav")

