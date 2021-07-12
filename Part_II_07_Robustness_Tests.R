
#-------------------------------------------------------------------------
# WRANGLING CANDIDATE FEATURE FOR TASK ORDER ROBUSTNESS TESTS
#-------------------------------------------------------------------------

# Read clean conjoint data file
Background <- readRDS("2_Clean/6_Analysis_II/Final_data/CJ_long.rds")


#-------------------------------------------------------------------------
# PLOTTING ROBUSTNESS TESTS
#-------------------------------------------------------------------------

# Add intuitive feature labels
attr(Background$Lokalisme, "label") <- "LOKALISME"
attr(Background$Køn, "label") <- "KØN"
attr(Background$Alder, "label") <- "ALDER"


# Test task order effect on strategy choice
mm_tasknumber <- cj(data = Background,
                    Repstil_choice ~ Lokalisme + Køn + Alder, 
                    by = ~ Task_order,
                    id = ~ Respondent,
                    estimate = "mm", h0 = 0.5)

# Plot
plot_mm_tasknumber <- plot(mm_tasknumber, 
     group = "Task_order", 
     # vline = 0.5, 
     xlim = c(0.05, 1),
     feature_headers = F,
     legend_title = "Opgavenummer",
     xlab="Sandsynlighed for valg af credit claiming")+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.text = element_text(size = 16, color = "black"),
  )+
  scale_color_manual(values = standard.col)


# Save plot
pdf("../3_Figurer/73_task_order.pdf", height = 8, width = 9)
plot_mm_tasknumber
dev.off()

# Marginal means difference
mm_diff_tasknumber <- mm_diffs(
  data = Background,
  Repstil_choice ~ Køn + Alder + Lokalisme, 
  by = ~ Task_order,
  id = ~ Respondent, 
  alpha = 0.05)

# Proportion of estimates different from each other
nrow(mm_diff_tasknumber[mm_diff_tasknumber$p<0.05, ])/nrow(mm_diff_tasknumber)

# Omnibus-F-test / ANOVA (p = 0.4441)
cj_anova(data = Background, Repstil_choice ~ Køn + Alder + Lokalisme,
         by = ~ Task_order)


#-------------------------------------------------------------------------
# PLOTTING PROFILE NUMBER TEST
#-------------------------------------------------------------------------

# Test profile number effect on strategy choice
mm_profilenumber <- cj(data = Background,
                    Repstil_choice ~ Lokalisme + Køn + Alder, 
                    by = ~ Display,
                    id = ~ Respondent,
                    estimate = "mm", h0 = 0.5)

# Plot
plot_mm_profilenumber <- plot(
  mm_profilenumber, 
  group = "Display", 
  # vline = 0.5, 
  xlim = c(0.05, 1),
  feature_headers = F,
  legend_title = "Profilnummer",
  xlab = "Sandsynlighed for valg af credit claiming") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.text = element_text(size = 16, color = "black"),
  ) +
  scale_color_manual(values = standard.col)

# Save plot
pdf("../3_Figurer/73_profile_number.pdf", height = 7, width = 9)
plot_mm_profilenumber
dev.off()

# Marginal means difference
mm_diff_profilenumber <- mm_diffs(
  data = Background,
  Repstil_choice ~ Køn + Alder + Lokalisme, 
  by = ~ Display,
  id = ~ Respondent, 
  alpha = 0.05)

# Proportion of estimates different from each other
nrow(mm_diff_profilenumber[mm_diff_profilenumber$p < 0.05, ])/nrow(mm_diff_profilenumber)

# Omnibus-F-test / ANOVA (p = 0.4441)
cj_anova(data = Background, Repstil_choice ~ Køn + Alder + Lokalisme,
         by = ~ Display)



#-------------------------------------------------------------------------
# BALANCE TEST (PROFILE RANDOMIZATION)
#-------------------------------------------------------------------------

### EDUCATION ###
# Create dataframe where NA's are removed
Background_udd <- Background %>% 
  filter(!is.na(Udd_resp)) %>% 
  mutate(Udd_resp = as.numeric(Udd_resp))

# Test profile number effect on strategy choice
mm_balance_udd <- cj(data = Background_udd,
                     Udd_resp ~ Lokalisme + Køn + Alder, 
                     id = ~ Respondent,
                     estimate = "mm", h0 = 0.5)

# Plot education
plot_mm_balance_udd <- plot(mm_balance_udd, 
                            vline = mean(Background_udd$Udd_resp), 
                            xlim = c(mean(Background_udd$Udd_resp) - 1, mean(Background_udd$Udd_resp) + 1),
                            feature_headers = F,
                            legend_title = "Opgavenummer",
                            xlab = "p(Højtuddannet)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
  ) +
  scale_color_manual(values = standard.col) +
  ggtitle("Uddannelse")



### AGE ###
# Create dataframe where NA's are removed
Background_age <- Background %>% 
  filter(!is.na(Alder_resp)) %>% 
  mutate(Alder_resp = as.numeric(Alder_resp))

# Test profile number effect on strategy choice
mm_balance_age <- cj(data = Background_age,
                     Alder_resp ~ Lokalisme + Køn + Alder, 
                     id = ~ Respondent,
                     estimate = "mm", h0 = 0.5)

# Plot education
plot_mm_balance_age <- plot(mm_balance_age, 
                            vline = mean(Background_age$Alder_resp), 
                            xlim = c(mean(Background_age$Alder_resp) - 5, mean(Background_age$Alder_resp) + 5),
                            feature_headers = F,
                            legend_title = "Opgavenummer",
                            xlab = "p(Alder)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
  ) +
  scale_color_manual(values = standard.col) +
  ggtitle("Alder")



### GENDER ###
# Create dataframe where NA's are removed
Background_gender <- Background %>% 
  filter(!is.na(Køn_resp)) %>% 
  mutate(Køn_resp = as.numeric(Køn_resp))

# Test profile number effect on strategy choice
mm_balance_gender <- cj(data = Background_gender,
                        Køn_resp ~ Lokalisme + Køn + Alder, 
                        id = ~ Respondent,
                        estimate = "mm", h0 = 0.5)

# Plot education
plot_mm_balance_gender <- plot(mm_balance_gender, 
                            vline = mean(Background_gender$Køn_resp), 
                            xlim = c(mean(Background_gender$Køn_resp) - 0.1, mean(Background_gender$Køn_resp) + 0.1),
                            feature_headers = F,
                            legend_title = "Opgavenummer",
                            xlab = "p(Kvinde)") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 18, color = "black"),
    axis.text = element_text(color = "black"),
    legend.position = "none",
  ) +
  scale_color_manual(values = standard.col) +
  ggtitle("Køn")

# Arrange the three plots next to each other and save
pdf("../3_Figurer/73_balance_plot.pdf", height = 10, width = 12)
ggarrange(plot_mm_balance_udd, 
          plot_mm_balance_age + rremove("y.text"), 
          plot_mm_balance_gender + rremove("y.text"),
          ncol = 3,
          widths = c(1.50, 1, 1))
dev.off()


