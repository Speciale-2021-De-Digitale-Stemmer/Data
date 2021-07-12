####################
## VISUALIZATION ##
###################

##############################
#### FOR METHODS CHAPTER ####

#### PLOTS REGARDING UNCERTAINTY MEASURES ####
uncertainty_plot = D3 %>%
  ggplot(aes(x = intrau1, y = intrau2,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Carey&Shugart-målet", y = "Hjorth-målet") +
  geom_point() +  
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"))

ggplotly(uncertainty_plot, tooltip = "text")

# Print to PDF
pdf("../3_Figurer/42_uncertainty.pdf", height = 6, width = 9)
uncertainty_plot
dev.off()


#### PLOTS REGARDING VARIANCE ON UNCERTAINTY AND DEPENDENT VARIABLES ####
# Credit claiming
variance_strategi_1 = D3_1 %>%
  ggplot(aes(x = intrau1, y = strategiw, color = Parti_19,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Usikkerhed Carey-mål på kredsmandat", y = "Andel credit claim") +
  geom_point() +
  scale_color_manual(values = party.col) +
  theme_minimal()

ggplotly(variance_strategi_1, tooltip = "text")


variance_strategi_2 = D3_1 %>%
  ggplot(aes(x = intrau2, y = strategiw, color = Parti_19,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Usikkerhed Hjorth-mål", y = "Andel credit claim") +
  geom_point() +
  scale_color_manual(values = party.col) +
  theme_minimal()

ggplotly(variance_strategi_2, tooltip = "text")


## Localism
variance_lokalisme_1 = D3_2 %>%
  ggplot(aes(x = intrau1, y = lokalismew, color = Parti_19,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Usikkerhed Carey-mål på kredsmandat", y = "Andel lokalisme") +
  geom_point() +
  scale_color_manual(values = party.col) +
  theme_minimal()

ggplotly(variance_lokalisme_1, tooltip = "text")


variance_lokalisme_2 = D3_2 %>%
  ggplot(aes(x = intrau2, y = lokalismew, color = Parti_19,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Usikkerhed Hjorth-mål", y = "Andel credit claim") +
  geom_point() +
  scale_color_manual(values = party.col) +
  theme_minimal()

ggplotly(variance_lokalisme_2, tooltip = "text")



##############################
#### FOR ANALYSIS ####

#### SCATTER PLOT ####
# Plot
plot_grande <- D5_1 %>%
  ggplot(aes(x = strategiw, y = lokalismew,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Andel credit claiming", y = "Andel lokalisme") +
  geom_point() + #aes(size = nads)) +
  scale_color_manual(values = party.col) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        text = element_text(size = 16, color = "black"))

# Interactive plot
ggplotly(plot_grande, tooltip = "text")

# Print to PDF
pdf("../3_Figurer/61_big_plot.pdf", height = 6, width = 9)
plot_grande
dev.off()


#### REGRESSION TABLES H2 ####
# Regression table to Latex - H2a_repstil
stargazer(h2_1_1, h2_2_1, h2_1_1fe, h2_2_1fe,
          covariate.labels = c("Carey usikkerhed", "Hjorth usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Brug af credit claiming",
          add.lines = list(c("Parti", "Ja", "Ja", "Ja", "Ja"),
                           c("Storkreds", "Ja", "Ja", "Ja", "Ja"),
                           c("FE for tid", "Nej", "Nej", "Ja", "Ja")))

# Regression table to Latex - H2a_lokalisme
stargazer(h2_1_2, h2_2_2, h2_1_2fe, h2_2_2fe,
          covariate.labels = c("Carey usikkerhed", "Hjorth usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Brug af lokalisme",
          add.lines = list(c("Parti", "Ja", "Ja", "Ja", "Ja"),
                           c("Storkreds", "Ja", "Ja", "Ja", "Ja"),
          c("FE for tid", "Nej", "Nej", "Ja", "Ja")))

# Regression table to Latex - H1 strategy and localism
stargazer(h2_1_1, h2_2_1, h2_1_2, h2_2_2,
          covariate.labels = c("Carey usikkerhed", "Hjorth usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Credit claiming",
          add.lines = list(c("Parti", "Ja", "Ja", "Ja", "Ja"),
                           c("Storkreds", "Ja", "Ja", "Ja", "Ja"),
                           c("FE for tid", "Nej", "Nej", "Nej", "Nej")))

#### REGRESSION IN ROBUSTNESS CHECK ####
stargazer(h2_1_1fe, h2_2_1fe, h2_1_2fe, h2_2_2fe,
          covariate.labels = c("Carey usikkerhed", "Hjorth usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Change in overleaf",
          add.lines = list(c("Parti", "Ja", "Ja", "Ja", "Ja"),
                           c("Storkreds", "Ja", "Ja", "Ja", "Ja"),
                           c("FE for tid", "Ja", "Ja", "Ja", "Ja")))



#### REGRESSION ON SCATTER PLOT ####
## Credit claiming
# Carey kredsmandat
VA1 <- D3_1 %>%
  ggplot(aes(x = intrau1, y = strategiw)) +
  labs(x = "Intraparti-usikkerhed (Carey & Shugart-mål)", y = "Andel credit claiming") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black")) +
  geom_abline(intercept = h2_1_1_intercept, slope = h2_1_1_slope) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.01,
                                   decimal.mark = '.')) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "0.25", "0.50", "0.75", "1"))

# Hjorth kredsmandat
VA2 <- D3_1 %>%
  ggplot(aes(x = intrau2, y = strategiw)) +
  labs(x = "Intraparti-usikkerhed (Hjorth-mål)", y = "Andel credit claim") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black")) +
  geom_abline(intercept = h2_2_1_intercept, slope = h2_2_1_slope)

## Localism
# Carey kredsmandat
VA3 <- D3_2 %>%
  ggplot(aes(x = intrau1, y = lokalismew)) +
  labs(x = "Intraparti-usikkerhed (Carey & Shugart-mål)", y = "Andel lokalisme") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black")) +
  geom_abline(intercept = h2_1_2_intercept, slope = h2_1_2_slope) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "0.25", "0.50", "0.75", "1"))

# Hjorth kredsmandat
VA4 <- D3_2 %>%
  ggplot(aes(x = intrau2, y = lokalismew)) +
  labs(x = "Intraparti-usikkerhed (Hjorth-mål)", y = "Andel lokalisme") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        text = element_text(size = 16, color = "black"),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 18, color = "black")) +
geom_abline(intercept = h2_2_2_intercept, slope = h2_2_2_slope) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "0.25", "0.50", "0.75", "1"))

# Save all four figures in one plot
pdf("../3_Figurer/62_H2_scatter_carey_hjorth.pdf", height = 10, width = 14)
ggarrange(VA1 + rremove("x.text") + rremove("x.title"),
          VA2 + rremove("x.text") + rremove("y.text") + rremove("x.title") + rremove("y.title"),
          VA3,
          VA4 + rremove("y.text") + rremove("y.title"),
          ncol = 2, 
          nrow = 2, 
          widths = c(1.1, 1))
dev.off()

#### DIFFERENCES BETWEEN INTRA-PARTY AND INTER-PARTY UNCERTAINTY ####
# In order to obtain these values you need to run 13_H1 first, then run the first two lines of code,
# Then run 13_H2 and then run the following two lines of code. 
UN1 <- D1 %>%
  select(Navn, intrau1, intrau2, strategiw)

UN2 <- D2 %>%
  select(Navn, intrau1, intrau2, strategiw)

UN1 <- merge(x = UN1, y = D1 [ , c("Navn", "interu")], by = "Navn")

UN2 <- merge(x = UN2, y = D2 [ , c("Navn", "interu")], by = "Navn")

UN1 <- UN1 %>%
  mutate(scoredifcarey = intrau1-interu)

UN1 <- UN1 %>%
  mutate(scoredifhjorth = intrau2-interu)

UN2 <- UN2 %>%
  mutate(scoredifcarey = intrau1-interu)

UN2 <- UN2 %>%
  mutate(scoredifhjorth = intrau2-interu)

UN11 <- gather(UN1, "score", "value", intrau1, interu)

#UN11 <- UN11 %>%
#  filter(value <= 1)

UN11$value[UN11$value > 1] <- 1

UN11 <- UN11 %>%
  mutate(scoredifcareyplus = ifelse(UN11$scoredifcarey >= 0, 1, 0))

UN11 %>%
  ggplot(aes(x = reorder(Navn, scoredifcarey), y = value, color = score)) +
  facet_wrap(~ scoredifcareyplus, scales = "free", ncol = 2) +
  geom_point() + 
  theme_minimal() +
  scale_color_manual(values = standard.col) +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00)) +
  coord_flip()

UN22 <- gather(UN2, "score", "value", intrau1, interu)

#UN22 <- UN22 %>%
#  filter(value <= 1)

UN22$value[UN22$value > 1] <- 1

UN22 <- UN22 %>%
  mutate(scoredifcareyplus = ifelse(UN22$scoredifcarey >= 0, 1, 0))

recodenames <- c("Martin Henriksen", "Mikkel Dencker", "Laura Lindahl", "Christina Egelund")

UN22 <- filter(UN22, !Navn %in% recodenames)

UN22$scoredifcarey2 <- ifelse(UN22$scoredifcarey < 0, UN22$scoredifcarey*(-1), UN22$scoredifcarey)

politicians_uncertainty = ggplot(UN22, aes(x = reorder(Navn, scoredifcarey2), y = value, color = score)) +
  facet_wrap(~ scoredifcareyplus, scales = "free", ncol = 2) +
  geom_point() + 
  theme_minimal() +
  xlab("") +
  ylab("") +
  scale_color_manual(values = standard.col, labels = c("Interparti-usikkerhed", "Intraparti-usikkerhed")) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "0.25", "0.50", "0.75", "1")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
                                  strip.text.x = element_blank(),
        text = element_text(size = 14, color = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 16, color = "black")) +
  xlab("") +
  ylab("Elektoral usikkerhed") +
  coord_flip()

# Print to PDF
pdf("../3_Figurer/63_inter-intra-party-politicians.pdf", height = 10, width = 14)
politicians_uncertainty
dev.off()

#########################
#### APPENDIX ####

## BILAG HYPOTHESIS 1 ##
# Regression table to Latex - credit claiming
stargazer(inkre_credit1, inkre_credit2, inkre_credit3, inkre_credit4, inkre_credit5, inkre_credit6,
          covariate.labels = c("Hjorth usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV", "Model V", "Model VI"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Brug af lokalisme",
          add.lines = list(c("Parti", "Nej", "Nej", "Nej", "Nej", "Ja", "Ja"),
                           c("Storkreds", "Nej", "Nej", "Nej", "Nej", "Nej", "Ja")))

# Regression table to Latex - localism
stargazer(inkre_lokalisme1, inkre_lokalisme2, inkre_lokalisme3, inkre_lokalisme4, inkre_lokalisme5, inkre_lokalisme6,
          covariate.labels = c("Hjorth usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV", "Model V", "Model VI"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Brug af lokalisme",
          add.lines = list(c("Parti", "Nej", "Nej", "Nej", "Nej", "Ja", "Ja"),
                           c("Storkreds", "Nej", "Nej", "Nej", "Nej", "Nej", "Ja")))
