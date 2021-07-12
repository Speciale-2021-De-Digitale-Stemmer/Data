#############################
##### HYPOTHESIS 3 ####

#### LOAD DATA ####
# Facebook ad extended version
D <- readRDS("2_Clean/5_Analysis_I/Final_data/FB_VOTE_final.rds")

# Data on mandates per party
M <- read.xlsx("0_Datalister/Mandat-fordeling-parti.xlsx") %>%
  mutate(Parti_19 = recode(Parti, "Socialdemokratiet" = "A", "Radikale Venstre" = "B",
                           "Det Konservative Folkeparti" = "C", "Nye Borgerlige" = "D",
                           "SF - Socialistisk Folkeparti" = "F", "Liberal Alliance" = "I",
                           "Dansk Folkeparti" = "O", "Venstre, Danmarks Liberale Parti" = "V",
                           "Alternativet" = "Å", "Enhedslisten" = "Ø")) %>%
  mutate(Parti_15 = recode(Parti, "Socialdemokratiet" = "A", "Radikale Venstre" = "B",
                           "Det Konservative Folkeparti" = "C",
                           "SF - Socialistisk Folkeparti" = "F", "Liberal Alliance" = "I",
                           "Dansk Folkeparti" = "O", "Venstre, Danmarks Liberale Parti" = "V",
                           "Alternativet" = "Å", "Enhedslisten" = "Ø"))

# Data on mandates per constituency
C <- read.xlsx("0_Datalister/Mandat-fordeling-storkreds.xlsx") %>%
  mutate(Storkreds_2019 = storkreds)

# Load data on all votes
P <- readRDS("2_Clean/5_Analysis_I/antal_kandidater.rds")

# Party leaders and other controls
K <- read.xlsx("0_Datalister/Liste_Del_I.xlsx")

# When we need to remove parties from dataset
partier <- c("Socialdemokratiet", "Venstre, Danmarks Liberale Parti", "Det Konservative Folkeparti", "Dansk Folkeparti", "Enhedslisten", "SF", "Radikale Venstre", "Nye Borgerlige", "Liberal Alliance", "Alternativet", "Kristendemokraterne", "Veganerpartiet")

#### RECODE DATA VARIABLE ####
D$date <- as.Date(D$date, origin = "1899-12-30")

### STRATEGY AND LOCALISM ###
# Make predictions to numeric
D <- D %>%
  mutate(strategi = recode(prediction_repstil, "position" = 0,
                           "credit" = 1))

D <- D %>%
  mutate(lokalisme = recode(prediction_local, "not" = 0,
                            "localism" = 1))

#### ADD PARTILEDER INFORMATION ####
D <- left_join(D, K [ , c("Partileder", "Navn", "Minister", "Minister_2019", "Minister_2011_2019")], by = "Navn")

# Make NA == 0
D$Minister[is.na(D$Minister)] <- 0
D$Partileder[is.na(D$Partileder)] <- 0

#### Merge with colums on how many mandates per constituency ####
D <- merge(x = D, y = C[ , c("kredsmandatfv19", "tillaegsmandatfv19", "Storkreds_2019")], by = "Storkreds_2019", all.x = TRUE)

#### Remove parties ####
D <- filter(D, !Navn %in% partier)

#### Join how many mandates parties get ####
M <- M %>%
  mutate(mandater15 = kredsmandat15 + tillaegsmandat15) %>%
  mutate(mandater19 = kredsmandat19 + tillaegsmandat19)

# Give Leif Mikkelsen his party and constituency back
D$Parti_19[D$Navn == "Leif Mikkelsen"] <- "I"
D$Storkreds_2019[D$Navn == "Leif Mikkelsen"] <- "Vestjyllands Storkreds"

#### RECODE DATA VARIABLE ####
D$date <- as.Date(D$date, origin = "1899-12-30")

# Include variable pr month to be used in FE
D$month <- str_remove_all(D$date, "[:punct:]") # remove the "-" between the date-elements
D$month <- str_sub(D$month, 1, -3)

# Include variable pr parliament setting
D <- D %>%
  mutate(parliament_setting = ifelse(D$date >= "2019-06-04", 1, 0))


################################
###### BEFORE ELECTION ######

#### FILTER ONLY ADS BEFORE ELECTION ####
D1 <- D %>%
  filter(date <= "2019-06-04")

### STRATEGY AND LOCALISM ###
# Add variable with how many ads politicians have made
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(nads = n())

## CREATE WEIGHT FOR EACH AD ##
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(weightad = spend_upper/sum(spend_upper))

#### Calculate politicians weighted strategy ####
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(strategiw = weighted.mean(strategi, weightad))

D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(lokalismew = weighted.mean(lokalisme, weightad))

D1 <- D1 %>%
  mutate(bred = (strategiw+lokalismew)/2)

# Merge with amount of mandates per party
D1 <- merge(x = D1, y = M[ , c("Storkreds_2015", "mandater15", "Parti_15", "mandater19")], by = c("Storkreds_2015", "Parti_15"))

# Remove duplicates from dataset
D1 <- D1[!duplicated(D1$Navn), ]

#### RANK POLITICIANS ON ELECTION DATA ####
P15 <- P %>%
  filter(Valg == "29") %>%
  group_by(Parti, StorKredsNr) %>%
  mutate(mandatnr = rank(-PV))

# Join ranked data on D
D1 <- merge(x = D1, y = P15[ , c("Navn", "mandatnr", "Valg")])         

# Make interparty uncertainty
D1 <- D1 %>%
  mutate(interu = mandatnr/mandater15)

# Since this is before the election we need to set Ane Halsboe, Peter Hummelgaard and Trine Bramsen to Minister == 0
D1$Minister[D1$Navn == "Ane Halsboe"] <- 0
D1$Minister[D1$Navn == "Peter Hummelgaard"] <- 0
D1$Minister[D1$Navn == "Trine Bramsen"] <- 0

# Remove with too few observations
D1 <- D1 %>%
  filter(nads >= 3)


################################
###### AFTER ELECTION ######

#### FILTER ONLY ADS BEFORE ELECTION ####
D2 <- D %>%
  filter(date >= "2019-06-04")

### STRATEGY AND LOCALISM ###
# Add variable with how many ads politicians have made
D2 <- D2 %>%
  group_by(Navn) %>%
  mutate(nads = n())

## CREATE WEIGHT FOR EACH AD ##
D2 <- D2 %>%
  group_by(Navn) %>%
  mutate(weightad = spend_upper/sum(spend_upper))

#### Calculate politicians weighted strategy ####
D2 <- D2 %>%
  group_by(Navn) %>%
  mutate(strategiw = weighted.mean(strategi, weightad))

D2 <- D2 %>%
  group_by(Navn) %>%
  mutate(lokalismew = weighted.mean(lokalisme, weightad))

D2 <- D2 %>%
  mutate(bred = (strategiw+lokalismew)/2)

# Merge with amount of mandates per party
D2 <- merge(x = D2, y = M[ , c("Storkreds_2019", "mandater19", "Parti_19", "mandater15")], by = c("Storkreds_2019", "Parti_19"))

# Remove duplicates from dataset
D2 <- D2[!duplicated(D2$Navn), ]

#### RANK POLITICIANS ON ELECTION DATA ####
P19 <- P %>%
  filter(Valg == "275") %>%
  group_by(Parti, StorKredsNr) %>%
  mutate(mandatnr = rank(-PV))

# Join ranked data on D
D2 <- merge(x = D2, y = P19[ , c("Navn", "mandatnr", "Valg")])         

# Make interparty uncertainty
D2 <- D2 %>%
  mutate(interu = mandatnr/mandater19)

# Remove with too few observations
D2 <- D2 %>%
  filter(nads >= 3)


#### COMBINED FRAMES ####
# Combine frames
D3 <- rbind(D1, D2)

# Some politicians get into parlament even though they were not originally elected
# Therefore we need to change those with more than 1 in uncertainty to 1

D3 <- D3 %>% 
  filter(interu <= 5)

D3$interu[D3$interu > 1] <- 1 

#### FORUDSAETNINGSTEST ####
# Before we can test our hypothesis we need to check for outliers
### Test for outliers ###
# Strategy
outlier_strategi <- lm(strategiw ~ interu + year_in_parliament + Parti_19 + Storkreds_2019 + Minister + Partileder, data = D3)
cooksd_strategi <- cooks.distance(outlier_strategi)

plot(cooksd_strategi, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(cooksd_strategi, na.rm=T), col="red") +  # add cutoff line 
  text(x=1:length(cooksd_strategi)+1, y=cooksd_strategi, labels=ifelse(cooksd_strategi>4*mean(cooksd_strategi, na.rm=T),names(cooksd_strategi),""), col="red")  # add labels
# Katrine Robsoe, Karen Ellemann, Ole Birk Olesen

# Localism
outlier_lokalisme <- lm(lokalismew ~ interu + year_in_parliament + Parti_19 + Storkreds_2019 + Minister + Partileder, data = D3)
cooksd_lokalisme <- cooks.distance(outlier_lokalisme)

plot(cooksd_lokalisme, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(cooksd_lokalisme, na.rm=T), col="red") +  # add cutoff line 
  text(x=1:length(cooksd_lokalisme)+1, y=cooksd_lokalisme, labels=ifelse(cooksd_lokalisme>4*mean(cooksd_lokalisme, na.rm=T),names(cooksd_lokalisme),""), col="red")  # add labels
# Removes: Christian Poll, Kristian Thulesen Dahl (before election), Rasmus Nordqvist (because EP), Pia Kjaersgaard (before election),

### REMOVE OUTLIERS ###
## Remove outliers for strategy
outlier_strategi_names <- c("Katrine Robsøe", "Karen Ellemann", "Ole Birk")

D3_1 <- filter(D3, !Navn %in% outlier_strategi_names)

## Remove outliers for localism
# First the easy ones
outlier_lokalisme_names <- c("Christian Poll", "Rasmus Nordqvist")

D3_2 <- filter(D3, !Navn %in% outlier_lokalisme_names)

# Then Kristian Thulesen Dahl and Pia Kjaersgaard before election
D3_2 <- filter(D3_2, (Navn!="Kristian Thulesen" | Valg == "29"))
D3_2 <- filter(D3_2, (Navn!="Pia Kjærsgaard" | Valg == "29"))


### Check model with the performance package
model5 <- lm(strategiw ~ interu + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019, data = D3_1)
model5check <- check_model(model5)

model6 <- lm(lokalismew ~ interu + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019, data = D3_2)
model6check <- check_model(model6)

# Save the plots
pdf("../3_Figurer/61_H2_forudsætningA.pdf", height = 12, width = 12.5)
model5check
dev.off()

pdf("../3_Figurer/61_H2_forudsætningB.pdf", height = 12, width = 12.5)
model6check
dev.off()

#### HYPOTHESIS TESTING ####
#### HYPOTHESIS 3 ####
# Strategy
h3_1fe <- felm(strategiw ~ interu + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
               |factor(D3_1$parliament_setting) # Fixed effects for tid
               |0 # Ingen instrumentel variabel
               |factor(D3_1$Navn), # Clustering paa Navn
               data = D3_1) # Dataset for H2

summary(h3_1fe)

# Strategy without FE
h3_1 <- felm(strategiw ~ interu + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
             |0 # Fixed effects for tid
             |0 # Ingen instrumentel variabel
             |factor(D3_1$Navn), # Clustering paa Navn
             data = D3_1) # Dataset for H2

summary(h3_1)

h3_1intercept <- h3_1$beta[1]
h3_1slope <- h3_1$beta[2]

# Localism
h3_2fe <- felm(lokalismew ~ interu + year_in_parliament + Parti_19 + Storkreds_2019 + Minister + Partileder
               |factor(D3_2$parliament_setting) # Fixed effects for enheder
               |0 # Ingen instrumentel variabel
               |factor(D3_2$Navn), # Clustering paa Navn
               data = D3_2)# Dataset for H2

summary(h3_2fe)

# Localism without FE
h3_2 <- felm(lokalismew ~ interu + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
               |0 # Fixed effects for enheder
               |0 # Ingen instrumentel variabel
               |factor(D3_2$Navn), # Clustering paa Navn
               data = D3_2)# Dataset for H2

summary(h3_2)

h3_2intercept <- h3_2$beta[1]
h3_2slope <- h3_2$beta[2]

# Regression table to Latex - H2a
stargazer(h3_1, h3_2,
          covariate.labels = c("Interparti-usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Change in overleaf",
          add.lines = list(c("Parti", "Ja", "Ja"),
                           c("Storkreds", "Ja", "Ja"),
                           c("FE for tid", "Nej", "Nej")))

# Regression table to Latex - H2a robustness check
stargazer(h3_1fe, h3_2fe,
          covariate.labels = c("Interparti-usikkerhed", "Antal år i FT", "Minister", "Partileder"),
          column.labels = c("Model I", "Model II"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Change in overleaf",
          add.lines = list(c("Parti", "Ja", "Ja"),
                           c("Storkreds", "Ja", "Ja"),
                           c("FE for tid", "Ja", "Ja")))



#### HYPOTHESIS 4 ####
#### FORUDSAETNINGSTEST ####
# Before we can test our hypothesis we need to check for outliers
### Test for outliers ###
# Strategy
outlier_h3 <- lm(lokalismew ~ strategiw, data = D3)
cooksd_h3 <- cooks.distance(outlier_h3)

plot(cooksd_h3, pch="*", cex=2, main="Influential Obs by Cooks distance") +  # plot cook's distance
  abline(h = 4*mean(cooksd_h3, na.rm=T), col="red") +  # add cutoff line 
  text(x=1:length(cooksd_h3)+1, y=cooksd_h3, labels=ifelse(cooksd_h3>4*mean(cooksd_h3, na.rm=T),names(cooksd_h3),""), col="red")  # add labels
# Katrine Robsoe, Lars Aslan, Martin Henriksen

### REMOVE OUTLIERS ###
## Remove outliers
outlier_h3_names <- c("Katrine Robsøe", "Lars Aslan", "Martin Henriksen")

D3_3 <- filter(D3, !Navn %in% outlier_h3_names)

# Check regression with the performance package
model7 <- lm(lokalismew ~ strategiw, data = D3_3)
model7check <- check_model(model7)

# Save the plot
pdf("../3_Figurer/61_H3_forudsætning.pdf", height = 12, width = 12.5)
model7check
dev.off()

# Test hypothesis
h4_1 <- lm(lokalismew ~ strategiw, data = D3_3)
summary(h4_1)

ggplot(D3_3, aes(x = strategiw, y = lokalismew)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Regression table to Latex - H4
stargazer(h4_1,
          covariate.labels = "Lokalisme",
          column.labels = c("Model I"),
          type = "latex", digits = 3, style = "apsr",
          dep.var.labels = "Credit claiming")


#### PLOTS REGARDING VARIANCE ON UNCERTAINTY AND DEPENDENT VARIABLES ####
## Strategy
variance_strategi = D3 %>%
  ggplot(aes(x = interu, y = strategiw, color = Parti_19,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Interparti usikkerhed", y = "Andel credit claim") +
  geom_point() +
  theme_minimal()

ggplotly(variance_strategi, tooltip = "text")

## Localism
variance_lokalisme = D3 %>%
  ggplot(aes(x = interu, y = lokalismew, color = Parti_19,
             text = paste(
               "Politiker: ", Navn, "\n",
               "Parti: ", Parti_19, "\n",
               "Andel credit claiming: ", strategiw, "\n",
               "Andel lokalisme: ", lokalismew, "\n",
               "Antal ads: ", nads
             ))) +
  labs(x = "Interparti usikkerhed", y = "Andel lokalisme") +
  geom_point() +
  theme_minimal()

ggplotly(variance_lokalisme, tooltip = "text")


#### REGRESSION ON SCATTER PLOT ####
## Credit claiming
# Carey kredsmandat
VA1 <- D3_1 %>%
  ggplot(aes(x = interu, y = strategiw)) +
  labs(x = "Interparti-usikkerhed", y = "Andel credit claiming") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black")) +
  geom_abline(intercept = h3_1intercept, slope = h3_1slope) +
  ggtitle("Andel credit claiming")

# Hjorth kredsmandat
VA2 <- D3_1 %>%
  ggplot(aes(x = interu, y = lokalismew)) +
  labs(x = "Interparti-usikkerhed", y = "Andel lokalisme") +
  geom_point(color = "grey62") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black")) +
  geom_abline(intercept = h3_2intercept, slope = h3_2slope) +
  ggtitle("Andel lokalisme")

# Save as pdf
pdf("../3_Figurer/63_H3_scatter_and_slope.pdf", height = 7.5, width = 10)
ggarrange(VA1 + rremove("y.title"),
          VA2 + rremove("y.text") + rremove("y.title"),
          ncol = 2, 
          nrow = 1,
          widths = c(1.05, 1))
dev.off()



######################
#### FOR APPENDIX ####
## Inkrementel tilfoejelse af kontrolvariable til bilag
inkre_credit1 <- felm(strategiw ~ interu
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit1)

inkre_credit2 <- felm(strategiw ~ interu +  year_in_parliament
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit2)

inkre_credit3 <- felm(strategiw ~ interu +  year_in_parliament + Minister
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit3)

inkre_credit4 <- felm(strategiw ~ interu +  year_in_parliament + Minister + Partileder
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit4)

inkre_credit5 <- felm(strategiw ~ interu +  year_in_parliament + Minister + Partileder + Parti_19
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit5)

inkre_credit6 <- felm(strategiw ~ interu +  year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit6)

inkre_lokalisme1 <- felm(lokalismew ~ interu 
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme1)

inkre_lokalisme2 <- felm(lokalismew ~ interu + year_in_parliament
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme2)

inkre_lokalisme3 <- felm(lokalismew ~ interu + year_in_parliament + Minister
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme3)

inkre_lokalisme4 <- felm(lokalismew ~ interu + year_in_parliament + Minister + Partileder
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme4)

inkre_lokalisme5 <- felm(lokalismew ~ interu + year_in_parliament + Minister + Partileder + Parti_19
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme5)

inkre_lokalisme6 <- felm(lokalismew ~ interu + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme6)

## BILAG HYPOTHESIS 2 ##
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



#### ROBUSTHED ####
## Party
# Add vertical lines at the median for each party using
D3_1$Parti_19 <- factor(D3_1$Parti_19, levels=c("A", "B", "C", "D", "F", "I", "O", "V", "Ø", "Å"))
D3_2$Parti_19 <- factor(D3_2$Parti_19, levels=c("A", "B", "C", "D", "F", "I", "O", "V", "Ø"))

# Credit claiming
plot_robusthed1 <- D3_1 %>%
  ggplot(aes(x = strategiw, y = Parti_19, color = Parti_19)) + 
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        text = element_text(size = 16, color = "black")) +
  scale_color_manual(values = party.col) +
  xlab("Andel credit claiming") +
  ylab("") #+
  #scale_y_discrete(limits=rev)

tw <- D3_1
tw <- aggregate(tw$strategiw, by = list(Parti_19 = tw$Parti_19), mean)
tw$xend <- tw$x
tw$Parti_19 <- factor(tw$Parti_19, levels=c("A", "B", "C", "D", "F", "I", "O", "V", "Ø", "Å"))
tw <- tw[order(tw$Parti_19, tw$x),]
tw$p <- c(1,2,3,4,5,6,7,8,9,10)
tw$y <- as.numeric(tw$p) - 0.35
tw$yend <- as.numeric(tw$p) + 0.35
plot_robusthed1 <- plot_robusthed1 + geom_segment(data=tw, aes(x=x, xend=xend, y=y, yend=yend), size=0.25)

# Print to PDF
pdf("../3_Figurer/61_credit_party.pdf", height = 6, width = 9)
plot_robusthed1
dev.off()

# Localism
plot_robusthed2 <- D3_2 %>%
  ggplot(aes(x = lokalismew, y = Parti_19, color = Parti_19)) + 
  geom_point() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        text = element_text(size = 16, color = "black")) +
  scale_color_manual(values = party.col) +
  xlab("Andel lokalisme") +
  ylab("Parti")

tw <- D3_2
tw <- aggregate(tw$lokalismew, by = list(Parti_19 = tw$Parti_19), mean)
tw$xend <- tw$x
tw$Parti_19 <- factor(tw$Parti_19, levels=c("A", "B", "C", "D", "F", "I", "O", "V", "Ø"))
tw <- tw[order(tw$Parti_19, tw$x),]
tw$p <- c(1,2,3,4,5,6,7,8,9)
tw$y <- as.numeric(tw$p) - 0.35
tw$yend <- as.numeric(tw$p) + 0.35
plot_robusthed2 <- plot_robusthed2 + geom_segment(data=tw, aes(x=x, xend=xend, y=y, yend=yend), size=0.25)

# Print to PDF
pdf("../3_Figurer/61_localism_party.pdf", height = 6, width = 9)
plot_robusthed2
dev.off()

## Storkreds
# Credit claiming
plot_robusthed3 <- D3_1 %>%
  ggplot(aes(x = strategiw, y = Storkreds_2019, color = Storkreds_2019)) + 
  geom_point() +
  theme_bw() +
  #scale_color_manual(values = party.col) +
  xlab("Andel credit claiming") +
  ylab("Storkreds")

tw <- D3_1
tw <- aggregate(tw$strategiw, by = list(Storkreds_2019 = tw$Storkreds_2019), mean)
tw$xend <- tw$x
#tw$Storkreds_2019 <- factor(tw$Storkreds_2019, levels=c("Københavns Storkreds", "Københavns Omegns Storkreds", "Nordsjællands Storkreds", "Sjællands Storkreds", "Fyns Storkreds", "Sydjyllands Storkreds", "Østjyllands Storkreds", "Vestjyllands Storkreds", "Nordjyllands Storkreds"))
tw <- tw[order(tw$Storkreds_2019, tw$x),]
tw$p <- c(1,2,3,4,5,6,7,8,9)
tw$y <- as.numeric(tw$p) - 0.35
tw$yend <- as.numeric(tw$p) + 0.35
plot_robusthed3 <- plot_robusthed3 + geom_segment(data=tw, aes(x=x, xend=xend, y=y, yend=yend), size=0.25)

# localism
plot_robusthed4 <- D3_2 %>%
  ggplot(aes(x = lokalismew, y = Storkreds_2019, color = Storkreds_2019)) + 
  geom_point() +
  theme_bw() +
  #scale_color_manual(values = party.col) +
  xlab("Andel lokalisme") +
  ylab("Storkreds")

tw <- D3_2
tw <- aggregate(tw$lokalismew, by = list(Storkreds_2019 = tw$Storkreds_2019), mean)
tw$xend <- tw$x
#tw$Storkreds_2019 <- factor(tw$Storkreds_2019, levels=c("Københavns Storkreds", "Københavns Omegns Storkreds", "Nordsjællands Storkreds", "Sjællands Storkreds", "Fyns Storkreds", "Sydjyllands Storkreds", "Østjyllands Storkreds", "Vestjyllands Storkreds", "Nordjyllands Storkreds"))
tw <- tw[order(tw$Storkreds_2019, tw$x),]
tw$p <- c(1,2,3,4,5,6,7,8,9)
tw$y <- as.numeric(tw$p) - 0.35
tw$yend <- as.numeric(tw$p) + 0.35
plot_robusthed4 <- plot_robusthed4 + geom_segment(data=tw, aes(x=x, xend=xend, y=y, yend=yend), size=0.25)

## variance and standard deviation for politicans, parti og storkreds
