#######################################################
####### HYPOTHESIS 1a - intraparty competition  #######
## This script will preprocess data and run models in order to answer hypothesis 1a
## Visualisations and tables can be found in "14_Vis_for_H1.R"

###############################################
#### LOAD DATA AND PERFORM BASIC RECODING ####
###############################################
#### LOAD DATA
# Facebook ad extended version
D <- readRDS("2_Clean/5_Analysis_I/Final_data/FB_VOTE_final.rds") # dataframe with output from supervised machine learning models

D %>% 
  group_by(prediction_local) %>% 
  tally() %>% 
  mutate(share = n / sum(n))

D %>% 
  group_by(prediction_repstil) %>% 
  tally() %>% 
  mutate(share = n / sum(n))

# Data on mandates per party
M <- read.xlsx("0_Datalister/Mandat-fordeling-parti.xlsx")

# Data on mandates per constituency
C <- read.xlsx("0_Datalister/Mandat-fordeling-storkreds.xlsx") %>%
  mutate(Storkreds_2019 = storkreds)

# Data on politicians Facebook Ad Library ID
ID <- read.xlsx("0_Datalister/0312-List_FB_ID.xlsx") %>%
  mutate(page_id = Page.ID)

# Load data on all votes for elections in 2015 and 2019
P <- readRDS("2_Clean/5_Analysis_I/antal_kandidater.rds")

# Uncertainty
# In our first analysis we added a measurement for uncertainty in terms of competition for tillaegsmandat
# We found this to bring more theoretical troubles than answers why we chose not to include these in the final work
# Therefore they are muted in this script
U15 <- read.xlsx("0_Datalister/210506-usikkerhed.xlsx", sheet = "Sheet3") # Kredsmandat uncertainty
U19 <- read.xlsx("0_Datalister/210506-usikkerhed.xlsx", sheet = "Sheet1") # Kredsmandat uncertainty
#U15_t <- read.xlsx("0_Datalister/210506-usikkerhed.xlsx", sheet = "Sheet4") # Tillaegsmandat uncertainty
#19_t <- read.xlsx("0_Datalister/210506-usikkerhed.xlsx", sheet = "Sheet2") # Tillaegsmandat uncertainty

# Data on party leaders and other controls
K <- read.xlsx("0_Datalister/Liste_Del_I.xlsx")


#### Remove parties ####
partier <- c("Socialdemokratiet", "Venstre, Danmarks Liberale Parti", "Det Konservative Folkeparti", "Dansk Folkeparti", "Enhedslisten", "SF", "Radikale Venstre", "Nye Borgerlige", "Liberal Alliance", "Alternativet", "Kristendemokraterne", "Veganerpartiet")

D <- filter(D, !Navn %in% partier)


#### RECODE DATE VARIABLE ####
D$date <- as.Date(D$date, origin = "1899-12-30") # Microsoft Excel operates with origin 1899-12-30

# Originally we used fixed effects on a monthly basis but chose to leave this out due to theoretically reasons
# Therefore the following lines are muted
#D$month <- str_remove_all(D$date, "[:punct:]") # remove the "-" between the date-elements
#D$month <- str_sub(D$month, 1, -3)

# Include variable that informs us on which parliamentary setting the observation is in
D <- D %>%
  mutate(parliament_setting = ifelse(D$date >= "2019-06-04", 1, 0))

### COMMUNICATIONSTRATEGY AND LOCALISM ###
# Make predictions to numeric
D <- D %>%
  mutate(strategi = recode(prediction_repstil, "position" = 0,
                           "credit" = 1))

D <- D %>%
  mutate(lokalisme = recode(prediction_local, "not" = 0,
                            "localism" = 1))

#### ADD PARTYLEADER AND MINISTER INFORMATION ####
D <- left_join(D, K [ , c("Partileder", "Navn", "Minister", "Minister_2019", "Minister_2011_2019")], by = "Navn")

# Make NA == 0
D$Minister[is.na(D$Minister)] <- 0
D$Partileder[is.na(D$Partileder)] <- 0

#### Merge with colums on how many mandates there is available per constituency ####
D <- merge(x = D, y = C[ , c("kredsmandatfv19", "tillaegsmandatfv19", "Storkreds_2019")], by = "Storkreds_2019", all.x = TRUE)



#### Make wrongdoings in earlier coding right ####
# Remove Kasper Nordborg (F) since he was not elected
D <- D %>%
  filter(!Navn == "Kasper Nordborg")

# Give Leif Mikkelsen (I) his party and constituency back
D$Parti_19[D$Navn == "Leif Mikkelsen"] <- "I"
D$Storkreds_2019[D$Navn == "Leif Mikkelsen"] <- "Vestjyllands Storkreds"
D$Liste_sideordnet_2019[D$Navn == "Leif Mikkelsen"] <- "Sideordnet"

# Give Jeppe Kofod his party back
D$Parti_19[D$Navn == "Jeppe Kofod"] <- "A"

# "Folketingskandidat Henrik" party is V
D$Parti_19[D$Navn == "Folketingskandidat Henrik"] <- "V"

#### Set order of variables ####
# Order of parties
D$Parti_19 = factor(D$Parti_19, levels = c("A", "B", "C", "D", "F", "I", "O", "V", "Ø", "Å"))

#### Make variable for blok ####
D <- D %>%
  mutate(redblok = recode(Parti_19, "A" = TRUE, "B" = TRUE, "C" = FALSE, "D" = FALSE, "F" = TRUE, "I" = FALSE,
                       "O" = FALSE, "V" = FALSE, "Å" = TRUE, "Ø" = TRUE))


################################
###### MAKE CALCULATIONS ######
###### BEFORE ELECTION ######
###############################

#### FILTER ONLY ADS BEFORE ELECTION ####
D1 <- D %>%
  filter(date <= "2019-06-04") # We chose to say that election day did count as after election

#### STRATEGY AND LOCALISM including weight ####
# Add variable with how many ads each politician have made in total
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(nads = n())

# CREATE WEIGHT FOR EACH AD
# Weight is calculated from the relative price of the ad.
# That means that if a politician have spend 80 percent of his total spending on 1 ad, that ad will count for 80 percent
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(weightad = spend_upper/sum(spend_upper))

#### Calculate politicians weighted strategy ####
# We use weighted means to find the individual politician score
# "strategiw" is therefore weighted mean of part credit claiming
# "lokalismew" is therefore weighted mean of part localism
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(strategiw = weighted.mean(strategi, weightad))

D1 <- D1 %>%
  group_by(Storkreds_2015) %>%
  mutate(strategiws = weighted.mean(strategi, weightad))

D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(lokalismew = weighted.mean(lokalisme, weightad))

## At start we also performed analysis on credit claiming and localism together but found this to have little emprical relevance
## Therefore muted
#D1 <- D1 %>%
#  mutate(bred = (strategiw+lokalismew)/2)

# Remove duplicates from dataset
D1 <- D1[!duplicated(D1$Navn), ]

#### CALCULATE UNCERTAINTY FROM ELECTION RESULT ####
#### MERGE TO CALCULATE UNCERTAINTY ####
## Kredsmandat uncertainty
D1 <- merge(x = D1, y = U15, by = c("Parti_15", "Storkreds_2015")) 

# (carey-)Measurement 1 (from Carey paper)
D1 <- D1 %>%
  group_by(Navn) %>%
  mutate(intrau1 = 1-(PV_2015 - PV_not_elected)/Sum_PV)

# Make above 1 to 1 in carey estimate
# This is neccesary since some politicians get elected even though they don't get the most votes
D1$intrau1[D1$intrau1 > 1] <- 1

## Tillaegsmandat uncertainty
# MUTED SINCE NOT USED
#D1 <- merge(x = D1, y = U15_t, by = c("Parti_15", "Storkreds_2015")) 

# Measurement 1 (from Carey paper)
#D1 <- D1 %>%
#  group_by(Navn) %>%
#  mutate(intrau3 = 1-(PV_2015 - PV_not_elected_t)/Sum_PV_t)

# Make above 1 to 1 in carey estimate
#D1$intrau3[D1$intrau3 > 1] <- 1

## (Hjorth-)Measurement 2 (from Frederik Hjorth paper)
P15 <- P %>%
  filter(Valg == "29")

P15 <- P15 %>%
  group_by(StorKredsNr, Parti) %>%
  mutate(intrau2 = 1-PV/sum(PV))

D1 <- merge(x = D1, y = P15[ , c("Valg", "Navn", "intrau2")])

# Since this is before the election we need to set Ane Halsboe, Peter Hummelgaard and Trine Bramsen to Minister == 0
# These are the only new ministers that are included in the dataset
D1$Minister[D1$Navn == "Ane Halsboe"] <- 0
D1$Minister[D1$Navn == "Peter Hummelgaard"] <- 0
D1$Minister[D1$Navn == "Trine Bramsen"] <- 0

# Remove politicians with fewer than 3 ads to avoid outliers
D1 <- D1 %>%
  filter(nads >= 3)

# Save dataset where Enhedslisten is included
D6 <- D1

# Remove Enhedslisten since they don't have intraparty competition
D1 <- D1 %>%
  filter(!Parti_15 == "Ø")



################################
###### MAKE CALCULATIONS ######
###### AFTER ELECTION ########
##############################

## Comments are the same as previous step
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
  group_by(Storkreds_2015) %>%
  mutate(strategiws = weighted.mean(strategi, weightad))

D2 <- D2 %>%
  group_by(Navn) %>%
  mutate(lokalismew = weighted.mean(lokalisme, weightad))

#D2 <- D2 %>%
#  mutate(bred = (strategiw+lokalismew)/2)

# Remove duplicates from dataset
D2 <- D2[!duplicated(D2$Navn), ]

#### CALCULATE UNCERTAINTY OF ELECTION ####
#### MERGE TO CALCULATE UNCERTAINTY ####
## Kredsmandat uncertainty
D2 <- merge(x = D2, y = U19, by = c("Parti_19", "Storkreds_2019"))

# Measurement 1 (from Carey paper)
D2 <- D2 %>%
  group_by(Navn) %>%
  mutate(intrau1 = 1-(PV_2019 - PV_not_elected)/Sum_PV)

# Make above 1 to 1 in carey estimate
D2$intrau1[D2$intrau1 > 1] <- 1

## Tillaegsmandat uncertainty
#D2 <- merge(x = D2, y = U19_t, by = c("Parti_19", "Storkreds_2019")) 

# Measurement 1 (from Carey paper)
#D2 <- D2 %>%
#  group_by(Navn) %>%
#  mutate(intrau3 = 1-(PV_2019 - PV_not_elected_t)/Sum_PV_t)

# Make above 1 to 1 in carey estimate
#D2$intrau3[D2$intrau3 > 1] <- 1

## Measurement 2 (from Frederik Hjorth paper)
P19 <- P %>%
  filter(Valg == "275")

P19 <- P19 %>%
  group_by(StorKredsNr, Parti) %>%
  mutate(intrau2 = 1-PV/sum(PV))

D2 <- merge(x = D2, y = P19[ , c("Valg", "Navn", "intrau2")])

# Remove politicians with fewer than 3 ads to avoid outliers
D2 <- D2 %>%
  filter(nads >= 3)

# Save dataset where Enhedslisten is included
D7 <- D2

# Remove Enhedslisten since they don't have intraparty competition
D2 <- D2 %>%
  filter(!Parti_19 == "Ø")



#########################
#### COMBINED FRAMES ####
#########################
# Combine frames
D3 <- rbind(D1, D2) # For hypothesis 1
D5 <- rbind(D6, D7) # For total dataset where Enhedslisten is included

D5$Blok <- factor(
  recode(
    as.numeric(
      D5$redblok), "0" = "Højrefløjen", "1" = "Venstrefløjen"), 
  levels = c("Venstrefløjen", "Højrefløjen"))

#### FORUDSAETNINGSTEST ####
# Before we can test our hypothesis we need to check for outliers
### Test for outliers ###
D3_f <- D3

# Strategy
outlier_strategi <- lm(strategiw ~ intrau2 + year_in_parliament + Parti_19 + Storkreds_2019 + Minister + Partileder, data = D3)
D3_f$cooksd_strategi <- cooks.distance(outlier_strategi)
D3_f$nrownames <- row.names(D3_f)

# Visualise outliers
outliers_1 = ggplot(data = D3_f, aes(x = desc(nrownames), y = cooksd_strategi)) +
  geom_point() +
  geom_hline(yintercept = 4*mean(D3_f$cooksd_strategi), linetype = "dashed") +
  geom_text(aes(label=ifelse(cooksd_strategi>4*mean(cooksd_strategi),as.character(Navn),'')),
            hjust=-0.1,vjust=-0.1, size = 4.5) +
  coord_cartesian(clip = 'off') +
  xlab("") +
  ylab("Cooks D, kommunikationsstrategi ~ usikkerhed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16, color = "black"))

# Print to PDF
pdf("../3_Figurer/45_outlier_credit.pdf", height = 6, width = 9)
outliers_1
dev.off()

# We go through all outliers manually to see if they have been categorised correctly
# We find five politicians who have been categorised wrongly:
# "Katrine Robsøe", "Karen Ellemann", "Lars Aslan", "Ole Birk", "Daniel Toft"

# Localism
outlier_lokalisme <- lm(lokalismew ~ intrau1 + year_in_parliament + Parti_19 + Storkreds_2019 + Minister + Partileder, data = D3)
D3_f$cooksd_lokalisme <- cooks.distance(outlier_lokalisme)

# Visualise outliers
outliers_2 = ggplot(data = D3_f, aes(x = desc(nrownames), y = cooksd_lokalisme)) +
  geom_point() +
  geom_hline(yintercept = 4*mean(D3_f$cooksd_lokalisme), linetype = "dashed") +
  # geom_text(aes(label=ifelse(cooksd_lokalisme>4*mean(cooksd_lokalisme),as.character(Navn),'')),
  #           hjust=-0.1,vjust=-0.1, size = 4.5) +
  geom_label_repel(aes(label=ifelse(cooksd_lokalisme>4*mean(cooksd_lokalisme),as.character(Navn),'')), 
                   label.size = NA, size = 4.5, box.padding = 1,
                   max.overlaps = 1000) +
  coord_cartesian(clip = 'off') +
  xlab("") +
  ylab("Cooks D, lokalisme ~ usikkerhed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.position = "none",
        axis.text = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18, color = "black"))

# Print to PDF
pdf("../3_Figurer/45_outlier_lokalisme.pdf", height = 7, width = 12)
outliers_2
dev.off()

# We go through all outliers manually to see if they have been categorised correctly
# We find two politicians who have been categorised wrongly and three observations that need to go out:
# Removes: Christian Poll, Kristian Thulesen Dahl (before election), Rasmus Nordqvist (because EP), Pia Kjaersgaard (before election),
# Katrine Robsoee,

### REMOVE OUTLIERS ###
## Remove outliers for strategy
outlier_strategi_names <- c("Katrine Robsøe", "Karen Ellemann", "Lars Aslan", "Ole Birk", "Heidi Bank", "Daniel Toft")

D3_1 <- filter(D3, !Navn %in% outlier_strategi_names)

D5_1 <- filter(D5, !Navn %in% outlier_strategi_names)

# Save data for analysis Del II
#saveRDS(D3_1, "2_Clean/6_Analysis_II/Final_data/FB.rds")

## Remove outliers for localism
# First the easy ones
outlier_lokalisme_names <- c("Christian Poll", "Katrine Robsøe", "Rasmus Nordqvist")

D3_2 <- filter(D3, !Navn %in% outlier_lokalisme_names)

# Then Kristian Thulesen Dahl and Pia Kjaersgaard before election
D3_2 <- filter(D3_2, (Navn!="Kristian Thulesen" | Valg == "29"))
D3_2 <- filter(D3_2, (Navn!="Pia Kjærsgaard" | Valg == "29"))



### CHECK THE MODELS WITH THE PERFORMANCE PACKAGE
model1 <- lm(strategiw ~ intrau1 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019, data = D3_1)
model1check <- check_model(model1)

model2 <- lm(strategiw ~ intrau2 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019, data = D3_1)
model2check <- check_model(model2)

model3 <- lm(lokalismew ~ intrau1 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019, data = D3_2)
model3check <- check_model(model3)

model4 <- lm(lokalismew ~ intrau2 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019, data = D3_2)
model4check <- check_model(model4)

# Save the plots
pdf("../3_Figurer/61_H1_forudsætningA.pdf", height = 12, width = 12.5)
model1check
dev.off()

pdf("../3_Figurer/61_H1_forudsætningB.pdf", height = 12, width = 12.5)
model2check
dev.off()

pdf("../3_Figurer/61_H1_forudsætningC.pdf", height = 12, width = 12.5)
model3check
dev.off()

pdf("../3_Figurer/61_H1_forudsætningD.pdf", height = 12, width = 12.5)
model4check
dev.off()


#############################
#### HYPOTEHESIS TESTING ####
#### HYPOTHESIS 1a - intraparty ##########
###############################

## Strategy
# Carey uncertainty measurement kredsmandat including fixed effects
h2_1_1fe <- felm(strategiw ~ intrau1 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
               |factor(D3_1$parliament_setting) # Fixed effects for time
               |0 # No instrumental variable
               |factor(D3_1$Navn), # Clustering on politician
               data = D3_1) # Dataset for H1

summary(h2_1_1fe)

# Carey uncertainty measurement kredsmandat without FE
h2_1_1 <- felm(strategiw ~ intrau1 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
               |0 
               |0 
               |factor(D3_1$Navn), 
               data = D3_1) 

summary(h2_1_1)

# Save intercept and slope values for visualisation
h2_1_1_intercept <- h2_1_1$beta[1]
h2_1_1_slope <- h2_1_1$beta[2]

# Hjorth uncertainty measurement including fixed effects
h2_2_1fe <- felm(strategiw ~ intrau2 +  year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019 
               |factor(D3_1$parliament_setting) 
               |0 
               |factor(D3_1$Navn), 
               data = D3_1) 

summary(h2_2_1fe)

# Hjorth uncertainty measurement without FE
h2_2_1 <- felm(strategiw ~ intrau2 +  year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019 
               |0 
               |0 
               |factor(D3_1$Navn),
               data = D3_1)

summary(h2_2_1)

# Save intercept and slope values for visualisation
h2_2_1_intercept <- h2_2_1$beta[1]
h2_2_1_slope <- h2_2_1$beta[2]


# Carey uncertainty measurement tillaegsmandat including fixed effects
#h2_3_1fe <- felm(strategiw ~ intrau3 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
#               |factor(D3_1$parliament_setting) 
#               |0 
#               |factor(D3_1$Navn), 
#               data = D3_1) 

#summary(h2_3_1fe)

# Carey uncertainty measurement tillaegsmandat without FE
#h2_3_1 <- felm(strategiw ~ intrau3 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
#               |0 
#               |0 
#               |factor(D3_1$Navn), 
#               data = D3_1) 

#summary(h2_3_1)

## Localism
# Carey uncertainty measurement kredsmandat including fixed effects
h2_1_2fe <- felm(lokalismew ~ intrau1 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019 
               |factor(D3_2$month) 
               |0 
               |factor(D3_2$Navn), 
               data = D3_2)

summary(h2_1_2fe)

# Carey uncertainty measurement kredsmandat without FE
h2_1_2 <- felm(lokalismew ~ intrau1 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019 
               |0 
               |0 
               |factor(D3_2$Navn), 
               data = D3_2)

summary(h2_1_2)

# Save intercept and slope values for visualisation
h2_1_2_intercept <- h2_1_2$beta[1]
h2_1_2_slope <- h2_1_2$beta[2]

# Hjorth uncertainty measurement including fixed effects
h2_2_2fe <- felm(lokalismew ~ intrau2 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
               |factor(D3_2$month) 
               |0 
               |factor(D3_2$Navn),
               data = D3_2) 

summary(h2_2_2fe)

# Hjorth uncertainty measurement without FE
h2_2_2 <- felm(lokalismew ~ intrau2 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
               |0 
               |0 
               |factor(D3_2$Navn),
               data = D3_2) 

summary(h2_2_2)

# Save intercept and slope values for visualisation
h2_2_2_intercept <- h2_2_2$beta[1]
h2_2_2_slope <- h2_2_2$beta[2]

# Carey uncertainty measurement tillaegsmandat including fixed effects
#h2_3_2fe <- felm(lokalismew ~ intrau3 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
#               |factor(D3_2$month) 
#               |0 
#               |factor(D3_2$Navn), 
#               data = D3_2)

#summary(h2_3_2fe)

# Carey uncertainty measurement tillaegsmandat without FE
#h2_3_2 <- felm(lokalismew ~ intrau3 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
#               |0 
#               |0 
#               |factor(D3_2$Navn), 
#               data = D3_2)

#summary(h2_3_2)



#########################
#### FOR SCATTERPLOT ####
#### USED FOR GENERATING IDEAS ####
########################

#### CREATE A NEW DATAFRAME ####
D4 <- D

### STRATEGY AND LOCALISM ###
# Add variable with how many ads politicians have made
D4 <- D4 %>%
  group_by(Navn) %>%
  mutate(nads = n())

## CREATE WEIGHT FOR EACH AD ##
D4 <- D4 %>%
  group_by(Navn) %>%
  mutate(weightad = spend_upper/sum(spend_upper))

#### Calculate politicians weighted strategy ####
D4 <- D4 %>%
  group_by(Navn) %>%
  mutate(strategiw = weighted.mean(strategi, weightad))

D4 <- D4 %>%
  group_by(Navn) %>%
  mutate(lokalismew = weighted.mean(lokalisme, weightad))

D4 <- D4 %>%
  mutate(bred = strategiw+lokalismew)

# Make dataframe without duplicates
D4 <- D4[!duplicated(D4$Navn), ]

# Remove with less than three ads. Removes 4 observations. Still 133 politicians
D4 <- D4 %>%
  filter(nads >= 3)

# Round decimals
D4$strategim <- round(D4$strategiw, digits = 2)
D4$lokalismem <- round(D4$lokalismew, digits = 2)


#################################
#### DIFFERENT VALIDITY TESTS ####
#################################

#### TEST FOR DIFFERENCES BETWEEN AMOUNT OF ADS ####
# Are there any differences between politicians with few ads and those with many?
D_test1 <- D3 %>%
  filter(nads <= 10)

D_test2 <- D3 %>%
  filter(nads >= 11)

t.test(D_test1$strategiw, D_test2$strategiw) # no difference
t.test(D_test1$lokalismew, D_test2$lokalismew) # no difference

D_test1 <- D3 %>%
  filter(nads <= 8)

D_test2 <- D3 %>%
  filter(nads >= 9)

t.test(D_test1$strategiw, D_test2$strategiw) # no difference
t.test(D_test1$lokalismew, D_test2$lokalismew) # no difference

D_test1 <- D3 %>%
  filter(nads <= 6)

D_test2 <- D3 %>%
  filter(nads >= 7)

t.test(D_test1$strategiw, D_test2$strategiw) # no difference
t.test(D_test1$lokalismew, D_test2$lokalismew) # no difference

D_test1 <- D3 %>%
  filter(nads <= 4)

D_test2 <- D3 %>%
  filter(nads >= 5)

t.test(D_test1$strategiw, D_test2$strategiw) # no difference
t.test(D_test1$lokalismew, D_test2$lokalismew) # no difference



#############################
#### FOR METHODS CHAPTER ####

#### DATA SECTION

#### OPERATIONALISERINGS SECTION
# Time (for fixed effects argument)
D %>%
  group_by(month) %>%
  summarise(monthvalue = mean(strategi)) %>%
  ggplot(aes(x = month, y = monthvalue)) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = mean(D$strategi), linetype = "dashed") +
  ylab("Andel credit claim i Facebook-annoncer") +
  xlab("Måned fra april 2019 til april 2021") +
  theme_minimal()

D %>%
  group_by(month) %>%
  summarise(monthvalue = mean(lokalisme)) %>%
  ggplot(aes(x = month, y = monthvalue)) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = mean(D$lokalisme), linetype = "dashed") +
  ylab("Andel lokalisme i Facebook-annoncer") +
  xlab("Måned fra april 2019 til april 2021") +
  theme_minimal()

# Party
D3_1 %>%
  group_by(Parti_19) %>%
  summarise(parti_credit = mean(strategiw)) %>%
  ggplot(aes(x = reorder(Parti_19, parti_credit), y = parti_credit)) +
  geom_bar(stat = "Identity") +
  #geom_hline(yintercept = mean(D$lokalisme), linetype = "dashed") +
  ylab("Andel credit i Facebook-annoncer") +
  xlab("Parti") +
  theme_minimal() +
  coord_flip()

# Storkreds
D3_1 %>%
  group_by(Storkreds_2019) %>%
  summarise(storkreds_credit = mean(strategiw)) %>%
  ggplot(aes(x = reorder(Storkreds_2019, storkreds_credit), y = storkreds_credit)) +
  geom_bar(stat = "Identity") +
  #geom_hline(yintercept = mean(D$lokalisme), linetype = "dashed") +
  ylab("Gennemsnitlig andel credit for politikere i storkredsen") +
  xlab("Storkreds") +
  theme_minimal() +
  coord_flip()

D3_2 %>%
  group_by(Storkreds_2019) %>%
  summarise(storkreds_lokalisme = mean(lokalismew)) %>%
  ggplot(aes(x = reorder(Storkreds_2019, storkreds_lokalisme), y = storkreds_lokalisme)) +
  geom_bar(stat = "Identity") +
  #geom_hline(yintercept = mean(D$lokalisme), linetype = "dashed") +
  ylab("Gennemsnitlig andel lokalisme for politikere i storkredsen") +
  xlab("Storkreds") +
  theme_minimal() +
  coord_flip()

# Ministre
D3_1 %>%
  group_by(Minister) %>%
  summarise(minister_credit = mean(strategiw)) %>%
  ggplot(aes(x = Minister, y = minister_credit)) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = mean(D3_1$strategiw), linetype = "dashed") +
  ylab("Gennemsnitlig andel credit for ministre") +
  xlab("Minister") +
  theme_minimal()

D3_2 %>%
  group_by(Minister) %>%
  summarise(minister_lokalisme = mean(lokalismew)) %>%
  ggplot(aes(x = Minister, y = minister_lokalisme)) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = mean(D3_2$lokalismew), linetype = "dashed") +
  ylab("Gennemsnitlig andel lokalisme for ministre") +
  xlab("Minister") +
  theme_minimal()

# Party leader
D3_1 %>%
  group_by(Partileder) %>%
  summarise(partileder_credit = mean(strategiw)) %>%
  ggplot(aes(x = Partileder, y = partileder_credit)) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = mean(D3_1$strategiw), linetype = "dashed") +
  ylab("Gennemsnitlig andel credit for partiledere") +
  xlab("Partileder") +
  theme_minimal()

D3_2 %>%
  group_by(Partileder) %>%
  summarise(partileder_lokalisme = mean(lokalismew)) %>%
  ggplot(aes(x = Partileder, y = partileder_lokalisme)) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = mean(D3_1$lokalismew), linetype = "dashed") +
  ylab("Gennemsnitlig andel lokalisme for partiledere") +
  xlab("Partileder") +
  theme_minimal()

#########################
#### APPENDIX ####
## Inkrementel adding controlvariables
inkre_credit1 <- felm(strategiw ~ intrau2
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit1)

inkre_credit2 <- felm(strategiw ~ intrau2 +  year_in_parliament
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit2)

inkre_credit3 <- felm(strategiw ~ intrau2 +  year_in_parliament + Minister
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit3)

inkre_credit4 <- felm(strategiw ~ intrau2 +  year_in_parliament + Minister + Partileder
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit4)

inkre_credit5 <- felm(strategiw ~ intrau2 +  year_in_parliament + Minister + Partileder + Parti_19
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit5)

inkre_credit6 <- felm(strategiw ~ intrau2 +  year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
                      |0 # Fixed effects for tid
                      |0 # Ingen instrumentel variabel
                      |factor(D3_1$Navn), # Clustering paa Navn
                      data = D3_1) # Dataset for H2

summary(inkre_credit6)

inkre_lokalisme1 <- felm(lokalismew ~ intrau2 
               |0 # Fixed effects for enheder
               |0 # Ingen instrumentel variabel
               |factor(D3_2$Navn), # Clustering paa Navn
               data = D3_2) # Dataset for H2

summary(inkre_lokalisme1)

inkre_lokalisme2 <- felm(lokalismew ~ intrau2 + year_in_parliament
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme2)

inkre_lokalisme3 <- felm(lokalismew ~ intrau2 + year_in_parliament + Minister
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme3)

inkre_lokalisme4 <- felm(lokalismew ~ intrau2 + year_in_parliament + Minister + Partileder
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme4)

inkre_lokalisme5 <- felm(lokalismew ~ intrau2 + year_in_parliament + Minister + Partileder + Parti_19
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme5)

inkre_lokalisme6 <- felm(lokalismew ~ intrau2 + year_in_parliament + Minister + Partileder + Parti_19 + Storkreds_2019
                         |0 # Fixed effects for enheder
                         |0 # Ingen instrumentel variabel
                         |factor(D3_2$Navn), # Clustering paa Navn
                         data = D3_2) # Dataset for H2

summary(inkre_lokalisme6)

