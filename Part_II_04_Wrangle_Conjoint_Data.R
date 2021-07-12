
#-------------------------------------------------------------------------
# CLEAN CONJOINT DATA
#-------------------------------------------------------------------------

# Read data
D <- readRDS("2_Clean/5_Analysis_I/Final_data/FB_VOTE.rds")
data_recode_raw <- readRDS("2_Clean/6_Analysis_II/Conjoint.rds")

# Recode variable containing background information
A <- str_split(data_recode_raw$bagg3_o1, ";")
B <- data.frame(do.call(rbind, A)) %>% 
  select(postnr = X1, by = X3, kreds_rec = X5, region = X7) %>% 
  mutate(postnr = as.numeric(postnr))

# Join with raw data
data_recode <- cbind(data_recode_raw, B)

# Select conjoint data
C <- data_recode %>% 
  select(contains(c("order_task", "Valgkreds", "FB_ads")))

# Write raw conjoint data to file
saveRDS(C, "1_Raw/2_Eksperiment/Raw_Conjoint_Data.rds")


### Wrangle ad choice (dependent variable)
# Create empty list for data storage
A <- list()

# String replace in loop
for (i in c(1:4)) {

  # Extract ad choice from each task
  D <- C %>% 
    select(ends_with(paste(i, "resp", sep = "_"))) %>% # select one of the four different tasks 
    rename(ad_choice = 1) %>% 
    mutate(Task = as.character(ad_choice),
           Task = as.numeric(str_extract(Task, "[:digit:]"))) # extract the ad choice
  
  # Store the data in list
  A[[i]] <- D$Task
    
}

# Name elements in list
names(A) <- c("Choice_1", "Choice_2", "Choice_3", "Choice_4")

# Cbind the different variables
Choice <- do.call(cbind, A)


### Wrangle type of exposure (independent variable)
# Create empty lists for data storage
A2 <- list()
A3 <- list()

# Wrangle data in loop
for (i in c(1:4)) {
  
  # Clean task info
  Task <- C %>% 
    select(starts_with(paste("qFB_ads", i, sep = "_")),
           - ends_with("order"), - ends_with("resp")) %>% 
    data.frame()

  # Left part of task
  Left_task <- str_split(Task[, 1], "/")
  Right_task <- str_split(Task[, 2], "/")

  # Row bind
  T1a <- do.call(rbind, Left_task)
  T1b <- do.call(rbind, Right_task)

  # Column bind
  T_col <- data.frame(cbind(T1a, T1b))
  
  # Remove "_" from all strings
  for (j in c(1:ncol(T_col))) {
    
    # Apply the str_remove function for each variable
    string <- as.numeric(str_remove(T_col[, j], "_"))
    
    # Input into list
    A3[[j]] <- string
    
  }

  # Column bind
  T1 <- data.frame(do.call(cbind, A3))
  
  # Name columns
  colnames(T1) <- c(paste("Politiker_", "a", i, sep = ""),
                    paste("Valgkreds_", "a", i, sep = ""),
                    paste("Lokalisme_", "a", i, sep = ""),
                    paste("Politiker_", "b", i, sep = ""),
                    paste("Valgkreds_", "b", i, sep = ""),
                    paste("Lokalisme_", "b", i, sep = ""))
  
  # Store in list
  A2[[i]] <- T1
  
}

# Column bind all elements from list
Type <- do.call(cbind, A2)


### Wrangle order variable
# Remove trailing digits from variable
Top <- C %>% 
  mutate(order_task = str_extract(order_task, ".{11}"))

# Split variable in four strings
Top2 <- str_split(Top$order_task, "/")

# Row bind
Top3 <- do.call(rbind, Top2)

# Empty list
A4 <- list()

# Remove "_" from all strings
for (k in c(1:ncol(Top3))) {
  
  # Apply the str_remove function for each variable
  Top_string <- as.numeric(str_remove(Top3[, k], "_"))
  
  # Input into list
  A4[[k]] <- Top_string
  
}

# Column bind all elements from list
Task_order <- data.frame(do.call(cbind, A4)) %>% 
  rename(Task_1 = 1, Task_2 = 2,
         Task_3 = 3, Task_4 = 4)


# Isolate order variables from original dataframe
Order_variables <- C %>% 
  select(ends_with("_order"))

### Join the cleaned elements
DF <- cbind(Valgkreds = C$Valgkreds, 
            Partivalg = data_recode$partivalg,
            Blok = data_recode$partivalg_blok,
            Alder = data_recode$alder,
            Alder_kat = data_recode$alder_kat,
            Alder_kat_det = data_recode$alder_kat_det,
            Uddannelse = data_recode$uddannelse_det,
            Køn = data_recode$bagg1,
            Saliens = data_recode$dag,
            Ideologi = data_recode$Qideologi,
            postnr = data_recode$postnr,
            by = data_recode$by,
            Weight = data_recode$weight,
            Task_order, Order_variables, Type, Choice) %>% 
  mutate(Respondent = as.character(c(1:nrow(C)))) %>% 
  select(Respondent, everything())


#-------------------------------------------------------------------------
# PIVOTING THE DATA FOR EACH RESPONDENT 
#-------------------------------------------------------------------------

# Pivoting topic that the respondent has been exposed to

# Empty list
A5 <- list()

# Create vector for selecting variables in loop
vector <- c("Task", "order", "Politiker_a", "Politiker_b",
            "Valgkreds_a", "Valgkreds_b", "Lokalisme_a", 
            "Lokalisme_b", "Choice")

# Pivot data in loop
for (l in vector) {

  # Select different types of variables
  a <- DF %>% 
    select(Respondent, Partivalg, Blok, 
           Valgkreds, Alder, Alder_kat,
           Alder_kat_det, Uddannelse, Køn,
           Saliens, Ideologi, Valgkreds, 
           postnr, by, Weight, contains(l))
    
  # Pivot these variables
  b <- a %>% 
    pivot_longer(contains(l))
  
  # Save respondent ID
  Respondent <- data.frame(Respondent = b$Respondent, 
                           Valgkreds = b$Valgkreds,
                           Partivalg = b$Partivalg,
                           Blok = b$Blok,
                           Alder = b$Alder,
                           Alder_kat = b$Alder_kat,
                           Alder_kat_det = b$Alder_kat_det,
                           Uddannelse = b$Uddannelse,
                           Køn = b$Køn,
                           Saliens = b$Saliens,
                           Ideologi = b$Ideologi,
                           postnr = b$postnr,
                           by = b$by,
                           Weight = b$Weight)
  
  # Select only value variable and rename it
  c <- b[, "value"]
  colnames(c) <- l
  
  # Put into list
  A5[[l]] <- c
  
}

# Column bind all elements from list
DF2 <- data.frame(Respondent, do.call(cbind, A5)) %>% 
  select(-starts_with("Valgkreds_"))


#-------------------------------------------------------------------------
# ADD VARIABLES FOR ANALYSIS 
#-------------------------------------------------------------------------

# Add variable with choice between repstil strategies
DF3 <- DF2 %>% 
  mutate(Repstil_choice = case_when(order == "Credit" & Choice == 1 ~ 1,
                                    order == "Credit" & Choice == 2 ~ 0,
                                    order == "Position" & Choice == 1 ~ 0,
                                    order == "Position" & Choice == 2 ~ 1),
         order_b = ifelse(order == "Credit", "Position", "Credit"),
         Repstil_fac = factor(Repstil_choice, 
                              labels = c("Politisk positionering", "Credit claiming")))

# Check raw distribution
table(DF3$Repstil_choice, useNA = "ifany")
table(DF3$Repstil_fac, useNA = "ifany")

# Check share of credit claim preference per party
DF3 %>% 
  group_by(Partivalg, Repstil_choice) %>% 
  tally() %>% 
  group_by(Partivalg) %>% 
  mutate(share = n / sum(n)) %>% 
  filter(Repstil_choice == 1) %>% 
  arrange(desc(share))


# Add variable with choice between localism/not localism and add 
# type of respondent for both candidates for each task
DF4 <- DF3 %>% 
  mutate(Localism_same = ifelse(Lokalisme_a == Lokalisme_b, "Same", "Different"),
         Localism_same = factor(ifelse(Lokalisme_a == 1 & Localism_same == "Same", "None", Localism_same)),
         Localism_choice = case_when(Localism_same == "Different" & Lokalisme_a == 1 & Choice == 1 ~ 1,
                                     Localism_same == "Different" & Lokalisme_a == 1 & Choice == 2 ~ 0,
                                     Localism_same == "Different" & Lokalisme_a == 2 & Choice == 1 ~ 0,
                                     Localism_same == "Different" & Lokalisme_b == 2 & Choice == 2 ~ 1,
                                     Localism_same == "Different" & Lokalisme_b == 1 & Choice == 2 ~ 1,
                                     Localism_same == "Different" & Lokalisme_b == 1 & Choice == 1 ~ 0,
                                     Localism_same == "Same" ~ 0),
         Type_1 = case_when(order == "Position" & Lokalisme_a == 1 ~ "Position local",
                            order == "Position" & Lokalisme_a == 2 ~ "Position not local",
                            order == "Credit" & Lokalisme_a == 1 ~ "Credit local",
                            order == "Credit" & Lokalisme_a == 2 ~ "Credit not local"),
         Type_2 = case_when(order == "Credit" & Lokalisme_b == 1 ~ "Position local",
                            order == "Credit" & Lokalisme_b == 2 ~ "Position not local",
                            order == "Position" & Lokalisme_b == 1 ~ "Credit local",
                            order == "Position" & Lokalisme_b == 2 ~ "Credit not local"),
         Type_gather = factor(case_when(
           Type_1 == "Credit local" & Type_2 == "Position not local" ~ "Credit lokalisme",
           Type_1 == "Credit not local" & Type_2 == "Position local" ~ "Position lokalisme",
           Type_1 == "Position local" & Type_2 == "Credit not local" ~ "Position lokalisme",
           Type_1 == "Position not local" & Type_2 == "Credit local" ~ "Credit lokalisme",
           Type_1 == "Credit local" & Type_2 == "Position local" ~ "Begge lokalisme",
           Type_1 == "Position local" & Type_2 == "Credit local" ~ "Begge lokalisme",
           Type_1 == "Credit not local" & Type_2 == "Position not local" ~ "Ingen lokalisme",
           Type_1 == "Position not local" & Type_2 == "Credit not local" ~ "Ingen lokalisme"),
           levels = c("Ingen lokalisme",
                      "Begge lokalisme",
                      "Credit lokalisme", 
                      "Position lokalisme")),
         Local_position = ifelse(Type_1 == "Position local" | Type_2 == "Position local", 1, 0),
         Local_credit = ifelse(Type_1 == "Credit local" | Type_2 == "Credit local", 1, 0))

# Check distribution
table(DF4$Repstil_choice, useNA = "ifany")
table(DF4$Local_position, useNA = "ifany")
table(DF4$Type_gather, useNA = "ifany")


# Add task order variable based on task variable
DF4$Topic <- rep(c(1:4), times = 1000)
DF4$Topic <- factor(DF4$Topic)
levels(DF4$Topic) = c("Sundhed", "Uddannelse", "Transport", "Skat")

# Add variable indicating level of agreement per topic
DF5 <- DF4 %>% 
  mutate(Agree = factor(
    case_when(Topic %in% c("Uddannelse", "Sundhed") & Blok == "Venstrefløjen (S, RV, SF, EL, Å og G)" ~ "Ideologisk uoverensstemmelse",
              Topic %in% c("Uddannelse", "Sundhed") & Blok == "De Borgerlige (V, C, DF, D, K og LA)" ~ "Ideologisk overensstemmelse",
              Topic %in% c("Transport", "Skat") & Blok == "Venstrefløjen (S, RV, SF, EL, Å og G)" ~ "Ideologisk overensstemmelse",
              Topic %in% c("Transport", "Skat") & Blok == "De Borgerlige (V, C, DF, D, K og LA)" ~ "Ideologisk uoverensstemmelse"),
    levels = c("Ideologisk overensstemmelse", "Ideologisk uoverensstemmelse")))

# Check recode
table(DF5$Topic, DF5$Agree, DF5$Blok)

# Add variable on whether respondent supports sitting government party
DF6 <- DF5 %>%
  mutate(
    Regering = ifelse(Partivalg == "A: Socialdemokratiet", 
           "Tilhænger af Socialdemokratiet", "Tilhænger af andet parti"),
    Regering = ifelse(Partivalg == "Ved ikke/i tvivl", "Tvivler", Regering),
    Regering = factor(Regering))


# Add variables on politician descriptives
DF7 <- DF6 %>%
  mutate(
    Køn_A = factor(ifelse(Politiker_a %in% c(1:10), "Mand", "Kvinde")),
    Køn_B = factor(ifelse(Politiker_b %in% c(1:10), "Mand", "Kvinde")),
    Alder_A = factor(case_when(Politiker_a %in% c(1, 6, 15, 16) ~ "56 år og derover",
                        Politiker_a %in% c(2:5, 7, 9, 11, 12, 14, 17, 18, 20) ~ "35 - 55 år",
                        Politiker_a %in% c(8, 10, 13, 19) ~ "18 - 34 år")),
    Alder_B = factor(case_when(Politiker_b %in% c(1, 6, 15, 16) ~ "56 år og derover",
                        Politiker_b %in% c(2:5, 7, 9, 11, 12, 14, 17, 18, 20) ~ "35 - 55 år",
                        Politiker_b %in% c(8, 10, 13, 19) ~ "18 - 34 år")),
    Alder_samme = ifelse(Alder_A == Alder_B, "Samme aldre", "Forskellige aldre"),
    Køn_strategi = factor(case_when(Køn_A == "Mand" & Køn_B == "Kvinde" & order == "Credit" ~ "Mand Credit Kvinde Position",
                             Køn_A == "Mand" & Køn_B == "Kvinde" & order == "Position" ~ "Mand Position Kvinde Credit",
                             Køn_B == "Mand" & Køn_A == "Kvinde" & order == "Credit" ~ "Mand Position Kvinde Credit",
                             Køn_B == "Mand" & Køn_A == "Kvinde" & order == "Position" ~ "Mand Credit Kvinde Position",
                             Køn_A == "Mand" & Køn_B == "Mand" ~ "Samme køn",
                             Køn_A == "Kvinde" & Køn_B == "Kvinde" ~ "Samme køn"),
                          levels = c("Samme køn", 
                                     "Mand Position Kvinde Credit", 
                                     "Mand Credit Kvinde Position")),
    Alder_strategi_A = paste(Alder_A, order, sep = " "),
    Alder_strategi_B = paste(Alder_B, order_b, sep = " "),
    Alder_strategi_A2 = ifelse(str_detect(Alder_strategi_A, "Credit") == TRUE, Alder_strategi_A, NA),
    Alder_strategi_A2 = ifelse(str_detect(Alder_strategi_B, "Credit") == TRUE, Alder_strategi_B, Alder_strategi_A2),
    Alder_strategi_B2 = ifelse(str_detect(Alder_strategi_A, "Position") == TRUE, Alder_strategi_A, NA),
    Alder_strategi_B2 = ifelse(str_detect(Alder_strategi_B, "Position") == TRUE, Alder_strategi_B, Alder_strategi_B2),
    Alder_strategi_C = paste(Alder_strategi_A2, Alder_strategi_B2, sep = " "),
    Alder_strategi = factor(recode(Alder_strategi_C, "18 - 34 år Credit 18 - 34 år Position" = "Samme alder",
                                   "35 - 55 år Credit 35 - 55 år Position" = "Samme alder",
                                   "56 år og derover Credit 56 år og derover Position" = "Samme alder"))
  )


# Add salience variable
DF8 <- DF7 %>% 
  mutate(Saliens_emne = ifelse(Topic == "Uddannelse" & Saliens == "Uddannelse", "Salient", "Ikke salient"),
         Saliens_emne = ifelse(Topic == "Sundhed" & Saliens == "Hospitaler og sundhed", "Salient", Saliens_emne),
         Saliens_emne = ifelse(Topic == "Skat" & Saliens == "Skat", "Salient", Saliens_emne),
         Saliens_emne = factor(Saliens_emne))

# Add gender and age choice
DF9 <- DF8 %>% 
  mutate(Køn_valg = factor(case_when(Køn_A == "Mand" & Køn_B == "Kvinde" & Choice == 1 ~ "Mand",
                              Køn_A == "Mand" & Køn_B == "Kvinde" & Choice == 2 ~ "Kvinde",
                              Køn_A == "Kvinde" & Køn_B == "Mand" & Choice == 1 ~ "Kvinde",
                              Køn_A == "Kvinde" & Køn_B == "Mand" & Choice == 2 ~ "Mand")))


### Add population per city based on postal code
# Read information with population per city
# Read population data from DST
Folketal_DST <- read.xlsx("1_Raw/2_Eksperiment/Folketal_DST_2021K2.xlsx") %>% 
  mutate(by = word(Område, start = 2)) %>%  # extract first word in string
  filter(!by %in% c("København", "Københavns"),
         !Indbyggertal < 100) %>% 
  mutate(by = recode(by, "Hovedstadsområdet" = "København")) %>%
  distinct(by, .keep_all = TRUE)

# Add city variable to data
DF10 <- left_join(DF9, Folketal_DST, by = "by") %>% 
  mutate(By = ifelse(is.na(by), "Ingen by", by))

# Check distribution
table(DF10$Indbyggertal, useNA = "ifany")

# Read stimulus data
data_list <- import_list("../0_Guides/1_VN/Variable_description.xlsx")

# Rbind the four sheets containing the different tasks
stimuli <- do.call(rbind, data_list[1:4])

# Wrangle stimulus data
stimuli_2 <- stimuli %>% 
  mutate(Attributes = na.locf(`Features/Attributes`)) %>% 
  select(Attributes, Features = `Feature values`) %>% 
  filter(str_detect(Attributes, "Lokal") == TRUE)

# Create a test city search string to detect similarities between 
# localism cues and the city variable
city_query_test <- str_c(unique(DF10$by), collapse = "|")

# Detect which specific cities are mentioned in the localism cues
stimuli_3 <- stimuli_2 %>%
  mutate(By_mentioned = str_detect(Features, city_query_test))

# Check which cities that are not detected
stimuli_3 %>% 
  filter(By_mentioned == "FALSE")

# Create the final city search string
city_query <- str_extract(stimuli_3$Features, city_query_test) # first detect cities
city_query_2 <- city_query[!is.na(city_query)] # then remove NA's
city_query_3 <- str_c(city_query_2, collapse = "|") # finally collapse the string

# Apply search string to the dataframe containing respondents' cities
DF11 <- DF10 %>% 
  mutate(By_mentioned = factor(ifelse(str_detect(by, city_query_3) == TRUE, "Sandt", "Falsk")))

# Check distribution
table(DF11$By_mentioned, useNA = "ifany")

# Write wrangled data to file
saveRDS(DF11, "2_Clean/6_Analysis_II/Final_data/Conjoint_clean.rds")



#-------------------------------------------------------------------------
# PIVOT LONGER FOR FURTHER ANALYSIS 
#-------------------------------------------------------------------------

# Add unique identifier to variable names for easier 
# selection in loop
Candidate <- DF11 %>% 
  rename(Køn_1A = Køn_A, Køn_1B = Køn_B,
         Alder_1A = Alder_A, Alder_1B = Alder_B) %>% 
  mutate(Display_1 = "Venstre", Display_2 = "Højre")

# Create selection query
vec <- c("order", "Lokalisme_", "Køn_1", "Alder_1", "Display_")

# Create empty list for data storage
A <- list()

# Select all variables containing info on candidates
for (i in vec) {
  
  # Pivot dataframe containing all relevant variables
  a <- Candidate %>%
    select(order, Respondent, Choice, Repstil_choice, Localism_choice,
           Task, Topic, Alder, Uddannelse, Køn, Agree, starts_with(i)) %>% 
    pivot_longer(starts_with(i),
                 names_to = "var", 
                 values_to = i)
  
  # Select only background variables as the others will 
  # be repeated in loop
  b <- a %>% 
    select(i)
  
  # Store in list
  A[[i]] <- b
  
}

# Collect data from list into dataframe by cbinding
# the vectors
CJ_long <- do.call(cbind, A) %>% 
  mutate(order = a$order,
         Choice = a$Choice - 1,
         Repstil_choice = a$Repstil_choice,
         Localism_choice = a$Localism_choice,
         Topic = a$Topic,
         Task_order = factor(a$Task),
         Respondent = a$Respondent,
         Køn = factor(Køn_1),
         Alder = factor(Alder_1),
         Alder_resp = factor(a$Alder),
         Udd_resp = a$Uddannelse,
         Køn_resp = a$Køn,
         Agree = a$Agree,
         Display = factor(Display_),
         Strategi = factor(case_when(
           Display == "Venstre" & order == "Credit" ~ "Credit claiming",
           Display == "Venstre" & order == "Position" ~ "Politisk positionering",
           Display == "Højre" & order == "Credit" ~ "Politisk positionering",
           Display == "Højre" & order == "Position" ~ "Credit claiming")),
         Lokalisme = factor(Lokalisme_, levels = c("2", "1")),
         Strategi_local = factor(case_when(
           Strategi == "Credit claiming" & Lokalisme == 1 ~ "Credit claiming \nmed lokalisme ",
           Strategi == "Credit claiming" & Lokalisme == 2 ~ "Credit claiming \nuden lokalisme ",
           Strategi == "Politisk positionering" & Lokalisme == 1 ~ "Politisk positionering \nmed lokalisme ",
           Strategi == "Politisk positionering" & Lokalisme == 2 ~ "Politisk positionering \nuden lokalisme ")),
         Selected = case_when(
           Display == "Venstre" & Choice == 0 ~ 1,
           Display == "Venstre" & Choice == 1 ~ 0,
           Display == "Højre" & Choice == 0 ~ 0,
           Display == "Højre" & Choice == 1 ~ 1)) %>% 
  select(-c(Køn_1, Alder_1, Lokalisme_, Display_))

# Add labels to lokalisme cue
levels(CJ_long$Lokalisme) = c("Ingen\n lokalisme", "Lokalisme")

# Write wrangled data to file
saveRDS(CJ_long, "2_Clean/6_Analysis_II/Final_data/CJ_long.rds")

