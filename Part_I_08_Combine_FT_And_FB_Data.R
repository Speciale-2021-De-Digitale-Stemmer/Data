
#-------------------------------------------------------------------------
# COMBINE FB DATA WITH ELECTION RESULTS
#-------------------------------------------------------------------------

### Read all relevant data

# Data includes credit claiming, position taking and localism
D <- readRDS("2_Clean/5_Analysis_I/FB_data_2021-05-09.rds")

# Data on mandates per party
M <- read.xlsx("0_Datalister/Mandat-fordeling-parti.xlsx")

# Data on mandates per constituency
C <- read.xlsx("0_Datalister/Mandat-fordeling-storkreds.xlsx")

# Data on mandates per constituency
Y <- read.xlsx("0_Datalister/0312-List_FB_ID.xlsx") %>% 
  mutate(page_id = as.character(Page.ID)) %>% 
  distinct(page_id, .keep_all = TRUE)

# ID from large .xlsx file
ID <- read.xlsx("0_Datalister/List_FB_ID.xlsx")

# Election results
VA1 <- read.xlsx("1_Raw/1_Valgdata/Valg_15_19.xlsx", sheet = "Personlige_stemmer")
VB1 <- read.xlsx("1_Raw/1_Valgdata/Valg_15_19.xlsx", sheet = "Kandidater_stamdata")
VC1 <- read.xlsx("1_Raw/1_Valgdata/Valg_15_19.xlsx", sheet = "Geografiske_stamdata")
VD1 <- read.xlsx("1_Raw/1_Valgdata/Valg_15_19.xlsx", sheet = "Valg_stamdata")
VE1 <- read.xlsx("1_Raw/1_Valgdata/Valg_15_19.xlsx", sheet = "Valgdata")


### Clean vote dataframe

# Pivot personal election result for each election separately
VA2a <- VA1 %>%  
  mutate_all(as.factor) %>% # convert all columns in dataframe to factor variables
  select(-starts_with("FV2019")) %>% 
  pivot_longer( 
    starts_with("FV2015"), # pivot dataframe longer 
    names_to = "Kandidat", 
    values_to = "PV") %>% 
  mutate(Kandidat = str_remove_all(as.character(Kandidat), "FV2015..."),
         Valg = "29") # code for 2015 election

VA2b <- VA1 %>% 
  mutate_all(as.factor) %>% # convert all columns in dataframe to factor variables
  select(-starts_with("FV2015")) %>% 
  pivot_longer( 
    starts_with("FV2019"), # pivot dataframe longer 
    names_to = "Kandidat", 
    values_to = "PV") %>% 
  mutate(Kandidat = str_remove_all(as.character(Kandidat), "FV2019..."),
         Valg = "275") # code for 2019 election

# Merge 2015 and 2019 again
VA3 <- rbind(VA2a, VA2b)


### Do the same for overall election result
# Pivot election result for each election separately
VE2a <- VE1 %>% 
  mutate_all(as.factor) %>% # convert all columns in dataframe to factor variables
  select(-starts_with("FV2019")) %>% 
  pivot_longer( 
    starts_with("FV2015"), # pivot dataframe longer 
    names_to = "Kandidat", 
    values_to = "Votes") %>% 
  mutate(Kandidat = str_remove_all(as.character(Kandidat), "FV2015..."),
         Valg = "29") # code for 2015 election

VE2b <- VE1 %>% 
  mutate_all(as.factor) %>% # convert all columns in dataframe to factor variables
  select(-starts_with("FV2015")) %>% 
  pivot_longer( 
    starts_with("FV2019"), # pivot dataframe longer 
    names_to = "Kandidat", 
    values_to = "Votes") %>% 
  mutate(Kandidat = str_remove_all(as.character(Kandidat), "FV2019..."),
         Valg = "275") # code for 2015 election

# Merge 2015 and 2019 again
VE3 <- rbind(VE2a, VE2b)


# Calculate personal votes by storkreds for each candidate
VA4 <- VA3 %>% 
  group_by(Valg, StorKredsNr, Kandidat) %>% 
  mutate(PV = str_replace(as.character(PV), "-", "0")) %>% 
  summarise(PV = sum(as.numeric(PV))) # calculate summarised votes per storkreds


# Create key variable to join with candidate information
# also clean the names of parties
VA5 <- VA4 %>% 
  mutate(Key = paste(Valg, StorKredsNr, Kandidat, sep = "-"),
         Key = str_remove(Key, "0(?=[:digit:])")) # remove leading zeroes from key

# Mutate key variable for candidate information data for
# each election year
VB2 <- VB1 %>% 
  mutate(Key = paste(ValgId, Storkreds.Nr, Parti, Orden, sep = "-"))

# Join relevant variables from vote data
VA6 <- left_join(VA5, VB2[, c("KandidatNavn", "Key", "Parti")], by = "Key")

# Add party personal votes before joining with Facebook data
VA7 <- VA6 %>% 
  mutate(Partinavn = case_when( # rename parties to match FB data
    Kandidat == "A.-.personlige.stemmer.i.alt" ~ "Socialdemokratiet",
    Kandidat == "B.-.personlige.stemmer.i.alt" ~ "Radikale Venstre",
    Kandidat == "C.-.personlige.stemmer.i.alt" ~ "Det Konservative Folkeparti",
    Kandidat == "D.-.personlige.stemmer.i.alt" ~ "Nye Borgerlige",
    Kandidat == "E.-.personlige.stemmer.i.alt" ~ "Klaus Riskær Pedersen",
    Kandidat == "F.-.personlige.stemmer.i.alt" ~ "SF",
    Kandidat == "I.-.personlige.stemmer.i.alt" ~ "Liberal Alliance",
    Kandidat == "K.-.personlige.stemmer.i.alt" ~ "Kristendemokraterne",
    Kandidat == "O.-.personlige.stemmer.i.alt" ~ "Dansk Folkeparti",
    Kandidat == "P.-.personlige.stemmer.i.alt" ~ "Stram Kurs",
    Kandidat == "V.-.personlige.stemmer.i.alt" ~ "Venstre, Danmarks Liberale Parti",
    Kandidat == "Ø.-.personlige.stemmer.i.alt" ~ "Enhedslisten",
    Kandidat == "Å.-.personlige.stemmer.i.alt" ~ "Alternativet"),
    KandidatNavn = ifelse(!is.na(Partinavn), Partinavn, KandidatNavn)) %>% 
  select(-Partinavn) %>% 
  ungroup()

# Also recode name of overall party votes
VE4 <- VE3 %>% 
  mutate(Navn = case_when( # rename parties to match FB data
    Kandidat == "A..Socialdemokratiet" ~ "Socialdemokratiet",
    Kandidat == "B..Radikale.Venstre" ~ "Radikale Venstre",
    Kandidat == "C..Det.Konservative.Folkeparti" ~ "Det Konservative Folkeparti",
    Kandidat == "D..Nye.Borgerlige" ~ "Nye Borgerlige",
    Kandidat == "E..Klaus.Riskær.Pedersen" ~ "Klaus Riskær Pedersen",
    Kandidat == "F..SF.-.Socialistisk.Folkeparti" ~ "SF",
    Kandidat == "I..Liberal.Alliance" ~ "Liberal Alliance",
    Kandidat == "K..Kristendemokraterne" ~ "Kristendemokraterne",
    Kandidat == "O..Dansk.Folkeparti" ~ "Dansk Folkeparti",
    Kandidat == "P..Stram.Kurs" ~ "Stram Kurs",
    Kandidat == "V..Venstre,.Danmarks.Liberale.Parti" ~ "Venstre, Danmarks Liberale Parti",
    Kandidat == "Ø..Enhedslisten.-.De.Rød-Grønne" ~ "Enhedslisten",
    Kandidat == "Å..Alternativet" ~ "Alternativet")) %>% 
  filter(!is.na(Navn)) %>% 
  mutate(Votes = as.numeric(str_replace(Votes, "-", ""))) %>% 
  group_by(Valg, StorKredsNr, Navn) %>% 
  summarise(Votes = sum(Votes, na.rm = TRUE)) %>% 
  ungroup()


### Run script that renames and joins vote data with FB data and FT 
### website data
source("3_Scripts/Endelige_scripts/Del 1/97_Recode_Politician_Names.R")

# Dataframe from sourced scripts
D1 <- D_joined

### Storkreds information
VC2 <- VC1 %>% 
  distinct(Storkreds.navn, .keep_all = TRUE) %>% 
  mutate(Storkreds_15 = as.factor(Storkreds.Nr),
         Storkreds_19 = as.factor(Storkreds.Nr))

# Add storkreds info to collective dataframe
D1b <- left_join(D1, VC2[, c("Storkreds_15", "Storkreds.navn")], by = "Storkreds_15") %>% 
  rename(Storkreds_2015 = Storkreds.navn) %>% 
  left_join(VC2[, c("Storkreds_19", "Storkreds.navn")], by = "Storkreds_19") %>% 
  rename(Storkreds_2019 = Storkreds.navn) %>% 
  select(-c(Storkreds_15, Storkreds_19))


### Year in parliament
D1c <- left_join(D1b, Y[, c("page_id", "Aar-i-ft")], by = "page_id") %>% 
  rename(year_in_parliament = `Aar-i-ft`)

### Recode party names
D1d <- D1c %>% 
  mutate(Parti_2015 = recode(
  Parti_15, # rename parties to match FB data
  "A" = "Socialdemokratiet",
  "B" = "Radikale Venstre",
  "C" = "Det Konservative Folkeparti",
  "D" = "Nye Borgerlige",
  "E" = "Klaus Riskær Pedersen",
  "F" = "Socialistisk Folkeparti",
  "I" = "Liberal Alliance",
  "K" = "Kristendemokraterne",
  "O" = "Dansk Folkeparti",
  "P" = "Stram Kurs",
  "V" = "Venstre",
  "Ø" = "Enhedslisten",
  "Å" = "Alternativet"),
  Parti_2015 = ifelse(Navn %in% c(
    "Radikale Venstre", "Socialdemokratiet", "Det Konservative Folkeparti",
    "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance",
    "Kristendemokraterne", "Dansk Folkeparti", "Stram Kurs",
    "Venstre, Danmarks Liberale Parti", "Enhedslisten", "Alternativet"), Navn, Parti_2015),
  Parti_2019 = recode(
    Parti_19, # rename parties to match FB data
    "A" = "Socialdemokratiet",
    "B" = "Radikale Venstre",
    "C" = "Det Konservative Folkeparti",
    "D" = "Nye Borgerlige",
    "E" = "Klaus Riskær Pedersen",
    "F" = "Socialistisk Folkeparti",
    "I" = "Liberal Alliance",
    "K" = "Kristendemokraterne",
    "O" = "Dansk Folkeparti",
    "P" = "Stram Kurs",
    "V" = "Venstre",
    "Ø" = "Enhedslisten",
    "Å" = "Alternativet"),
  Parti_2019 = ifelse(Navn %in% c(
    "Radikale Venstre", "Socialdemokratiet", "Det Konservative Folkeparti",
    "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance",
    "Kristendemokraterne", "Dansk Folkeparti", "Stram Kurs",
    "Venstre, Danmarks Liberale Parti", "Enhedslisten", "Alternativet"), Navn, Parti_2019),
  Liste_sideordnet_2015 = ifelse(Parti_2015 == "Enhedslisten", "Listeopstilling", "Sideordnet"),
  Liste_sideordnet_2019 = ifelse(Parti_2019 == "Enhedslisten", "Listeopstilling", "Sideordnet"))


# Write data to file
saveRDS(D1d, "2_Clean/5_Analysis_I/Final_data/FB_VOTE_final.rds")

