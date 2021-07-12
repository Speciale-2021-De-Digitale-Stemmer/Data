
#-------------------------------------------------------------------------
# RECODE POLITICIAN NAMES
#-------------------------------------------------------------------------

# Jeppe Kofod
# Holger K. Nielsen

# Read ft homepage data
ft <- readRDS("0_Datalister/FT_webscrape.rds")

# Look for similarities between the names from FB ad library and 
# the two other dataframes
names_1 <- D %>%
  mutate(similar_av = ifelse(page_name %in% VA7$KandidatNavn, TRUE, FALSE),
         similar_aft = ifelse(page_name %in% ft$navn, TRUE, FALSE))

# Inspect how many similarities there are
table(names_1$similar_av, useNA = "ifany")
table(names_1$similar_aft, useNA = "ifany")

# Take a closer look at the dissimilarities
check_1 <- names_1 %>% 
  filter(similar_av == FALSE) %>% 
  select(page_name)

check_2 <- names_1 %>% 
  filter(similar_aft == FALSE) %>% 
  select(page_name)

# Recode these names
names_2 <- names_1 %>% 
  mutate(page_name_recoded = 
           recode(page_name, 
                  "Birgitte Vinds side" = "Birgitte Vind",
                  "Brigitte Klintskov Jerkel Politiker" = "Brigitte Klintskov Jerkel",
                  "Brigitte Klintskov Jerkel, MF, Kons." = "Brigitte Klintskov Jerkel",
                  "Egil F. Hulgaard - Politik" = "Egil Hulgaard",
                  "Folketingskandidat Henrik Møller" = "Henrik Møller",
                  "Ina Strøjer-Schmidt - SF’s Folketingsmedlem i Københavns omegnskommuner" = "Ina Strøjer-Schmidt",
                  "Karina Due - Dansk Folkeparti" = "Karina Due",
                  "Lisbeth Bech-Nielsen" = "Lisbeth Bech Poulsen",
                  "Lise Bech - Dansk Folkeparti" = "Lise Bech",
                  "Marlene Harpsøe - Dansk Folkeparti" = "Marlene Harpsøe",
                  "Mette Gjerskov - Socialdemokratiet" = "Mette Gjerskov",
                  "MF Jane Heitmann" = "Jane Heitmann",
                  "Orla Østerby i Folketinget" = "Orla Østerby",
                  "Per Larsen i Folketinget" = "Per Larsen",
                  "René Christensen - Dansk Folkeparti" = "René Christensen",
                  "Iværksætter Thomas Thomsen" = "Thomas Thomsen",
                  "Tanja Larsson - Socialdemokratiet" = "Tanja Larsson",
                  "Victoria Velásquez" = "Victoria Velasquez"),
         page_name_recoded = ifelse(str_detect(page_name, "Susanne Zimmer") == TRUE,
                                    "Susanne Zimmer", page_name_recoded))


### Streamline same names written differently
# Create new name variable without party names in it
navn_election <- VA7 %>% 
  mutate(navn = ifelse(KandidatNavn %in% c(
    "Radikale Venstre", "Socialdemokratiet", "Det Konservative Folkeparti",
    "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance",
    "Kristendemokraterne", "Dansk Folkeparti", "Stram Kurs",
    "Venstre, Danmarks Liberale Parti", "Enhedslisten", "Alternativet"), 
    "Parti Navn", KandidatNavn),
    navn = ifelse(is.na(KandidatNavn), "NA Navn", navn))

# Split name elements from name variable
navn_election_2 <- str_split(navn_election$navn, boundary("word"))

# Create empty list
A <- list()

# For each name keep only first name and the 
# name right thereafter
for (i in c(1:length(navn_election_2))) {
  
  # Keeping only first to elements for each name
  a <- data.frame(navn_election_2[i]) %>% 
    t() %>% 
    data.frame() %>% 
    mutate(Navn = paste(X1, X2, sep = " ")) %>% 
    select(Navn)
  
  # Store in list
  A[[i]] <- a
  
}

# Collect newly coded names
B <- do.call(rbind, A) %>% 
  mutate(Navn = ifelse(Navn == "NA Navn", NA, Navn),
         Navn = ifelse(Navn == "Parti Navn", navn_election$KandidatNavn, Navn))

# Add name variable to original election dataframe
VA7$Navn <- B$Navn


### Go through the same procedure for FB name variable
# Create new name variable without party names in it
navn_FB <- names_2 %>% 
  mutate(navn = ifelse(page_name_recoded %in% c(
    "Radikale Venstre", "Socialdemokratiet", "Det Konservative Folkeparti",
    "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance",
    "Kristendemokraterne", "Dansk Folkeparti", "Stram Kurs",
    "Venstre, Danmarks Liberale Parti", "Enhedslisten", "Alternativet"), 
    "Parti Navn", page_name_recoded),
    navn = ifelse(is.na(page_name_recoded), "NA Navn", navn))

# Split name elements from name variable
navn_FB_2 <- str_split(navn_FB$navn, boundary("word"))

# Create empty list
A2 <- list()

# For each name keep only first name and the 
# name right thereafter
for (i in c(1:length(navn_FB_2))) {
  
  # Keeping only first to elements for each name
  a <- data.frame(navn_FB_2[i]) %>% 
    t() %>% 
    data.frame() %>% 
    mutate(Navn = paste(X1, X2, sep = " ")) %>% 
    select(Navn)
  
  # Store in list
  A2[[i]] <- a
  
}

# Collect newly coded names
B2 <- do.call(rbind, A2) %>% 
  mutate(Navn = ifelse(Navn == "NA Navn", NA, Navn),
         Navn = ifelse(Navn == "Parti Navn", navn_FB$page_name_recoded, Navn))

# Add name variable to original FB dataframe
names_2$Navn <- B2$Navn


### Go through the same procedure for FT website name variable
# Create new name variable without party names in it
navn_ft <- ft %>% 
  mutate(Navn = ifelse(navn %in% c(
    "Radikale Venstre", "Socialdemokratiet", "Det Konservative Folkeparti",
    "Nye Borgerlige", "Klaus Riskær Pedersen", "SF", "Liberal Alliance",
    "Kristendemokraterne", "Dansk Folkeparti", "Stram Kurs",
    "Venstre, Danmarks Liberale Parti", "Enhedslisten", "Alternativet"), 
    "Parti Navn", navn),
    Navn = ifelse(is.na(navn), "NA Navn", Navn))

# Split name elements from name variable
navn_ft_2 <- str_split(navn_ft$Navn, boundary("word"))

# Create empty list
A3 <- list()

# For each name keep only first name and the 
# name right thereafter
for (i in c(1:length(navn_ft_2))) {
  
  # Keeping only first to elements for each name
  a <- data.frame(navn_ft_2[i]) %>% 
    t() %>% 
    data.frame() %>% 
    mutate(Navn = paste(X1, X2, sep = " ")) %>% 
    select(Navn)
  
  # Store in list
  A3[[i]] <- a
  
}

# Collect newly coded names
B3 <- do.call(rbind, A3) %>% 
  mutate(Navn = ifelse(Navn == "NA Navn", NA, Navn),
         Navn = ifelse(Navn == "Parti Navn", navn_ft$navn, Navn))

# Add name variable to original FB dataframe
ft$Navn <- B3$Navn


### Check for similarities again
# Look for similarities between the new names from FB ad library and 
# FT homepage names
names_3 <- names_2 %>%
  mutate(similar_av = ifelse(Navn %in% VA7$Navn, TRUE, FALSE),
         similar_aft = ifelse(Navn %in% ft$Navn, TRUE, FALSE)) %>% 
  distinct(Navn, .keep_all = TRUE)

# Inspect how many similarities there are
table(names_3$similar_av, useNA = "ifany")
table(names_3$similar_aft, useNA = "ifany")

# Take a final look at the dissimilarities
names_3 %>% 
  filter(similar_av == FALSE) %>% 
  select(Navn)

names_3 %>% 
  filter(similar_aft == FALSE) %>% 
  select(Navn)

# There are still a few dissimilarities because some members simply 
# do not exist on the FT homepage

# Get rid of potential duplicates
VA8 <- VA7 %>% 
  distinct(Valg, Navn, StorKredsNr, .keep_all = TRUE) %>% 
  select(Valg, StorKredsNr, Navn, PV, Parti) %>% 
  left_join(VE4, by = c("Valg", "StorKredsNr", "Navn")) %>% 
  rename(Votes_overall_party = Votes)

# Save for calculation in analysis part I
saveRDS(VA8, "2_Clean/5_Analysis_I/antal_kandidater.rds")


# Add number of candidates per party
Parti_antal <- VA8 %>% 
  group_by(Valg, Parti) %>% 
  tally() %>% 
  data.frame() %>% 
  rename(antal_kandidater_per_parti = n) %>% 
  filter(!is.na(antal_kandidater_per_parti))

# Join with election data
VA9 <- left_join(VA8, Parti_antal, by = c("Valg", "Parti"))


#### KIG HER EFTER DUBLETTERNE

D_joined_test <- merge(names_2, VA8[VA8$Valg == "29", ], by = "Navn")

### Join data
D_joined <- left_join(names_2, VA8[VA8$Valg == "29", ], by = "Navn") %>% 
  mutate(PV_2015 = round(PV)) %>%
  select(-c(PV, Valg)) %>% 
  rename(Storkreds_15 = StorKredsNr,
         Parti_15 = Parti) %>% 
  left_join(VA8[VA8$Valg == "275", ], by = "Navn") %>% 
  rename(PV_2019 = PV, 
         Storkreds_19 = StorKredsNr,
         Parti_19 = Parti) %>% 
  select(-c(Valg))

# # Remove duplicated names from FT website
# ft_2 <- ft %>% 
#   distinct(Navn, .keep_all = TRUE)
# 
# # Left join the biografi data from FT webpage onto FB ad data
# D_joined_2 <- left_join(D_joined, ft[, c("Navn", "biografi")], by = "Navn")


# ### Ordfører string search
# # Extract all words from biografi and put to list
# a <- str_extract_all(D_joined$biografi, boundary("word"))
# 
# # Create empty list to store data
# A4 <- list()
# 
# # Create search string
# for (i in 1:length(a)) {
# 
#   # Find all the words that contain string "ordfører"
#   b <- data.frame(a[i]) %>%
#     rename(ord = 1) %>% 
#     filter(str_detect(ord, "ordfører") == TRUE) %>% 
#     distinct(ord)
# 
#   # Put these words into the list
#   A4[[i]] <- b  
#     
# }
# 
# # Gather all words containing string "ordfører"
# B4 <- do.call(rbind, A4) %>% 
#   filter(!ord == "ordfører")
# 
# # Gather search string
# search1 <- str_c(B4$ord, collapse = ".{12}|") 

# # Search for them in original data
# d <- data %>%
#   distinct(page_name_recoded, .keep_all = TRUE) %>% 
#   mutate(ordfører = str_extract_all(biografi, search1)) # extract ordfører information from biografi
# 
# # Join with original data
# data_2 <- data %>% 
#   left_join(d[, c("page_name_recoded", "ordfører")], by = "page_name_recoded")

# Save data
# saveRDS(data, "2_Clean/1_Ads/FT_Ads_2021-04-20.rds")
