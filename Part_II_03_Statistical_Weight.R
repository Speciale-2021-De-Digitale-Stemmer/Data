
#-------------------------------------------------------------------------
# WEIGHT FOR CONJOINT DATA FROM EPINION
#-------------------------------------------------------------------------

# Read raw file
data <- read_sav("1_Raw/2_Eksperiment/data_interns.sav") %>%
  haven::as_factor()

### 1. Create updated population matrix
pop_matrix <- readRDS("3_Scripts/Endelige_scripts/Populationsmatrice/99_Final_Data/Populationsdata.rds")

# Remove irrelevant overview from pop_matrix
pop_matrix_2 <- pop_matrix[c(2, 5, 8, 9)] # chose relevant elements of pop_matrix

# Calculate frequencies rather than shares
pop_matrix_3 <- map(pop_matrix_2, ~.x %>%
                      mutate(Freq = Andel*nrow(data)) %>% 
                      select(-Andel))

# Convert partivalg to dataframe to recode values
Partivalg_FT19 <- data.frame(pop_matrix_3[4]) %>% 
  rename(Partivalg_FT19 = 1, Freq = 2) %>% 
  mutate(Partivalg_FT19 = ifelse(Partivalg_FT19 %in% 
                                   c("E: Klaus Riskær Pedersen", 
                                     "K: Kristendemokraterne", 
                                     "P: Stram Kurs"), "Uden for partierne", Partivalg_FT19)) %>% 
  group_by(Partivalg_FT19) %>% 
  summarise(Freq = sum(Freq))

# Insert this recoded df into population matrix
pop_matrix_4 <- pop_matrix_3[-4]

pop_matrix_4[[4]] <- Partivalg_FT19
names(pop_matrix_4[4]) <- "Partivalg FT19"

# Recode values from dataset
data_recode <- data %>% 
  filter(!is.na(region),
         !is.na(uddannelse2),
         !is.na(person),
         !is.na(husstand),
         !is.na(køn_alder),
         !is.na(partivalg_sidste_fv)) %>% 
  mutate(region = as.factor(paste("Region", region)),
         partivalg_sidste_fv = recode(
           partivalg_sidste_fv, "Andre partier" = "Uden for partierne")) %>% 
  rename(Region = region,
         Uddannelse = uddannelse2,
         # Husstandstype = person,
         # Husstandsstørrelse = husstand,
         Køn_alder = køn_alder,
         Partivalg_FT19 = partivalg_sidste_fv)


### 2. Make design object
suppressWarnings(data.svy.unweighted <- svydesign(ids = ~1, data = data_recode))


### 3. Rake
data.svy.weighted <- rake(
  design = data.svy.unweighted,
  sample.margins = list(~Region, ~Uddannelse, ~Køn_alder, ~Partivalg_FT19),
  population.margins = pop_matrix_4, control = list(maxit = 200, epsilon = 0.00000000000001, verbose = FALSE))


### 4. Calculate weights -------------------------
data_recode$weight2 <- weights(data.svy.weighted)

# Check Epinion weight against our own weight
check_wgt <- data_recode %>% 
  select(weight, weight2)

# Mean differences
mean(check_wgt$weight)
mean(check_wgt$weight2) # mean weight not equal to zero as party variable in population data
                        # contains "ikke stemt" while party vote shares are calculated from
                        # eligible votes

### 5. Save weighted data --------------------

# Gem RDS
saveRDS(data_recode, "2_Clean/6_Analysis_II/Conjoint.rds") 

