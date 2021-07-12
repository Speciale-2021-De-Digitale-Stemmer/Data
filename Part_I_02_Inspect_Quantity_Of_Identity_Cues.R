
#-------------------------------------------------------------------------
# FIND OUT HOW MANY DIFFERENT SOCIAL IDENTITY CUES ARE PRESENT IN DATA
#-------------------------------------------------------------------------

# Read coded data
soc_1 <- read.xlsx("2_Clean/4_Coding/coding-sociden.xlsx")

# Calculate sum for each social identity
soc_2 <- soc_1 %>% 
  filter(!ad_creative_body == "i ALT") %>% 
  summarise(Køn = sum(Køn, na.rm = TRUE),
            Klasse = sum(Klasse, na.rm = TRUE),
            Etnicitet = sum(Etnicitet, na.rm = TRUE),
            Lokalisme = sum(Lokalisme, na.rm = TRUE),
            Nationalisme = sum(Nationalisme, na.rm = TRUE),
            Uddannelse = sum(Uddannelse, na.rm = TRUE)) %>% 
  pivot_longer(cols = 1:6, names_to = "Social_identity", values_to = "Frequency") %>% 
  mutate(Share = Frequency / sum(Frequency)) %>% 
  arrange(Social_identity)

# The above results are inserted in table 3.3.1 in the main document
