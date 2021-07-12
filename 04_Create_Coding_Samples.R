
#-------------------------------------------------------------------------
# CREATE CODING SAMPLES
#-------------------------------------------------------------------------

# Read all ads
a <- readRDS("2_Clean/1_Ads/Ads_2021-04-20.rds")

# Read already coded ads
coded <- readRDS("2_Clean/4_Coding/Final_Coding/Manual_coding.rds")

# Filter for ads created after the election and remove 
# duplicates
data <- a %>%
  mutate(date = ymd(str_extract(ad_creation_time, ".{4}-.{2}-.{2}"))) %>%
  filter(date > "2019-04-01",
         !is.na(ad_creative_body)) %>% 
  distinct(adlib_id, .keep_all = TRUE) %>% # make sure that no ads are duplicated
  mutate(char_count = nchar(ad_creative_body)) %>%  # add character count
  filter(!char_count < 150, # filter out too short ads
         !ad_creative_body %in% c("{{product.brand}}"), # filter out ads without text content
         !page_name %in% c("Alternativet", "Dansk Folkeparti", # filter out ads from political parties
                  "Enhedslisten", "Liberal Alliance",
                  "Nye Borgerlige", "Radikale Venstre",
                  "SF", "Venstre, Danmarks Liberale Parti",
                  "Det Konservative Folkeparti", "Jeppe Kofod", 
                  "Holger K. Nielsen", "Leif Mikkelsen"))


# Create sample of 1000
sample <- data %>%
  filter(!adlib_id %in% coded$adlib_id,
         adlib_id %in% c(sample(adlib_id, 300))) %>%
  select(adlib_id, ad_creative_body) %>%
  mutate(Rep_stil = NA, Positionsemne = NA, Konkret = NA, Lokalpolitik = NA, Lokalisme = NA)

# Write data to file
write.xlsx(sample, paste("2_Clean/1_Ads/Sample_Frederik_", format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = ""))
write.xlsx(sample2, paste("2_Clean/1_Ads/Sample_Emil_", format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = ""))
