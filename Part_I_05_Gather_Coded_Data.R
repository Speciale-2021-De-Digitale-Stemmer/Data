
#-------------------------------------------------------------------------
# GATHER CODED DATA
#-------------------------------------------------------------------------

# Read all ads
a <- readRDS("2_Clean/1_Ads/FT_Ads_2021-04-20.rds")

# Read already coded ads
coded <- read.xlsx("2_Clean/4_Coding/Interkoder_Reliabilitet_3/Sample_2021-03-09 - Frederik.xlsx")

# Filter for ads created after the election and remove 
# duplicates
b <- a %>%
  mutate(date = ymd(str_extract(ad_creation_time, ".{4}-.{2}-.{2}"))) %>%
  filter(date > "2019-04-01", # filter by date
         !is.na(ad_creative_body)) %>% # filter out empty ads
  distinct(adlib_id, .keep_all = TRUE) %>% # make sure that no ads are duplicated
  mutate(char_count = nchar(ad_creative_body))  # add character count

# Which candidates write short ads
short <- b %>% 
  filter(char_count < 150)

table(short$page_name_recoded)

summary(b$char_count)

# Filter out too short ads
data <- b %>% 
  filter(!char_count < 114, 
         !ad_creative_body %in% c("{{product.brand}}")) # filter out specific 'brand' ads


# Read all files containing coded data
# Extend the working directory of the R project 
setwd("2_Clean/1_Ads/Coding_Sample")

# Create list element containing the file names
filenames <- list.files(pattern = "*.xlsx", full.names = TRUE)

# Read all the files and put the elements in a list
ldf <- lapply(filenames, read.xlsx, 1)

# Rbind all elements 
coded <- do.call(rbind, ldf) %>% 
  filter(!is.na(Rep_stil)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # replace all NA's with zeroes
  select(-ad_creative_body) # remove text variable from manually coded data 

# Retrieve original working directory
setwd("../../..")

# Join with original data
D1 <- left_join(data, coded, by = c("adlib_id"))

# Write data
write.xlsx(D1, "2_Clean/4_Coding/Final_Coding/Coded_data.xlsx")
saveRDS(coded, "2_Clean/4_Coding/Final_Coding/Manual_coding.rds")


# Compare identical ads and see if they have been coded similarly
coded_test <- do.call(rbind, ldf) %>% 
  filter(!is.na(Rep_stil)) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) # replace all NA's with zeroes

coded_test_2 <- coded_test %>% 
  distinct(ad_creative_body, .keep_all = TRUE)

test <- coded_test %>% 
  filter(!adlib_id %in% coded_test_2$adlib_id)
