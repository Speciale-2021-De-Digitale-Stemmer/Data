
#-------------------------------------------------------------------------
# LOADING LIST OF POLITICIANS ID TO DOWNLOAD FROM FACEBOOK AD LIBRARY
#-------------------------------------------------------------------------

# Create long-lasting access token via
# https://developers.facebook.com/apps/1837359893107294/settings/basic/
# adlib_setup()

# Store it on computer for 60 days
# adlib_set_longterm_token()

# Exchange short-term token for long-term via
# https://developers.facebook.com/tools/explorer/

# Read FT politicians names from FT homepage
FT_names <- readRDS("0_Datalister/FT_navne_fra_hjemmeside.rds")

# Load dataset with data cointaining relevant page ids
# for Danish politicians including if they have been in FT at the relevant times periods
ID <- read.xlsx("0_Datalister/0218-List_FB_ID.xlsx") %>% 
  mutate(Page.ID = as.character(Page.ID), # convert ID's into character class
         Page.ID = recode(Page.ID, "2241084689449200\n" = "2241084689449200")) %>% 
  filter(!Page.ID %in% c(
    "402996140498931"), # filter out ID that cannot be run
    `2015` == 1 | `2019` == 1)


#-------------------------------------------------------------------------
# DOWNLOAD AD DATA
#-------------------------------------------------------------------------

### Search queries
# Create queries for each politician as Facebook does 
# not allow more than 10 downloads at a time

# Create empty list to store queries
A <- list()

# Create query for each ID in a loop
for(i in ID$Page.ID) {
  
  # Printing info to console
  cat("Query for ID number", i, "...\n")
  
  # Create query for given ID
  query <- adlib_build_query(ad_reached_countries = 'DK', 
                             ad_active_status = 'ALL', 
                             # impression_condition = 'HAS_IMPRESSIONS_LAST_90_DAYS', 
                             search_page_ids = i,
                             fields = c("ad_creation_time",
                                        "ad_creative_body",
                                        "ad_creative_link_caption",
                                        "ad_creative_link_description",
                                        "ad_creative_link_title",
                                        "ad_delivery_start_time",
                                        "ad_delivery_stop_time",
                                        "ad_snapshot_url",
                                        "currency",
                                        "demographic_distribution",
                                        "funding_entity",
                                        "impressions",
                                        "page_id",
                                        "page_name",
                                        "potential_reach",
                                        "publisher_platforms",
                                        "region_distribution",
                                        "spend"))
  
  # Store in the list
  A[[i]] <- query
  
}


### Download from API
# Create empty list to store data from API
B <- list()
C <- list()
D <- list()

# Download data for each query in a loop
for(i in names(A)) {
  
  # Printing info to console
  cat("Downloading data for user", i, "...\n")
  
  # Submit queries
  response <- adlib_get(params = A[[i]], token = token_get())

  ## Convert to dataframe
  # Ad data
  results.tibble <- as_tibble(response, type = "ad",
                              censor_access_token = NULL)
    
  # Store in different lists
  B[[i]] <- results.tibble

}

# Rbind all elements from the list containing 
# downloaded ads
DF1 <- do.call(rbind, B)

# Save as rds marked with today's date
saveRDS(DF1, paste("2_Clean/1_Ads/Ads_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))