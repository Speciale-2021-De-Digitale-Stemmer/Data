
#-------------------------------------------------------------------------
# SML_local ANALYSIS FOR LOKALISME
#-------------------------------------------------------------------------

# Read coded data
D1 <- read.xlsx("2_Clean/4_Coding/Final_Coding/Coded_data.xlsx")

# Sample to equalize
sample <- D1 %>%
  filter(Lokalisme == 0) %>% 
  mutate(sample_adlib_id = ifelse(adlib_id %in% sample(adlib_id, size = 320), adlib_id, NA)) %>% 
  distinct(sample_adlib_id) %>% 
  filter(!is.na(sample_adlib_id))

# Equalizing quantity for each type of coded labels
D2 <- D1 %>% 
  mutate(SML_local2 = ifelse(adlib_id %in% c(sample$sample_adlib_id), 0, NA),
         SML_local2 = ifelse(Lokalisme == 1, 1, SML_local2),
         SML_local = as.factor(SML_local2)) # DENNE SIDSTE DEL SKAL KODES FLOTTERE

# Find politicians with less than four ads
less_than_three <- data.frame(table(D2$page_name_recoded)) %>% 
  rename(Politiker = 1) %>% 
  filter(Freq < 5)

# Filter out these politicians from the FB data before 
# running the model
D <- D2 %>% 
  filter(!page_name_recoded %in% less_than_three$Politiker)


# Check recoding
table(D$SML_local, useNA = "ifany")

# Remove special characters
D$ad_creative_body <- str_replace_all(D$ad_creative_body, "\u202F", " ")

# Turn into a corpus object (C)
C <- corpus(D, text_field = "ad_creative_body")

# Pre-process and tokenize the data
Tokens <- tokens(C, remove_punct = TRUE, remove_separators = TRUE,
                 remove_url = TRUE, split_hyphens = FALSE,
                 remove_numbers = FALSE)

# Save tokens
saveRDS(Tokens, paste("2_Clean/2_SML/SML_Lokalisme_Tokens_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))
Tokens <- readRDS(paste("2_Clean/2_SML/SML_Lokalisme_Tokens_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))



# Lower case
Tokens_low <- tokens_tolower(Tokens)

# Inspect stopwords from Quanteda package
quanteda::stopwords("danish")

# Remove stop words
Tokens_stop <- tokens_remove(Tokens_low, pattern = stopwords("da"))

# Stem the tokens 
Tokens_stem <- tokens_wordstem(Tokens_stop, language = "da")

# Add bigrams
Tokens_final <- tokens_ngrams(Tokens_stem, n = c(1, 2))

# Create a document-frequency matrix (document-feature matrix)
DFM_raw <- dfm(Tokens_final)

# Remove rare, frequent and short features
DFM <- DFM_raw %>% 
  dfm_select(min_nchar = 2) %>% 
  dfm_trim(termfreq_type = "count", min_termfreq = 10) %>% # Cut out the lower 1%-tile  
  dfm_trim(termfreq_type = "quantile", max_termfreq = 0.99) # and the upper 99%-tile

# Check the dimensions of our document-feature matrix after trimming
dim(DFM) 

# Set seed
set.seed(42)

# Randomly order data. This ensures that the training set and test
# set are both random samples and that any biases in the ordering 
# of the dataset as Facebook arranges by adlib_id by default
rows <- sample(nrow(DFM))
DFM <- DFM[rows, ]

# Creating training and test data with a 90% / 10% split
# First setting unlabeled data aside
U <- DFM[is.na(DFM$SML_rep), ]

# Secondly creating a data.frame with just the labeled data
L <- DFM[!is.na(DFM$SML_rep), ]

# Create training and test sets that are an 90% / 10% split
training_set <- createDataPartition(L$SML_rep, p = 0.9)$Resample1

# Create dataframes containing data from the dfm based on whether
# they are in the traning or test set
L_training <- L[training_set, ]
L_test <- L[-training_set, ]


#-------------------------------------------------------------------------
# APPLY RANDOM FOREST MODEL
#-------------------------------------------------------------------------


# Experimenting with different values of mtry using a grid search 
# to determine which value of mtree performs the best
floor(sqrt(ncol(L_training))) # The ranger() default is the sqrt() of features
rf_tuning_grid <- expand.grid(mtry = c(10, 50, 93, 160, 280, 300, 320, 360, 500, 1000),
                              splitrule = "gini", # ranger Default
                              min.node.size = 1)  # ranger Default

# Setting the number of cores to be used according to the maximum capacity
# of the computer
cl <- makeCluster(parallel::detectCores(logical = FALSE), setup_strategy = "sequential") 
registerDoParallel(cl)

# Specifying the train function
rf_model_local <- train(
  y = as_factor(L_training$SML_local),
  x = convert(L_training, to = "data.frame"),
  method = "ranger",
  metric = "ROC", # performance measure
  tuneGrid = rf_tuning_grid, # cross-validate based on tuning parameters
  trControl = trainControl(
    method = "cv",
    number = 10,
    search = "grid",
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE)) 

# Stop using all computer cores
stopCluster(cl)

# Save model to file with current date
saveRDS(rf_model_local, paste("2_Clean/2_SML/rf_model_lokalisme_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Read latest file
rf_model_local <- readRDS("2_Clean/2_SML/rf_model_lokalisme_2021-05-12.rds")

# Plot the model and save
pdf("../3_Figurer/42_model2_bilag.pdf", height = 7.5, width = 11.25)
plot(rf_model_local)
dev.off()

# What are the results?
rf_model_local$results

# How accurate was the best model?
max(rf_model_local$results$ROC)

# Which was the most accurate model?
which.max(rf_model_local$results$ROC)

# Select the best model
rf_model_local_best <- rf_model_local$finalModel


##### APPLYING THE BEST PERFORMING MODEL TO THE TEST SET

# Restructuring data from model results
rf_test <- data.frame(
  predict(rf_model_local$finalModel, quanteda::convert(L_test, to = "data.frame"))$predictions) %>% 
  mutate(prediction = ifelse(localism < 0.5, "not", "localism"))

# Turn predictions into factor variable with correct levels
rf_y_hat <- factor(rf_test$prediction, levels = c("not", "localism"))

# Create confusion matrix
ConfM_loc <- confusionMatrix(factor(rf_y_hat), factor(L_test$SML_local),
                positive = "localism", mode = "everything")

# Print as stargazer table
stargazer(as.matrix(ConfM_loc), type = "latex")


##### ESTIMATE UNLABELED DATA FROM TEST SET

# Estimating using rf model
rf_predict <- data.frame(predict(rf_model_local$finalModel, convert(U, to = "data.frame"))$predictions) %>%
  mutate(prediction = ifelse(localism < 0.50, "not", "localism"))


# Combine with original data
final_data <- cbind(D[is.na(D$SML_local), ], rf_predict) %>%
  arrange(adlib_id)

# Write data
saveRDS(final_data, paste("2_Clean/2_SML/estimated_data_lokalisme_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Combine data from both predictions
local <- readRDS(paste("2_Clean/2_SML/estimated_data_lokalisme_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = "")) %>% 
  rename(prediction_local = prediction)

repstil <- readRDS(paste("2_Clean/2_SML/estimated_data_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = "")) %>%
 rename(prediction_repstil = prediction)

collected <- merge(local, repstil) 

# Write in analysis directories
saveRDS(collected, paste("2_Clean/5_Analysis_I/FB_data_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(collected, paste("2_Clean/6_Analysis_II/FB_data_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# CHECK: Calculate relationship between CC and PT
a <- local %>% 
  group_by(prediction_local) %>% 
  tally() %>% 
  mutate(andel = n / sum(n)) %>% 
  arrange(desc(prediction_local))

b <- D %>% 
  group_by(Lokalisme) %>% 
  filter(!is.na(Lokalisme)) %>%
  tally() %>% 
  mutate(andel = n / sum(n),
         Lokalisme = ifelse(Lokalisme == 0, "not", "localism")) %>% 
  arrange(desc(Lokalisme))

c <- data.frame(final = a$andel,
           manual = b$andel,
           diff = a$andel - b$andel) 

# Check localism
Local <- final_data %>% 
  filter(prediction == "localism") %>% 
  select(ad_creative_body, prediction)
