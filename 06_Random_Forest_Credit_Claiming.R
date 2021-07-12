
#-------------------------------------------------------------------------
# READY DATA FOR SML_rep ANALYSIS
#-------------------------------------------------------------------------

# Read coded data
D1 <- read.xlsx("2_Clean/4_Coding/Final_Coding/Coded_data.xlsx")

# Check median character count
median(D1$char_count)

# Recode coded values
D2 <- D1 %>% 
  mutate(SML_rep = ifelse(Rep_stil == 3, 2, Rep_stil),
         SML_rep = ifelse(SML_rep == 0, NA, SML_rep),
         SML_rep = ifelse(SML_rep == 1, 0, SML_rep),
         SML_rep = ifelse(SML_rep == 2, 1, SML_rep),
         SML_rep = ifelse(SML_rep == 0 & Positionsemne == 1, 1, SML_rep))

# Sample to equalize
sample <- D2 %>% 
  filter(SML_rep == 1) %>% 
  mutate(sample_adlib_id = ifelse(adlib_id %in% sample(adlib_id, size = 235), adlib_id, NA)) %>% 
  distinct(sample_adlib_id) %>% 
  filter(!is.na(sample_adlib_id))

# Equalizing quantity for each type of coded labels
D3 <- D2 %>% 
  mutate(SML_rep2 = ifelse(adlib_id %in% c(sample$sample_adlib_id), 1, NA),
         SML_rep2 = ifelse(SML_rep == 0, 0, SML_rep2),
         SML_rep = as.factor(SML_rep2)) # DENNE SIDSTE DEL KAN KODES FLOTTERE

# Find politicians with less than four ads
less_than_three <- data.frame(table(D3$page_name_recoded)) %>% 
  rename(Politiker = 1) %>% 
  filter(Freq < 5)

# Filter out these politicians from the FB data before 
# running the model
D <- D3 %>% 
  filter(!page_name_recoded %in% less_than_three$Politiker) %>% 
  mutate(SML_rep = as.factor(case_when(SML_rep == 1 ~ 0,
                             SML_rep == 0 ~ 1)))

# Check
data.frame(table(D$page_name_recoded)) %>% 
  arrange(Freq)

# Check recoding
table(D$SML_rep, useNA = "ifany")
class(D$SML_rep)


# Applying machine learning to predict which ads can be 
# categorised as credit-claiming or position-taking.

# Turn into a corpus object (C)
C <- corpus(D, text_field = "ad_creative_body")

# Pre-process and tokenize the data (50 seconds to run on a fast laptop)
Tokens <- tokens(C, remove_punct = TRUE, remove_separators = TRUE,
                 remove_url = TRUE, split_hyphens = FALSE,
                 remove_numbers = FALSE)

# Save tokens
saveRDS(Tokens, paste("2_Clean/2_SML/SML_Tokens_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))
Tokens <- readRDS(paste("2_Clean/2_SML/SML_Tokens_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))


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

# Specifying levels for strategy factor variable
levels(L_training$SML_rep) <- c("position", "credit")
levels(L_test$SML_rep) <- c("position", "credit")

# Setting the number of cores to be used according to the maximum capacity
# of the computer
cl <- makeCluster(parallel::detectCores(logical = FALSE), setup_strategy = "sequential") 
registerDoParallel(cl)

# Specifying the train function
rf_model_repstil <- train(
  y = as_factor(L_training$SML_rep), # outcome variable
  x = convert(L_training, to = "data.frame"), # dataframe of the features
  method = "ranger",
  metric = "ROC", # performance measure
  tuneGrid = rf_tuning_grid, # cross-validate based on tuning parameters from our tuning grid
  trControl = trainControl(
    method = "cv", # cross-validation specification
    number = 10, # number of folds
    search = "grid", # tuning grid specification
    summaryFunction = twoClassSummary, # 
    classProbs = TRUE,
    verboseIter = TRUE) # printing the calculation process in the output window
  )  

# Stop using all computer cores
stopCluster(cl)

# Save model to file with current date
saveRDS(rf_model_repstil, paste("2_Clean/2_SML/rf_model_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Read latest file
rf_model_repstil <- readRDS("2_Clean/2_SML/rf_model_2021-05-12.rds")


# Plot the model and save
pdf("../3_Figurer/42_model1_bilag.pdf", height = 7.5, width = 11.25)
plot(rf_model_repstil)
dev.off()

# What are the results?
rf_model_repstil$results

# How accurate was the best model?
max(rf_model_repstil$results$ROC)

# Which was the most accurate model?
which.max(rf_model_repstil$results$ROC)

# Select the best model
rf_model_repstil_best <- rf_model_repstil$finalModel


##### APPLYING THE BEST PERFORMING MODEL TO THE TEST SET

# Restructuring data from model results
rf_test <- data.frame(
  predict(rf_model_repstil$finalModel, quanteda::convert(L_test, to = "data.frame"))$predictions) %>% 
  mutate(prediction = ifelse(position > 0.50, "position", "credit")) # setting threshold at the central value (0.5)

# Turn predictions into factor variable with correct levels
rf_y_hat <- factor(rf_test$prediction, levels = c("position", "credit"))

# Create confusion matrix
ConfM_rep <- confusionMatrix(factor(rf_y_hat), factor(L_test$SML_rep),
                positive = "credit", mode = "everything")

# Print as stargazer table
stargazer(as.matrix(ConfM_rep), type = "latex")


##### ESTIMATE UNLABELED DATA FROM TEST SET

# Estimating using rf model
rf_predict <- data.frame(predict(rf_model_repstil$finalModel, convert(U, to = "data.frame"))$predictions) %>% 
  mutate(prediction = ifelse(position > 0.50, "position", "credit"))


# Combine with original data
final_data <- cbind(D[is.na(D$SML_rep), ], rf_predict) %>% 
  arrange(adlib_id)


final_data_svm <- cbind(D, REPSTIL_svm, REPSTIL_elastic) %>% 
  arrange(adlib_id)

# Write data
saveRDS(final_data, paste("2_Clean/2_SML/estimated_data_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(final_data_svm, "2_Clean/2_SML/estimated_data_13maj.rds")

# Read data
final_data <- readRDS(paste("2_Clean/2_SML/estimated_data_", format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))


# CHECK: Calculate relationship between CC and PT
a <- final_data %>% 
  group_by(prediction) %>% 
  tally() %>% 
  mutate(andel = n / sum(n))

b <- D2 %>% 
  group_by(SML_rep) %>% 
  filter(!is.na(SML_rep)) %>% 
  tally() %>% 
  mutate(andel = n / sum(n))

c <- data.frame(final = a$andel,
                manual = b$andel,
                diff = a$andel - b$andel) 

# Check credit-claim
CC <- final_data %>% 
  filter(prediction == "credit")
