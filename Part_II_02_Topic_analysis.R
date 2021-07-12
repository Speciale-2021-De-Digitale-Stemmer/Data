
#-------------------------------------------------------------------------
# PERFORM TOPIC ANALYSIS
#-------------------------------------------------------------------------

# Set "threads" equal to the number of cores on computer
quanteda::quanteda_options(threads = 2)

# Read data
D <- readRDS("2_Clean/6_Analysis_II/FB_data_2021-04-27.rds")

# Turn into a corpus object (C)
C <- corpus(D, text_field = "ad_creative_body")

# Pre-process and tokenize the data
Tokens <- tokens(C, remove_numbers = TRUE, remove_punct = TRUE,
                 remove_separators = TRUE, split_hyphens = FALSE,
                 remove_url = TRUE)

# Save tokens
saveRDS(Tokens, "2_Clean/3_Topic/Tokens.rds")
Tokens <- readRDS("2_Clean/3_Topic/Tokens.rds")

# Lower case
Tokens <- tokens_tolower(Tokens)

# Remove stop words
Tokens <- tokens_remove(Tokens, pattern = stopwords("da"))

# Stem the tokens
Tokens <- tokens_wordstem(Tokens)

# Add bigrams
Tokens <- tokens_ngrams(Tokens, n = c(1, 2))

# Create document-feature matrix
DFM <- dfm(Tokens)

# Convert the dfm to a format that is usable
# by the "topicmodels" library in R
DFM_topic <- quanteda::convert(DFM, to = "topicmodels")


# Run model with k = 60
system.time({
model_lda_60 <- LDA(DFM_topic, k = 60, method = "Gibbs",
                    control = list(verbose = 100, seed = 1,
                                   burnin = 200, iter = 1000))

# Save model
saveRDS(model_lda_60, "2_Clean/3_Topic/model_lda_60.rds")
})

# Read saved model
model_lda <- readRDS("2_Clean/3_Topic/model_lda_60.rds")

# Retrieve the estimated beta parameters from the model
beta_raw <- model_lda@beta

# Change column and rownames for later visualization
colnames(beta_raw) <- model_lda@terms
beta_raw <- exp(beta_raw)
beta_raw <- t(as.data.frame(beta_raw))
beta_raw <- data.frame(term = rownames(beta_raw), beta_raw)
names(beta_raw)[2:ncol(beta_raw)] <- str_c("Topic ", 1:(ncol(beta_raw)-1))

# Pivot dataframe to long format
beta <- beta_raw %>%
        pivot_longer(cols = 2:ncol(beta_raw))


# Retrieve the estimated gamma parameters from the model
Gamma <- as.data.frame(model_lda@gamma)
names(Gamma) <- str_c("topic_", 1:ncol(Gamma))

# Merge the data with the original document data
Gamma <- data.frame(D[, c("date", "page_name", "prediction_repstil", "prediction_local")], Gamma)


# Get a glimpse of how much each politician fits in topics
# 5, 51, 19, 49
Gamma_pol <- Gamma %>% 
  select(date, page_name, prediction_repstil, prediction_local, 
         syg_1 = topic_5, udd_1 = topic_51,
         trans_1 = topic_19, skat_1 = topic_49) %>% 
  mutate(median_syg_1 = ifelse(syg_1 > median(syg_1), 1, 0),
         median_udd_1 = ifelse(udd_1 > median(udd_1), 1, 0),
         median_trans_1 = ifelse(trans_1 > median(trans_1), 1, 0),
         median_skat_1 = ifelse(skat_1 > median(skat_1), 1, 0))

# Check use of each topic per politician
count <- data.frame(sum(Gamma_pol$median_syg_1), sum(Gamma_pol$median_udd_1),
                    sum(Gamma_pol$median_trans_1), sum(Gamma_pol$median_skat_1))

### Check use of credit claiming per topic
# Create empty lists for storage
A <- list()
B <- list()

# Credit for each topic
for (i in colnames(Gamma_pol[, c(10:ncol(Gamma_pol))])) {
  
  # Calculate proportion of credit/position
  # for each topic
  a <- Gamma_pol %>% 
    select(prediction_repstil, gamma = i) %>% 
    group_by(prediction_repstil, gamma) %>% 
    filter(gamma == 1) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(share = n / sum(n))
  
  b <- Gamma_pol %>% 
    select(prediction_local, gamma = i) %>% 
    group_by(prediction_local, gamma) %>% 
    filter(gamma == 1) %>% 
    tally() %>% 
    ungroup() %>% 
    mutate(share = n / sum(n))
  
  # Store in list
  A[[i]] <- a[1, 4]
  B[[i]] <- b[1, 4]
  
}

# Rbind data
C <- do.call(rbind, A) %>% 
  rownames_to_column() %>% 
  rename(topic_median = rowname)

D <- do.call(rbind, B) %>% 
  rownames_to_column() %>% 
  rename(topic_median = rowname)

# Compare with raw share of credit vs position
Gamma_pol %>% 
  group_by(prediction_repstil) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(share = n / sum(n))

# Compare with raw share of localism
Gamma_pol %>% 
  group_by(prediction_local) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(share = n / sum(n))

# The above shows that both credit-claimers and
# position-takers exist for each of the topics 
# of interest


# Identify top terms for each topic
top_terms <- terms(model_lda, 15)

# Order each topic according to their top 15 terms
G1 <- Beta %>%
  group_by(topic) %>%
  filter(!nchar(term) < 4) %>% 
  slice_max(n = 15, order_by = beta, with_ties = FALSE) %>%
  arrange(topic, -beta) %>%
  ungroup()

# Graph the terms most predictive of each topic
plot1 <- G1 %>%
  mutate(order = as.numeric(str_remove(topic, "Topic ")),
         term = reorder_within(term, beta, order)) %>%
  filter(topic %in% c("Topic 5", "Topic 19",
                      "Topic 49", "Topic 51")) %>% 
  ggplot(aes(y = term, x = beta)) +
  facet_wrap(~ fct_reorder(topic, order), scales = "free", ncol = 2) +
  geom_segment(aes(xend = 0, yend = term)) +
  geom_point() +
  scale_y_reordered() +
  theme_minimal() +
  theme(axis.text = element_text(size = 16, color = "black"),
        text = element_text(size = 16, color = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18, color = "black")) +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.001,
                                   decimal.mark = '.'))

# Save plot
pdf("../3_Figurer/Top_10_Beta_trigram2.pdf", 12, 10)
plot1
dev.off()
