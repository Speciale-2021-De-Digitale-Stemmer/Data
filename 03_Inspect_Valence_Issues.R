
#-------------------------------------------------------------------------
# INVESTIGATE VALENCE OF ISSUES FROM ELECTION DATA
#-------------------------------------------------------------------------

# Load data from Danish election data
load("1_Raw/1_Valgdata/Valgundersøgelserne 1971-2015/Valgundersøgelsen 2015 (DDA-31083)/Data/R/data31083.rdata")
spss_data <- read_sav("1_Raw/1_Valgdata/Valgundersøgelserne 1971-2015/Valgundersøgelsen 2015 (DDA-31083)/Data/SPSS/data31083.sav")

# Add party preference as binary variable
D <- data31083 %>% 
  mutate(parti_fløj = case_when(data31083$V51 %in% c(3, 5:8) ~ "Højrefløjen",
                                data31083$V51 %in% c(1:2, 4, 9:10) ~ "Venstrefløjen"))

# Check recoding
table(D$parti_fløj, D$V51, useNA = "ifany")

# Specify relevant variables containing attitude towards
# public spending concerning different political issues
vec <- colnames(D[, 144:158])

# Create empty list to store data from loop
A <- list()

# Set up loop
for (i in vec) {

  # Printing info to console
  cat("Sampling for", i, "...\n")
  
  # Calculate grouped mean
  a <- D[, c(i, "parti_fløj")] %>% 
    mutate(value = D[, i],
           weight = D[, 366]) %>% 
    filter(!is.na(parti_fløj),
           !value %in% c(8, 9, 10)) %>% 
    group_by(parti_fløj) %>% 
    summarise(mean = mean(value, na.rm = TRUE), # calculate grouped mean and multiply by weight
              label = get_label(spss_data[, i])) # retrieve variable labels from spss dataframe
  
  # Put dataframes into list
  A[[i]] <- a
  
}

# rbind grouped mean results
DF_mean <- data.frame(do.call(rbind, A)) %>% 
  rownames_to_column() %>% # convert rownames to a variable
  rename(variable = rowname) %>%   # rename variables 
  mutate(diff = abs(c(0, diff(mean))),
         variable = str_remove(variable, "[:punct:][:digit:]"), # clean variable name
         label_plot = str_to_sentence(str_remove(label, "OFFENTLIGE UDGIFTER "))) 

# Isolate dataframe containing differences between means
DF_diff <- DF_mean %>% 
  filter(parti_fløj == "Venstrefløjen") %>% 
  select(label_plot, diff) %>% 
  arrange(diff) 

# Add sorting variable
DF_diff$sorting <- c(1:nrow(DF_diff))

# Illustrate mean difference in scatterplot
p40_valens <- DF_mean %>% 
  left_join(DF_diff[, c("label_plot", "sorting")], by = "label_plot") %>% 
  ggplot(aes(x = reorder(label_plot, desc(sorting)), y = mean, color = parti_fløj)) +
  geom_point() +
  xlab(element_blank()) +
  theme_bw() +
  scale_color_manual(values = c("blue", "red")) +
  geom_vline(xintercept = "Miljøproblemer", linetype = "dotted") +
  # geom_text(aes(x = 13.5, y = 0.035 + quantile(diff, 0.50, na.rm = TRUE), label = "Median")) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 14, color = "black"),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 14, color = "black"),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_line()) +
  scale_y_continuous(
    limits = c(1,3),
    breaks = c(1:3), 
    labels = c("1: For mange \npenge", 
             "2: Passende",
             "3: For få \npenge")) +
  coord_flip()

# Save plot
pdf("../3_Figurer/40_valens_mean.pdf", height = 7.5, width = 10)
p40_valens
dev.off()

# Inspect which topic is situated at 75th percentile
DF_mean %>% 
  filter(parti_fløj == "Venstrefløjen") %>% 
  mutate(quantile(diff, 0.75, na.rm = TRUE))
