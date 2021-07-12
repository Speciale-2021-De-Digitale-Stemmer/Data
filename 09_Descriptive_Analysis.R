#### DESCRIPTIVE STATISTICS

D <- readRDS("2_Clean/5_Analysis_I/Final_data/FB_VOTE.rds") # Full data


N <- readRDS("2_Clean/5_Analysis_I/FT_INFO_DATA.rds") %>%
  filter(status == "nuværende")

##################
## FOR DESIGN 1 ##
##################

### Who have not been running any Facebook Ads?
Dn <- as.data.frame(D$KandidatNavn[!duplicated(D$KandidatNavn)])
Nn <- as.data.frame(N$navn[!duplicated(N$navn)])

rows.in.a1.that.are.not.in.a2  <- function(Dn,Nn)
{
  a1.vec <- apply(Dn, 1, paste, collapse = "")
  a2.vec <- apply(Nn, 1, paste, collapse = "")
  a1.without.a2.rows <- Dn[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}
rows.in.a1.that.are.not.in.a2(Nn,Dn) # OBS Lars Christian Lilleholt



### To what extend does politician use FB ads?
partier <- c("Socialdemokratiet", "Venstre, Danmarks Liberale Parti", "Det Konservative Folkeparti", "Dansk Folkeparti", "Enhedslisten", "SF", "Radikale Venstre", "Nye Borgerlige", "Liberal Alliance", "Alternativet", "Kristendemokraterne", "Veganerpartiet")

D1 <- D %>%
  group_by(KandidatNavn) %>%
  summarise(ads = n())

D1 <- filter(D1, !KandidatNavn %in% partier)

ggplot(D1, aes(x = reorder(KandidatNavn, ads), y = ads)) +
  geom_point() +
  coord_flip() +
  theme_minimal()


### Which parties use FB ads?



##################################
#### DESCRIPTIVE DATA SECTION ####
# Party colors
#party.col <- c("red3", "darkviolet", "olivedrab3", "aquamarine3", "hotpink3", "deepskyblue1", "yellow2", "navy", "darkorange2", "green4")
party.col <- c("#F04D46", "#E82583", "#00571F", "#004450", "#BF0418", "#12213f", "#E7D01E", "#005392", "#C21B3E", "#00FF00")

# Standard color-palette
standard.col <- c("darkorange2", "blue3")

# Blok colors
blok.col <- c("navy", "red3")

# Read all ads
A <- readRDS("2_Clean/1_Ads/Ads_2021-04-20.rds")

#### Remove parties ####
partier <- c("Socialdemokratiet", "Venstre, Danmarks Liberale Parti", "Det Konservative Folkeparti", "Dansk Folkeparti", "Enhedslisten", "SF", "Radikale Venstre", "Nye Borgerlige", "Liberal Alliance", "Alternativet", "Kristendemokraterne", "Veganerpartiet")

A1 <- filter(A, !page_name %in% partier)

#### TABLE: How many politicians do we have after each exlusion? ####
unique(A1$page_id) # 148
unique(D$page_id) # 137
unique(D5$page_id) # 123

# Politicians that are excluded due to being not elected at the time: 
# Rasmus Stoklund, Heidi Bank, Marlene Ambo, Jeppe Kofod, Anne Sophie Callesen, Kasper Nordborg, 
# Folketingskandidat Henrik (V), Marie Bjerre, Susanne Zimmer (AA), Victoria Velasquez, Thomas Thomsen
# And Holger K because he did not seek reelection (12 in total)
# That means that we have 136 politicans at start
# Then excluded due to too few characters: Bertel Haarder, Inger Stoejberg, Kenneth Berth, Morten Marinus,
# Sussanne Eilersen, Hans Andersen, Ulla Toernes (7 in total)
# That means that we have 129 politicans after characters
# Then excluded due to too few ads: Ellen Thrane, Jens Rohde, Soeren Espersen, Simon Emil, Soeren Soendergaard, Thomas Jensen (6 in total)
# That means that we have 123 politicians after too few ads

# Who are we exluding?
table(A1$page_name[!A1$page_id %in% D$page_id]) # Not party specific
table(D$Navn[!D$Navn %in% D5$Navn]) # Not party specific

#### What is the mean of ads politicians run in our dataset? ####
mediannads <- median(D5$nads) # Use D5 since it includes Enhedslisten

#### How many politicians do advertise and how many ads do they have? ####
# Who advertise the most?
ads_most1 <- D5 %>%
  group_by(Navn, redblok) %>%
  summarise(nads1 = sum(nads)) %>% 
  mutate(Blok = factor(recode(as.numeric(redblok), "0" = "Højrefløjen", "1" = "Venstrefløjen"),
                       levels = c("Venstrefløjen", "Højrefløjen")))

ads_most2 <- D %>%
  group_by(Navn, redblok) %>%
  summarise(nads1 = sum(spend_upper)) %>% 
  mutate(Blok = factor(recode(as.numeric(redblok), "0" = "Højrefløjen", "1" = "Venstrefløjen"),
                       levels = c("Venstrefløjen", "Højrefløjen")))

ads_most1$Navn[ads_most1$Navn == "Lars Chr"] <- "Lars Chr. Lilleholt"
ads_most2$Navn[ads_most2$Navn == "Lars Chr"] <- "Lars Chr. Lilleholt"

sort(ads_most1$nads1, decreasing = TRUE) # 116 ads for the 10th most politician
sort(ads_most2$nads1, decreasing = TRUE) # 88723 kr. for the 10th most politician

ads_most_plot1 <- ads_most1 %>%
  filter(nads1 >= 116) %>%
  ggplot(aes(x = reorder(Navn, nads1), y = nads1, fill = Blok)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        text = element_text(size = 20, color = "black"),
        axis.title = element_text(color = "black")) +
  xlab("") +
  ylab("Antal annoncer") +
  scale_fill_manual(values = blok.col) +
  coord_flip()

ads_most_plot2 <- ads_most2 %>%
  filter(nads1 >= 88723) %>%
  ggplot(aes(x = reorder(Navn, nads1), y = nads1/1000, fill = Blok)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.title = element_blank(),
        axis.text = element_text(size = 16, color = "black"),
        text = element_text(size = 20, color = "black"),
        axis.title = element_text(color = "black")) +
  xlab("") +
  ylab("Øvre estimat for 1.000 kr. brugt") +
  scale_y_continuous(breaks = c(100, 200, 300, 400, 500)) +
  scale_fill_manual(values = blok.col) +
  coord_flip() 

# Print PDF
pdf("../3_Figurer/41_politician_ads_most.pdf", height = 10, width = 14, onefile = FALSE)
ggarrange(ads_most_plot1, 
          ads_most_plot2, 
          nrow = 1,
          ncol = 2,
          common.legend = TRUE,
          legend = "bottom",
          widths = c(1, 1))
dev.off()

# Big plot
pol_ads <- D5 %>%
  group_by(Navn, Blok) %>%
  summarise(nads1 = sum(nads)) %>%
  ggplot(aes(x = reorder(Navn, nads1), y = nads1, fill = Blok)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("") +
  scale_y_continuous(breaks = c(10, 50, 100, 150, 200, 250, 300, 350, 400)) +
  scale_fill_manual(values = blok.col) +
  geom_hline(yintercept = 10, linetype = "dashed", size = 1)

D5 %>% # 105 politicians have more than 10 ads
  group_by(Navn) %>%
  filter(nads>=10)

pdf("../3_Figurer/41_politician_ads.pdf", height = 5, width = 7.5)
pol_ads
dev.off()

#### How many ads are released throughout the observed period? ####
#average pr. day
adsperday1 <- D %>%
  filter(date <= "2019-06-05") %>%
  group_by(date) %>%
  summarise(adsprday = sum(n()))

meanads1 <- mean(adsperday1$adsprday) # 50 ads pr day on average up to election

adsperday2 <- D %>%
  filter(date >= "2019-06-06") %>%
  group_by(date) %>%
  summarise(adsprday = sum(n()))

meanads2 <- mean(adsperday2$adsprday) # 4 ads pr day on average after election

# Up to election
date_ads1 <- D %>%
  filter(date <= "2019-06-05") %>%
  ggplot(aes(x = date)) +
  geom_bar(size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = meanads1, linetype = "dashed", size = 1)

date_ads2 <- D %>%
  filter(date >= "2019-06-06") %>%
  ggplot(aes(x = date)) +
  geom_bar(size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = meanads2, linetype = "dashed", size = 1)

# Use ggarange before printing to PDF
date_ads <- ggarrange(date_ads1, date_ads2)

# Print to PDF
pdf("../3_Figurer/41_date_ads.pdf", height = 7.5, width = 11.25)
date_ads
dev.off()

# Not divided into two plots
date_ads3 <- D %>%
  ggplot(aes(x = date)) +
  geom_bar(size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("")# +
  #geom_hline(yintercept = meanads2, linetype = "dashed", size = 1)

# Print to PDF
pdf("../3_Figurer/41_date_ads_big.pdf", height = 6, width = 9)
date_ads3
dev.off()

#### How many characters do politicians use? ####
mean(D$char_count) # 697
median(D$char_count) # 466
max(D$char_count) # 5769

D %>% # 254 above 2000 characters
  filter(char_count >= 2000)

D %>% # 2907 ads below 500 characters
  filter(char_count <= 500)

characters_dist <- D %>%
  filter(char_count <= 2000) %>%
  ggplot(aes(x = char_count)) +
  geom_bar(size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("Antal tegn") +
  ylab("Antal annoncer")

# Print to PDF
pdf("../3_Figurer/41_char_dist.pdf", height = 6, width = 9)
characters_dist
dev.off()


#### How many ads and money do politicians from different parties have/spend? ####
partinads <- D %>%
  drop_na(Parti_19) %>%
  group_by(Parti_19) %>%
  summarise(nadsparti = n()) %>%
  ggplot(aes(x = reorder(Parti_19, nadsparti), y = nadsparti, fill = Parti_19)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("Antal annoncer") +
  scale_fill_manual(values = party.col) +
  coord_flip()

partimoney <- D %>%
  drop_na(Parti_19) %>%
  group_by(Parti_19) %>%
  summarise(nspendparti = sum(spend_upper)) %>%
  ggplot(aes(x = reorder(Parti_19, nspendparti), y = nspendparti, fill = Parti_19)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("Øvre estimat for kr. brugt") +
  scale_fill_manual(values = party.col) +
  coord_flip()

partiadnspend <- ggarrange(partinads, partimoney)

# Print to PDF
pdf("../3_Figurer/41_nads_money_parti.pdf", height = 7.5, width = 11.25)
partiadnspend
dev.off()


#### How many ads and money do politicians from different constituencies have/spend? ####
kredsnads <- D %>%
  drop_na(Storkreds_2019) %>%
  group_by(Storkreds_2019) %>%
  summarise(nadsstorkreds = n()) %>%
  ggplot(aes(x = reorder(Storkreds_2019, nadsstorkreds), y = nadsstorkreds)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("Antal annoncer") +
  #scale_fill_manual(values = party.col) +
  coord_flip()

kredsmoney <- D %>%
  drop_na(Storkreds_2019) %>%
  group_by(Storkreds_2019) %>%
  summarise(nspendkreds = sum(spend_upper)/1000) %>%
  ggplot(aes(x = reorder(Storkreds_2019, nspendkreds), y = nspendkreds)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(color = "black"),
        text = element_text(size = 14, color = "black")) +
  xlab("") +
  ylab("Øvre estimat for 1.000 kr. brugt") +
  #scale_fill_manual(values = party.col) +
  coord_flip()

kredsadnspend <- ggarrange(kredsnads, kredsmoney)

# Print to PDF
pdf("../3_Figurer/41_nads_money_kreds.pdf", height = 7.5, width = 11.25)
kredsadnspend
dev.off()


#### HOW MUCH MORE CREDIT CLAIMING DOES MINISTER AND PARTYLEADER? ####
D3_1 %>%
  group_by(Minister) %>%
  summarise(value = mean(strategiw)) %>%
  ggplot(aes(x = Minister, y = value)) +
  geom_bar(stat = "Identity", color = "black", size = 0.35) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")) +
  xlab("Minister") +
  ylab("Gennemsnitlig credit claiming")


#### Number of unique politicians in out dataset
D5 %>% 
  arrange(Navn) %>% 
  select(Politiker = Navn, Parti = Parti_19) %>% 
  distinct(Politiker, .keep_all = TRUE) %>% 
  reactable(resizable = TRUE,
            defaultPageSize = 100,
            highlight = TRUE,
            showSortable = TRUE)
