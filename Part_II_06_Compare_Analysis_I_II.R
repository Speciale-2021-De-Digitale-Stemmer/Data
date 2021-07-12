
#-------------------------------------------------------------------------
# COMPARE ANALYSIS I OG II 
#-------------------------------------------------------------------------

# Read clean conjoint data file
CJ <- readRDS("2_Clean/6_Analysis_II/Final_data/Conjoint_clean.rds")

# Facebook ad extended version
D <- readRDS("2_Clean/6_Analysis_II/Final_data/FB.rds")

# MM score per storkreds
attr(CJ$Valgkreds, "label") <- "VALGKREDS"

mm_storkreds <- cj(data = CJ,
                   Repstil_choice ~ Valgkreds,
                   id = ~ Respondent,
                   estimate = "mm", h0 = 0.5) %>% 
  mutate(Storkreds_2019 = paste(str_extract(level, ".{1,}(?=[:space:])"), "Storkreds", sep = " ")) # Uppercase the word "storkreds"


# Calculate share of credit claim per storkreds
D_storkreds <- D %>% 
  group_by(Storkreds_2019) %>% 
  summarise(strategi_storkreds = mean(strategiw)) %>% 
  left_join(mm_storkreds[, c("Storkreds_2019", "estimate")], by = "Storkreds_2019") %>% 
  mutate(Storkreds_2019 = str_remove(Storkreds_2019, "Storkreds"), # remove 'storkreds' for plotting purposes
         Storkreds_2019 = str_remove(Storkreds_2019, "s[:space:]$")) # remove trailing space and 's'

# Plot it
Plot_UE <- D_storkreds %>% 
  ggplot(aes(x = strategi_storkreds, y = estimate)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_label_repel(label = D_storkreds$Storkreds_2019, 
                   label.size = NA, size = 4.5, box.padding = 1,
                   max.overlaps = 100) +
  labs(y = "Andel foretrukne credit claiming annoncer", 
       x = "Andel opslåede credit claiming annoncer") +
  xlim(0, 0.80) +
  ylim(0, 0.80) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9),
        text = element_text(size = 20, color = "black"),
        panel.grid = element_blank())

# Save plot
pdf("../3_Figurer/84_udbud_efterspørgsel.pdf", height = 8, width = 10)
Plot_UE
dev.off()

# Calculate number of unique candidates per storkreds
antal_kandidater <- D %>% 
  distinct(Storkreds_2019, Navn) %>% 
  group_by(Storkreds_2019) %>% 
  tally()


# Plot for appendix
plot_antal_kandidater <- antal_kandidater %>% 
  ggplot(aes(x = reorder(Storkreds_2019, n), y = n)) +
  geom_bar(stat = "identity", fill = "black") +
  geom_text(aes(label = n), 
            position = position_stack(vjust = .5),
            colour = "white",
            size = 8) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = "black"),
        text = element_text(size = 20, color = "black")) +
  coord_flip()

# Save plot
pdf("../3_Figurer/83_bilag_antal_kandidater.pdf", height = 8, width = 12)
plot_antal_kandidater
dev.off()

