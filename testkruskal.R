SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")

# ========================================
# QUESTION BIOLOGIQUE 3
# Significativité de la présence bactérienne
# ========================================

# 1. TEST KRUSKAL-WALLIS (non-paramétrique)
# Teste si au moins un genre a une abondance significativement différente des autres

# Préparer les données au format long
library(tidyr)

data_long <- data %>%
  select(genus, starts_with("barcode")) %>%
  pivot_longer(cols = -genus, names_to = "station", values_to = "abondance")

# Test Kruskal-Wallis
kw_test <- kruskal.test(abondance ~ genus, data = data_long)
print(kw_test)  # p-value indique si différences significatives

# 2. VISUALISATION : Boîtes à moustaches des 10 genres
library(ggplot2)

top10_data <- data_long %>%
  filter(genus %in% top10_genres) %>%
  mutate(genus = factor(genus, levels = top10_genres))

ggplot(top10_data, aes(x = genus, y = abondance, fill = genus)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Abondance des 10 genres bactériens principaux",
    y = "Abondance", x = "Genre") +
  theme(legend.position = "none")

# 4. TABLEAU RÉSUMÉ
summary_stats <- data_long %>%
  filter(genus %in% top10_genres) %>%
  group_by(genus) %>%
  summarise(
    moyenne = mean(abondance, na.rm = TRUE),
    mediane = median(abondance, na.rm = TRUE),
    ecart_type = sd(abondance, na.rm = TRUE),
    min = min(abondance, na.rm = TRUE),
    max = max(abondance, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(moyenne))

print(summary_stats)

# 5. TEST ANOVA (paramétrique alternative)
anova_result <- aov(abondance ~ genus, data = filter(data_long, genus %in% top10_genres))
summary(anova_result)

# 6. HEATMAP : Abondance par genre et station

install.packages("pheatmap")

library(pheatmap)

heatmap_data <- data %>%
  filter(genus %in% top10_genres) %>%
  column_to_rownames(var = "genus") %>%
  select(starts_with("barcode"))

pheatmap(heatmap_data,
  main = "Heatmap : Abondance des 10 genres par station",
  scale = "row",
  color = colorRampPalette(c("lightgreen", "white", "skyblue"))(50))
