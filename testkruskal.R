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
  filter(genus != "Unknown")%>%
  pivot_longer(cols = -genus, names_to = "station", values_to = "abondance")

# Test Kruskal-Wallis
kw_test <- kruskal.test(abondance ~ genus, data = data_long)
# p-value indique si différences significatives
tabularise(kw_test)

# 2. VISUALISATION : Boîtes à moustaches des 10 genres

library(ggplot2)

top10_genres <- data_long %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 10) %>%
  pull(genus)

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

# 3. RASTER : Abondance par genre et station
# sur tout

chart(top10_data, genus ~ station %fill=% abondance) +
  geom_raster()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8))+
  scale_fill_gradient2(low="white", mid="#98D33A", high="#46ACBE", midpoint=50000)


# sur nos cultures

data_long_culture <- data %>%
  select(genus, barcode10:barcode19) %>%
  filter(genus != "Unknown")%>%
  pivot_longer(cols = -genus, names_to = "station", values_to = "abondance")

top10_genres_culture <- data_long_culture %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 10) %>%
  pull(genus)

top10_data_culture <- data_long_culture %>%
  filter(genus %in% top10_genres) %>%
  mutate(genus = factor(genus, levels = top10_genres))

top10_data_culture %>.%
chart(., genus ~ station %fill=% abondance) +
  geom_raster()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_gradient2(low="white", mid="#98D33A", high="#46ACBE", midpoint=50000)
