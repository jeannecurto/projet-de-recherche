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
kw_test
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

library(dplyr)

# 1. récupérer tous les genres présents dans tes 3 graphiques
genres_all <- unique(c(
  levels(factor(top10_data$genus)),
  levels(factor(top10_data_brut$genus)),
  levels(factor(top10_data_culture$genus))
))

# 2. créer une palette fixe
set.seed(123)  # pour reproductibilité

colors_genres <- setNames(
  scales::hue_pal()(length(genres_all)),
  genres_all
)



ggplot(top10_data, aes(x = genus, y = abondance, fill = genus)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Abondance des 10 genres bactériens principaux",
    y = "Abondance", x = "Genre") +
  theme(legend.position = "none")+
  scale_fill_manual(values = colors_genres)


data_longless <- data_long %>%
  filter(station%in% c("barcode01","barcode02","barcode03","barcode04","barcode05","barcode06","barcode07","barcode08","barcode09"))

top10_genres_brut <- data_longless %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 10) %>%
  pull(genus)

top10_data_brut <- data_longless %>%
  filter(genus %in% top10_genres_brut) %>%
  mutate(genus = factor(genus, levels = top10_genres_brut))

ggplot(top10_data_brut, aes(x = genus, y = abondance, fill = genus)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Abondance des 10 genres bactériens principaux dans les échantillons bruts",
    y = "Abondance", x = "Genre") +
  theme(legend.position = "none")+
  scale_fill_manual(values = colors_genres)

#################################

data_longlost <- data_long %>%
  filter(station%in% c("barcode01","barcode02","barcode03","barcode04","barcode05","barcode06","barcode07","barcode08","barcode09"))

top10_genres_cult <- data_longlost %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 10) %>%
  pull(genus)

top10_data_brut <- data_longless %>%
  filter(genus %in% top10_genres_brut) %>%
  mutate(genus = factor(genus, levels = top10_genres_brut))

ggplot(top10_data_culture, aes(x = genus, y = abondance, fill = genus)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Abondance des 10 genres bactériens principaux dans les cultures",
    y = "Abondance", x = "Genre") +
  theme(legend.position = "none")+
  scale_fill_manual(values = colors_genres)

# 3. RASTER : Abondance par genre et station
# sur tout

chart(top10_data, genus ~ station %fill=% abondance) +
  geom_raster()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8), axis.title.y=element_blank(), axis.title.x=element_blank())+
  scale_fill_gradient2(low="white", mid="#98D33A", high="#46ACBE", midpoint=50000)+
  labs(title= "Abondance des 10 genres")


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
  filter(genus %in% top10_genres_culture) %>%
  mutate(genus = factor(genus, levels = top10_genres_culture))

top10_data_culture %>.%
chart(., genus ~ station %fill=% abondance) +
  geom_raster()+
  labs(title= "Abondance des 10 genres dans les différents milieux de culture")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.title.x=element_blank(), axis.title.y=element_blank(), plot.title=element_text(size=10))+
  scale_fill_gradient2(low="white", mid="#98D33A", high="#46ACBE", midpoint=50000)


library(vegan)

shannon <- diversity(data_t, index = "shannon")

shannon_df <- data.frame(
  station = names(shannon),
  Shannon = shannon
)

shannon_df$type <- ifelse(
  shannon_df$station %in% paste0("barcode0", 1:9),
  "Brut",
  "Culture"
)

kruskal.test(Shannon ~ type, data = shannon_df)%>%
  tabularise()



ggplot(shannon_df, aes(x = type, y = Shannon)) +
  geom_boxplot(fill = "#79B15B") +
  theme_minimal() +
  labs(
    title = "Diversité bactérienne (Shannon)",
    x = "Type d’échantillon",
    y = "Indice de Shannon"
  )
