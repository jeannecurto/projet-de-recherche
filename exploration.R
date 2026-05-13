
#Question biologique 1
#Quelles sont les stations qui se ressemblent le plus et lesquels sont les plus différentes
#--> ACP

#Question biologique 2
#Quelles sont les 10 genres bactériens les plus abondants ? Parmi ces 10 genres quels sont les plus corrélés avec les conditions environnementales ? Prendre un genre (par ex : pseudomonas) et voir avec quelles conditions c'est corrélés (par ex : le pH ou la température)

#Il est important de se focaliser sur les genres les plus connus

#Question biologiques 3
#Est-ce qu'une bactérie est plus significativement présentes qu'une autre ?

#Le but est quand-même de voir si on a des genres bactériens qui sont en lien avec la condition dans laquelle ils se trouvaient

SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")
mesures <-readxl::read_excel("data/Mesures.xlsx")

data_order <- arrange(data, desc(total))

#Les 10 types de bactéries les plus présents sont:
#   1. Aeromonas
#   2. Acinetobacter
#   3. Limnohabitans
#   4. Comanonas
#   5. Raoultella
#   6. Rummeliibacillus
#   7. Klebsiella
#   8. Peribacillus
#   9. Arcobacter
#   10. Rhodoferax


  # ACP des stations
  
  # 1. Garder uniquement les colonnes barcode
  data_num <- data[, grep("barcode", names(data))]
  
  # 2. Convertir en numérique (sécurité)
  data_num <- as.data.frame(lapply(data_num, as.numeric))
  
  # 3. Remplacer les NA par 0 (si nécessaire)
  data_num[is.na(data_num)] <- 0
  
  # 4. Enlever les colonnes constantes (important)
  data_num <- data_num[, apply(data_num, 2, var) != 0]
  
  # 4. Transposer (lignes = stations)
  data_t <- t(data_num)
  
  data_t <- data_t[, apply(data_t, 2, var) != 0]
  
  # 5. ACP
  pca <- prcomp(data_t, scale. = TRUE)
  
  # 6. Résumé de l'ACP
  summary(pca)
  
  # 7. Graphique de l'ACP
  plot(pca$x[,1], pca$x[,2],
    xlab = "PC1",
    ylab = "PC2",
    main = "ACP des stations")
  
  text(pca$x[,1], pca$x[,2],
    labels = rownames(data_t),
    pos = 3)
  
  # 8. Version améliorée (optionnelle)
  library(factoextra)
  
  fviz_pca_ind(pca,
    repel = TRUE,
    title = "ACP des stations",
    col.ind = "#46ACBE")

  
  library(ggplot2)
  
  # Récupérer les coordonnées PCA
  pca_df <- as.data.frame(pca$x)
  
  # Ajouter les noms des stations
  pca_df$station <- rownames(pca_df)
  
  # ✅ OPTION : créer des groupes (à adapter selon ton graphique)
  pca_df$groupe <- "Groupe1"
  
  pca_df$groupe[pca_df$station %in% c("barcode05","barcode07","barcode08","barcode09")] <- "Groupe2"
  pca_df$groupe[pca_df$station %in% c("barcode03","barcode04")] <- "Groupe3"

# Graphique avec ellipses

colors_ordered <- c("#E78AC3", "#98D33A", "#46ACBE", "#bc80bd", "#fdb462", "#ffed6f", "#d9d9d9")

ggplot(pca_df, aes(x = PC1, y = PC2, color = groupe)) +
  geom_point(size = 3) +
  geom_text(aes(label = station), vjust = -1) +
  stat_ellipse(level = 0.95) +
  scale_color_manual(values = colors_ordered)+
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "ACP des stations avec ellipses", x = "Dim1 (26,4%)", y = "Dim2 (17,4%)")


pca_df$groupe [pca_df$station %in% c("barcode01","barcode02","barcode03","barcode04", "barcode05", "barcode06")] <- "Eaux"

pca_df$groupe[pca_df$station %in% c("barcode07","barcode08","barcode09")] <- "Sédiments"

pca_df$groupe[pca_df$station %in% c("barcode11","barcode12","barcode13","barcode14", "barcode15", "barcode16", "barcode17","barcode18","barcode19")] <- "Cultures"

library(ggplot2)
library(ggrepel)

ggplot(pca_df, aes(x = PC1, y = PC2, color = groupe)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = station)) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Eaux" = "#bc80bd",
    "Sédiments" = "#fdb462",
    "Cultures" = "#ffed6f"
  )) +
  labs(
    title = "ACP des stations",
    color = "Groupes"
  )
