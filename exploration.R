
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

data_pca <- data %>%
  column_to_rownames("genus") %>%  # Utiliser le genre comme nom de ligne
  select(starts_with("barcode")) %>%  # Sélectionner uniquement les colonnes barcode
  t() %>%  # Transposer
  as.data.frame()

data_pca_clean <- data_pca %>%
  select(where(~var(., na.rm = TRUE) > 0))

# Effectuer l'ACP
commfin_pca <- data_pca_clean %>.%
  pca(., scale = TRUE)

summary(commfin_pca)


data_pca %>.%
select(., Actinomarinicola : Lactivibrio )%>.%
pca(., scale=TRUE)

data_corr <- correlation(data_pca)
tabularise(data_corr)

plot(data_corr)

# Effectuer l'ACP
commfin_pca <- pca(data = data_pca, scale = TRUE)

# Résumé
summary(commfin_pca)

# Visualisations
chart$scree(commfin_pca, fill = "#FFDBF3")

chart$loadings(commfin_pca, max.overlaps=Inf)

chart$scores(commfin_pca, max.overlaps=Inf)

chart$biplot(commfin_pca)
