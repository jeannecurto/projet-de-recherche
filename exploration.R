
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
head(data_order)

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

