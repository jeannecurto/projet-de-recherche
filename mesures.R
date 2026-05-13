SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")

data %>.%
  select(., genus:barcode06, )%>.%
  pivot_longer(., cols=-genus, names_to ="station", values_to="abondance") -> data_small

mesures <-readxl::read_excel("data/Mesures.xlsx")

mesures%>.%
t(.)%>.%
as.data.frame(.) -> mesures_pi

# prendre la 1ère ligne comme noms de colonnes
colnames(mesures_pi) <- mesures_pi[1, ]

# supprimer la ligne devenue inutile
mesures_pi <- mesures_pi[-1, ]

mesures_pi[,-1] <- lapply(mesures_pi[,-1], function(x) {
  as.numeric(gsub(",", ".", trimws(x)))
})

mesures_pi %>%
mutate(as.numeric(pH))%>%
select(-pH) -> kfqb

rename(kfqb, pH=`as.numeric(pH)`)-> new

rownames_to_column(new, var="station")->lol

left_join(data_small, lol, by="station")-> full_data


