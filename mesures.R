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

data_small%>.%
arrange(., desc(abondance)) %>.%
  filter(., genus != "Unknown") %>.%
  slice_head(., n = 50)-> mini_full_data

# analyses

# =========================
# CORRELATIONS + HEATMAP
# =========================

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# 1. Renommer les variables environnementales
full_clean <- full_data %>%
  rename(
    sel = `sel (‰)`,
    O2_T0 = `dioxygène (T0) (mg/L)`,
    O2_T4 = `dioxygène (T4) (mg/L)`,
    DBO4 = `DBO4 (mg/L)`,
    Temp = `T° (°C)`,
    cond = `conductivité (µS/cm)`,
    MES = `matière en suspension (mg/L)`
  )

# 2. Calcul des corrélations (Spearman)
cor_results <- full_clean %>%
  group_by(genus) %>%
  summarise(
    cor_pH = cor(abondance, pH, method = "spearman", use = "complete.obs"),
    cor_sel = cor(abondance, sel, method = "spearman", use = "complete.obs"),
    cor_O2 = cor(abondance, O2_T0, method = "spearman", use = "complete.obs"),
    cor_DBO4 = cor(abondance, DBO4, method = "spearman", use = "complete.obs"),
    cor_temp = cor(abondance, Temp, method = "spearman", use = "complete.obs"),
    cor_cond = cor(abondance, cond, method = "spearman", use = "complete.obs"),
    cor_MES = cor(abondance, MES, method = "spearman", use = "complete.obs")
  )

cor_strong <- cor_long %>%
  filter(abs(correlation) > 0.6)


cor_stronger <- cor_long %>%
  filter(abs(correlation) > 0.8)

plot(cor_stronger)

ggplot(cor_stronger, aes(x = correlation, y = genus)) +
  geom_col() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal()
