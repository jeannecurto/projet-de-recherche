SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")

# barcode03

data %>.%
  select(., "genus", "barcode03") %>.%
  filter(., genus != "Unknown") %>.%
  pivot_longer(., cols = -genus, names_to = "station", values_to = "abondance") -> data_03

top20_genres_03 <- data_03 %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 20) %>%
  pull(genus)

top20_data_03 <- data_03 %>%
  filter(genus %in% top20_genres_03) %>%
  mutate(genus = factor(genus, levels = top20_genres_03))

top20_data_03_col <- top20_data_03 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    genus %in% c("Klebsiella","Citrobacter","Enterobacter","Raoultella","Cronobacter","Kluyvera", "Lelliottia","Pseudocitrobacter") ~ "Coliformes",
    TRUE ~ "Non-coliformes"
  ))

chart(data=top20_data_03_col, abondance~ genus %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3",
    "Non-coliformes" = "#d9d9d9"
  )) +
  labs(fill = "Type bactérien", title="Abondance des 20 premiers genres bactériens")+
  theme(axis.title.x=element_blank())

data%>.%
  select(., "genus", "barcode03","family")%>.%
  filter(.,!(genus %in% c("Unknown", "Salmonella", "Shigella", "Yokenella", "Cedecea", "Trabulsiella", "Scandinavium", "Atlantibacter", "Symbiopectobactérium")), family=="Enterobacteriaceae") %>.%
  pivot_longer(., cols = barcode03, names_to = "station", values_to = "abondance") -> data_entero_03

data_entero_03 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    TRUE ~ "Coliformes"
  ))%>%
  arrange(desc(abondance))%>%
  mutate(genus = factor(genus, levels = genus))-> data_entero_order_03

chart(data=data_entero_order_03, abondance ~ fct_reorder(genus, abondance, .desc=TRUE) %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3"
  )) +
  labs(fill = "Type bactérien", title="Abondance des coliformes")+
  theme(axis.title.x=element_blank())

# barcode14

SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")

data %>.%
  select(., "genus", "barcode14") %>.%
  filter(., genus != "Unknown") %>.%
  pivot_longer(., cols = -genus, names_to = "station", values_to = "abondance") -> data_14

top20_genres_14 <- data_14 %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 20) %>%
  pull(genus)

top20_data_14 <- data_14 %>%
  filter(genus %in% top20_genres_14) %>%
  mutate(genus = factor(genus, levels = top20_genres_14))

top20_data_14_col <- top20_data_14 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    genus %in% c("Klebsiella","Citrobacter","Enterobacter","Raoultella","Cronobacter","Kluyvera", "Lelliottia","Pseudocitrobacter") ~ "Coliformes",
    TRUE ~ "Non-coliformes"
  ))

chart(data=top20_data_14_col, abondance~ genus %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3",
    "Non-coliformes" = "#d9d9d9"
  )) +
  labs(fill = "Type bactérien", title="Abondance des 20 premiers genres bactériens")+
  theme(axis.title.x=element_blank())

data%>.%
  select(., "genus", "barcode14","family")%>.%
  filter(.,!(genus %in% c("Unknown", "Salmonella", "Shigella", "Yokenella", "Cedecea", "Trabulsiella", "Scandinavium", "Atlantibacter", "Symbiopectobactérium")), family=="Enterobacteriaceae") %>.%
  pivot_longer(., cols = barcode14, names_to = "station", values_to = "abondance") -> data_entero_14

data_entero_14 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    TRUE ~ "Coliformes"
  ))%>%
  arrange(desc(abondance))%>%
  mutate(genus = factor(genus, levels = genus))-> data_entero_order_14

chart(data=data_entero_order_14, abondance ~ fct_reorder(genus, abondance, .desc=TRUE) %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3"
  )) +
  labs(fill = "Type bactérien", title="Abondance des coliformes")+
  theme(axis.title.x=element_blank())
