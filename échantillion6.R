# voir si l'échantillon n°6 a été pollué suite à un orage

SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")

data %>.%
  select(., "genus", "barcode05") %>.%
  filter(., genus != "Unknown") %>.%
  pivot_longer(., cols = -genus, names_to = "station", values_to = "abondance") -> data_05

top20_genres_05 <- data_05 %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 20) %>%
  pull(genus)

top20_data_05 <- data_05 %>%
  filter(genus %in% top20_genres_05) %>%
  mutate(genus = factor(genus, levels = top20_genres_05))

top20_data_05_col <- top20_data_05 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    genus %in% c("Klebsiella","Citrobacter","Enterobacter","Raoultella","Cronobacter","Kluyvera", "Lelliottia","Pseudocitrobacter") ~ "Coliformes",
    TRUE ~ "Non-coliformes"
  ))

chart(data=top20_data_05_col, abondance~ genus %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3",
    "Non-coliformes" = "#d9d9d9"
  )) +
  labs(fill = "Type bactérien")+
  theme(axis.title.x=element_blank())

data%>.%
  select(., "genus", "barcode05","family")%>.%
  data %>%
  filter(., family %in% c("Clostridiaceae", "Enterococcaceae") | (family == "Enterobacteriaceae" & genus == "Escherichia"))%>.%
  pivot_longer(., cols = barcode05, names_to = "station", values_to = "abondance") -> data_entero_05

data_entero_05 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    TRUE ~ "Coliformes"
  ))%>%
  arrange(desc(abondance))%>%
  mutate(genus = factor(genus, levels = genus))-> data_entero_order_05

chart(data=data_entero_order_05, abondance ~ fct_reorder(genus, abondance, .desc=TRUE) %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE"
  )) +
  labs(fill = "Type bactérien", title="Abondance des indicateurs de contamination bactérienne")+
  theme(axis.title.x=element_blank())

# 15

data %>.%
  select(., "genus", "barcode15") %>.%
  filter(., genus != "Unknown") %>.%
  pivot_longer(., cols = -genus, names_to = "station", values_to = "abondance") -> data_15

top20_genres_15 <- data_15 %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 20) %>%
  pull(genus)

top20_data_15 <- data_15 %>%
  filter(genus %in% top20_genres_15) %>%
  mutate(genus = factor(genus, levels = top20_genres_15))

top20_data_15_col <- top20_data_15 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    genus %in% c("Klebsiella","Citrobacter","Enterobacter","Raoultella","Cronobacter","Kluyvera", "Lelliottia","Pseudocitrobacter") ~ "Coliformes",
    TRUE ~ "Non-coliformes"
  ))

chart(data=top20_data_15_col, abondance~ genus %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3",
    "Non-coliformes" = "#d9d9d9"
  )) +
  labs(fill = "Type bactérien")+
  theme(axis.title.x=element_blank())

data%>.%
  select(., "genus", "barcode15","family")%>.%
  filter(., family %in% c("Clostridiaceae", "Enterococcaceae") | (family == "Enterobacteriaceae" & genus == "Escherichia")) %>.%
  pivot_longer(., cols = barcode15, names_to = "station", values_to = "abondance") -> data_entero_15

data_entero_15 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    TRUE ~ "Coliformes"
  ))%>%
  arrange(desc(abondance))%>%
  mutate(genus = factor(genus, levels = genus))-> data_entero_order_15

chart(data=data_entero_order_15, abondance ~ fct_reorder(genus, abondance, .desc=TRUE) %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE"
  )) +
  labs(fill = "Type bactérien", title="Échantillon n°6 en aérobie")+
  theme(axis.title.x=element_blank())

# 18

data %>.%
  select(., "genus", "barcode18") %>.%
  filter(., genus != "Unknown") %>.%
  pivot_longer(., cols = -genus, names_to = "station", values_to = "abondance") -> data_18

top20_genres_18 <- data_18 %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 20) %>%
  pull(genus)

top20_data_18 <- data_18 %>%
  filter(genus %in% top20_genres_18) %>%
  mutate(genus = factor(genus, levels = top20_genres_18))

top20_data_18_col <- top20_data_18 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    genus %in% c("Klebsiella","Citrobacter","Enterobacter","Raoultella","Cronobacter","Kluyvera", "Lelliottia","Pseudocitrobacter") ~ "Coliformes",
    TRUE ~ "Non-coliformes"
  ))

chart(data=top20_data_18_col, abondance~ genus %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3",
    "Non-coliformes" = "#d9d9d9"
  )) +
  labs(fill = "Type bactérien")+
  theme(axis.title.x=element_blank())

data%>.%
  select(., "genus", "barcode18","family")%>.%
  filter(., family %in% c("Clostridiaceae", "Enterococcaceae") | (family == "Enterobacteriaceae" & genus == "Escherichia")) %>.%
  pivot_longer(., cols = barcode18, names_to = "station", values_to = "abondance") -> data_entero_18

data_entero_18 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    TRUE ~ "Coliformes"
  ))%>%
  arrange(desc(abondance))%>%
  mutate(genus = factor(genus, levels = genus))-> data_entero_order_18

chart(data=data_entero_order_18, abondance ~ fct_reorder(genus, abondance, .desc=TRUE) %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE"
  )) +
  labs(fill = "Type bactérien", title="Échantillon n°6 en fermentation")+
  theme(axis.title.x=element_blank())

data%>.%
  select(., barcode05, barcode15, barcode18, family, genus)%>.%
  filter(., family %in% c("Clostridiaceae", "Enterococcaceae") | (family == "Enterobacteriaceae" & genus == "Escherichia"))%>.%
  pivot_longer(.,cols = c("barcode05", "barcode15", "barcode18"), names_to="station", values_to="abondance")-> data_ech6


chart(data_ech6, log(abondance)~station %fill=% abondance)+
  geom_col(fill="#79B15B")+
  labs(title="Évolution de l'abondance bactérienne selon les conditions de mise en culture")
