SciViews::R("explore", "model", "infer")

data <-readxl::read_excel("data/Séquençage.xls")

data %>.%
  select(., "genus", "barcode16") %>.%
  filter(., genus != "Unknown") %>.%
  pivot_longer(., cols = -genus, names_to = "station", values_to = "abondance") -> data_16

top20_genres_16 <- data_16 %>%
  group_by(genus) %>%
  summarise(total_abondance = sum(abondance, na.rm = TRUE)) %>%
  arrange(desc(total_abondance)) %>%
  slice_head(n = 20) %>%
  pull(genus)

top20_data_16 <- data_16 %>%
  filter(genus %in% top20_genres_16) %>%
  mutate(genus = factor(genus, levels = top20_genres_16))

top20_data_16_col <- top20_data_16 %>%
  mutate(type = case_when(
    genus == "Escherichia" ~ "E.coli",
    genus %in% c("Klebsiella","Citrobacter","Enterobacter","Raoultella","Cronobacter","Kluyvera", "Lelliottia","Pseudocitrobacter") ~ "Coliformes",
    TRUE ~ "Non-coliformes"
  ))

chart(data=top20_data_16_col, abondance~ genus %fill=% type)+
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
  select(., "genus", "barcode16","family")%>.%
filter(., genus != "Unknown", family=="Enterobacteriaceae") %>.%
  pivot_longer(., cols = barcode16, names_to = "station", values_to = "abondance") -> data_entero

data_entero %>%
mutate(type = case_when(
  genus == "Escherichia" ~ "E.coli",
  TRUE ~ "Coliformes"
))%>%
arrange(desc(abondance))%>%
mutate(genus = factor(genus, levels = genus))-> data_entero_order

chart(data=data_entero_order, abondance ~ fct_reorder(genus, abondance, .desc=TRUE) %fill=% type)+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c(
    "E.coli" = "#46ACBE",
    "Coliformes" = "#E78AC3"
  )) +
  labs(fill = "Type bactérien")+
  theme(axis.title.x=element_blank())
