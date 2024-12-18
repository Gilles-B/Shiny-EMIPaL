# Rapport automatisé Interventions EMIPAL - Octobre 2024 #
# Script pour le chargement des données #

# CHARGEMENT DES DONNEES ----
url <- "https://docs.google.com/spreadsheets/d/1YgsL6uNEdB0FokdMGu6VXrm8ZeGH-_IF8cQZY_AGQcY/edit?usp=sharing"
bdd <- as.data.frame(read_delim(gsheet2text(url,
                                          format = 'csv', ),
                              skip_empty_rows = T,
                              show_col_types = F,
                              locale = locale(date_names = "fr",
                                              encoding = 'UTF-8'),
                              trim_ws = T))

# NETTOYAGE & TRANSFORMATION & CREATION DE VARIABLES ----

## Remplacement " " par "_" dans les noms des colonnes
colnames(bdd) <- gsub(" ", "_", colnames(bdd))

## Suppression espace avant et après chaînes de caractères dans la bdd
bdd <- as.data.frame(apply(bdd, 2, function (x) sub("\\s+$", "", x)))

bdd <- bdd %>%
  # remove NAs
  filter(!is.na(nom) & !is.na(prenom) & !is.na(ddn) & !is.na(sexe)) %>%
  # transformation date_mission en format date
  mutate(date_mission = as.Date(date_mission, format = "%d/%m/%Y")) %>%
  # transformation ddn en format date
  mutate(ddn = as.Date(ddn, format = "%d/%m/%Y")) %>%
  # transformation sexe en format factor
  mutate(sexe = factor(sexe, levels = c("feminin", "masculin", "autre"))) %>%
  # création de la variable "mois"
  mutate(mois2 = str_to_title(format(date_mission, "%B"))) %>%
  mutate(mois3 = as.character(month(date_mission))) %>%
  mutate(mois3 = if_else(nchar(mois3)<2, paste0('0', mois3), mois3)) %>%
  unite(mois, mois3:mois2, sep = "-") %>%
  # age
  mutate(age = year(Sys.Date()) - year(ddn)) %>%
  mutate(age_class = cut(age, breaks = c(0,14,24,44,64,100),
          inlcude.lowest = T,
          labels = c('[0,14]', '[15,24]', '[25,44]', '[45,64]','65+')))

# PSEUDONYMISATION ----
bdd <- bdd %>%
  mutate(ID = hash_names(bdd$nom, bdd$prenom, bdd$ddn, bdd$sexe)$hash_short) %>%
  select(-nom, -prenom) 

saveRDS(bdd, file = "data/bdd.rds")
save(bdd, file = "data/mydata.rda")

cat(insight::print_color(paste0("\n** Les données sont chargées et pseudonymisées **\n"),
                         color = "green"))

# FIN ----

#  # création de la variable ID
#  mutate(ID = paste0(
#  sprintf("%.4s", gsub("[[:space:]]", "", prenom)),"_",
#  sprintf("%.4s", gsub("[[:space:]]", "", nom)),"_",
#  sprintf("%.4s", ddn))) %>%
#  mutate(ID = stringi::stri_trans_general(ID, "Latin-ASCII"))


