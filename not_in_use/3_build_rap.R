# Rapport automatisé Interventions EMIPAL - Octobre 2024 #
# Script pour le génération du rapport sur word #

if (nrow(bdd) > 0) {

# Initialisation du document word ----
doc <- read_docx(file.path('template/temp_rapport_palu.docx')) %>%
  body_add_par(paste0('Date du rapport : ',
                format(Sys.Date(), format = '%d/%m/%Y')), style = 'Normal') %>%
  body_add_par(paste0("Période des données : du ",
                      format(min(bdd$date_mission), format = '%d/%m/%Y'), " au ",
                      format(max(bdd$date_mission), format = '%d/%m/%Y'))) %>%
  body_add_par(paste0("Lieu d'intervention : ", site))


# GRAPHIQUES ----
## FIGURE 1 : Âge & Sexe ----
doc <-    body_add_par(doc,
                       "Âge et sexe des personnes consultées",
                       style = 'heading 1')
df <- bdd %>%
# Reshape df
  select(ID, ddn, sexe) %>%
  mutate(age = year(Sys.Date()) - year(ddn)) %>%
  mutate(age_class = cut(age, breaks = c(0,14,24,44,64,max(age)),
          inlcude.lowest = T,
          labels = c('[0,14]', '[15,24]', '[25,44]', '[45,64]','65+'))) %>%
  select(ID, sexe, age_class) %>% unique()

doc <- body_add_par(doc, paste0(
                       "Au total, ", length(unique(df$ID)), " personnes ont été consultées sur ce site depuis le début des interventions."), style = 'Normal')

# Plot
fig_age_sexe <- df %>%
  ggplot(aes(x = age_class, y = after_stat(count), fill = sexe)) + 
  geom_bar(position = position_dodge2(preserve = 'single'), width = .5) + 
  scale_fill_manual(values = c('#b286c7', '#e99052', '#06c2ac'),
                    labels = c('Femmes','Hommes', "Autre")) +
  geom_text(aes(label = after_stat(count)),
            stat = 'count',
            vjust = -.5,
            position = position_dodge2(width = .5, preserve = 'single'),
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  labs(x = '') +
  # Format axis y
  scale_y_continuous(name = "Nombre de personnes",
                     labels = function(x) round(as.numeric(x)),
                     limits = c(0,max(table(df$sexe, df$age_class))+5),
                     breaks = seq(0,max(table(df$sexe, df$age_class))+5,
                                 by = 5), 
                     expand = expansion(mult = c(0, 0))) +
  # Thème
  theme_emipal()
fig_age_sexe

# Enregistrement de la figure
ggsave('figures/fig_age_sexe.png', fig_age_sexe,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = 'figures/fig_age_sexe.png',
              width = 6.2,
              height = 3,
              style = 'Figure') %>% body_add_break()


## FIGURE 2 : Nombre d'orpailleurs par pays d'origine ----
doc <-    body_add_par(doc, " Nombre d'orpailleurs selon le pays d'origine",
                       style = 'heading 1')
# Reshape
df <- bdd %>%
  select(ID, orpaillage, pays_de_naissance) %>%
  filter(orpaillage == "Oui" & pays_de_naissance != "Donnée manquante") %>%
  group_by(pays_de_naissance) %>%
  unique() %>%
  summarise(freq = n())
# Plot
fig_nb_orpa_pays <- df %>%
  ggplot(aes(x = pays_de_naissance, y = freq)) +
  geom_bar(position = position_dodge2(preserve = 'single'),
           stat = 'identity', fill = '#06c2ac',  # '#053948',
           width = .5) +
  geom_text(aes(label = freq),
            vjust = - .5,
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  labs(x = '') +
  scale_x_discrete(labels = function(x){sub("\\s", "\n", x)}) +
  # Format axis y
  scale_y_continuous(name = "Nombre de personnes",
                     labels = function(x) round(as.numeric(x)),
                     limits = c(0, max(df$freq)*1.1),
                     breaks = seq(0,max(df$freq)*1.1,
                                  by = 5),
                     expand = expansion(mult = c(0.01, 0.06))) +
  # Thème
  theme_emipal()
fig_nb_orpa_pays

# Enregistrement de la figure
ggsave('figures/fig_nb_orpa_pays.png', fig_nb_orpa_pays,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = 'figures/fig_nb_orpa_pays.png',
              width = 6.2,
              height = 3,
              style = 'Figure')


## FIGURE 3 : Provenance des personnes selon les sites d'orpaillage ----
doc <-    body_add_par(doc,
                       "Provenance des personnes selon les sites d'orpaillage ",
                       style = 'heading 1')

df <- bdd %>%
# Reshape sf
  filter(orpaillage == "Oui" & !is.na(camps_orpaillage__1)) %>%
  select('ID', "orpaillage",
         "camps_orpaillage__1", date_mission) %>%
  unique()
# Plot
fig_prop_orpa_site <- df %>%
  ggplot(aes(x = date_mission, fill = camps_orpaillage__1)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = 
                      colorRampPalette(brewer.pal(8, "Set2"))(length(table(df$camps_orpaillage__1)))) +
  scale_x_date(name = "",
               breaks = unique(df$date_mission), 
               labels = unique(df$date_mission), date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -0,
                                   hjust = -0.6,
                                   color = "grey30")) +
  # Format axis y
  scale_y_continuous(name = "Proportion",
                     labels = scales::percent,
                     breaks = scales::breaks_pretty(10)) +
  # Legend
  theme(legend.margin=margin(t=-25),) +
  # Thème
  theme_emipal()
fig_prop_orpa_site

# Enregistrement de la figure
ggsave('figures/fig_prop_orpa_site.png', fig_prop_orpa_site,
       width = 9,
       height = 6,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = 'figures/fig_prop_orpa_site.png',
              width = 6.2,
              height = 4,
              style = 'Figure') %>% body_add_break()

## FIGURE 4 : Nombre et type de consultations ----
doc <-    body_add_par(doc, 'Nombre et type de consultations',
                       style = 'heading 1')

# Reshape
df <- bdd %>%
  select(date_mission, Motif_Principal)

doc <- body_add_par(doc, paste0("Au total, ", sum(!is.na(df$Motif_Principal)),
                    " consultations médicales ont été réalisées."))
# Plot
fig_nb_consult <- df %>%
  ggplot(aes(x = date_mission, fill = Motif_Principal)) +
  geom_bar(position = position_dodge2(preserve = 'single'), width = 5) +
  geom_text( aes(label = after_stat(count)), stat = 'count', vjust = -0.5,
            position = position_dodge2(width = 5, preserve = 'single'),
            color = "grey30",
            size = 9, size.unit = 'pt') +
  scale_fill_manual(values = c('#06c2ac', 'firebrick3')) +
  # Format axis x
  labs(x = '') +
  scale_x_date(breaks = unique(df$date_mission),
               labels = unique(df$date_mission), date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -.4,
                                   hjust = -0.4,
                                   size = 9)) +
  # Format axis y
  scale_y_continuous(name = "Nombre de consultations",
                     labels = function(x) round(as.numeric(x)),
                     expand = expansion(mult = c(0, 0.09))) +
  labs(y = 'Nombre de consultations') +
  # Legend
  theme(legend.margin = margin(t = -15),) +
  # Thème
  theme_emipal()
fig_nb_consult

# Enregistrement de la figure
ggsave('figures/fig_nb_consult.png', fig_nb_consult,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = 'figures/fig_nb_consult.png',
              width = 6.2,
              height = 3,
              style = 'Figure')

## FIGURE 5 : Consultations médicales pour introduction de Primaquine et rendu BS ----
doc <-    body_add_par(doc, "Consultations médicales pour introduction ou suivi de primaquine et rendu de bilan sanguin", style = 'heading 1')

# Reshape
df <- bdd %>%
  select(mois, Motif_Principal, Motifs_secondaires) %>%
  filter(Motif_Principal == "Suivi" &
        grepl(paste(c("Introduction PQ", "PQ J7", "PQ J14", "PQ J21",
                      "Rendu BS sans PQ"), collapse="|"),
             bdd$Motifs_secondaires) == T |
          (Motif_Principal == "Primo-consultation" &
        grepl(paste(c("Introduction PQ"), collapse="|"),
             bdd$Motifs_secondaires) == T)) %>%
    mutate(motif = as.character(stringi::stri_extract_all_regex(Motifs_secondaires,
    '(Introduction PQ|PQ J7|PQ J14|PQ J21|Rendu BS sans PQ)'))) %>%
  group_by(mois, motif) %>%
  summarise(freq = n()) %>%
  mutate(percent = round(prop.table(freq),2))

if (nrow(df) == 0) {
  doc <- body_add_par(doc, paste0("Aucune consultation médicale de suivi n'a été réalisée pour introduction et suivi de primaquine, rendu de bilan sanguin."))
  }

if (nrow(df) != 0) {
doc <- body_add_par(doc, paste0("Au total, ", sum(df$freq),
          " consultations médicales ont été réalisées pour introduction et suivi de primaquine, et rendu de bilan sanguin."))

# Plot
fig_consult_suivi <- df %>%  
  ggplot(aes(x = mois, y = percent, fill = motif)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(aes(label = paste0(percent*100,"%")),
            position = position_fill(0.5), size = 3) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  # Format axis x
  labs(x = '') +
  scale_x_discrete(labels = function(x){gsub(".*-","",x)}) +
  theme(axis.text.x = element_text(vjust = 6,
                                   hjust = 0.5,
                                   size = 9)) +
  # Format axis y
  scale_y_continuous(name = "Proportion",
                     labels = scales::percent) +
  labs(y = '') +
  # Thème
  theme_emipal()
fig_consult_suivi  

# Enregistrement de la figure 
ggsave('figures/fig_consult_suivi.png', fig_consult_suivi,
       width = 9,
       height = 6,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = 'figures/fig_consult_suivi.png',
              width = 6.2,
              height = 4,
              style = 'Figure')
}

## FIGURE 6 : Nombre d'interventions réalisées par site d'interventions ----
doc <- body_add_par(doc,
  "Nombre d'interventions réalisées par site et nombre de sites d'intervention",
  style = "heading 1")

# Reshape
df <- bdd %>%
  select(lieu, date_mission) %>% unique() %>%
  group_by(lieu) %>%
  summarise(freq = n())

doc <- body_add_par(doc, paste0("Au total, ", length(unique(bdd$date_mission)),
      " missions ont été réalisées sur ", nrow(df), " site(s) d'intervention."),
      style = "Normal")

# Plot
fig_sites_inter <- df %>%  
  ggplot(aes(x = factor(lieu), y = freq)) +
  geom_bar(position = "stack", stat = "identity",
           fill = 'royalblue', width = 0.5) + # '#1663a7'
  geom_text(aes(label = freq), vjust = -1,
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  labs(x = '') +
  # Format axis y
  scale_y_continuous(name = "Nombre d'interventions",
                     labels = function(x) round(as.numeric(x)),
                     limits = c(0, max(df$freq)*1.1),
                     breaks = seq(0,max(df$freq)*1.1,
                                  by = 5),
                     expand = expansion(mult = c(0, 0.09))) +
  # Thème
  theme_emipal()
fig_sites_inter

# Enregistrement de la figure
ggsave('figures/fig_sites_inter.png',
       fig_sites_inter,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
                    src = "figures/fig_sites_inter.png",
                    width = 6.2,
                    height = 3,
                    style = "Figure") %>% body_add_break()

## FIGURE 7 : PCR - Taux de positivité des personnes dépistées par PCR  ----
doc <- body_add_par(doc,
        "Taux de positivité des personnes dépistées par PCR",
        style = "heading 1")

# Reshape df
df <- bdd %>%
  filter(! PCR %in% c("NR", "EC", NA_character_)) %>%
  select(ID, orpaillage, date_mission, PCR, riamet, pq1, pq_2) %>%
  select(-ID, -orpaillage, -riamet, -pq1, -pq_2) %>%
  group_by(date_mission, PCR) %>%
  summarise(n = n()) %>%
  spread(PCR, n) %>%
  right_join(x = .,
             y = tibble('date_mission' = as.Date(names(table(bdd$date_mission)))),
             by = "date_mission")
if (!"Positive à P. falciparum" %in% names(df) == T) {
  df$`Positive à P. falciparum` <- NA
}
if (!"Positive à P. vivax" %in% names(df) == T) {
  df$`Positive à P. vivax` <- NA
}
if (!"Négative" %in% names(df) == T) {
  df$`Négative` <- NA
}

df <- df %>%
  replace(is.na(.), 0) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(Tot = rowSums(across(starts_with(c('Dis', 'Pos', 'Nég'))))) %>%
  mutate(Pos = rowSums(across(starts_with(c('Dis', 'Pos'))))) %>%
  mutate(prop_posF = if_else(Tot == 0, 0,
                  as.numeric(round(`Positive à P. falciparum`/Tot*100,1)))) %>%
  mutate(prop_posV = if_else(Tot == 0, 0,
                  as.numeric(round(`Positive à P. vivax`/Tot*100,1)))) %>%
  mutate(prop_posF = ifelse(!is.na(`Négative`) &
                              is.na(prop_posF) & is.na(`Négative`),
         100, prop_posF)) %>%
  mutate(prop_posV = ifelse(!is.na(`Positive à P. vivax`) &
                              is.na(prop_posV) & is.na(`Négative`),
         100, prop_posV))

if (sum(df$Tot) == 0) {
doc <- body_add_par(doc,
      "Aucun test PCR n'a été effectué.", style = "Normal")
}

doc <- body_add_par(doc,
        paste0("Au total, ", sum(df$Tot),
      " tests PCR ont été effectués parmi lesquels ",
      sum(df$Pos), " se sont révélés positifs."), style = "Normal")

# Plot
fig_PCR <- df %>%
  ggplot() +
  # Total tests négatifs
  geom_bar(aes(x = date_mission, y = Tot, fill = "Tests négatifs"), 
           stat = "identity",
           color = "white",
           position = position_dodge2(preserve = "single"),
           width = 3) +
  # Total tests positifs Vivax+Falciparum
  geom_bar(aes(x = date_mission,
               y = `Positive à P. falciparum` + `Positive à P. vivax`,
               fill = "Tests positifs"),
           stat = "identity",
           position = position_dodge2(preserve = "single"),
           color = "white",
          width = 3) +
  # Taux de positivité Vivax
  geom_line(aes(x = date_mission,
                y = prop_posV*(30/100), group = 1, lty = "Vivax"),
            color = "red", inherit.aes = FALSE, linewidth = 0.6) +
  # Taux de positivité Falciarum
  geom_line(aes(x = date_mission,
                y = prop_posF*(30/100), group = 1, lty = "Falciparum"),
            color = "firebrick4", inherit.aes = FALSE, linewidth = 0.6) +
  scale_linetype_manual(values=c('Vivax'='solid','Falciparum'='dotted')) +
  scale_fill_manual(values = c('#06c2ac', 'firebrick3')) +
  # Format axis y
  scale_y_continuous(name = "Nombre de tests",
                     breaks = seq(0,50, by = 2),
                     sec.axis = sec_axis(transform=~./(30/100),
                                         name = "Taux de positivité (%)",
                                         breaks = seq(0,100, by = 10)),
                     expand = expansion(mult = c(0, 0.1))) +
  # Format axis x
  labs(x = '') +
  scale_x_date(name = "",
               breaks = unique(df$date_mission),
               labels = unique(df$date_mission), date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -0.2,
                                   hjust = -0.2,
                                   size = 9)) +
  # Legend
  # Thème
  theme_emipal()
  # Annotate if no results
if (all(df$Tot == 0, na.rm = T) == T) {
  fig_PCR <- fig_PCR +
    annotation_custom(grid::textGrob("-- Résultats des tests non disponibles --",
                                     gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}

fig_PCR

# Enregistrement de la figure
ggsave('figures/fig_PCR.png', fig_PCR,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_PCR.png",
              width = 6.2,
              height = 3,
              style = "Figure")

## Autres indicateurs PCR ----
df <- bdd %>% select(ID, PCR) %>% unique() %>% filter(PCR != "NR")
doc <-    body_add_par(doc,
  paste0("Au total, ", nrow(df),
         " personnes ont été testées par TDR au moins une fois. Parmi elles, ",
         nrow(df) - length(df$PCR[which(df$PCR == "Négative")]),
         " ont été testées positives."), style = "Normal")

## FIGURE 8 : TDR - Taux de positivité des personnes dépistées par TDR  ----
doc <- body_add_par(doc, "Taux de positivité des personnes dépistées par TDR",
                    style = "heading 1")

# Reshape
df <- bdd %>%
  filter(! TDR %in% c("NR", NA_character_)) %>%
  select(date_mission, TDR) %>%
  group_by(date_mission, TDR) %>%
  summarise(n = n()) %>%
  spread(TDR, n) %>%
  right_join(x=.,
             y=tibble('date_mission' = as.Date(names(table(bdd$date_mission)))),
             by = "date_mission") %>%
  replace(is.na(.), 0) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(Pos = rowSums(across(starts_with(c('Dis', 'Pos'))))) %>%
  mutate(Tot = Pos+`Négatif`) %>%
  mutate(prop_pos = if_else(Tot == 0, 0, as.numeric(round(Pos/Tot*100,1))))

if (sum(df$Tot) == 0) {
doc <- body_add_par(doc,
      "Aucun test TDR n'a été effectué.", style = "Normal")
}

doc <- body_add_par(doc,
        paste0("Au total, ", sum(df$Tot),
      " tests TDR ont été effectués parmi lesquels ",
      sum(df$Pos), " se sont révélés positifs et ", sum(df$Négatif),
      " se sont révélés négatifs."), style = "Normal")

# Plot
fig_TDR <- df %>%
  ggplot() +
  # Total tests négatifs
  geom_bar(aes(x = date_mission, y = Tot, fill = "TDR négatifs"), 
           stat = "identity",
           color = "white",
           position = position_dodge2(preserve = "single"),
           width = 3) +
  # Total tests positifs
  geom_bar(aes(x = date_mission, y = Pos, fill = "TDR positifs"),
           stat = "identity",
           color = "white",
           position = position_dodge2(preserve = "single"),
           width = 3) +
  # Taux de positivité
  geom_line(aes(x = date_mission, y = prop_pos*.35,
                lty = "Tx positivité", group = 1), color = "red", linewidth = 0.6) +
  scale_fill_manual(values = c('#06c2ac', 'firebrick3')) +
  # Format axis y
  scale_y_continuous(name = "Nombre de tests",
                     breaks = seq(0,50, by = 2),
                     sec.axis = sec_axis(transform=~./(30/100),
                                         name = "Taux de positivité (%)",
                                         breaks = seq(0,100, by = 10)),
                     expand = expansion(mult = c(0, 0.1))) +
  # Format axis x
  scale_x_date(name = "",
               breaks = unique(df$date_mission),
               labels = unique(df$date_mission),
               date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -0.2,
                                   hjust = -0.2,
                                   size = 9)) +
  # Legend
  theme(legend.margin=margin(t=-10),) +
  # Thème
  theme_emipal()
  # Annotate if no results
if (all(df$Tot == 0, na.rm = T) == T) {
  fig_TDR <- fig_TDR +
    annotation_custom(grid::textGrob("-- Résultats des tests non disponibles --",
                                     gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
fig_TDR

# Enregistrement de la figure
ggsave('figures/fig_TDR.png', fig_TDR,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_TDR.png",
              width = 6.2,
              height = 3,
              style = "Figure") %>% body_add_break()


## FIGURE 9 : FGE - Taux de positivité des personnes dépistées par FGE  ----
doc <- body_add_par(doc, "Taux de positivité des personnes dépistées par FGE",
                    style = "heading 1")
# Reshape df
df <- bdd %>%
  filter(!FGE %in% c("NR", NA_character_)) %>%
  select(date_mission, FGE) %>%
  group_by(date_mission, FGE) %>%
  summarise(n = n()) %>%
  spread(FGE, n) %>%
  right_join(x=.,
             y=tibble('date_mission' = as.Date(names(table(bdd$date_mission)))),
             by = "date_mission")

if (!"Négatif" %in% names(df) == T) {
  df$`Négatif` <- NA
}

df <- df %>%
  replace(is.na(.), 0) %>%
  mutate_if(is.integer, as.numeric) %>%
  mutate(Pos = rowSums(across(starts_with(c('Dis', 'Pos'))))) %>%
  mutate(Tot = Pos + `Négatif`) %>%
  mutate(prop_pos = if_else(Tot == 0, 0, as.numeric(round(Pos/Tot*100,1)))) %>%
  ungroup()

if (sum(df$Tot) == 0) {
doc <- body_add_par(doc,
      "Aucun test FGE n'a été effectué.", style = "Normal")
}
  
doc <- body_add_par(doc,
        paste0("Au total, ", sum(df$Tot),
      " tests FGE ont été effectués parmi lesquels ",
      sum(df$Pos), " se sont révélés positifs et ", sum(df$Négatif),
      " se sont révlés négatifs"), style = "Normal")

# Plot
fig_FGE <-
  df %>%
  ggplot() +
  # Total tests négatifs
  geom_bar(aes(x = date_mission, y = Tot, fill = "Tests négatifs"), 
           stat = "identity",
           color = "white",
           position = position_dodge2(preserve = "single"),
           width = 3) +
  # Total tests négatifs
  geom_bar(aes(x = date_mission, y = Pos, fill = "Tests positifs"),
           stat = "identity",
           position = position_dodge2(preserve = "single"),
           color = "white",
          width = 3) +
  # Taux de positivité
  geom_line(aes(x = date_mission, y = prop_pos*.35,
                lty = "Tx positivité", group = 1), linewidth = 0.6, color = "red") +
  scale_fill_manual(values = c('#06c2ac', 'firebrick3')) +
  # Format axis y
  scale_y_continuous(name = "Nombre de tests",
                     breaks = seq(0,50, by = 2),
                     sec.axis = sec_axis(transform=~./(30/100),
                                         name = "Taux de positivité (%)",
                                         breaks = seq(0,100, by = 10)),
                     expand = expansion(mult = c(0, 0.1))) +
  # Format axis x
  scale_x_date(name = "",
               breaks = unique(df$date_mission),
               labels = unique(df$date_mission), date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -0.2,
                                   hjust = -0.2,
                                   size = 9)) +
  # Legend
  theme(legend.margin=margin(t=-10),) +
  # Thème
  theme_emipal()
  # Annotate if no results
if (all(df$Tot == 0, na.rm = T) == T) {
  fig_FGE <- fig_FGE +
    annotation_custom(grid::textGrob("-- Résultats des tests non disponibles --",
                                     gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
fig_FGE

# Enregistrement de la figure
ggsave('figures/fig_FGE.png', fig_FGE,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_FGE.png",
              width = 6.2,
              height = 3,
              style = "Figure") %>% body_add_break()

## FIGURE 10 : Nombre de personnes dépistées G6PD  ----
doc <- body_add_par(doc, "Nombre de personnes dépistées G6PD (rapide)",
                    style = "heading 1")

# Reshape
df <- bdd %>%
  select(date_mission, g6pd_rapide) %>%
  filter(!is.na(g6pd_rapide)) %>%
  group_by(date_mission) %>%
  summarise(freq = n())

if (nrow(df) == 0) {
  df[1, ] <- list(bdd$date_mission[1], 0)
}

doc <- body_add_par(doc, paste0("Au total, ", sum(df$freq),
      " personnes ont été dépistées G6PD."), style = "Normal")

# Plot
fig_g6pd <- df %>%
  ggplot(aes(x = date_mission, y = freq)) +
  geom_bar(position = position_dodge2(preserve = 'single'),
           stat = 'identity', fill = '#06c2ac',
           width = 3) +
  geom_text(aes(label = freq), vjust = - .5,
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  scale_x_date(name = '',
               breaks = unique(df$date_mission),
               labels = unique(df$date_mission), date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -0.4,
                                   hjust = -0.5,
                                   size = 9)) +
  # Format axis y
  scale_y_continuous(name = "Nombre de tests",
                     breaks = seq(0,50, by = 2),
                     expand = expansion(mult = c(0.02, 0.2))) +
  # Legend
  theme(legend.margin=margin(t=-35),) +
  # Thème
  theme_emipal()
  # Annotate if no results
if (sum(df$freq, na.rm = T) == 0) {
fig_g6pd <- fig_g6pd +
  annotation_custom(grid::textGrob("-- Résultats des tests non disponibles --",
                                  gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
fig_g6pd

# Enregistrement de la figure  
ggsave('figures/fig_g6pd.png', fig_g6pd,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_g6pd.png",
              width = 6.2,
              height = 3,
              style = "Figure") %>% body_add_break() 

## FIGURE 11 : Nombre de consultations médicales ayant conduit à l'instauration et délivrance d'un traitement ----
doc <-    body_add_par(doc,
"Nombre de consultations médicales ayant conduit à l'instauration et délivrance d'un traitement", style = "heading 1")

# Reshape
df <- bdd %>%
  select(ID, date_mission, Motif_Principal, TDR, PCR, riamet, pq1, pq_2) %>%
  filter(riamet == 'oui' | pq1 == 'oui' | pq_2 == 'oui') %>%
  mutate(pq = ifelse(pq1 =='oui' | pq_2 == 'oui', 'oui', 'non')) %>%
  select(date_mission, riamet, pq) %>%
  gather(TTT, value, 2:3) %>%
  filter(value == "oui")

doc <-    body_add_par(doc,
    paste0("Au total, ", nrow(df), " consultations médicales ont conduit à l'instauration et à la délivrance d'un traitement."))

# Plot
fig_consult_instau_TTT <- df %>%
  ggplot(aes(x = date_mission, fill = TTT)) +
  geom_bar(position = position_dodge2(preserve = 'single'), width = 5) +
  geom_text(aes(label = after_stat(count)),
            stat = 'count',
            vjust = -0.5,
            position = position_dodge2(width = 5, preserve = 'single'),
            color = "grey30",
            size = 9, size.unit = 'pt') +
  scale_fill_manual(values = c('#06c2ac', 'firebrick3'),
                    labels = c('Primaquine','Riamet')) +
  # Format axis x
  scale_x_date(name = '',
               breaks = unique(df$date_mission),
               labels = unique(df$date_mission), date_labels = "%d/%m") +
  theme(axis.text.x = element_text(angle = 85,
                                   vjust = -.2,
                                   hjust = -0.5,
                                   size = 9)) +
  # Format axis y
  scale_y_continuous(name = "Nombre de consultations",
                     breaks = seq(0, nrow(df)*1.1, by = 2),
                     expand = expansion(mult = c(0.02, 0.08))) +
  # Legend
  theme(legend.margin=margin(t=-15),) +
  # Thème
  theme_emipal()
  # Annotate if no results
if (nrow(df) == 0) {
fig_consult_instau_TTT <- fig_consult_instau_TTT +
  annotation_custom(grid::textGrob("-- Aucune consultation médicale --",
                                  gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
fig_consult_instau_TTT

# Enregistrement de la figure
ggsave('figures/fig_consult_instau_TTT.png', fig_consult_instau_TTT,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_consult_instau_TTT.png",
              width = 6.2,
              height = 3,
              style = "Figure") %>% body_add_break()

## FIGURE 12 : Nombre de consultations médicales ayant conduit à l'instauration et délivrance d'un traitement éradicateur par PQ sur FR  ----

doc <-    body_add_par(doc,
"Nombre de consultations médicales ayant conduit à l'instauration et délivrance d'un traitement éradicateur par primaquine sur facteurs de risques", style = "heading 1")

# Reshape
df <- bdd %>%
  select(ID, prenom, PCR, mois, orpaillage, symptome_3j,
         date_dernier_acces_palustre, pq1) %>%
  filter(orpaillage %in% c('Oui','oui') &
           symptome_3j %in% c('Non','non') &
           !date_dernier_acces_palustre %in% c('dm', 'non', 'Non', 'DM') &
           pq1 %in% c('oui', 'Oui')) %>%
  group_by(mois) %>%
  summarise(freq = n())

doc <-    body_add_par(doc,
    paste0("Au total, ", sum(df$freq), " consultations médicales ont conduit à l'instauration et à la délivrance d'un traitement éradicateur par primaquine sur facteurs de risques."))

if (nrow(df) == 0) {
  df[1, ] <- list(bdd$mois[1], 0)
}

# Plot
fig_consult_instau_PQ <- df %>%
  ggplot(aes(x = mois, y = freq)) +
  geom_bar(position = position_dodge2(),
           stat = 'identity',
           fill = '#06c2ac' #, width = 2
           )+
  geom_text(aes(label = freq), vjust = - .5,
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  labs(x = '') +
  scale_x_discrete(labels = function(x){gsub(".*-","",x)}) +
  theme(axis.text.x = element_text(vjust = -.2,
                                   hjust = 0.5,
                                   size = 9)) +
  # Format axis y
  scale_y_continuous(name = "Nombre de consultations",
                     breaks = seq(0, max(df$freq)*10, by = 2),
                     expand = expansion(mult = c(0, 0.8))) +
  # Legend
  theme(legend.margin=margin(t=-15),) +
  # Thème
  theme_emipal()
  # Annotate if no results
if (sum(df$freq, na.rm = T) == 0) {
fig_consult_instau_PQ <- fig_consult_instau_PQ +
  annotation_custom(grid::textGrob("-- Aucune consultation médicale --",
                                  gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
}
fig_consult_instau_PQ

# Enregistrement de la figure
ggsave('figures/fig_consult_instau_PQ.png', fig_consult_instau_PQ,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_consult_instau_PQ.png",
              width = 6.2,
              height = 3,
              style = "Figure") %>% body_add_break()

## FIGURE 13 : Nombre de personnes ayant bénéficié de l'instauration et de la délivrance d'un traitement ----

doc <-    body_add_par(doc,
"Nombre de personnes ayant bénéficié de l'instauration et de la délivrance d'un traitement", style = "heading 1")

# Reshape
df <- bdd %>%
  select(date_mission, ID, PCR, date_mission, Motif_Principal, riamet, pq1, pq_2) %>%
  
  filter(riamet %in% c('oui','Oui') | pq1 %in% c('oui','Oui') |
           pq_2 %in% c('oui','Oui')) %>%
  mutate(pq = ifelse(pq1 %in% c('oui','Oui') |
                       pq_2 %in% c('oui','Oui'), 'oui', 'non')) %>%
  select(-date_mission, -PCR, -Motif_Principal, -pq1, -pq_2) %>%
  distinct()

df_riamet <- df %>% select(ID, riamet) %>% distinct() %>% filter(riamet %in% c("oui", "Oui"))
df_pq <- df %>% select(ID, pq) %>% distinct() %>% filter(pq %in% c("oui", "Oui"))
df <- full_join(df_riamet, df_pq)
rm(df_pq, df_riamet)
df$TTT <- ifelse(df$riamet == "oui" & is.na(df$pq), "riamet",
           ifelse(df$pq == "oui" & is.na(df$riamet), "pq", "riamet+pq"))

doc <- body_add_par(doc,
      paste0("Au total, ", nrow(df), " personnes ont bénéficié de l'instauration et de la délivrance d'un traitement."), style = "Normal")

doc <- body_add_par(doc,
      paste0("Parmi ces ", nrow(df), " personnes, ", table(df$TTT)[1],
             " ont été traitées par Primaquine seule, ", table(df$TTT)[2],
             " par Riamet seul et ", table(df$TTT)[3],
             " par Primaquine et Riamet."), style = "Normal")

## FIGURE 14 : Nombre de personnes ayant bénéficié de l'instauration et délivrance d'un traitement éradicateur par PQ sur facteurs de risques ----

doc <-    body_add_par(doc,
"Nombre de personnes ayant bénéficié de l'instauration et de la délivrance d'un traitement éradicateur par primaquine sur facteurs de risques.", style = "heading 1")

# Reshape
df <- bdd %>%
  select(ID, orpaillage, symptome_3j,
         date_dernier_acces_palustre, pq1) %>%
  filter(orpaillage %in% c('Oui','oui') &
           symptome_3j %in% c('Non','non') &
           !date_dernier_acces_palustre %in% c('dm', 'non', 'Non', 'DM') &
           pq1 %in% c('oui', 'Oui')) %>%
  select(ID) %>% unique()

doc <- body_add_par(doc,
      paste0("Au total, ", nrow(df), " personnes ont bénéficié de l'instauration et de la délivrance d'un traitement éradicateur par primaquine sur facteurs de risques."),
      style = "Normal")



## FIGURE 15 : Nombre de personnes avec PCR positif et TTT complet ou incomplet ----
doc <-    body_add_par(doc, "Nombre de personnes avec PCR positif et TTT complet.",
                       style = "heading 1")

df <- bdd %>%
# Reshape
  select(ID, mois, PCR, riamet, pq1, pq_2, Délivrance_complète_ce_jour) %>%
  filter(PCR %in% c("Positive à P. vivax", "Positive à P. falciparum")) %>%
  mutate(statut_TTT = ifelse(PCR == "Positive à P. vivax" &
                        riamet %in% c('non', 'Non') &
                        pq1 %in% c('non', 'Non') &
                        pq_2 %in% c('non', 'Non'), "PDV",
         ifelse(PCR == "Positive à P. falciparum" &
                  riamet %in% c('non', 'Non'), "PDV", "TTT complet"))) %>%
  select(ID, statut_TTT) %>% unique()

if (nrow(df) == 0) {
  doc <- body_add_par(doc, paste0("Aucune personne avec PCR positif en attente de traitement, sans traitement ou traitement incomplet."))
  }

if (nrow(df) != 0) {
doc <-    body_add_par(doc,
          paste0("Au total, ", sum(df$statut_TTT =='PDV'),
                 " personnes avec PCR positif sont en attente de traitement, sans traitement ou traitement incomplet et ", sum(df$statut_TTT =='TTT complet'),
  " personnes avec PCR positif ont bénéficié d'un traitement complet."),
  style = "Normal")

# Plot
fig_pdv <- df %>%
  ggplot(aes(x = statut_TTT, fill = statut_TTT)) +
  geom_bar(width = 0.2) +
  geom_text(aes(label = after_stat(count)), stat = 'count',
            vjust = - .5, 
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  labs(x = '') +
  scale_x_discrete(labels = c('Traitement incomplet', 'Traitement complet')) +
  theme(axis.text.x = element_text(vjust = 2,
                                   hjust = 0.5,
                                   size = 9)) +
  # Axis y
  scale_y_continuous(labels = function(x) round(as.numeric(x)),
                     breaks = seq(0,nrow(df)*1.1, by = 5),
                     expand = expansion(mult = c(0.01, 0.3))) +
  labs(y = "Nombre de personnes") +
  # Thème
  theme_emipal() + theme(legend.position="none")
fig_pdv

# Enregistrement de la figure
ggsave('figures/fig_pdv.png', fig_pdv,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_pdv.png",
              width = 6.2,
              height = 3,
              style = "Figure") %>% body_add_break()
}


## FIGURE 16 : Nombre de patients accès palustre avec TTT complet ----
doc <-    body_add_par(doc, "Nombre de personnes accès palustre avec traitement complet",
                       style = "heading 1")
# Reshape
df <- bdd %>%
  filter((TDR == 'Positif Pan' | PCR == 'Positive à P. vivax') &
           Délivrance_complète_ce_jour == 'oui') %>%
  group_by(mois) %>%
  summarise(freq = n())

if (nrow(df) == 0) {
 doc <-    body_add_par(doc,
          paste0("Aucune personne avec accès palustre n'a bénéficié d'un traitement complet."), style = "Normal") 
}

if (nrow(df) != 0) {
doc <-    body_add_par(doc,
          paste0("Au total ", sum(df$freq),
                 " personnes avec accès palustre ont bénéficié d'un traitement complet."),
          style = "Normal")
# Plot
fig_acces_palu <- df %>%
  ggplot(aes(x = factor(mois), y = freq)) +
  geom_bar(position = "stack", stat = "identity", fill = "royalblue", width=0.5) +
  geom_text(aes(label=freq), vjust = -1,
            color = "grey30",
            size = 9, size.unit = 'pt') +
  # Format axis x
  labs(x = '') +
  scale_x_discrete(labels = function(x){gsub(".*-","",x)}) +
  # Format axis y
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.1))) +
  labs(y = "Nombre de personnes") +
  # Thème
  theme_emipal()
  
fig_acces_palu

# Enregistrement de la figure
ggsave('figures/fig_acces_palu.png', fig_acces_palu,
       width = 9,
       height = 4,
       units = 'in')
# Insertion de la figure dans le document word
doc <- body_add_img(doc,
              src = "figures/fig_acces_palu.png",
              width = 6.2,
              height = 3,
              style = "Figure")
}

# EDITER RAPPORT ----
print(doc, target = paste0("rapports/rapport_", site, "_", Sys.Date(),
                           ".docx"))

rm(list = ls()[grepl("fig", ls())], doc, df)

cat(insight::print_color(paste0("** LE RAPPORT DE ", site, " EST DISPONIBLE DANS LE REPERTOIRE R-EMIPAL/rapports **"), color = "green"))

  }
if (nrow(bdd) == 0) {
cat(insight::print_color(paste0("** PAS DE DONNEES - Changez de date **"),
                         color = "red"))
  
}

# FIN ---- 
  