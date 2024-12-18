# Rapport automatisé Interventions EMIPAL - Octobre 2024 #
# Script pour le génération des fonctions plots #


## PLOT 1 : Âge & Sexe des personnes consultées ----
plot_age_sexe <- function(df) {
  df %>%
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
}


## PLOT 2 : Nombre d'orpailleurs selon leur pays d'origine ----
plot_nb_orpa_pays <- function(df) {
  df %>%
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
}



## PLOT 3 : Provenance des personnes selon les sites d'orpaillage ----
plot_prop_orpa_site <- function(bdd_EMIPaL) {
  # Reshape bdd 
  df <-  bdd_EMIPaL %>%
    filter(orpaillage == "Oui" & !is.na(camps_orpaillage__1)) %>%
    select('ID', "orpaillage",
         "camps_orpaillage__1", date_mission) %>%
    unique()
  # Plot
  df %>%
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
}



## PLOT 4 : Nombre et type de consultations ----
plot_nb_consult <- function(bdd_EMIPaL) {
  # Reshape bdd 
  df <-  bdd_EMIPaL %>%
    select(date_mission, Motif_Principal)

# Plot
df %>%
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
}


## PLOT 5 : Consultations médicales pour introduction ou suivi de Primaquine et rendu BS ----
plot_consult_suivi <- function(bdd_EMIPaL) {
  # Reshape bdd
  df <- bdd_EMIPaL %>%
    select(mois, Motif_Principal, Motifs_secondaires) %>%
    filter(Motif_Principal == "Suivi" &
        grepl(paste(c("Introduction PQ", "PQ J7", "PQ J14", "PQ J21",
                      "Rendu BS sans PQ"), collapse="|"),
             bdd_EMIPaL$Motifs_secondaires) == T |
          (Motif_Principal == "Primo-consultation" &
        grepl(paste(c("Introduction PQ"), collapse="|"),
             bdd_EMIPaL$Motifs_secondaires) == T)) %>%
    mutate(motif = as.character(stringi::stri_extract_all_regex(Motifs_secondaires,
    '(Introduction PQ|PQ J7|PQ J14|PQ J21|Rendu BS sans PQ)'))) %>%
    group_by(mois, motif) %>%
    summarise(freq = n()) %>%
    mutate(percent = round(prop.table(freq),2))
  x <- sum(df$freq)
  print(x)
  
  if(nrow(df) == 0) {
    return(
      df %>%
        ggplot() +
        theme_void() +
        geom_text(aes(0, 0, label =
      "-- Aucune consultation médicale réalisée --"),
      col = "red", size = 13, size.unit = "pt" ) +
        xlab(NULL) +
        theme(
          panel.background = element_rect(fill = NA,
                                    colour = "white"),
          plot.background = element_rect(colour = "black",
                                   fill = NULL,
                                   linewidth = .5)
        )
      )
    }
  
  if (nrow(df) != 0) {
    # Plot
    df %>%
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
  }
}




## PLOT 6 : Nombre d'interventions réalisées par site d'interventions ----
plot_sites_inter <- function(bdd_EMIPaL) {
  # Reshape bdd
  df <- bdd_EMIPaL %>%
    select(lieu, date_mission) %>% unique() %>%
    group_by(lieu) %>%
    summarise(freq = n())
  # Plot
  df %>%  
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
}

## PLOT 7 : PCR - Taux de positivité des personnes dépistées par PCR  ----
plot_PCR <- function(bdd_EMIPaL) {
  # Reshape bdd
  df <- bdd_EMIPaL %>%
    filter(! PCR %in% c("NR", "EC", NA_character_)) %>%
    select(ID, orpaillage, date_mission, PCR, riamet, pq1, pq_2) %>%
    select(-ID, -orpaillage, -riamet, -pq1, -pq_2) %>%
    group_by(date_mission, PCR) %>%
    summarise(n = n()) %>%
    spread(PCR, n) %>%
    right_join(x = .,
               y = tibble('date_mission' = as.Date(names(table(bdd_EMIPaL$date_mission)))),
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

  # Plot
  if (all(df$Tot == 0, na.rm = T) == T) {
    return(
      df %>%
        ggplot() +
        theme_void() +
        geom_text(aes(0, 0, label =
      "-- Résultats des tests non disponibles --"),
      col = "red", size = 13, size.unit = "pt" ) +
        xlab(NULL) +
        theme(
          panel.background = element_rect(fill = NA,
                                    colour = "white"),
          plot.background = element_rect(colour = "black",
                                   fill = NULL,
                                   linewidth = .5)
        )
      )
    }
  df %>%
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
        # Thème
        theme_emipal()
}



## PLOT 8 : TDR - Taux de positivité des personnes dépistées par TDR  ----
plot_TDR <- function(bdd_EMIPaL) {
  # Reshape
  df <- bdd_EMIPaL %>%
    filter(! TDR %in% c("NR", NA_character_)) %>%
    select(date_mission, TDR) %>%
    group_by(date_mission, TDR) %>%
    summarise(n = n()) %>%
    spread(TDR, n) %>%
    right_join(x=.,
               y=tibble('date_mission' = as.Date(names(table(bdd_EMIPaL$date_mission)))),
               by = "date_mission") %>%
    replace(is.na(.), 0) %>%
    mutate_if(is.integer, as.numeric) %>%
    mutate(Pos = rowSums(across(starts_with(c('Dis', 'Pos'))))) %>%
    mutate(Tot = Pos+`Négatif`) %>%
    mutate(prop_pos = if_else(Tot == 0, 0, as.numeric(round(Pos/Tot*100,1)))) %>%
    ungroup()

  # Plot
  plot_TDR <- df %>%
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
  plot_TDR <- plot_TDR +
    annotation_custom(grid::textGrob("-- Résultats des tests non disponibles --",
                                     gp=gpar(col="red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  }
  return(plot_TDR)
}



## PLOT 9 : FGE - Taux de positivité des personnes dépistées par FGE  ----
plot_FGE <- function(bdd_EMIPaL) {
  # Reshape
  df <- bdd_EMIPaL %>%
  filter(!FGE %in% c("NR", NA_character_)) %>%
  select(date_mission, FGE) %>%
  group_by(date_mission, FGE) %>%
  summarise(n = n()) %>%
  spread(FGE, n) %>%
  right_join(x=.,
             y=tibble('date_mission' = as.Date(names(table(bdd_EMIPaL$date_mission)))),
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

  # Plot
  plot_FGE <-
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
  plot_FGE <- plot_FGE +
    annotation_custom(grid::textGrob("-- Résultats des tests non disponibles --",
                                     gp = gpar(col = "red")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  }
  return(plot_FGE)
}

