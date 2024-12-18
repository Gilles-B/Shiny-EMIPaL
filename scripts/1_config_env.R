# Rapport automatisé Interventions EMIPAL - Octobre 2024 #
# Script de configuration de l'environnement RStudio #

# CHARGEMENT DES PACKAGES ----
library(gsheet)
library(tidyverse)
library(officer)
library(RColorBrewer)
library(grid)
library(rmarkdown)
library(shiny)
library(rsconnect)
library(cronR)
library(epitrix)

## système en français et UTF-8
Sys.setlocale("LC_ALL", "fr_CA.UTF-8")

# FONCTIONS ----
## Fonction clean_punct
clean_punct <- function(s) {
  # "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
  s <- gsub("[[:punct:]]", "", s)
  return(s)
}

## THEME PAR DEFAUT POUR LES GRAPHIQUES ----
theme_emipal <- function(){
  theme(
    text = element_text(family = 'calibri'),
    plot.margin = unit(c(1, .5, .5, .5),"cm"),
    panel.background = element_rect(fill = NA,
                                    colour = "white"),
    plot.background = element_rect(colour = "black",
                                   fill = NULL,
                                   linewidth = .5),
    panel.grid.major.y = element_line(linetype = 'solid',
                                      colour = "grey90",
                                      linewidth = 0.3),
    axis.title.y.right = element_text(vjust = 2),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 12,
                             color = "black"),
    axis.title.y = element_text(size = 13,
                                face = "bold",
                                vjust = 5,
                                color = "black"),
    axis.title.x = element_text(size = 13,
                                face = "bold",
                                vjust = 2,
                                color = "black"),
    legend.key.size = unit(0.4, units = "cm"),
    legend.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_blank()) 
}

cat(insight::print_color(paste0("** La configuration de l'environnement est faite **"),
                         color = "green"))

## FIN ----

