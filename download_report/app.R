# CHARGEMENT DES PACKAGES ----
library(gsheet)
library(tidyverse)
library(officer)
library(RColorBrewer)
library(rmarkdown)
library(shiny)
library(rsconnect)

## date système en français
Sys.setlocale("LC_TIME", "fr_CA.UTF-8")

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
    plot.margin = unit(c(1,0.5,0.5,.5),"cm"),
    panel.background = element_rect(fill = NA,
                                    colour = "white"),
    plot.background = element_rect(colour = "black",
                                   fill = NA,
                                   linewidth = .5),
    panel.grid.major.y = element_line(linetype = 'solid',
                                      colour = "grey90",
                                      linewidth = 0.3),
    axis.title.y.right = element_text(vjust = 2),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 10,
                             color = "black"),
    axis.title.y = element_text(size = 11,
                                face = "bold",
                                vjust = 5,
                                color = "black"),
    axis.title.x = element_text(size = 11,
                                face = "bold",
                                vjust = 2,
                                color = "black"),
    legend.key.size = unit(0.4, units = "cm"),
    legend.position = "bottom",
    legend.title = element_blank()) 
}
## FIN ----

# CHARGEMENT DES DONNEES ----
url <- "https://docs.google.com/spreadsheets/d/1wWOxDN2IgdhBwB11Z86QKzrrbIqtBhMgUqJN4n7mkDk/edit?usp=sharing"
bdd <- as.data.frame(read_csv(gsheet2text(url, format = 'csv'),
              locale = locale(date_names = "fr"),
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
          labels = c('[0,14]', '[15,24]', '[25,44]', '[45,64]','65+'))) %>%
  # création de la variable ID
  mutate(ID = paste0(
  sprintf("%.3s", gsub("[[:space:]]", "", prenom)),"_",
  sprintf("%.3s", gsub("[[:space:]]", "", nom)),"_",
  sprintf("%.4s", ddn))) %>%
  mutate(ID = stringi::stri_trans_general(ID, "Latin-ASCII"))




ui <- shinyUI(
  fluidPage(
  tags$style('.container-fluid {background-color: #43a4d9;}'),
  #fluidRow(column(2, align ="center",
  #                            div(img(src = "download_report/logo_both.png", height=100, width=150)))),
  inputPanel(
    h4("Options"),
      dateRangeInput(inputId = "daterange",
                                       label = "Période",
                                       start = min(bdd$date_mission),
                                       min = min(bdd$date_mission),
                                       end = paste(Sys.Date()),
                                       max = paste(Sys.Date())
                                         ),
  
      selectInput("site", label = "Site d'intervention",
              choices = c("pk6", "Saint Elie"), selected = "pk6"),
  

      radioButtons('format', 'Format du document', c('Word', 'PDF'),
                          inline = TRUE),
  
      downloadButton(outputId = "downloadReport",
                            label =  "Télécharger Rapport",
                     style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
  )
)

server <- shinyServer(
    function(input, output, session) {
        output$downloadReport <- downloadHandler(
            filename = function() {
                paste('mon_rapport', sep = '.',
                      switch(input$format,
                             PDF = 'pdf',
                             Word = 'docx'
                             )
                      )
                },
            content = function(file) {
                src <- normalizePath(c('download_report/rapport.Rmd',
                                       'download_report/temp_rapport_palu.docx'))
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
                owd <- setwd(tempdir())
                on.exit(setwd(owd))
                file.copy(src, c('rapport.Rmd', 'temp_rapport_palu.docx'), overwrite = TRUE)
                out <- rmarkdown::render('rapport.Rmd', encoding = "UTF-8",
                                         params = list(
                                             daterange1 = input$daterange[1],
                                             daterange2 = input$daterange[2],
                                             site = input$site),
                                         switch(input$format,
                                                PDF = pdf_document(), 
                                                Word = word_document(reference_docx = "temp_rapport_palu.docx")
                                                )
                                         )
                file.rename(out, file)
            })
    }
    )

shinyApp(ui = ui, server = server)
