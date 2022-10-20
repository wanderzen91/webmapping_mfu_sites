library(shiny)
library(leaflet)
library(dplyr)
library(cssloaders)
library(geojsonsf)
library(shinythemes)
library(bslib)
library(sf)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(pier)
library(tidyverse)
library(pals)
library(colourvalues)
library(ggplot2)
library(plotly)
library(forcats)
library(highcharter)
library(shinydashboard)

setwd("C:/Users/Romain/Desktop/Github/mfu_sites_CEN/")

sites_mfu <- geojson_sf("sites_cen.geojson")

parcelles <- geojson_sf("parcelles_cen_na_mfu.geojson")


parts_habitats = aggregate(surf_ha ~ nom_site +libhabitat, data=parcelles, FUN=sum)

types_mfu = aggregate(surf_ha ~ nom_site + type_mfu, data=parcelles, FUN=sum)

types_mfu_1 = types_mfu %>% 
  rename(
    label = type_mfu,
    value = surf_ha)


parts_habitats_1 = parts_habitats %>% 
  rename(
    label = libhabitat,
    value = surf_ha)


parts_habitats_1$color <- colour_values(parts_habitats_1$label, palette = "viridis")

types_mfu_1$color <- colour_values(types_mfu_1$label, palette = "viridis")


parts_habitats_depts = aggregate(surf_ha ~ nom_dept +libhabitat, data=parcelles, FUN=sum)

parts_mfu_depts = aggregate(surf_ha ~ nom_dept +type_mfu, data=parcelles, FUN=sum)



nb_sites_dept = aggregate(codesite ~ nom_dept, data=parcelles, function(codesite) length(unique(codesite)))

surf_geree_dept = aggregate(surf_ha ~ nom_dept, data=parcelles, FUN=sum)



depts_na <- geojson_sf("depts_na.geojson")


dark <- bs_theme(version = 5, bootswatch = "flatly")
light <- bs_theme(version = 5, bootswatch = "minty")


ui <- navbarPage(title = "Sites CEN & MFU",
                 ###### Here : insert shinydashboard dependencies ######
                 header = tagList(
                   useShinydashboard()
                 ),
                 
                 # setBackgroundColor(
                 #   color = "darkgreen",
                 #   gradient = c("linear", "radial"),
                 #   direction = c("bottom", "top", "right", "left")
                 # ),
                 
                 theme = dark, tags$style(type = "text/css", "#map {height: calc(100vh - 72px) !important;}"),
                 useShinyjs(), 
                 
          tabPanel("Information par site",
                   leafletOutput('map', width = "105vw") %>% withSpinner(color="#000000"),
                   
                   
                fixedPanel(id = "controls", draggable = FALSE,
                              top = "10%", right = "auto", left = 20, bottom = "auto",
                              width = "20%", height = "auto",
                              wellPanel(style = "background: #e4eddf",
                                fluidRow(
                                  prettyCheckbox(
                                    inputId = "light_mode", label = "Mode clair",
                                    shape = "round", outline = TRUE, status = "info"
                                  ),

                                  selectInput(
                                    inputId = "depts", 
                                    width = '300px',
                                    label = "Sélectionner un département:",
                                    choices = c("", as.character(sort(unique(depts_na$nom))))),
                                
                          conditionalPanel(
                            condition = "input.depts != ''",    
                                  selectInput(
                                    inputId = "sites_cen", 
                                    width = '300px',
                                    label = "Sélectionner un site CEN:",
                                    choices = c("", as.character(sort(unique(sites_mfu$nom_site))))))))),
              
                conditionalPanel(
                  condition = "input.sites_cen != ''",                    
                absolutePanel(id = "controls", class = "panel panel-default", draggable = TRUE,
                           top = 100, left = "auto", right = 30, bottom = "auto",
                           width = "40%", height = "auto",
                           wellPanel(id="dataviz", p("Indicateurs :")), 
                           fluidRow(pierOutput("type_habitats") %>% withSpinner(color="#000000"), align="center"),
                           fluidRow(br()),
                           fluidRow(pierOutput("types_mfu") %>% withSpinner(color="#000000"), align="center"),
                           fluidRow(br()),
                           style = "opacity: 0.8; background-color: white;z-index: 10;"))
                
                ),
                           # shinyjs::hidden(wellPanel(id="dataviz", p("Indicateurs :"))), style = "opacity: 0.8; background-color: white;z-index: 10;")),
          
          tabPanel("Comparaison par départements",
                   fluidRow(
                     column(width = 6, align = "center", 
                            selectInput(
                              inputId = "dept1", 
                              width = '300px',
                              label = "Sélectionner un département:",
                              choices = as.character(sort(unique(parts_habitats_depts$nom_dept))),
                              selected = "Charente")
                     ),
                     column(width = 6, align = "center", 
                            selectInput(
                              inputId = "dept2", 
                              width = '300px',
                              label = "Sélectionner un département:",
                              choices = as.character(sort(unique(parts_habitats_depts$nom_dept))),
                              selected = "Charente-Maritime")
                     )),  
                  
                   
                   br(), 
                   fluidRow(align = "center",
                            column(width = 1),
                            column(width = 4, align = "center",
                               box(title = "Nombre et surface totale de sites gérés :",
                                   status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                   br(),
                                   shinydashboard::valueBoxOutput(outputId = "box1", width = 8) %>% withSpinner(color="#000000"),
                                   shinydashboard::valueBoxOutput(outputId = "box2", width = 8) %>% withSpinner(color="#000000"))),
                            column(width = 2),
                            column(width = 4, align = "center",
                                   box(title = "Nombre et surface totale de sites gérés :",
                                       status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 12,
                                       br(),
                                   shinydashboard::valueBoxOutput(outputId = "box3", width = 8) %>% withSpinner(color="#000000"),
                                   shinydashboard::valueBoxOutput(outputId = "box4", width = 8) %>% withSpinner(color="#000000")))),
                   column(width = 1),
                   br(), 
                   
                   h5("Types d'habitats et types de MFU :"),
                   br(), 
  
                   fluidRow(column(width = 5,
                                   fluidRow(
                                  highchartOutput(outputId = "plot1", height = 600) %>% withSpinner(color="#000000")),
                                   fluidRow(style = "height:10%;"),
                                   fluidRow(
                                     highchartOutput(outputId = "plot2", height = 600) %>% withSpinner(color="#000000"))),
                   
                            column(width = 1),
                   
                            column(width = 5,
                                   fluidRow(
                                     highchartOutput(outputId = "plot3", height = 600) %>% withSpinner(color="#000000")),
                                   fluidRow(style = "height:10%;"),
                                   fluidRow(
                                     highchartOutput(outputId = "plot4", height = 600) %>% withSpinner(color="#000000"))
              
                            ))
),
          
          tags$style('
                #map {
                        position: relative;
                        margin: -20px;
                        padding: 0px;
                        }'
          ),
          tags$head( 
            tags$style(HTML(".fa{font-size: 40px;}"))
          )

)
