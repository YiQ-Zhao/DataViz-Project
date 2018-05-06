library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
library(mapview)
library(crosstalk)
library(plotly)
library(shinydashboard)
library(chorddiag)

shinyUI(dashboardPage(
  dashboardHeader(title = "World War II"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Timeline",
        tabName = "timeline",
        icon = icon("line-chart")
      ),
      menuItem(
        "Alliance",
        tabName = "alliance",
        icon = icon("fighter-jet")),
      menuItem(
        "Map",
        tabName = "maps", 
        icon = icon("globe")
      ))
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "maps",
        fluidRow(
          tags$style("#instruction {font-size:15px;
               font-style:italic; }"),
          column(width = 5, htmlOutput("instruction"))),
        fluidRow(
        box(
          width = "100%",
          height = "100%",
          leafletOutput("mymap", height = 350)
         )
        ),
        fluidRow(
          box(
            width = 6, 
            plotlyOutput("barplot", height = 300)
          ),
          box(
            width = 6,
            plotlyOutput("piechart", height = 300)
          )
        )
      ),
      tabItem(
        tabName = "alliance",
        fluidRow(
        box(title = tags$b("Military Alliance in WW2"), status = "primary",
            solidHeader = F, collapsible = F, width = 12,
            fluidRow(
              tags$style("#allies_axis {font-size:15px;
               display:block; }"),
              column(width = 9, 
                     htmlOutput("allies_axis")),
              column(width = 3, align = "center",
                     img(src = "allied powers.jpg", height = '170px', width = '220px'))))),
        fluidRow(
          box(splitLayout(
              cellWidths = c("12%", "38%", "50%"),
              cellArgs = list(style = "padding: 6px"),
              column(width = 1, img(src = "Legend.png", height = '84px', width = '100px'), height = 450),
              chorddiagOutput("chorddiag", height = 450),
              plotlyOutput("barplot2", height = 450)), width = 12))),
      tabItem(
        tabName = "timeline",
        fluidRow(
          box(title = tags$b("Count of Bombing Mission in WW2"), solidHeader = TRUE,
              plotlyOutput("timeseries"), width = 12)),
        fluidRow(box(title = tags$b("WW2 Introduction"), status = "primary",
                     solidHeader = F, collapsible = F, width = 12,
                     fluidRow(
                       tags$style("#allies_axis {font-size:15px;
               display:block; }"),
                       column(width = 9, htmlOutput("intro")),
                       column(width = 3, align = "center",
                              img(src = "uss_arizona_burning_sinking-P.jpeg", height = '170px', width = '240px')))
                 )
                 ))
    ))
)
)
