library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
library(mapview)
library(crosstalk)
library(plotly)
library(shinydashboard)


shinyUI(dashboardPage(
  dashboardHeader(title = "World War II Viz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps", 
        icon = icon("globe")
      ),
      menuItem(
        "Time",
        tabName = "time",
        icon = icon("calendar")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "maps",
        fluidRow(
        box(
          # title = "Overview of World War II",
          # collapsible = T,
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
        tabName = "time",
        # fluidRow(box(column(width=12,
        #                 img(src="world-war-ii-facts.jpg", width=1000)))),
        
        fluidRow(
        box(title = tags$b("Military Allies in WW2"),
            status = "primary",
            solidHeader = F,
            collapsible = F,
            width = 12,
            fluidRow(
              tags$style("#allies_axis {font-size:15px;
               display:block; }"),
              column(width = 9, 
                     htmlOutput("allies_axis")),
              column(width = 3, align = "center",
                     img(src = "allied powers.jpg", height = '170px', width = '220px'))
              ))
            ),
        fluidRow(box(
          splitLayout(
            cellWidths = c("40%", "60%"),
            cellArgs = list(style = "padding: 6px"),
            chorddiagOutput("chorddiag", height = 450),
            plotlyOutput("barplot2", height = 450)
          ), width = 12
        ))

      )
    )
    # fluidRow(
    #   leafletOutput("mymap")
    # ),
    # fluidRow(
    #   column(6,
    #   plotlyOutput("barplot")),
    #   column(6,
    #          plotlyOutput("piechart"))
    # )
  )
)
)
