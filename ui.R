library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
library(mapview)
library(crosstalk)
library(plotly)
library(shinydashboard)
# df <- read_csv("operations.csv")
# df <- st_as_sf(df) %>% filter(is.na(Country) == FALSE & is.na(`Target Longitude`) == FALSE &
#                       is.na(`Takeoff Longitude`) == FALSE) %>% 
#   filter(`Takeoff Latitude` != '4248', `Takeoff Longitude` != 1355) %>%
#   filter(`Target Longitude` > 0, `Takeoff Longitude` > 0)
# df <- SharedData$new(df)

shinyUI(dashboardPage(
  dashboardHeader(title = "World War II Viz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Maps",
        tabName = "maps", 
        icon = icon("globe")
      )
    )
  ),
  dashboardBody(
    tabItem(
      tabName = "World War II",
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
