library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
library(mapview)
library(crosstalk)

# df <- read_csv("operations.csv")
# df <- st_as_sf(df) %>% filter(is.na(Country) == FALSE & is.na(`Target Longitude`) == FALSE &
#                       is.na(`Takeoff Longitude`) == FALSE) %>% 
#   filter(`Takeoff Latitude` != '4248', `Takeoff Longitude` != 1355) %>%
#   filter(`Target Longitude` > 0, `Takeoff Longitude` > 0)
# df <- SharedData$new(df)

shinyUI(
  fluidPage(
    
    # Application title
    column(12, leafletOutput("mymap", width="70%", height = 500))
  )
)
