library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
####Data Loading####
# setwd("./Viz/")
df <- read_csv("operations.csv")
df <- df %>% filter(is.na(Country) == FALSE & is.na(`Target Longitude`) == FALSE &
                      is.na(`Takeoff Longitude`) == FALSE) %>% 
    filter(`Takeoff Latitude` != '4248', `Takeoff Longitude` != 1355) %>%
  filter(`Target Longitude` > 0, `Takeoff Longitude` > 0)


####Sever####
shinyServer(function(input, output, session) {
   
  output$mymap <- renderLeaflet({
    gcIntermediate(df[ , c("Target Longitude", "Target Latitude")],
                   df[ , c("Takeoff Longitude", "Takeoff Latitude")],
                   n=20, 
                   addStartEnd=TRUE,
                   sp=TRUE) %>% leaflet() %>%
      addTiles(
        urlTemplate = "https://api.mapbox.com/styles/v1/yiqiang/cjg1txxq83nha2so1hv5pt6cm/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoieWlxaWFuZyIsImEiOiJjamI2MjJ2aDgzZTJiMzdvMTEza25vN3czIn0.da7McqdO9XgggUE0LTCx3Q"
      ) %>% addPolylines(opacity = 0.3, fillOpacity = 0.2,
                         color = "white", stroke = TRUE, weight = 1) %>%
      addCircleMarkers(data = df, lat = ~`Target Latitude`, lng = ~`Target Longitude`,
                       fillOpacity = 0.1, color = "red", radius = 1) %>%
      addCircleMarkers(data = df, lat = ~`Takeoff Latitude`, lng = ~`Takeoff Longitude`,
                       fillOpacity = 0.1, color = "blue", radius = 1) %>%
      fitBounds(lng1=65.298593, lat1 = -15.124991, lng2 = -7.286442, lat2 = -173.134400) %>% 
      setView(lat=24.719341, lng=68.718978, zoom=3) 
  })
})

# addCircleMarkers(
#   radius = ~ifelse(type == "ship", 6, 10),
#   color = ~pal(type),
#   stroke = FALSE, fillOpacity = 0.5
# )