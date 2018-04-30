library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
library(mapview)
library(crosstalk)
library(sf)
library(plotly)
####Data Loading####
# setwd("./Viz/")
# df <- read_csv("/media/yiqiang/D/USF/622DataViz/final project/finalproject/Viz/operations.csv")
# df <- df %>% filter(is.na(Country) == FALSE & is.na(`Target Longitude`) == FALSE & is.na(`Takeoff Longitude`) == FALSE) %>%
#         filter(`Takeoff Latitude` != '4248', `Takeoff Longitude` != 1355) %>%
#           filter(`Target Longitude` > 0, `Takeoff Longitude` > 0, `Target Latitude` > 0, `Target Longitude` <=360) %>% 
#             mutate(id=1:n())
# df$`Mission Date` <- as.Date(anytime::anydate(df$`Mission Date`))
# # df$year <- as.numeric(strftime(df$`Mission Date`, format = "%Y"))
# # df$week <- as.numeric(strftime(df$`Mission Date`, format = "%V"))
# df$start_week <- cut(df$`Mission Date`, "week")
df <- read_csv("simplified_data.csv")
df_takeoff <- df %>% st_as_sf(., coords = c("Takeoff Longitude", "Takeoff Latitude"))
df_target <- df %>% st_as_sf(., coords = c("Target Longitude", "Target Latitude"))



####Sever####
shinyServer(function(input, output, session) {
  target_map <- SharedData$new(df_target, key = ~id, group = "grp1")
  takeoff_map <- SharedData$new(df_takeoff, key = ~id, group = "grp2")
  # df_map <- target_map$data(TRUE)
  lf <-   leaflet(target_map) %>%
    addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/yiqiang/cjglw67l500262tqmguz0efum/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoieWlxaWFuZyIsImEiOiJjamI2MjJ2aDgzZTJiMzdvMTEza25vN3czIn0.da7McqdO9XgggUE0LTCx3Q"
    ) %>%  addPolylines(data = gcIntermediate(df[ , c("Takeoff Longitude", "Takeoff Latitude")],
                                                  df[ , c("Target Longitude", "Target Latitude")],
                                                  n=20,
                                                  addStartEnd=TRUE,
                                                  sp=TRUE),  opacity = 0.3, fillOpacity = 0.2, color = "black", stroke = TRUE, weight = 1) %>%
    addCircleMarkers(fillOpacity = 0.1, color = "red", radius = 1, group = "grp1") %>%
    addCircleMarkers(data = takeoff_map, fillOpacity = 0.1, color = "blue", radius = 1, group = "grp2") %>%
    fitBounds(lng1=65.298593, lat1 = -15.124991, lng2 = -7.286442, lat2 = -173.134400) %>%
    setView(lat=24.719341, lng=68.718978, zoom=3) %>% 
    addMiniMap(tiles = "https://api.mapbox.com/styles/v1/yiqiang/cjglw67l500262tqmguz0efum/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoieWlxaWFuZyIsImEiOiJjamI2MjJ2aDgzZTJiMzdvMTEza25vN3czIn0.da7McqdO9XgggUE0LTCx3Q",
              toggleDisplay = T)
  

  not_rendered <- TRUE
  output$mymap <- renderLeaflet({
      if(req(not_rendered,cancelOutput=TRUE)) {
        not_rendered <- FALSE
        lf
      }
    })
  
  observe({
    tmp_selected_target <- target_map$data(withSelection = TRUE) %>% filter(selected_ == TRUE)
    tmp_selected_takeoff <- takeoff_map$data(withSelection = TRUE) %>% filter(selected_ == TRUE)
    
  output$barplot <- renderPlotly(({
    tmp_data <- tmp_selected_target %>% group_by(`Aircraft Series`) %>% count() %>% arrange(n)
    tmp_data$Aircraft_Series <- tmp_data$`Aircraft Series`
    p <- ggplot(tmp_data) + 
      aes(x = `Aircraft_Series`, y = `n`, fill=`Aircraft_Series`,
          text=sprintf("Aircraft Series: %s<br>Num: %s", `Aircraft_Series`, `n`)) + 
      geom_bar(stat = 'identity') + 
      ylab("Count") + xlab("")  +
      theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.background = element_rect(fill = "grey"), 
            plot.margin = unit(c(0.5,0.5,1,0.5), "cm"), legend.position="none") 

    p <- ggplotly(p, tooltip="text")
    p$elementId <- NULL
    p
  }))
  
  output$piechart <- renderPlotly(({
    tmp_data <- tmp_selected_target %>% group_by(`TargetCountry`) %>% count()
    tmp_data <- as.data.frame(tmp_data)
    # p <- ggplot(tmp_data) + aes(x = `TargetCountry`, y = `n`, fill=`TargetCountry`) + geom_bar(stat = 'identity') + 
    #   ggtitle("Distribution of Target Countries") + ylab("Count") + xlab("") + theme(legend.position="none") +
    #   theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.background = element_rect(fill = "grey"))
    # p <- ggplotly(p)
    # p$elementId <- NULL
    # p
    p2 <- plot_ly(tmp_data, labels = ~TargetCountry, values = ~n, type = 'pie')
    p2
  }))
  leafletProxy("mymap") %>% clearControls()
  leafletProxy("mymap") %>% addLegend("topright", colors= c("blue", "red"), 
                                       labels=c(paste("Takeoff:", nrow(tmp_selected_takeoff)), paste("Target:", nrow(tmp_selected_target))),
                                       title="Num",
                                       opacity = 0.5)
  })

})

