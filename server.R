library(tidyverse)
library(shiny)
library(leaflet.extras)
library(geosphere)
library(htmltools)
library(mapview)
library(crosstalk)
library(sf)
library(plotly)
library(chorddiag)
####Loading Data####
attack_data <- read_csv("data/attack_data.csv")
df <- read_csv("data/simplified_data.csv")
load("data/flight_route.rda") # to accelerate the loading speed
df_takeoff <- df %>% st_as_sf(., coords = c("Takeoff Longitude", "Takeoff Latitude"))
df_target <- df %>% st_as_sf(., coords = c("Target Longitude", "Target Latitude"))
ts_df <- read_csv("data/monthly_obs_count.csv", col_types = list("start_month" = col_datetime()))
load("data/groupColors.rda")
load("data/ajmatrix.rda")
allies_axis_text <- read_file("data/axis_allies.txt")
intro_text <- read_file("data/intro.txt")
####Sever####
shinyServer(function(input, output, session) {
  target_map <- SharedData$new(df_target, key = ~id, group = "grp1")
  takeoff_map <- SharedData$new(df_takeoff, key = ~id, group = "grp2")
  lf <-   leaflet(target_map) %>%
    addTiles(urlTemplate = "https://api.mapbox.com/styles/v1/yiqiang/cjglw67l500262tqmguz0efum/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoieWlxaWFuZyIsImEiOiJjamI2MjJ2aDgzZTJiMzdvMTEza25vN3czIn0.da7McqdO9XgggUE0LTCx3Q"
    ) %>%  addPolylines(data = flight_route,  opacity = 0.3, fillOpacity = 0.2, color = "black", stroke = TRUE, weight = 1) %>%
    addCircleMarkers(fillOpacity = 0.1, color = "#08519c", radius = 1, group = "grp1") %>%
    addCircleMarkers(data = takeoff_map, fillOpacity = 0.1, color = "#969696", radius = 1, group = "grp2") %>%
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
    #if no points selected
    if(nrow(tmp_selected_target) == 0 & nrow(tmp_selected_takeoff) == 0){
      tmp_selected_target <- target_map$data(withSelection = TRUE)
      tmp_selected_takeoff<- takeoff_map$data(withSelection = TRUE)
    }

  output$barplot <- renderPlotly(({
    tmp_data <- tmp_selected_target %>% group_by(`Aircraft Series`) %>% count() %>% arrange(n)
    tmp_data$Aircraft_Series <- tmp_data$`Aircraft Series`
    p <- ggplot(tmp_data) + 
      aes(x = `Aircraft_Series`, y = `n`,
          text=sprintf("Aircraft Series: %s<br>Num: %s", `Aircraft_Series`, `n`)) + 
      geom_bar(stat = 'identity') + 
      ylab("Count") + xlab("")  + ggtitle("Aircraft Series Barplot") +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, colour = "#4f4f4f"), 
            plot.title = element_text(size = 14, face = "bold", colour = "#4f4f4f"),
            plot.background = element_rect(fill = "#f0f0f0"), 
            plot.margin = unit(c(0.5,0.5,1,0.5), "cm"), 
            axis.title=element_text(size=12), 
            legend.position="none"
            ) 

    p <- ggplotly(p, tooltip="text")
    p$elementId <- NULL
    p
  }))
  
  output$piechart <- renderPlotly(({
    colors <- c(rev(c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c")),
                rep("#deebf7", 20))
    tmp_data <- tmp_selected_target %>% group_by(`Target Country`) %>% count() %>% arrange(., desc(n))
    tmp_data <- as.data.frame(tmp_data)
    textpos <- ifelse(tmp_data$n/sum(tmp_data$n, na.rm = TRUE) < 0.04, "none", "auto")
    m <- list(l = 50, r = 50, b = 20, t = 55, pad = 2)
    p2 <- plot_ly(tmp_data, labels = ~`Target Country`, values = ~n, type = 'pie',
                  insidetextfont = list(color = '#FFFFFF'),
                  textposition = textpos, textinfo = 'label+percent',
                  marker = list(colors = colors,
                                line = list(color = '#FFFFFF', width = 0.5)
                                )) %>%
          layout(paper_bgcolor = "#f0f0f0", title = "<b>Attacked Country Pie Chart<b>",
                 margin = m, font = list(size = 14))
    p2$elementId <- NULL
    p2
  }))
  leafletProxy("mymap") %>% clearControls()
  leafletProxy("mymap") %>% addLegend("topright", colors= c("#969696", "#08519c"), 
                                       labels=c(paste("Takeoff:", nrow(tmp_selected_takeoff)), paste("Target:", nrow(tmp_selected_target))),
                                       title="Num",
                                       opacity = 0.5)
  })
  
  annot1 <- list(xref = 'paper', yref = 'y', x = 0.02, y = 3000, xanchor = 'left', yanchor = 'middle',
    text = ~paste("On 10 June 1940,\n Italy declares war on\nFrance and Britain\nand invades France."),
    font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  annot2 <- list(xref = 'paper', yref = 'y', x = 0.2, y = 4000, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("On 7 December 1941, Japanses\nattack Pearl Harbor.U.S. declares\nwar onAxis powers."),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  annot3 <- list(xref = 'paper', yref = 'y', x = 0.39, y = 2000, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("On 8 November 1942,\nU.S. and Britain land in\nFrench North Africa"),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  annot4 <- list(xref = 'paper', yref = 'y', x = 0.55, y = 3500, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("On 31 January 1943, German\n6th Army surrenders at Stalingrad"),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  annot5 <- list(xref = 'paper', yref = 'y', x = 0.60, y = 5500, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("On 6 June 1943, D-Day-Allie\ninvade Normandy"),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  annot6 <- list(xref = 'paper', yref = 'y', x = 0.77, y = 8000, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("On 7 May 1945, Germany\nsigns unconditional surrender"),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  annot7 <- list(xref = 'paper', yref = 'y', x = 0.83, y = 0, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("Yalta Conference"),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = T)
  
  annot8 <- list(xref = 'paper', yref = 'y', x = 0.96, y = 4500, xanchor = 'left', yanchor = 'middle',
                 text = ~paste("A-bombs dropped on Hiroshima\nand Nagasaki; Japan Surrenders"),
                 font = list(family = 'Arial', size = 10, color = 'rgba(67,67,67,1)'), showarrow = F)
  
  output$timeseries <- renderPlotly({
    p3 <- ts_df %>% plot_ly(x = ~start_month, y = ~ct, 
                            color = ~`Theater of Operations`,
                            type = 'scatter', mode = 'lines', 
                            colors = c("#8dd3c7", "#bebada", "#fb8072", "#80b1d3", "#377eb8")) %>% 
      layout(yaxis=list(title='Count'), 
             xaxis=list(title=''), shapes = list(list(type = "line", x0 = as.Date("1940-06-10"), 
                                                      x1 = as.Date("1940-06-10"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)),
                                                 list(type = "line", x0 = as.Date("1941-12-07"), 
                                                      x1 = as.Date("1941-12-07"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)),
                                                 list(type = "line", x0 = as.Date("1942-11-08"), 
                                                      x1 = as.Date("1942-11-08"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)),
                                                 list(type = "line", x0 = as.Date("1943-02-02"), 
                                                      x1 = as.Date("1943-02-02"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)),
                                                 list(type = "line", x0 = as.Date("1944-06-01"), 
                                                      x1 = as.Date("1944-06-01"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)),
                                                 list(type = "line", x0 = as.Date("1945-05-07"), 
                                                      x1 = as.Date("1945-05-07"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)),
                                                 list(type = "line", x0 = as.Date("1945-08-06"), 
                                                      x1 = as.Date("1945-08-06"), y0 = 0, y1 = 11000,
                                                      line=list(dash='dot', width=1)))) %>%
      layout(annotations = annot1) %>% layout(annotations = annot2) %>% layout(annotations = annot3) %>%
      layout(annotations = annot4) %>% layout(annotations = annot5) %>% layout(annotations = annot6) %>%
      layout(annotations = annot7) %>% layout(annotations = annot8)
             
    p3$elementId <- NULL
    p3
  })
  
  output$chorddiag <- renderChorddiag(
    chorddiag(ajmatrix, groupColors = groupColors, groupnamePadding = 40, 
              showGroupnames = FALSE, tooltipGroupConnector = ' <- ',
              clickAction = "Shiny.onInputChange('sourceIndex', d.source.index+1);
                                 Shiny.onInputChange('targetIndex', d.target.index+1);",
              margin = 35, width = 10) 
  )
  
  groupNames <- dimnames(ajmatrix)[[1]]
  
  city_data <- reactive({
    if(is.null(input$sourceIndex)){
      t <- attack_data %>% filter(., Country == "USA", 
                         `Target Country` == "GERMANY") %>% 
        group_by(., `Target City`) %>% count() %>% arrange(desc(n)) %>% head(10) %>% as.data.frame()
      return(t)
    }else{
      t <- attack_data %>% filter(., Country == groupNames[input$targetIndex], 
                         `Target Country` == groupNames[input$sourceIndex]) %>% 
        group_by(., `Target City`) %>% count() %>% arrange(desc(n)) %>% head(10) %>% as.data.frame()
      return(t)
  }
    })
  observe({
    tmp_city_data <- city_data()
    output$barplot2 <- renderPlotly({p3 <- ggplot(tmp_city_data) + 
                       aes(x = `Target City`, y = `n`,
                           text=sprintf("Target City: %s<br>Num: %s", `Target City`, `n`)) + 
                       geom_bar(stat = 'identity') + 
                       ylab("Count") + xlab("")  + ggtitle("Target City Barplot") +
                       theme(axis.text.x = element_text(angle = 20, hjust = 1, colour = "#4f4f4f"), 
                             plot.title = element_text(size = 14, face = "bold", colour = "#4f4f4f"),
                             plot.background = element_rect(fill = "white"), 
                             plot.margin = unit(c(0.5,0.5, 0.5, 0.5), "cm"), 
                             axis.title=element_text(size=12), 
                             legend.position="none") 
                     p3 <- ggplotly(p3, tooltip="text")
                     p3$elementId <- NULL
                     p3})

})
  
  output$allies_axis <- renderText(allies_axis_text)
  output$intro <- renderText((intro_text))
  output$instruction <- renderText(paste("Use the selection tool on the map to highlight the target area and update plots"))
})

