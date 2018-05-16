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
  # Headers including Github and Linkedin
  dashboardHeader(title = "World War II",
                  # Linkedin
                  dropdownMenu(
                    type = "notifications", 
                    icon = icon("linkedin"),
                    badgeStatus = NULL,
                    headerText = "Contact",
                    notificationItem("Yiqiang Zhao", icon = icon("linkedin-square"),
                                     href = "https://www.linkedin.com/in/yqzhao52/"),
                    notificationItem("Jay Xu", icon = icon("linkedin-square"),
                                     href = "https://www.linkedin.com/in/zhengjie-xu-b75311a7/")
                  ),
                  # Github
                  dropdownMenu(type = "notifications", icon = icon("github"),
                               badgeStatus = NULL,
                               headerText = "Github & Data",
                               notificationItem("Source Code", icon = icon("github-square"),
                                                href = "https://github.com/zhengjxu/dataviz"),
                               notificationItem("Data", icon = icon("database"),
                                                href = "https://www.kaggle.com/usaf/world-war-ii/data")
                               )),
  # Sidebar
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
  # Body
  dashboardBody(
    tabItems(
      # Maps
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
      # Alliances
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
                     fluidRow(img(src = "allied powers.jpg", height = '170px', width = '250px')),
                     fluidRow(tags$style("#fig3_text {font-size:12px; font-style:italic;}"),
                              htmlOutput("fig3_text"), align = "center"))))),
        fluidRow(
          box(splitLayout(
              cellWidths = c("12%", "38%", "50%"),
              cellArgs = list(style = "padding: 6px"),
              column(width = 1, img(src = "Legend.png", height = '84px', width = '100px'), height = 450),
              chorddiagOutput("chorddiag", height = 450),
              plotlyOutput("barplot2", height = 450)), width = 12))),
      # Timeline
      tabItem(
        tabName = "timeline",
        fluidRow(
          box(title = tags$b("Count of Bombing Mission in WW2"), solidHeader = TRUE,
              plotlyOutput("timeseries"), width = 12)),
        fluidRow(
          tabBox(width=12,
            tabPanel(tags$b("WW2 Introduction"), 
                                 fluidRow(
                                   tags$style("#allies_axis {font-size:15px;
               display:block; }"),
                                   column(width = 9, htmlOutput("intro")),
                                   column(width = 3, 
                                          fluidRow(align = "center",
                                          img(src = "uss_arizona_burning_sinking-P.jpeg", height = '170px', width = '240px')),
                                          fluidRow(tags$style("#fig1_text {font-size:12px; font-style:italic;}"),
                                                   htmlOutput("fig1_text"), align = "center")))),
            tabPanel(tags$b("Aircraft Series in WW2"), fluidRow(
              tags$style("#allies_axis {font-size:15px;
               display:block; }"),
              column(width = 9, plotlyOutput("aircraft_series")),
              column(width = 3, 
                     fluidRow(div(img(src = "b24b25b17.jpg", height = '190px', width = '270px')), style="text-align: center;"),
                     fluidRow(tags$style("#fig2_text {font-size:12px; font-style:italic;}"),
                       htmlOutput("fig2_text"), align = "center"),
                     fluidRow(tags$style("#fig2_text {font-size:14px;}"),htmlOutput("aircraft_text")))))
          )
        )
    ))
)))

