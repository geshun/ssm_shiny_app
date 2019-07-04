library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(DT)

header <- dashboardHeader(title = "SSM Interactive")
#----
sidebar <- dashboardSidebar(
  sidebarMenu(menuItem(text = "Event Level", icon = icon("bar-chart-o"), 
                       tabName = "demand"
                       ),
              menuItem(text = "Unit Level", icon = icon("th"), 
                       menuSubItem("Response Time", tabName = "response"),
                       menuSubItem("Hospital Wait Time", tabName = "destination")
                       )
              ), 
  hr(), 
  selectInput(inputId = "event_ward", label = "Event Ward", 
              choices = c("all" = 0
                          , "Ward 1" = 1
                          , "Ward 2" = 2
                          , "Ward 3" = 3
                          , "Ward 4" = 4
                          , "Ward 5" = 5
                          , "Ward 6" = 6
                          , "Ward 7" = 7
                          , "Ward 8" = 8
                          , "Ward 9" = 9
                          , "Ward 10" = 10
                          , "Ward 11" = 11
                          , "Ward 12" = 12
                          ),
              selected = 0,
              multiple = TRUE
              ),
  selectInput(inputId = "call_hour", label = "Event Hour", 
              choices = -1:23, selected = -1, multiple = TRUE
              ),
  selectInput(inputId = "percentile_type", label = "Percentile Type", 
              choices = c(50, 90), selected = 90
              ),
  sliderInput(inputId = "travel", label = "Travel Time (minutes)", 
              min = 0, max = 20, value = 7, step = 0.5
              ),
  selectInput(inputId = "station", label = "Unit Dispatch Location", 
              choices = c('any', 'in station', 'out station'),
              selected = 'any'
              ),
  sliderInput(inputId = "destination", label = "Wait Time (minutes)", 
              min = 60, max = 180, value = 90, step = 10
              )
  ) #---- end sidebar
#----
body <- dashboardBody(
  tabItems(tabItem(tabName = "demand", 
                   fluidRow(column(width = 12,
                                   leafletOutput(outputId = "map_event_location", height = 400),
                                   br()
                                   )
                            ),
                   fluidRow(column(width = 6,
                                   plotlyOutput(outputId = "bar_event_location"),
                                   br()
                                   ),
                            column(width = 6,
                                   plotlyOutput(outputId = "heatmap_event_location")
                                   )
                            )
                   ), #---- end demand tab
           tabItem(tabName = "response", 
                   fluidRow(column(width = 12,
                                   tabsetPanel(
                                     tabPanel(title = 'Response Components',
                                              plotlyOutput(outputId = "response_percentile")),
                                     tabPanel(title = "Coverage Time",
                                              dataTableOutput(outputId = "coverage_time")),
                                     tabPanel(title = 'Dispatch Location',
                                              plotlyOutput(outputId = "station_boxplot"))
                                     ), 
                                   br()
                                   )
                            ),
                   fluidRow(column(width = 6,
                                   plotlyOutput(outputId = "travel_distribution"), 
                                   br()
                                   ),
                            column(width = 6,
                                   plotlyOutput(outputId = "dist_distribution")
                                   )
                            )
                   ), #---- end response tab
           tabItem(tabName = "destination", 
                   fluidRow(column(width = 12,
                                   tabsetPanel(
                                     tabPanel(title = "Destination Interval",
                                              plotlyOutput(outputId = "destination_percentile")),
                                     tabPanel(title = "Hour Utilization", 
                                              plotlyOutput(outputId = "clear_percenetile"))
                                     ),
                                   br()
                                   )
                            ),
                   fluidRow(column(width = 12,
                                   plotlyOutput(outputId = "transport_volume")
                                   )
                            )
                   ) #---- end destination tab
           ) #----end tabItems
  ) #----end body
#----
page <- dashboardPage(header = header, sidebar = sidebar, body = body)
#----
shinyUI(page)