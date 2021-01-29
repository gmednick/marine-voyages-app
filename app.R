library(tidyverse)
library(shiny)
library(shinydashboard)
#library(shiny.semantic)
#library(semantic.dashboard)
library(janitor)
library(leaflet)
library(dashboardthemes)
library(lubridate)


marine_df <- read_rds('marine_df.rds') 

ship_types <- marine_df %>% 
  select(ship_type) %>% 
  distinct()

ship_names <- marine_df  %>% 
  distinct(shipname, .keep_all = TRUE)

icons <- awesomeIcons(
  icon = 'fa-ship',
  iconColor = 'blue',
  library = 'fa',
  markerColor = "lightred"
)
ui <- dashboardPage(
  dashboardHeader(title = "Marine Travel Routes"),
  dashboardSidebar(
    selectInput("ship_class", 
              label = "Select a Ship Type:",
              choices = ship_types$ship_type, 
              selected = 'Cargo',
              selectize = TRUE,
              multiple = FALSE),
    selectInput("ship_name", 
                label = "Select a Ship:",
                choices = ship_names$shipname, 
                selected = 'Karoli',
                selectize = TRUE,
                multiple = FALSE),
  box(
    title = "This app lets you select specific ships by ship type and name. The 
    app displays the ship's route and provides details such as trip duration,  
    port of origin and destination, and max speed.", 
    width = 12, background = "aqua"
  )),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),  
    valueBoxOutput("name", width = 12),
    valueBoxOutput("days_orig_dest", width = 6),
    valueBoxOutput("max_dist", width = 6), # change to max speed
    fluidRow(box(leafletOutput("marine_map"), width = 12))
  ))
server <- function(input, output, session) {
  
  observeEvent(input$ship_class,{
    updateSelectInput(session,'ship_name',
                      choices = unique(ship_names$shipname[ship_names$ship_type 
                                                           == input$ship_class]))
  })
  
  orig_dest <- reactive({
    marine_df %>% 
      arrange(datetime) %>% 
      group_by(shipname) %>% 
      slice(c(1, n())) %>% 
      ungroup() %>% 
      filter(ship_type %in% input$ship_class) %>% 
      filter(shipname %in% input$ship_name)
  })
  
  dat <- reactive({
    marine_df %>%
      group_by(shipname) %>% 
      arrange(datetime) %>% 
      mutate(dist_max = speed*0.514*120) %>% 
      filter(ship_type %in% input$ship_class) %>% 
      filter(shipname %in% input$ship_name)
  })
  
  max_dist_obs <- reactive({
    dat() %>% 
      arrange(desc(datetime)) %>% 
      group_by(shipname) %>% 
      slice(c((which.max(speed) + 1), which.max(speed))) %>% 
      ungroup() %>% 
      filter(ship_type %in% input$ship_class) %>% 
      filter(shipname %in% input$ship_name)
  })
  
  trip_duration <- reactive({
    marine_df %>% 
    group_by(shipname) %>% 
    summarize(min = min(date), 
              max = max(date),
              days = difftime(max, min, unit = "day")) %>% 
    ungroup() %>% 
    filter(shipname %in% input$ship_name)
  })
  
  output$name <- renderValueBox({
    valueBox(
      value = paste0("Ship Name: ", unique(max_dist_obs()$shipname)),
      subtitle = "",
      color = 'light-blue',
      icon =icon("")
    )
  })
  
  output$days_orig_dest <- renderValueBox({
    valueBox(
      value = prettyNum(round(max(trip_duration()$days)), big.mark = ","),
      subtitle = paste0(unique(max_dist_obs()$shipname), " started at ", 
                        dat()$port[1], " and arrived at ",  dat()$destination[1], "."),
      color = 'light-blue',
      icon =icon("fas fa-ship") 
    )
  })
  
  output$max_dist <- renderValueBox({
    valueBox(
      value =paste0(prettyNum(round(max(max_dist_obs()$speed)), big.mark = ","), " knots"),
      subtitle = paste0(unique(max_dist_obs()$shipname), " travelled ", 
                        round(max(max_dist_obs()$dist_max)), " meters at top 
                        speed recorded over a two-minute
                        interval (fuchsia circle marker on map)."),
      color = 'light-blue',
      icon = icon("") # fas fa-water, fas fa-tachometer-alt
    )
  })
  output$marine_map <- renderLeaflet({

  leaflet() %>%
    addTiles() %>%
    addAwesomeMarkers(data = orig_dest(), ~lon, ~lat, icon = ~icons) %>%
    addCircles(data = max_dist_obs(), ~lon, ~lat, color = 'fuchsia', popup = "max speed interval") %>%
    addPolylines(data = dat(), ~lon, ~lat, weight = 2) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addScaleBar('bottomleft')
  })
  }

shinyApp(ui = ui, server = server)
