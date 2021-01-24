library(tidyverse)
library(shiny)
library(shiny.semantic)
library(janitor)
library(leaflet)
library(scales)
library(shinythemes)
library(geosphere)

marine_df <- read_csv('ships.csv') %>% 
  clean_names() %>% 
  mutate(shipname = str_to_title(shipname))

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


ui <- semanticPage(
  titlePanel("Marine Travel Routes"),
  theme = "superhero",
  selectInput("ship_class", 
              label = "Select a Ship Type:",
              choices = ship_types$ship_type, 
              selected = 'Karoli',
              selectize = TRUE,
              multiple = FALSE),
  selectInput("ship_name", 
              label = "Select a Ship:",
              choices = ship_names$shipname, 
              selected = 'Cargo',
              selectize = TRUE,
              multiple = FALSE),
  fluidRow(valueBoxOutput("voyage_dist")), #voyage_distance
  fluidRow(leafletOutput("marine_map"))
)
server <- function(input, output, session) {
  
observeEvent(input$ship_class,{
    updateSelectInput(session,'ship_name',
                      choices=ship_names %>% 
                        filter(ship_names$ship_type == input$ship_class) %>% 
                        select(shipname))
  }) 
  orig_dest <- reactive({
    marine_df %>% 
      mutate(dist_max = speed*0.514*120) %>% 
      group_by(shipname) %>% 
      slice(c((which.max(dist_max) - 1), which.max(dist_max))) %>% 
      ungroup() %>% 
      filter(ship_type %in% input$ship_class) %>% 
      filter(shipname %in% input$ship_name)
  })
  
  dat <- reactive({
    marine_df %>% 
      filter(ship_type %in% input$ship_class) %>% 
      filter(shipname %in% input$ship_name)
  })
  
  output$marine_map <- renderLeaflet({
    
  leaflet() %>% 
    addTiles() %>% 
    addAwesomeMarkers(data = orig_dest(), ~lon, ~lat, icon = ~icons) %>% 
    addPolylines(data = dat(), ~lon, ~lat, weight = 2) %>% 
    addProviderTiles(providers$Esri.WorldImagery) %>%
    addScaleBar('bottomleft') %>%
    addControl('', position = "topleft")
  })
  }

shinyApp(ui = ui, server = server)
