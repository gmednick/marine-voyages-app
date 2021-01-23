library(tidyverse)
library(shiny)
library(shiny.semantic)
library(janitor)
library(leaflet)
library(scales)
theme_set(theme_light())

marine_df <- read_csv('ships.csv') %>% 
  clean_names()

ship_names <- marine_df %>% 
  select(shipname) %>% 
  distinct() %>% 
  mutate(shipname = str_to_title(shipname))


icons <- awesomeIcons(
  icon = 'fa-ship',
  iconColor = 'blue',
  library = 'fa',
  markerColor = "lightred"
)


ui <- fluidPage(title = "Marine Travel Routes",
               theme = "slate",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("ship_name", 
                               label = "Select Ship:",
                               choices = ship_names$shipname, 
                               selected = c('Karoli'),
                               selectize = TRUE,
                               multiple = FALSE)
                   ),
                 mainPanel(
                   # fluidRow(valueBoxOutput(""), #voyage_distance
                   #          valueBoxOutput("")),  #voyage_length
                   fluidRow(box(leafletOutput("marine_map")))
                 )
))
server <- function(input, output, session) {
  
  orig_dest <- reactive({
    marine_df %>% 
      group_by(shipname) %>% 
      slice(c(1, n())) %>%
      ungroup() %>% 
      filter(shipname == input$ship_name)
  })
  
  dat <- reactive({
    marine_df %>% 
      filter(shipname == input$ship_name)
  })
  
  output$marine_map <- renderLeaflet({
    
  leaflet() %>% 
    addTiles() %>% 
    addAwesomeMarkers(data = orig_dest(), ~lon, ~lat, icon = ~icons) %>% 
    addPolylines(data = dat(), ~lon, ~lat, weight = 2) %>% 
    addScaleBar('bottomleft') %>%
    addControl('', position = "topleft")
  })
  }

shinyApp(ui = ui, server = server)
