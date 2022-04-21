library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(shinythemes)

# read data

map_ar <- readRDS("map_ar.RDS") %>% 
  pivot_longer(cols = prop_non_white:prop_hispanic,
               names_to = "series", values_to = "values") 

binpal <- colorBin(palette = "Blues", domain = 0:100, 
                   bins = round(seq.int(0, 100, length.out = 7)))
mile <- 1609.34

ui <- 
  
  # Create a page for the Leaflet map. Use HTML and CSS to style the elements
  # of the page. leafletOutput("map1") generates the map, while
  # plotOut("tract") generates the demographic bar chart that appears in the
  # information panel.
  
  navbarPage(
    "LA's Oil Wells",
    theme = shinytheme("cosmo"),
    tabPanel("Map", 
             div(class = "map", tags$head(includeCSS("styles.css")),
                 leafletOutput("map", width = "100%", height = "100%"),
                 
                 absolutePanel(top = 10, right = 10, class = "input", 
                               style = "color: white",
                                   selectInput("pop_input",
                                           label = "Population density", 
                                           choices = c("White" = "prop_white",
                                                       "Non-White" = "prop_non_white",
                                                       "Black" = "prop_black",
                                                       "American Indian" = "prop_native",
                                                       "Asian" = "prop_asian",
                                                       "Hispanic" = "prop_hispanic")),
                               selectInput("status_input", 
                                           label = "Well status", 
                                           choices = c("Active or idle" = "active_idle",
                                                       "Plugged" = "plugged")),
                               radioButtons("buffer_input",
                                            label = "Buffer size",
                                            choices = c("1/4 mile" = mile * 0.25,
                                                        "1/2 mile" = mile * 0.5,
                                                        "3/4 mile" = mile * 0.75,
                                                        "1 mile" = mile)))
                 
                 # absolutePanel(id = "info", class = "panel panel-default", 
                 #               style = "color: white", bottom = 40, left = 20, 
                 #               width = 300, fixed = TRUE, draggable = FALSE, 
                 #               height = "auto", 
                 #               h3("Race on the T"),
                 #               p(),
                 #               br())
    )
    ),
    
    # Add a model page to explain the model and findings from the analysis.
    # fluidPage columns create a flexible, dynamic 3 column layout, where the
    # blank outer columns pad the content in the middle column. Display the
    # plots and tables as images and style with CSS.
    
    tabPanel("Model", 
             fluidPage(
               column(2),
               column(8),
               column(2)
             )
    ),
    
    # Add an about page to explain the motivations and background of the
    # project.
    
    tabPanel("About", 
             fluidPage(
               # img(class = "bg", src = "metro-boston-2.jpg"),
               column(2),
               column(8),
               column(2)
             )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # create reactive function for reactive leaflet inputs
  
  filtered_map <- reactive({
    map_ar %>% 
      filter(status == input$status_input & buffer == input$buffer_input) %>% 
      filter(series == input$pop_input)
  })
  
  # static leaflet (base map that doesn't need to be refreshed on input)
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = T)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      # fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>% 
      setView(-118.250110, 34, zoom = 10) %>% 
      addScaleBar(position = "bottomleft",
                  scaleBarOptions(maxWidth = 300)) %>% 
      addLegend(data = map_ar, 
                position = "bottomleft",
                pal = binpal,
                values = ~values,
                title = "Percentage",
                opacity = 1)
  })
  
  # add well buffers with leafletProxy with reactive filtered data
  
  observe({
    
    leafletProxy("map", data = filtered_map()) %>%
      clearShapes() %>%
      addPolygons(weight = 1,
                  fillOpacity = 0.75,
                  highlightOptions = highlightOptions(weight = 4),
                  popup = ~str_c(values, "% ", 
                                 case_when(input$pop_input == "prop_white" ~ "White",
                                           input$pop_input == "prop_black" ~ "Black",
                                           input$pop_input == "prop_native" ~ "American Indian",
                                           input$pop_input == "prop_asian" ~ "Asian",
                                           input$pop_input == "prop_hispanic" ~ "Hispanic",
                                           input$pop_input == "prop_non_white" ~ "Non-White")),
                  fillColor = ~binpal(values))
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
