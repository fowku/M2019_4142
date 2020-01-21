library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

nyc <- read.csv(file = 'nyc.csv')

# Clean N/A rows
nyc <- na.omit(nyc)

# Represent as factors
nyc$neighbourhood_group <- as.factor(nyc$neighbourhood_group);
nyc$room_type <- as.factor(nyc$room_type);

districts <- c('All', 'Bronx', 'Queens', 'Staten Island', 'Manhattan', 'Brooklyn')
roomTypes <- c('All', 'Entire home/apt', 'Private room', 'Shared room')

# Colors for districts
colors <- colorFactor(c('navy', 'red', 'green', 'blue', 'orange'), domain = districts)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width: 100%; height: 100%; padding: 10px}"),
  
  tabsetPanel(
    tabPanel("Map", leafletOutput('map', height = '765px')), 
    tabPanel("Number of reviews / Price", plotOutput(outputId = 'reviewPrice', height = '765px', width = '1100px')),
    tabPanel("Minimum nights / Price", plotOutput(outputId = 'nightsPrice', height = '765px', width = '1100px'))
  ),

  absolutePanel(right = 30, top = 70,
                sliderInput(inputId = 'houses',
                            label = 'Number of points',
                            min = 1,
                            max = nrow(nyc),
                            value = 200,
                            step = 1
                ),

                selectInput(inputId = 'district',
                            label = 'District',
                            choices = districts,
                            selected = 'All'
                ),

                selectInput(inputId = 'room',
                            label = 'Room type',
                            choices = roomTypes,
                            selected = 'All'
                )
  )
)

# Define server
server <- function(input, output) {
  newData <- reactive({
    tempData <- nyc %>% slice(1:input$houses)
    
    if (input$district != 'All') {
      tempData <- tempData %>% filter(neighbourhood_group == input$district)
    }
    
    if (input$room != 'All') {
      tempData <- tempData %>% filter(room_type == input$room)
    }
    
    return(tempData)
  })
  
  output$map <- renderLeaflet({
    leaflet(newData()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(lng = ~longitude,
                       lat = ~latitude,
                       color = ~colors(neighbourhood_group))
  })
  
  output$reviewPrice <- renderPlot({
    ggplot(newData(), aes(x = price, 
                          y = number_of_reviews, 
                          color = neighbourhood_group)) +
      geom_point() +
      xlab("Price, $") +
      ylab("Number of reviews") +
      theme_linedraw()
  })
  
  output$nightsPrice <- renderPlot({
    ggplot(newData(), aes(x = price, 
                          y = minimum_nights, 
                          color = neighbourhood_group)) +
      geom_point() +
      xlab("Price, $") +
      ylab("Minimum nights") +
      theme_linedraw()
  })
}

# Run the application 
shinyApp(ui, server)