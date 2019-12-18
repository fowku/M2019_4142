library(shiny)
library(dplyr)
library(ggplot2)

data <- mtcars

select <- c('mpg', 
             'disp', 
             'hp', 
             'drat', 
             'wt', 
             'qsec');
names(select) <- c('Miles/(US) gallon', 
                   'Displacement (cu.in.)',
                   'Gross horsepower', 
                   'Rear axle ratio', 
                   'Weight (1000 lbs)', 
                   '1/4 mile time');

colors <- c('cyl',
            'vs',
            'am',
            'gear',
            'carb');
names(colors) <- c('Number of cylinders',
                   'Engine (0 = V-shaped, 1 = straight)',
                   'Transmission (0 = automatic, 1 = manual)',
                   'Number of forward gears',
                   'Number of carburetors');

data$cyl <- as.factor(data$cyl);
data$vs <- as.factor(data$vs);
data$am <- as.factor(data$am);
data$gear <- as.factor(data$gear);
data$carb <- as.factor(data$carb);

ui <- fluidPage(

  sidebarLayout(

    sidebarPanel(
      textInput(inputId = 'title',
                label = 'Plot name',
                placeholder = 'Enter plot name'),

      selectInput(inputId = 'x',
                  label = 'X-axis',
                  choices = select,
                  selected = 'mpg'),

      selectInput(inputId = 'y',
                  label = 'Y-axis',
                  choices = select,
                  selected = 'disp'),
      
      selectInput(inputId = 'color',
                  label = 'Color by:',
                  choices = colors,
                  selected = 'cyl'),
      
      sliderInput(inputId = 'slider',
                  label = 'Alpha:',
                  min = 0, max = 1,
                  value = 0.5),
      
      numericInput(inputId = 'dotSize',
                   label = 'Dot size:',
                   value = 3,
                   min = 1, max = 9),
      
      checkboxInput(inputId = 'showData',
                    label = 'Show data?',
                    value = TRUE),
      
      checkboxInput(inputId = 'showSummary',
                    label = 'Show summary?',
                    value = TRUE)
    ),

    mainPanel(
      plotOutput(outputId = 'scatter'),
      
      fluidRow(
        column(width = 6,
               tableOutput(outputId = 'tableWhat')
        ),
        column(width = 6,
               tableOutput(outputId = 'tableSummary')
        )
      )
    )
  )
)

# Define server
server <- function(input, output) {
  output$scatter <- renderPlot({
    req(input$dotSize);
    
    ggplot(data, aes_string(x = input$x, y = input$y, color = input$color)) +
      geom_point(alpha = input$slider, size = input$dotSize) +
      ggtitle(tools::toTitleCase(isolate(input$title)))
  })
  
  newData <- reactive({
    data %>% select_(input$x, input$y, input$color)
  })
  
  output$tableWhat <- renderTable({
    if (input$showData) {
      newData()
    }
  })
  
  output$tableSummary <- renderTable({
    if (input$showSummary) {
      newData() %>%
        group_by_(input$color) %>%
        summarise_all(funs(mean, sd))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)