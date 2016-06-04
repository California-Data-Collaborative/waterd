library(shiny)
source("common.R")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  model <- reactive({
    
    modelfile <- MODEL_LIST[input$modelType][[1]]$modelfile
    
    load(modelfile)
    
    model
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$totalUsage <- renderPlot({
    
    days_per_step <- MODEL_LIST[input$modelType][[1]]$days_per_step
    
    plot(forecast(model(),input$nDays/days_per_step))
  })
  
  # Generate a summary of the data
  output$perCustomerUsage <- renderPrint({
    
  })
  
})
