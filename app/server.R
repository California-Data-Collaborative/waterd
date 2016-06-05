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
    days_since_training_end <- 
      1 + interval(TRAINING_DATA_END_DATE,Sys.Date())/ddays(1)
    x_range_left <- 
      1 + interval(TRAINING_DATA_START_DATE,Sys.Date()-ddays(input$nDaysHist))/dyears(1)
    x_range_right <- 
      1 + interval(TRAINING_DATA_START_DATE,Sys.Date()+ddays(input$nDays))/dyears(1)
    x_today <- 
      1 + interval(TRAINING_DATA_START_DATE,Sys.Date())/dyears(1)
    
    plot(forecast(model(),(input$nDays+days_since_training_end)/days_per_step)
         ,xlim=c(x_range_left,x_range_right)
         ,xlab="Year number since start of training data"
         ,ylab="Amount Delivered (mg)"
         )
    abline(v=x_today)
    text(x=x_today,y=par("usr")[3],label='today',adj = c(1.1,-2))
  })
  
  # Generate a summary of the data
  output$perCustomerUsage <- renderPrint({
    
  })
  
})
