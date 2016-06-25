library(shiny)
source("common.R")
library(ggplot2)
library(plyr)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  model <- reactive({
    modelfile <- MODEL_LIST[input$modelType][[1]]$modelfile
    load(modelfile)
    model
  })
  
  plotdf <- reactive({
    
    modelObj <- model()
    
    if (input$modelType %in% c('tbats','autoarima','ets')) {
      
      days_per_step <- MODEL_LIST[input$modelType][[1]]$days_per_step
      days_since_training_end <- 
        1 + interval(TRAINING_DATA_END_DATE,Sys.Date())/ddays(1)
      
      forecastOut <- forecast(modelObj,
                              (input$nDays+days_since_training_end)/days_per_step,
                              level=input$confidenceLevel)
      dateAxis <- yearsToDate(c(index(forecastOut$x),index(forecastOut$mean)))
      plotdf <- data.table(date=dateAxis,
                           mean=c(forecastOut$x,forecastOut$mean),
                           upper=c(rep(NA,length(forecastOut$x)),forecastOut$upper),
                           lower=c(rep(NA,length(forecastOut$x)),forecastOut$lower))
    }
    
    plotdf
    
  })
  
  output$totalUsage <- renderPlot({
    
    x_range_left <- 
      yearsToDate(1 + interval(TRAINING_DATA_START_DATE,Sys.Date()-ddays(input$nDaysHist))/dyears(1))
    x_range_right <- 
      yearsToDate(1 + interval(TRAINING_DATA_START_DATE,Sys.Date()+ddays(input$nDays))/dyears(1))
    x_today <- 
      yearsToDate(1 + interval(TRAINING_DATA_START_DATE,Sys.Date())/dyears(1))
    
    #plot(forecast(model(),(input$nDays+days_since_training_end)/days_per_step)
    #     ,xlim=c(x_range_left,x_range_right)
    #    ,xlab="Year number since start of training data"
    #     ,ylab="Amount Delivered (mg)"
    #     )
    #abline(v=x_today)
    #text(x=x_today,y=par("usr")[3],label='today',adj = c(1.1,-2))
    
    ggplot(plotdf(), aes(date)) +
      geom_ribbon(aes(ymin=lower,ymax=upper), alpha = 0.3) +
      geom_line(aes(y=mean)) +
      labs(x = "Date", y="Amount Delivered (mg)") +
      xlim(c(x_range_left,x_range_right)) +
      geom_vline(xintercept=as.numeric(x_today)) +
      geom_text(data=data.table(date=x_today,mean=0),aes(date,mean),label="today",angle=90,vjust=-0.5)
  })
  
  
  # Generate a summary of the data
  output$perCustomerUsage <- renderPrint({
    
  })
  
})
