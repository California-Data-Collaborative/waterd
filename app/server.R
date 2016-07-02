library(shiny)
source("common.R")
library(ggplot2)
library(plyr)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  model <- reactive({
    modelfile <- MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$modelfile
    load(modelfile)
    model
  })
  
  plotdf <- reactive({
    
    modelObj <- model()
    
    days_per_step <- MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$days_per_step
    days_since_training_end <- 
      1 + interval(TRAINING_DATA_END_DATE,Sys.Date())/ddays(1)
    x_today <- 
      yearsToDate(1 + interval(TRAINING_DATA_START_DATE,Sys.Date())/dyears(1))
    steps_to_forecast <- (input$nDays+days_since_training_end)/days_per_step
    
    trainDf <- getTrainingData()

    if (MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$shortname %in% c('tbats','autoarima','ets')) {
      
      forecastOut <- forecast(modelObj,
                              steps_to_forecast,
                              level=input$confidenceLevel)

      df1 <- data.table(date=yearsToDate(index(forecastOut$x)),
                        mean=c(forecastOut$x),
                        upper=NA,
                        lower=NA)
      df2 <- data.table(date=yearsToDate(index(forecastOut$mean)),
                        mean=c(forecastOut$mean),
                        upper=c(forecastOut$upper),
                        lower=c(forecastOut$lower))
      
      plotdf <- rbind(df1,df2)
      
    } else if (MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$shortname == 'linearmodel') {
      
      scoreDf <- engineerFeatures(data.table(Date=seq(TRAINING_DATA_END_DATE+1,TRAINING_DATA_END_DATE+steps_to_forecast+1,'days')))
      pred <- data.table(predict(modelObj,
                                 scoreDf,
                                 interval='predict',
                                 level=input$confidenceLevel/100))
      
      df1 <- data.table(date=trainDf$Date,
                        mean=trainDf$Amount_Delivered_mg,
                        upper=NA,
                        lower=NA)
      df2 <- data.table(date=scoreDf$Date,
                        mean=pred$fit,
                        upper=pred$upr,
                        lower=pred$lwr)
      
      plotdf <- rbind(df1,df2)
      
    } else if (MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$shortname == 'gbm') {
      
      scoreDf <- engineerFeatures(data.table(Date=seq(TRAINING_DATA_END_DATE+1,TRAINING_DATA_END_DATE+steps_to_forecast+1,'days')))
      pred <- data.table(fit=predict(modelObj,
                                     scoreDf,
                                     n.trees=MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$n.trees))
      
      df1 <- data.table(date=trainDf$Date,
                        mean=trainDf$Amount_Delivered_mg,
                        upper=NA,
                        lower=NA)
      df2 <- data.table(date=scoreDf$Date,
                        mean=pred$fit,
                        upper=pred$fit,
                        lower=pred$fit)
      
      plotdf <- rbind(df1,df2)
      
    }
    
    plotdf$max_daily = trainDf[Date > Sys.Date()-dyears(3),input$maxDailyPull+max(Amount_from__or_to__Storage_mg)]
    
    print(plotdf)
    
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
    
    maxDailyFlowStr <- 'Estimated max daily flow (RW + storage)'
    consumptStr <- 'Total daily consumption'
    
    p <- ggplot(plotdf(), aes(date))
    p <- p + geom_ribbon(aes(ymin=lower,ymax=upper), alpha = 0.3)
    p <- p + geom_line(aes(y=mean,color='Total daily consumption'))
    p <- p + geom_line(aes(y=max_daily,color='Estimated max daily flow (RW + storage)'))
    p <- p + labs(x = "Date", y="Amount Delivered (mg)")
    p <- p + xlim(c(x_range_left,x_range_right))
    p <- p + geom_vline(xintercept=as.numeric(x_today))
    p <- p + geom_text(data=data.table(date=x_today,mean=0),aes(date,mean),label="today",angle=90,vjust=-0.5,hjust=0)
    p <- p + scale_colour_manual("", 
                                 values = c('Total daily consumption' = "black",
                                            'Estimated max daily flow (RW + storage)' = "red"))
    
    p
    
  })
  
  
  # Generate a summary of the data
  output$perCustomerUsage <- renderPrint({
    
  })
  
})
