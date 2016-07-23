library(shiny)
source("common.R")
library(ggplot2)
library(plyr)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  x_range_left <- reactive({
    yearsToDate(1 + interval(TRAINING_DATA_START_DATE,getTodayLocal()-ddays(input$nDaysHist))/dyears(1))
  })
  x_range_right <- reactive({
    yearsToDate(1 + interval(TRAINING_DATA_START_DATE,getTodayLocal()+ddays(input$nDays))/dyears(1))
  })
  x_today <- reactive({
    yearsToDate(1 + interval(TRAINING_DATA_START_DATE,getTodayLocal())/dyears(1))
  })
  
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
#     x_today <- 
#       yearsToDate(1 + interval(TRAINING_DATA_START_DATE,Sys.Date())/dyears(1))
    steps_to_forecast <- (input$nDays+days_since_training_end)/days_per_step
    
    trainDf <- getTrainingData()
    
    if (MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$shortname %in% c('tbats','autoarima','ets')) {
      
      forecastOut <- forecast(modelObj,
                              steps_to_forecast,
                              level=input$confidenceLevel)
      predStd <- forecast(modelObj,
                          steps_to_forecast,
                          level=(pnorm(1,0,1)-pnorm(-1,0,1))*100)
      
      df1 <- data.table(date=yearsToDate(index(forecastOut$x)),
                        mean=c(forecastOut$x),
                        upper=NA,
                        lower=NA,
                        sigma=NA)
      df2 <- data.table(date=yearsToDate(index(forecastOut$mean)),
                        mean=c(forecastOut$mean),
                        upper=c(forecastOut$upper),
                        lower=c(forecastOut$lower),
                        sigma=c(predStd$upper-forecastOut$mean))
      
      plotdf <- rbind(df1,df2)
      
    } else if (MODEL_LIST[grep(input$modelType,MODEL_LIST)][[1]]$shortname == 'linearmodel') {
      
      scoreDf <- engineerFeatures(data.table(Date=seq(TRAINING_DATA_END_DATE+1,TRAINING_DATA_END_DATE+steps_to_forecast+1,'days')))
      pred <- data.table(predict(modelObj,
                                 scoreDf,
                                 interval='predict',
                                 level=input$confidenceLevel/100))
      # get sigma
      predStd <- data.table(predict(modelObj,
                                    scoreDf,
                                    interval='predict',
                                    level=(pnorm(1,0,1)-pnorm(-1,0,1))))
      
      df1 <- data.table(date=trainDf$Date,
                        mean=trainDf$Amount_Delivered_mg,
                        upper=NA,
                        lower=NA,
                        sigma=NA)
      df2 <- data.table(date=scoreDf$Date,
                        mean=pred$fit,
                        upper=pred$upr,
                        lower=pred$lwr,
                        sigma=predStd$upr-predStd$fit)
      
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
    
    # plotdf$max_daily = trainDf[Date > Sys.Date()-dyears(3),input$maxDailyPull+max(Amount_from__or_to__Storage_mg,na.rm=T)]
    plotdf$max_daily = trainDf[Date > Sys.Date()-dyears(3),input$maxDailyPull+input$maxDailyStorage]
    
    print(plotdf)
    
    plotdf
    
  })
  
  output$totalUsage <- renderPlot({
    
    maxDailyFlowStr <- 'Estimated max daily flow (RW + storage)'
    consumptStr <- 'Total daily consumption'
    
    waterColor <- '#00406A'
    
    p <- ggplot(plotdf(), aes(date))
    p <- p + geom_ribbon(aes(ymin=lower,ymax=upper), alpha = 0.2, fill=waterColor)
    p <- p + geom_line(aes(y=mean,color='Total daily demand'))
    p <- p + geom_line(aes(y=max_daily,color='Estimated max daily supply (RW + storage)'))
    p <- p + labs(x = "Date", y="Amount Delivered (mg)")
    p <- p + xlim(c(x_range_left(),x_range_right()))
    p <- p + geom_vline(xintercept=as.numeric(x_today()),color='gray50',linetype=2)
    p <- p + geom_text(data=data.table(date=x_today(),mean=0),aes(date,mean),label="today",color='gray50',angle=90,vjust=-0.5,hjust=-0.5)
    p <- p + scale_colour_manual("Legend:", 
                                 values = c('Total daily demand' = waterColor,
                                            'Estimated max daily supply (RW + storage)' = "#CE3D32"))
    p <- p + theme_bw()
    p <- p + theme(legend.position="top",
                   legend.background = element_rect(linetype="solid",color='gray80'))
    
    p
    
  })
  
  output$dailyExcess <- renderUI({
    excessNDays <- 0

    df <- plotdf()[date >= x_today() & date <= x_range_right(),]

    # http://stats.stackexchange.com/questions/9510/probability-distribution-for-different-probabilities
    # For N Bernoulli trials with *different probabilities p_i*, for N large and p_i never too small,
    # the distribution of n successes is normal with mean = sum(p_i) and variance 
    # sigma^2 = sum(p_i*(1-p_i)).
    # Trial i = day
    # N = number of days from now to the end of the forecast period
    # p_i = chance of exceeding the max daily supply
    # Assumptions: 
    #  - normally distributed model errors in daily forecast values
    #  - model errors are uncorrelated across days
    #  - p_i never too small
    #  - N large enough to apply central limit theorem
    df[,prob_exceeding:=1-pnorm(max_daily,mean,sigma)]
    excessNDaysMu <- df[,sum(prob_exceeding)]
    excessNDaysSigma <- sqrt(df[,sum(prob_exceeding*(1-prob_exceeding))])
    excessNDays <- max(qnorm(1-input$excessProb/100,excessNDaysMu,excessNDaysSigma),0)
    
    calloutStyle <- "text-align:center;font-weight:bold;font-size:150%;color:#CE3D32;padding:10px"

    tagList(
      tags$p(style="margin-top:50px",
        "Between today and ",x_range_right(),", ",
        " there is a ",
        tags$div(format(input$excessProb,digits=2)," percent chance",style=calloutStyle),
        " demand will exceed supply for ",
        tags$div("at least ",round(excessNDays)," days.",style=calloutStyle)
      )
    )
  })
  
  # Generate a summary of the data
  output$perCustomerUsage <- renderPrint({
    
  })
  
  output$aboutCopy <- renderUI({
    val <- tags$p("Faced with a historic drought and future uncertainty, California water managers need all 
                      the tools they can get to ensure water reliability.  Short term demand forecasting has a long track 
                      record of helping manage peak loads in energy, and this new tool created by ",
                  tags$a(href="http://www.datakind.org/",target="_blank","DataKind"),
                  ", sponsored by ",
                  tags$a(href="http://www.bloomberg.com/",target="_blank","Bloomberg"),
                  ", brings that 
                      approach to help manage recycled water demand peak loads and reduce the need for potable makeup water 
                      for irrigation watering.  That solves a key need for ",
                  tags$a(href="http://www.mnwd.com/",target="_blank","Moulton Niguel Water District"),
                  " in South Orange 
                      County, which aside from recycled water, is 100% reliant on expensive imported water supplies and 
                      serves as a leading example of how improved use of data can ensure water reliability with broader 
                      statewide implications.  With more than a thousand water providers serving some 35 million 
                      Californians, California's water world is three orders of magnitude more decentralized than energy 
                      where 80% of Californians' electricity is supplied by three major utilities.  The ",
                  tags$a(href="http://californiadatacollaborative.com/",target="_blank","California Data Collaborative"),
                  " is a unique water manager led public private partnership bringing together utilities 
                      together across the state to integrate data and deploy tools like this to help water managers ensure 
                      reliability."
    )
  })
  
  
})
