# This is a helper file for a simple linear regression model
# More features can be added as needed
# We include the day of the week, and the number of the day, i.e.
# the 3rd Saturday would be labeled with 3 and "Saturday"
# We also include weather, and previous day's demand data (not yet)


library(shiny)
library(data.table)
library(ggplot2)
library(xts)
library(forecast)
library(weatherData)
library(gbm)


buildLinearModel <- function(df,use_weather=F) {
  newdf <- engineerFeatures(df,use_weather=use_weather)
  
  if (use_weather) {
    model <- lm(Amount_Delivered_mg ~ factor(week_of_year) + factor(day_of_week) + TemperatureAvgF + PrecipitationSumIn, data=newdf)
  } else {
    model <- lm(Amount_Delivered_mg ~ factor(week_of_year) + factor(day_of_week), data=newdf)
  # model <- lm(Amount_Delivered_mg ~ factor(day_of_week), data=newdf)
  #model <- lm(Amount_Delivered_mg ~ factor(week_of_year), data=newdf)
  }
    
  return(model)
}


buildGBM <- function(df,use_weather=F) {
  newdf <- engineerFeatures(df,use_weather=use_weather)
  
  if (use_weather) {
    model <- gbm(Amount_Delivered_mg ~ factor(week_of_year) + factor(day_of_week) + TemperatureAvgF + PrecipitationSumIn, data=newdf)
  } else {
    model <- gbm(Amount_Delivered_mg ~ factor(week_of_year) + factor(day_of_week), 
                 data=newdf,
                 distribution='gaussian',
                 n.trees=MODEL_LIST['gbm'][[1]]$n.trees,
                 interaction.depth=MODEL_LIST['gbm'][[1]]$interaction.depth,
                 shrinkage=MODEL_LIST['gbm'][[1]]$shrinkage)
    # model <- lm(Amount_Delivered_mg ~ factor(day_of_week), data=newdf)
    #model <- lm(Amount_Delivered_mg ~ factor(week_of_year), data=newdf)
  }
  
  return(model)
}

scoreLinearModel <- function(df) {
  dateCutoff <- "2010-12-31"
  
  #trainingdf <- df[df$Date <= as.Date(dateCutoff)]
  model <- buildLinearModel(df, dateCutoff)
  
  testingdf <- engineerFeatures(df[df$Date > as.Date(dateCutoff),])

  actual <- testingdf$Amount_Delivered_mg
  predicted <- predict(model, testingdf)
  
  rmse <- sqrt(mean((actual - prediction)^2, na.rm = T))
  
  return(rmse)
  
}


justInTimePrediction <- function(df) {
  testingStart <- as.Date("2011-01-01") #pick not too close to beginning of dataset
  testingEnd <- as.Date("2015-12-31")
  predictionPeriod <- 10
  
  newdf <- engineerFeatures(df) #add weather and time features
  
  #prediction <- rep(NA, testingEnd - testingStart + 1)
  numDays <- testingEnd - testingStart + 1
  prediction <- data.frame(rep(NA, numDays), row.names = as.Date("2011-01-01") + (0:(numDays-1)))
  colnames(prediction) <- "InTimePrediction"
  currDate <- testingStart
  #currIndex <- 1
  
  while (currDate < as.Date(testingEnd)) {
    trainingdf <- newdf[newdf$Date < currDate,]
    model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday) + factor(day_of_week) +
                                    TemperatureAvgF + PrecipitationSumIn + previousWeekAvg +
                                    previousMonthAvg, data=trainingdf)
    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday) + factor(day_of_week) +
    #                                TemperatureAvgF + PrecipitationSumIn, data=trainingdf)
    
    preddf <- newdf[newdf$Date >= currDate & newdf$Date < (currDate + predictionPeriod),]
    rownames(preddf) <- preddf$Date
    miniprediction <- data.frame(predict(model, preddf))
    colnames(miniprediction) <- "InTimePrediction"
    
    for (date in rownames(miniprediction)) {
      prediction[date,"InTimePrediction"] <- miniprediction[date,"InTimePrediction"]
    }
    
#    for (i in 1:predictionPeriod) {
#      if (currIndex <= length(prediction)) {
#        prediction[currIndex] <- miniprediction[i]
#       if (is.na(miniprediction[i])) {print(miniprediction)}
#       if (is.na(miniprediction[i])) {print(currDate)}
#       #if (sum(is.na(miniprediction))) {print(currIndex)}
#        currIndex <- currIndex + 1
#      }
#    }
    
    currDate <- currDate + predictionPeriod # probably do this in a for loop instead
  }
  
  return(prediction)
  
}


scoreJustInTimePrediction <- function(df) {
  testingStart <- as.Date("2011-01-01") #pick not too close to beginning of dataset
  testingEnd <- as.Date("2015-12-31")
  
  prediction <- justInTimePrediction(df)
  
  reldf <- df[df$Date >= testingStart & df$Date <= testingEnd,]
  actual <- reldf$Amount_Delivered_mg
  
}



