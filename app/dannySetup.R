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



getDateNumber <- function(date) {
    #dayIndex <- yday(date)
    #dayOfWeek <- wday(date)
    return(ceiling(yday(date)/7))
}

engineerFeatures <- function(df) {
  #df$Date <- as.Date(df$Date)
  df$day_of_week <- weekdays(as.Date(df$Date))
  df$number_of_weekday <- getDateNumber(df$Date)
  
  #newdf <- df[c("Date", "day_of_week", "number_of_weekday", "Amount_Delivered_mg")]
  #newdf <- data.frame(df$Date, df$day_of_week, df$number_of_weekday, df$Amount_Delivered_mg)
  
  stationId <- "KCALAGUN9"
  start <- "2006-06-26" # first available
  weatherPredictor <- getSummarizedWeather(stationId,
                                           station_type = "id",
                                           start_date = start,
                                           end_date = "2015-12-31", # do something reactive here (?)
                                           opt_all_columns = TRUE
                                           #opt_custom_columns = TRUE,
                                           #custom_columns = c(2,3,4,16) #mean temp and precip -- maybe add humidity later
  )
  
  weatherPredictor$Date <- as.Date(weatherPredictor$Date)
  row.names(weatherPredictor) <- weatherPredictor$Date
  
  newdf <- merge(as.data.frame(df), as.data.frame(weatherPredictor), by = "Date")
  
  #add previous week's and previous month's average
  newdf[,"previousWeekAvg"] <- NA
  newdf[,"previousMonthAvg"] <- NA
  
  # very slow loop
  for (rownum in 1:nrow(newdf)) {
    currDate <- as.Date(newdf[rownum,"Date"])
    dayOfWeek <- wday(currDate)
    endOfPrevWeek <- currDate - dayOfWeek
    startOfPrevWeek <- endOfPrevWeek - 6
    
    # go into df so that you get the averages for the first week
    reldf <- df[df$Date >= startOfPrevWeek & df$Date <= endOfPrevWeek,] # ASK WILL WHY I NEED COMMA HERE AND NOT LATER
    prevWeekAvg <- mean(reldf$Amount_Delivered_mg)
    
    newdf[rownum,"previousWeekAvg"] <- prevWeekAvg
    
    
    dayOfMonth <- mday(currDate)
    endOfPrevMonth <- currDate - dayOfMonth
    startOfPrevMonth <- endOfPrevMonth - mday(endOfPrevMonth) + 1
    
    reldf <- df[df$Date >= startOfPrevMonth & df$Date <= endOfPrevMonth,]
    prevMonthAvg <- mean(reldf$Amount_Delivered_mg)
    
    newdf[rownum,"previousMonthAvg"] <- prevMonthAvg
    
  }
  
  return(newdf)
  
}

buildLinearModel <- function(df, endDate) {
    
    newdf <- engineerFeatures(df[df$Date <= as.Date(endDate),])

    model <- glm(Amount_Delivered_mg ~ TemperatureAvgF + PrecipitationSumIn + HumidityAvg, data=newdf)
    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday) + factor(day_of_week) + TemperatureAvgF + PrecipitationSumIn + HumidityAvg, data=newdf)
    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday) + factor(day_of_week), data=newdf)
    #model <- glm(Amount_Delivered_mg ~ factor(day_of_week), data=newdf)
    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday), data=newdf)

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



