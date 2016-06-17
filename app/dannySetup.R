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
  
  return(newdf)
  
}

buildLinearModel <- function(df) {
    newdf <- engineerFeatures(df)

    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday) + factor(day_of_week) + TemperatureAvgF + PrecipitationSumIn, data=newdf)
    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday) + factor(day_of_week), data=newdf)
    model <- glm(Amount_Delivered_mg ~ factor(day_of_week), data=newdf)
    #model <- glm(Amount_Delivered_mg ~ factor(number_of_weekday), data=newdf)

    return(model)
}

scoreLinearModel <- function(df) {
  dateCutoff <- "2010-12-31"
  
  trainingdf <- df[df$Date <= as.Date(dateCutoff)]
  model <- buildLinearModel(df)
  
  testingdf <- engineerFeatures(df[df$Date > as.Date(dateCutoff)])

  actual <- testingdf$Amount_Delivered_mg
  predicted <- predict(model, testingdf)
  
  rmse <- sqrt(sum((actual - predicted)^2))
  
  return(rmse)
  
}