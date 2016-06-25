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