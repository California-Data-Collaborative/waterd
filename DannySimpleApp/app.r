library(shiny)
library(data.table)
library(ggplot2)
library(xts)
library(forecast)
library(weatherData)

ui <- fluidPage(
  dateInput("date", label = h3("Forecast For"), value = "2015-01-01", min = "2015-01-01", max = "2015-12-31"),
  textOutput("predicted"),
  textOutput("actual"),
  plotOutput("plot1", click = "plot_click")
)

server <- function(input, output) {
  
  df <- fread('MNWD_RW_Production_and_Consumption_Time_Series.csv')
  
  setnames(df,gsub("[()]","",gsub(" ","_",colnames(df))))
  df$Date <- as.Date(df$Date)
  df <- df[!duplicated(df,by='Date'),]
  
  water.ts <- xts(as.data.frame(df)[c("Date","Amount_Delivered_mg")], order.by = df$Date)
  #water.ts <- xts(as.data.frame(df)["Amount_Delivered_mg"], order.by = df$Date)
  #water.ts.smoothed <- filter(water.ts, filter = c(1/16, 1/16, 1/8, 1/4, 1/8, 1/16, 1/16), sides=2)
  #water.ts.weekly <- to.weekly(water.ts)
  #water.ts.reg <- to.monthly(water.ts)
  
  
  # plot(water.ts,xlab="Date",ylab="Amount Delivered (mg)")
  
  # fit <- HoltWinters(water.ts.reg)
  # forecast <- predict(hw, n.ahead = 12, prediction.interval = 1, level = 0.95)
  
  # predictor <- window(water.ts,  end = "2014-12-31")
  
  stupidPredictor <- function(date) {
    splitDate <- unlist(strsplit(date, split = "-"))
    month <- splitDate[2]
    day <- splitDate[3]
    prediction <- 0
    counter <- 0
    for (i in 2004:2014) {
      currDate <- paste(i, month, day, sep = "-")
      if (as.Date(currDate) %in% index(water.ts)) {
        prediction <- prediction + as.double(water.ts[currDate]$Amount_Delivered_mg)
        counter <- counter + 1
      }
    }
    return(prediction/counter)
  }
  
  stationId <- "KCALAGUN9"
  start <- "2006-06-26" # first available
  weatherPredictor <- getSummarizedWeather(stationId,
                                           station_type = "id",
                                           start_date = start,
                                           end_date = "2014-12-31",
                                           opt_custom_columns = TRUE,
                                           custom_columns = c(2,3,4,16) #mean temp and precip
                                           )
  row.names(weatherPredictor) <- weatherPredictor$Date
  
  weatherTrain.df <- as.data.frame(weatherPredictor)
  waterTrain.df <- as.data.frame(water.ts)
  
  waterForTraining.ts <- window(water.ts, start = start, end = "2014-12-31")
  
  
  #predictWithWeather <- function(date) {
    
    
  #}
  
  output$predicted <- renderText(paste("Predicted demand value using simple averaging is", stupidPredictor(as.character(input$date))))
  output$actual <- renderText(paste("Actual demand value is", water.ts[input$date]$Amount_Delivered_mg))
  
  
  
}

shinyApp(ui = ui, server = server)