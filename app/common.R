library(forecast)
library(lubridate)
library(data.table)
library(gbm)
library(weatherData)
library(shiny)

# get environment
ENV <- Sys.getenv("MNWD_ENV")

# directories
DATA_DIR <- Sys.getenv("MNWD_DATA_DIR")
if (ENV == "prod") {
  MODEL_DIR <- file.path(DATA_DIR,"models")
} else if (ENV == "test") {
  MODEL_DIR <- file.path(DATA_DIR,"models",Sys.info()[['login']])
} else {
  stop(paste0("Don't recognize MNWD_ENV ",ENV))
}
dir.create(file.path(MODEL_DIR))

# input files
PROD_CONS_TIME_SERIES_FILE <- file.path(DATA_DIR,'MNWD_RW_Production_and_Consumption_Time_Series.csv')

# model list specification
MODEL_LIST <- list(tbats=list(shortname='tbats',
                              name='TBATS pure time series',
                              modelfile=file.path(MODEL_DIR,"total_cons_tbats.Rdata"),
                              days_per_step=1),
                   linearmodel=list(shortname='linearmodel',
                                    name='Linear regression',
                                    modelfile=file.path(MODEL_DIR,"total_cons_linearmodel.Rdata"),
                                    days_per_step=1,
                                    use_weather=F,
                                    use_prev_week_average=F),
                   gbm=list(shortname='gbm',
                            name='Gradient boosted decision tree regression',
                            modelfile=file.path(MODEL_DIR,"total_cons_gbm.Rdata"),
                            days_per_step=1,
                            n.trees=1e4,
                            interaction.depth=2,
                            shrinkage=1e-3,
                            use_weather=F,
                            use_prev_week_average=F)
)
TRAINING_DATA_START_DATE <- as.Date("2004-07-01")
# TRAINING_DATA_END_DATE <- as.Date("2015-12-31")
TRAINING_DATA_END_DATE <- as.Date("2016-05-01") # bad data from mid May



yearsToDate <- function(x) {
  TRAINING_DATA_START_DATE + dyears(x-1)
}

getTrainingData <- function() {
  df <- fread(PROD_CONS_TIME_SERIES_FILE)
  setnames(df,gsub("[-(\\+)]","",gsub(" ","_",colnames(df))))
  df$Date <- as.Date(df$Date)
  df <- df[!duplicated(df,by='Date'),]
  df[Date >= TRAINING_DATA_START_DATE & Date <= TRAINING_DATA_END_DATE,]
}

getDateNumber <- function(date) {
  #dayIndex <- yday(date)
  #dayOfWeek <- wday(date)
  return(ceiling(yday(date)/7))
}

engineerFeatures <- function(df,use_weather=F,use_prev_week_average=F) {
  
  newdf <- as.data.frame(df)
  
  #df$Date <- as.Date(df$Date)
  newdf$day_of_week <- weekdays(as.Date(newdf$Date))
  newdf$week_of_year <- getDateNumber(newdf$Date)
  
  #newdf <- df[c("Date", "day_of_week", "week_of_year", "Amount_Delivered_mg")]
  #newdf <- data.frame(df$Date, df$day_of_week, df$week_of_year, df$Amount_Delivered_mg)
  
  #add previous week's and previous month's average
  newdf[,"previousWeekAvg"] <- NA
  newdf[,"previousMonthAvg"] <- NA
  
  if (use_prev_week_average) {
    print("using previous week's average")
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
  } else {
    print("not using previous week's average")
  }
  
  if (use_weather) {
    print("using weather")
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
    
    newdf <- merge(newdf, as.data.frame(weatherPredictor), by = "Date")
  } else {
    print("not using weather")
  }
  
  return(newdf)
  
}

aboutCopy <- tags$p("Faced with a historic drought and future uncertainty, California water managers need all 
the tools they can get to ensure water reliability.  Short term demand forecasting has a long track 
record of helping manage peak loads in energy, and this new tool created by ",
tags$a(href="http://www.datakind.org/",target="_blank","DataKind"),
" brings that 
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
