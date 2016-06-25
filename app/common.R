library(forecast)
library(lubridate)
library(data.table)

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
MODEL_LIST <- list(tbats=list(modelfile=file.path(MODEL_DIR,"total_cons_tbats.Rdata"),
                              days_per_step=1),
#                    autoarima=list(modelfile=file.path(MODEL_DIR,"total_cons_autoarima.Rdata"),
#                                   days_per_step=30.25),
#                    ets=list(modelfile=file.path(MODEL_DIR,"total_cons_ets.Rdata"),
#                             days_per_step=30.25),
                   linearmodel=list(modelfile=file.path(MODEL_DIR,"total_cons_linearmodel.Rdata"),
                                    days_per_step=1,
                                    use_weather=F)
)
TRAINING_DATA_START_DATE <- as.Date("2004-07-01")
TRAINING_DATA_END_DATE <- as.Date("2015-12-31")



yearsToDate <- function(x) {
  TRAINING_DATA_START_DATE + dyears(x-1)
}

getTrainingData <- function() {
  df <- fread(PROD_CONS_TIME_SERIES_FILE)
  setnames(df,gsub("[-(\\+)]","",gsub(" ","_",colnames(df))))
  df$Date <- as.Date(df$Date)
  df <- df[!duplicated(df,by='Date'),]
  df
}

getDateNumber <- function(date) {
  #dayIndex <- yday(date)
  #dayOfWeek <- wday(date)
  return(ceiling(yday(date)/7))
}

engineerFeatures <- function(df,use_weather=F) {
  
  newdf <- as.data.table(df)
  
  #df$Date <- as.Date(df$Date)
  newdf$day_of_week <- weekdays(as.Date(newdf$Date))
  newdf$week_of_year <- getDateNumber(newdf$Date)
  
  #newdf <- df[c("Date", "day_of_week", "week_of_year", "Amount_Delivered_mg")]
  #newdf <- data.frame(df$Date, df$day_of_week, df$week_of_year, df$Amount_Delivered_mg)
  
  
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
