library(data.table)
library(ggplot2)
library(xts)
library(forecast)

source("app/common.R")
source("app/dannySetup.R")

trainList <- c('linearmodel','tbats','gbm')

# get the data
df <- getTrainingData()

# create time series
# water.ts <- ts(df$Amount_Delivered_mg)
water.ts <- xts(df$Amount_Delivered_mg,df$Date)
# water.ts <- zoo(df$Amount_Delivered_mg,df$Date)

pdf(file.path(MODEL_DIR,"amount_delivered_ts.pdf"))
plot(water.ts,xlab="Date",ylab="Amount Delivered (mg)")
dev.off()

water.ts.reg <- to.monthly(water.ts)
# water.ts.reg <- to.daily(water.ts)

water.ts.reg2 <- msts(water.ts,seasonal.periods=c(7,30.42,365.25))


#
#
#
# fit <- stl(water.ts, s.window="period")


#
# Holt Winters
#
# fit <- HoltWinters(water.ts.reg$water.ts.Close, beta=FALSE, gamma=FALSE)

#
# Exponential smoothing state space model
#
if ("ets" %in% trainList) {
  model <- ets(water.ts.reg$water.ts.Close)
  save(model,file=MODEL_LIST$ets$modelfile)
}

#
# auto arima
#
if ("autoarima" %in% trainList) {
  model <- auto.arima(water.ts.reg$water.ts.Close)
  save(model,file=MODEL_LIST$autoarima$modelfile)
  
  pdf(file.path(MODEL_DIR,"autoarima_fit.pdf"))
  plot(forecast(model,5*12),xlab="Year number",ylab="Amount Delivered (mg)")
  dev.off()
}

#
# multi seasonality TBATS
#
if ("tbats" %in% trainList) {
  model <- tbats(water.ts.reg2)
  save(model,file=MODEL_LIST$tbats$modelfile)
  
  pdf(file.path(MODEL_DIR,"tbats_fit.pdf"))
  plot(model)
  dev.off()
  
  pdf(file.path(MODEL_DIR,"tbats_forecastt.pdf"))
  plot(forecast(model,5*365),xlab="Year number",ylab="Amount Delivered (mg)")
  dev.off()
}

#
# GLM for day of year and weather
#
if ("linearmodel" %in% trainList) {
  model <- buildLinearModel(df,use_weather=MODEL_LIST['linearmodel'][[1]]$use_weather)
  save(model,file=MODEL_LIST$linearmodel$modelfile)
  
  # sqrt(mean((predict(model,newdf)-df$Amount_Delivered_mg)^2))
}


#
# GBM for day of year and weather
#
if ("gbm" %in% trainList) {
  model <- buildGBM(df,use_weather=MODEL_LIST['gbm'][[1]]$use_weather)
  save(model,file=MODEL_LIST$gbm$modelfile)
  
  # sqrt(mean((predict(model,newdf)-df$Amount_Delivered_mg)^2))
}





