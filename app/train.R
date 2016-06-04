library(data.table)
library(ggplot2)
library(xts)
library(forecast)

source("app/common.R")

# get the data
df <- fread(PROD_CONS_TIME_SERIES_FILE)
setnames(df,gsub("[-(\\+)]","",gsub(" ","_",colnames(df))))
df$Date <- as.Date(df$Date)
df <- df[!duplicated(df,by='Date'),]

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
model <- ets(water.ts.reg$water.ts.Close)
save(model,file=MODEL_LIST$ets$modelfile)


#
# auto arima
#
model <- auto.arima(water.ts.reg$water.ts.Close)
save(model,file=MODEL_LIST$autoarima$modelfile)

pdf(file.path(MODEL_DIR,"autoarima_fit.pdf"))
plot(forecast(model,5*12),xlab="Year number",ylab="Amount Delivered (mg)")
dev.off()


#
# multi seasonality TBATS
#
model <- tbats(water.ts.reg2)
save(model,file=MODEL_LIST$tbats$modelfile)

pdf(file.path(MODEL_DIR,"tbats_fit.pdf"))
plot(model)
dev.off()

pdf(file.path(MODEL_DIR,"tbats_forecastt.pdf"))
plot(forecast(model,5*365),xlab="Year number",ylab="Amount Delivered (mg)")
dev.off()
