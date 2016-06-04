library(data.table)
library(ggplot2)
library(xts)
library(forecast)

df <- fread('~/data/datakind_water/MNWD_RW_Production_and_Consumption_Time_Series.csv')

setnames(df,gsub("[-(\\+)]","",gsub(" ","_",colnames(df))))
df$Date <- as.Date(df$Date)
df <- df[!duplicated(df,by='Date'),]

# water.ts <- ts(df$Amount_Delivered_mg)
water.ts <- xts(df$Amount_Delivered_mg,df$Date)
# water.ts <- zoo(df$Amount_Delivered_mg,df$Date)

plot(water.ts,xlab="Date",ylab="Amount Delivered (mg)")

water.ts.reg <- to.monthly(water.ts)
# water.ts.reg <- to.daily(water.ts)

# fit <- stl(water.ts, s.window="period")

# fit <- HoltWinters(water.ts.reg$water.ts.Close, beta=FALSE, gamma=FALSE)
fit <- ets(water.ts.reg$water.ts.Close)
fit <- auto.arima(water.ts.reg$water.ts.Close)

plot(forecast(fit,5*12),xlab="Year number",ylab="Amount Delivered (mg)")

# multi seasonality 
water.ts.reg2 <- msts(water.ts,seasonal.periods=c(7,30.42,365.25))
tbats.fit <- tbats(water.ts.reg2)
plot(tbats.fit)
plot(forecast(tbats.fit,5*365),xlab="Year number",ylab="Amount Delivered (mg)")
