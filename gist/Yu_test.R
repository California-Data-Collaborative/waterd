library(ggplot2)
library(reshape2)
library(plyr)
library(sqldf)
library(forecast)


sqldf("select AVG(usage_ccf), cust_loc_class_from_utility, COUNT(distinct(cust_id_from_utility)) from rw_with_et group by cust_loc_class_from_utility")
##1. because the most accounts are RC1, I will only focus on RC1

RC1<-sqldf(" select * from rw_with_et where cust_loc_class_from_utility='RC1'")
RC1$Month<-sprintf("%02d", RC1$usage_month)
RC1$Date<-paste(RC1$usage_year,RC1$Month,"01",sep="-")
RC1$Date<-as.Date(RC1$Date,"%Y-%m-%d")
ggplot(data = RC1, aes(x = Date, y = usage_ccf, group = cust_id_from_utility))+geom_line()+ggtitle("usage_ccf spaghetti plot")
ggplot(data = RC1, aes(x = usage_month, y = usage_ccf, group = cust_id_from_utility))+geom_line()+ggtitle("usage_ccf spaghetti plot")

##2.usage is specific to month of year, therefore we should make monthly useage predictions
##2.1 make usage predictions for 58827
RC1_58827<-sqldf(" select * from RC1 where cust_id_from_utility='58827'")
boxplot(usage_ccf~usage_month,data=RC1_58827)
myts_usage<-ts(RC1_58827$usage_ccf,RC1_58827$usage_month)
plot(myts_usage)
fit <- HoltWinters(myts_usage, gamma=FALSE)
plot(forecast(fit, 3))

myts_budget<-ts(RC1_58827$usage_outdoor_budget_ccf,RC1_58827$usage_month)
lines(myts_budget, col="red")