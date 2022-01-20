setwd('/Users/rctrj/Downloads/time_series')

install.packages('ggfortify')
install.packages('fpp2')
install.packages('GGally')
install.packages('ts_plot')

library(ts_plot)
library(ggplot2)
library(ggfortify)
library(fpp2)
library(forecast)
library(GGally)



#### reading the file ####

data <- read.csv('DailyDelhiClimateTrain.csv')
df <- ts(data, start=c(2013,1,1), frequency=365)

data1 <- read.csv('DailyDelhiClimateTest.csv')
df1 <- ts(data1, start=c(2017,1,1), frequency=365)


colnames(df)

autoplot(df[,"humidity"]) +
  ggtitle("Humidity") + xlab("Year") +
  ylab("Humidity")

colnames(df)



colnames(df)
autoplot(df[,"humidity"], xlab = "Year", ylab = "Number of Newly Joined Members")


###### decomposing data into trend and season #######

fit <- tslm(humidity ~ trend + season, data = df)
summary(fit)

#### plot this #####
df %>% as.data.frame() %>% GGally::ggpairs()


#### naive, seasonal naive, mean method
autoplot(df[,'humidity']) +
  autolayer(meanf(df[,'humidity'], h=365), series="Mean", PI=FALSE) +
  autolayer(naive(df[,'humidity'], h=365), series="Naïve", PI=FALSE) +
  autolayer(rwf(df[,'humidity'], drift=TRUE, h=365), series="Drift", PI=FALSE) +
  autolayer(snaive(df[,'humidity'], h=365), series="Seasonal naïve", PI=FALSE) +
  autolayer(df1[,'humidity'], series="Actual", PI=FALSE) +
  ggtitle("Comparison of actual humidity with humidity forecast") + xlab("Year") + 
  ylab("Humidity") + guides(colour=guide_legend(title="Forecast"))



#### bias adjusted and back-transformed ####

fc <- rwf(df[,'humidity'], drift=TRUE, lambda=0, h=50, level=80) 
fc2 <- rwf(df[,'humidity'], drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE) 
autoplot(df[,'humidity']) +
  autolayer(fc, series="Simple back transformation") + 
  autolayer(fc2, series="Bias adjusted", PI=FALSE) + 
  guides(colour=guide_legend(title="Forecast"))

#### season and trend on tslm ####

ts.model <- tslm(df[,'humidity'] ~ trend + season)
checkresiduals(ts.model)

forecast(ts.model, h = 365)

plot(forecast(ts.model, h = 365))

autoplot(df[,'humidity']) +
  autolayer(forecast(ts.model, h = 100), series = "Linear Regression") +
  autolayer(df1[,'humidity'], series="Actual", PI=FALSE) +
  ggtitle("Comparison of actual humidity with humidity forecast") + xlab("Year") + 
  ylab("Megalitres") + guides(colour=guide_legend(title="Forecast"))


checkresiduals(ts.model)


##### simple exponential smoothning method ######

fc <- ses(df[,'humidity'], h=100)
# Accuracy of one-step-ahead training errors round(accuracy(fc),2)
round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted", PI=FALSE) + 
  autolayer(df[,'humidity'], series="Actual Data", PI=FALSE) +
  ylab("Humidity") + xlab("Year")



#### holt's method #####


fc2 <- holt(df[,'humidity'], h=100)
autoplot(fc) +
  autolayer(fitted(fc), series="Forecast") + 
  autolayer(df[,'humidity'], series="Train Data", PI=FALSE) +
  autolayer(df1[,'humidity'], series="Test Data", PI=FALSE) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

####### yahoo stock prices #####

data_week <- read.csv('data_week.csv')
week <- ts(data_week, start=c(2015), frequency=52.25)
xx = ts(temp, frequency = 52, start = c(2015,1))
plot(xx) + ylabel('Temperature')

#### damped holt's method #####

fc2 <- holt(xx, h=100)
fc <- holt(xx, damped=TRUE, phi = 0.8, h=100)
fit1 <- HoltWinters(xx, gamma = TRUE, seasonal="additive")
fit2 <- HoltWinters(xx, gamma = TRUE, seasonal="multiplicative")
fit3 <- HoltWinters(xx,beta = TRUE, gamma = TRUE)


autoplot(xx) +
  autolayer(forecast(fc), series="Holt Damped", PI=FALSE) + 
  autolayer(forecast(fc2), series="Holt", PI=FALSE) +
  autolayer(forecast(fit1), series="Holt-Winter's (additive)", PI = FALSE) +
  autolayer(forecast(fit2), series="Holt-Winter's (Multiplicative)", PI = FALSE) +
  ylab("Temperature") + xlab("Year") + 
  guides(colour=guide_legend(title="Forecast")) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") 


autoplot(gold_train[,'United.States.USD.']) + autolayer(fit2, series="Holt-Winter's (multiplicative)", PI = FALSE)


