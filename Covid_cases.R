setwd("/Users/rctrj/UCD/Winter/BAX 493/Classes/Class2")

install.packages("nortsTest")

# Loading
library("readxl")
library(tseries)
library(nortest)
library(nortsTest)
library(forecast)

library(proto)
library(gsubfn)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(forecast)
library(TTR)

install.packages("ggfortify")

library(readr)
library(ggplot2)
library(ggfortify)
library(vars)
library(forecast)
library(psych)

# xls files
data <- read_excel("Assignment of States.xlsx")

typeof(data)

data <- as.data.frame(data)
State <- data[data$Name == "Harsh, Harsh",]

cases <- read_excel('United_States_COVID-19_Confirmed_Cases.xlsx', sheet = "United_States_COVID-19_Confirme")
cases <- as.data.frame(cases)

head(cases)
colnames(cases)
length(row.names(cases))

montana <- cases[cases$state == "MT",]
head(montana)

montana <- as.data.frame(montana)
montana.index <- NULL
montana <- na.omit(montana)
montana <- montana[order(montana$submission_date),]

montana_ts <- ts(montana[,'conf_cases'], frequency = 365, start = c(2020,4, 19))

### plotting montana ###
plot.ts(montana_ts)


### checking for d value ###
adf.test(montana_ts)

### p value is greater than 0.1 and hence, there is multicolinearity ###

montana_ts_d1 <- diff(montana_ts,differences = 1)
adf.test(montana_ts_d1)

### p value greater than 0.1 and hence, there is multicolinearity ###

montana_ts_d2 <- diff(montana_ts,differences = 2)
adf.test(montana_ts_d2)

### p value much less than 0.1 and hence, we have gotten rid of multicolinearity 
### at d = 2


###### checking for p value #######
pacf(montana_ts_d2, lag.max = 20)

## The 7th lag has significant correlation and hence, we will
## use that p value 

##### checking q value ######

acf(montana_ts_d2, lag.max = 10)

### the third lag is nearly significant and hence, we will use
### q value to be 3 

criterion_df <- data.frame()

for (p in seq(1,7)) {
  for (q in seq(1, 3)) {
   for (d in seq(1, 2)) {
     aic <- Arima(montana_ts, order = c(p,q,d))$aic
     aicc <- Arima(montana_ts, order = c(p,q,d))$aicc
     bic <- Arima(montana_ts, order = c(p,q,d))$bic
     new_data <- c(p, q, d, 0, 0, 0, aic, aicc, bic)  
     criterion_df <- rbind(criterion_df, new_data)
     
     s1 <- Arima(montana_ts, order=c(p,q,d), seasonal = list(order = c(0,0,1), period = 52), method="ML")
     new_data <- c(p, q, d, 0, 0, 1, s1$aic, s1$aicc, s1$bic) 
     criterion_df <- rbind(criterion_df, new_data)
     
     s2 <- Arima(montana_ts, order=c(p,q,d), seasonal = list(order = c(0,1,0), period = 52), method="ML")
     new_data <- c(p, q, d, 0, 1, 0, s2$aic, s2$aicc, s2$bic) 
     criterion_df <- rbind(criterion_df, new_data)
     
     
     print(p, q, d)
   } 
  }
}
colnames(criterion_df) <- c("p", "q", "d", "P", "Q", "D", "AIC", "AICc", "BIC")

dummy <- criterion_df

#### ordering on the basis of AICc ####
dummy <- dummy[order(dummy$AICc),]

### taking the first 5 models based on AICc ###
part.data <- head(dummy, 5)
predict_df <- data.frame()

for (i in seq(1, 5)){
  
  print(i)
  
  p = part.data[i, 1]
  q = part.data[i, 2]
  d = part.data[i, 3]
  P = part.data[i, 4]
  Q = part.data[i, 5]
  D = part.data[i, 6]

  arima.model <- Arima(montana_ts, order=c(p,q,d), seasonal = list(order = c(P,Q,D), period = 52), method="ML")
  predict <- forecast:::forecast.Arima(arima.model, h = 7, level = c(68, 90))
  predict_df <- rbind(predict_df, predict$mean)
}


colnames(predict_df) <- seq(1, 7)
forecast.7days <- apply(predict_df, 2, mean)
