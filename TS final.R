library(readr)
#install.packages("xts")
library(xts)
#install.packages('TSstudio')
library(TSstudio)
#install.packages("forecast")
library(forecast)

df<- read_csv("Desktop/U Chicago/Spring 2023/Time Series/candy_production.csv")
colnames(df)<-c("date", "production")
df$date<-as.Date(df$date, format="%Y-%m-%d")
CandyXTS<- xts(df[-1], df[[1]])
CandyTS <- ts(df$production, start=c(1972,1),end=c(2017,8), frequency=12 )

ts_plot(CandyXTS, title="Candy Production Time Series")

ts_decompose(CandyTS)

ts_seasonal(CandyTS, type = "all")

# Create an ACF plot
Acf(CandyTS)

# ------------------------------------------------------------------------------
# Simple models: Average, Naive, Seasonal Naive
# ------------------------------------------------------------------------------
train<-window(CandyTS, start=c(1972,1), end=c(2009,12))
test<-window(CandyTS, start=c(2010,1))

# Average: forecast will be equal to the average of past data
m_mean<-meanf(train, h=92)

# Naive: forecast will be equal to the last observation
m_naive<-naive(train, h=92)
accuracy(m_naive, test)  

# Seasonal Naive: forecast will be equal to the last observation of same season
m_snaive<-snaive(train, h=92)
accuracy(m_snaive, test)

autoplot(train)+
  autolayer(m_mean, series="Mean", PI=FALSE)+
  autolayer(m_naive, series="Naive", PI=FALSE)+
  autolayer(m_snaive, series="Seasonal Naive", PI=FALSE)+
  xlab('Year')+ylab('Candy Production')+
  ggtitle('Forecasts for Candy Production')+
  guides(colour=guide_legend(title='Forecast'))

# ------------------------------------------------------------------------------
# 3 - Exponential Smoothing Models: SES, Holt, Holt Winters, ETS
# ------------------------------------------------------------------------------
# Simple exponential: smoothing for level
m_ses<-ses(train, h=92)
accuracy(m_ses, test)

# Holt: smoothing for level and for trend
m_holt<-holt(train, h=92)
accuracy(m_holt, test)

# Holt Winters: smoothing for level, trend and seasonality
m_holtw<-hw(train, seasonal="additive", h=92)
accuracy(m_holtw, test)

# ETS: It allows other combinations of trend and seasonal components
m_ets<-forecast(ets(train), h=92)
summary(m_ets)

accuracy(m_ets, test)

autoplot(train)+
  autolayer(m_ses, series="Simple Exponential", PI=FALSE)+
  autolayer(m_holt, series="Holt Method", PI=FALSE)+
  autolayer(m_holtw, series="Holt_Winters", PI=FALSE)+
  autolayer(m_ets, series="ETS", PI=FALSE)+
  xlab('Month/Year')+ylab('Candy Production')+
  ggtitle('Forecasts for Candy Production')+
  guides(colour=guide_legend(title='Forecast'))

# ------------------------------------------------------------------------------
# 4 - ARIMA
# ------------------------------------------------------------------------------
autoarima <- auto.arima(train, allowdrift=F)
autoarima

arima=Arima(train, order=c(4,1,1),
            seasonal=list(order=c(2,1,2), period=12))
arima

arima_f =forecast(arima, h=92)
checkresiduals(arima)

residuals <- residuals(arima)
Acf(residuals)

library(TSA)

p <- periodogram(CandyTS)
p
max_freq <- p$freq[which.max(p$spec)]
seasonality <- 1/max_freq
seasonality


# Identify the indices of the two largest spectral densities
largest_indices <- order(p$spec, decreasing = TRUE)[1:2]

# Retrieve the corresponding frequencies
largest_freqs <- p$freq[largest_indices]

# Select the second largest frequency
second_max_freq <- largest_freqs[2]

# Estimate the seasonality based on the second maximum frequency
seasonality_2 <- 1/second_max_freq

# Print the seasonality
seasonality_2

# 2nd highest seasonality is 12, yearly 

p_resid <- periodogram(residuals)
max_freq_resid <- p_resid$freq[which.max(p_resid$spec)]
seasonality_resid <- 1/max_freq_resid
seasonality_resid

accuracy(arima_f, test)

autoplot(train)+
  autolayer(arima_f, series="ARIMA", PI=FALSE)+
  xlab('Month/Year')+ylab('Candy Production')+
  ggtitle('Forecasts for Candy Production')+
  guides(colour=guide_legend(title='Forecast'))

#################
library(tibble)

# Create forecasts for each model
forecasts <- list(
  Mean = m_mean,
  Naive = m_naive,
  Seasonal_Naive = m_snaive,
  ETS = m_ets,
  Holt = m_holt,
  Holt_Winters = m_holtw,
  Simple_Exponential = m_ses,
  ARIMA = arima_f
)

# Calculate RMSE for each forecast
rmse_values <- sapply(forecasts, function(forecast) {
  accuracy_values <- accuracy(forecast, test)
  rmse <- accuracy_values[2, "RMSE"]
  return(rmse)
})

# Create a table of RMSE values
model_names <- names(forecasts)
rmse_table <- tibble(Model = model_names, RMSE = rmse_values)

# Sort the table by RMSE in ascending order
rmse_table <- rmse_table[order(rmse_table$RMSE), ]

rmse_table

mape_values <- sapply(forecasts, function(forecast) {
  accuracy_values <- accuracy(forecast, test)
  mape <- accuracy_values[2, "MAPE"]
  return(mape)
})

# Create a table of RMSE values
model_names <- names(forecasts)
mape_table <- tibble(Model = model_names, MAPE = mape_values)

# Sort the table by RMSE in ascending order
mape_table <- mape_table[order(mape_table$MAPE), ]

mape_table

library(dplyr)
library(knitr)

# Combine RMSE and MAPE tables
combined_table <- left_join(rmse_table, mape_table, by = "Model")

# Format the combined table using kable
combined_table

