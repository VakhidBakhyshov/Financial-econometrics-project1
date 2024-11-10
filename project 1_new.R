library(forecast)
library(fracdiff)
library(tseries)
library(rusquant)
library(quantmod)
library(readr)
library(dplyr)
library(fpp)
library(fracdiff)

trades_pepe <- read_csv("C:/Users/HONOR/Downloads/financial econometrics/trades_1000pepeusdt.csv")
#trades_pepe
#View(trades_pepe)
trades_doge <- read_csv("C:/Users/HONOR/Downloads/financial econometrics/trades_dogeusdt.csv")
#trades_doge
#View(trades_doge)

#install.packages("lubridate")
library(lubridate)

create_candles <- function(T = 10, trades,
                           day = FALSE,
                           hour = FALSE,
                           min = FALSE,
                           sec = FALSE) {
  
  # Constants for time conversions
  ms_per_day <- 86400000
  ms_per_hour <- 3600000
  ms_per_min <- 60000
  ms_per_sec <- 1000
  
  # Convert T to milliseconds based on specified time unit
  if (day) {
    T <- T * ms_per_day
  } else if (hour) {
    T <- T * ms_per_hour
  } else if (min) {
    T <- T * ms_per_min
  } else if (sec) {
    T <- T * ms_per_sec
  }
  
  # Calculate the starting timestamp for grouping
  min_time <- trades$local_timestamp[1]
  
  # Create a grouping variable based on time window
  trades$group <- (trades$local_timestamp - min_time) %/% (1000 * T)
  
  # Aggregate data for each group to create candles
  candles <- trades %>%
    group_by(group) %>%
    summarize(
      time = first(local_timestamp),
      open = first(price),
      high = max(price),
      low = min(price),
      close = last(price),
    )
  
  # Aggregate buy and sell data separately
  cand1 <- trades %>%
    filter(side == "buy") %>%
    group_by(group) %>%
    summarize(
      buy_mean = mean(price),
      buy_volume = sum(amount)
    )
  
  cand2 <- trades %>%
    filter(side == "sell") %>%
    group_by(group) %>%
    summarize(
      sell_mean = mean(price),
      sell_volume = sum(amount)
    )
  
  # Merge aggregated data with candles
  candles <- left_join(candles, cand1, by = "group") %>%
    left_join(cand2, by = "group")
  
  # Replace missing values with 0
  candles[is.na(candles)] <- 0
  
  # Convert timestamps and extract hour
  candles$time <- as.POSIXct(candles$time, origin = "1970-01-01", tz = "UTC")
  candles$hour <- hour(candles$time)
  
  # Add additional columns
  candles$mean_buy <- cand1$buy_mean
  candles$mean_sell <- cand2$sell_mean
  candles$buy_volume <- cand1$buy_volume
  candles$`sell_volume` <- cand2$sell_volume
  candles$total_volume <- candles$buy_volume + candles$sell_volume
  
  return(candles)
}

lol <- create_candles(1, trades_pepe, hour = TRUE)
lol
lol1 <- create_candles(1, trades_pepe, min = TRUE)
lol1
lol2 <- create_candles(1, trades_pepe, sec = TRUE)
lol2

lol$total_volume

lol1$total_volume

lol2$total_volume

# Normalize volume data using MinMax scaling
lol$volume_normalized <- (lol$total_volume - min(lol$total_volume)) / 
  (max(lol$total_volume) - min(lol$total_volume))
lol$volume_normalized

lol1$volume_normalized <- (lol1$total_volume - min(lol1$total_volume)) / 
  (max(lol1$total_volume) - min(lol1$total_volume))
lol1$volume_normalized

lol2$volume_normalized <- (lol2$total_volume - min(lol2$total_volume)) / 
  (max(lol2$total_volume) - min(lol2$total_volume))
lol2$volume_normalized

#Cross-Validation
maxHorizon = 5
testLast = 50

errors_autoARIMA <- matrix(nrow = maxHorizon, 
                           ncol = testLast)
errors_autoARIMA -> errors_ARIMA111
#errors_autoARIMA -> errors_ARFIMA111
errors_autoARIMA -> errors_ETS
errors_autoARIMA -> errors_Naive

for (k in 1:testLast){
  tmpData <- ts(lol1$volume_normalized[1:(nrow(lol1) - k - maxHorizon + 1)])
  m_autoARIMA <- auto.arima(tmpData, allowdrift = F)
  m_arima111 <- arima(tmpData, order = c(1,1,1))
  #m_arfima111 <- arfima(tmpData, nar = 1, nma = 1, d = 1, fit = TRUE)
  m_ETS <- ets(tmpData, model = "ZZZ", damped = TRUE)
  m_naive <- naive(tmpData)
  
  fact <- ts(lol1$volume_normalized[(nrow(lol1) - k - maxHorizon + 2):(nrow(lol1) - k + 1)])
  
  errors_autoARIMA[,k] <- c(fact) - c(forecast(m_autoARIMA, h = 5)$mean)
  errors_ARIMA111[,k] <- c(fact) - c(forecast(m_arima111, h = 5)$mean)
  #errors_ARFIMA111[,k] <- c(fact) - c(forecast(m_arfima111, h = 5)$mean)
  errors_ETS[,k] <- c(fact) - c(forecast(m_ETS, h = 5)$mean)
  errors_Naive[,k] <- c(fact) - c(forecast(m_naive, h = 5)$mean)
  print(k)
}

plot(forecast(m_autoARIMA, h = 50))
plot(forecast(m_ETS, h = 50))
plot(forecast(m_naive, h = 50))

apply(errors_ARIMA111, 1, function(x) mean(abs(x)))
#apply(errors_ARFIMA111, 1, function(x) mean(abs(x)))
rowMeans(abs(errors_ARIMA111))
#rowMeans(abs(errors_ARFIMA111))

apply(errors_ARIMA111, 1, function(x) sqrt(mean(x^2)))
#apply(errors_ARFIMA111, 1, function(x) sqrt(mean(x^2)))

rowMeans(abs(errors_autoARIMA))
rowMeans(abs(errors_ARIMA111))
#rowMeans(abs(errors_ARFIMA111))
rowMeans(abs(errors_ETS))
rowMeans(abs(errors_Naive))

# MASE
rowMeans(abs(errors_ETS)) / rowMeans(abs(errors_Naive))

mape_autoARIMA <- rowMeans(abs(errors_autoARIMA) / c(fact))
mape_ARIMA111 <- rowMeans(abs(errors_ARIMA111) / c(fact))
mape_ETS <- rowMeans(abs(errors_ETS) / c(fact))
mape_Naive <- rowMeans(abs(errors_Naive) / c(fact))

# Calculate R-squared (R^2) for each model
# r_squared_autoARIMA <- 1 - (sum(errors_autoARIMA^2) / sum((c(fact) - mean(c(fact)))^2))
# r_squared_ARIMA111 <- 1 - (sum(errors_ARIMA111^2) / sum((c(fact) - mean(c(fact)))^2))
# r_squared_ETS <- 1 - (sum(errors_ETS^2) / sum((c(fact) - mean(c(fact)))^2))
# r_squared_Naive <- 1 - (sum(errors_Naive^2) / sum((c(fact) - mean(c(fact)))^2))

# Print R-squared results
# print(paste("AutoARIMA R-squared:", r_squared_autoARIMA))
# print(paste("ARIMA(1,1,1) R-squared:", r_squared_ARIMA111))
# print(paste("ETS R-squared:", r_squared_ETS))
# print(paste("Naive R-squared:", r_squared_Naive))


# Calculate MAE, MSE, and RMSE
mae_autoARIMA <- rowMeans(abs(errors_autoARIMA))
mae_ARIMA111 <- rowMeans(abs(errors_ARIMA111))
mae_ETS <- rowMeans(abs(errors_ETS))
mae_Naive <- rowMeans(abs(errors_Naive))

mse_autoARIMA <- rowMeans(errors_autoARIMA^2)
mse_ARIMA111 <- rowMeans(errors_ARIMA111^2)
mse_ETS <- rowMeans(errors_ETS^2)
mse_Naive <- rowMeans(errors_Naive^2)

rmse_autoARIMA <- sqrt(mse_autoARIMA)
rmse_ARIMA111 <- sqrt(mse_ARIMA111)
rmse_ETS <- sqrt(mse_ETS)
rmse_Naive <- sqrt(mse_Naive)

# MASE
rowMeans(abs(errors_ETS)) / rowMeans(abs(errors_Naive))

# MAPE results
print(paste("AutoARIMA MAPE:", mean(mape_autoARIMA)))
print(paste("ARIMA(1,1,1) MAPE:", mean(mape_ARIMA111)))
print(paste("ETS MAPE:", mean(mape_ETS)))
print(paste("Naive MAPE:", mean(mape_Naive)))

# MAE results
print(paste("AutoARIMA MAE:", mean(mae_autoARIMA)))
print(paste("ARIMA(1,1,1) MAE:", mean(mae_ARIMA111)))
print(paste("ETS MAE:", mean(mae_ETS)))
print(paste("Naive MAE:", mean(mae_Naive)))

# MSE results
print(paste("AutoARIMA MSE:", mean(mse_autoARIMA)))
print(paste("ARIMA(1,1,1) MSE:", mean(mse_ARIMA111)))
print(paste("ETS MSE:", mean(mse_ETS)))
print(paste("Naive MSE:", mean(mse_Naive)))

# RMSE results
print(paste("AutoARIMA RMSE:", mean(rmse_autoARIMA)))
print(paste("ARIMA(1,1,1) RMSE:", mean(rmse_ARIMA111)))
print(paste("ETS RMSE:", mean(rmse_ETS)))
print(paste("Naive RMSE:", mean(rmse_Naive)))

#  Diebold-Mariano Test 
dm_result <- dm.test(errors_autoARIMA[, 1], errors_ARIMA111[, 1], alternative = "two.sided") 
print(paste("Diebold-Mariano Test p-value:", dm_result$p.value))

data_1h_LKOH <- read_csv("C:/Users/HONOR/Downloads/data_1h_moex/LKOH_volume.csv")
head(data_1h_LKOH)
data_1h_LKOH$volume


#Cross-Validation
maxHorizon = 5
testLast = 50

errors_autoARIMA <- matrix(nrow = maxHorizon, 
                           ncol = testLast)
errors_autoARIMA -> errors_ARIMA111
#errors_autoARIMA -> errors_ARFIMA111
errors_autoARIMA -> errors_ETS
errors_autoARIMA -> errors_Naive

for (k in 1:testLast){
  tmpData <- ts(data_1h_LKOH$volume[1:(nrow(data_1h_LKOH) - k - maxHorizon + 1)])
  m_autoARIMA <- auto.arima(tmpData, allowdrift = F)
  m_arima111 <- arima(tmpData, order = c(1,1,1))
  #m_arfima111 <- arfima(tmpData, nar = 1, nma = 1, d = 1, fit = TRUE)
  m_ETS <- ets(tmpData, model = "ZZZ", damped = TRUE)
  m_naive <- naive(tmpData)
  
  fact <- ts(data_1h_LKOH$volume[(nrow(data_1h_LKOH) - k - maxHorizon + 2):(nrow(data_1h_LKOH) - k + 1)])
  
  errors_autoARIMA[,k] <- c(fact) - c(forecast(m_autoARIMA, h = 5)$mean)
  errors_ARIMA111[,k] <- c(fact) - c(forecast(m_arima111, h = 5)$mean)
  #errors_ARFIMA111[,k] <- c(fact) - c(forecast(m_arfima111, h = 5)$mean)
  errors_ETS[,k] <- c(fact) - c(forecast(m_ETS, h = 5)$mean)
  errors_Naive[,k] <- c(fact) - c(forecast(m_naive, h = 5)$mean)
  print(k)
}

plot(forecast(m_autoARIMA, h = 5))
plot(forecast(m_ETS, h = 5))
plot(forecast(m_naive, h = 5))

apply(errors_ARIMA111, 1, function(x) mean(abs(x)))
#apply(errors_ARFIMA111, 1, function(x) mean(abs(x)))
rowMeans(abs(errors_ARIMA111))
#rowMeans(abs(errors_ARFIMA111))

apply(errors_ARIMA111, 1, function(x) sqrt(mean(x^2)))
#apply(errors_ARFIMA111, 1, function(x) sqrt(mean(x^2)))

rowMeans(abs(errors_autoARIMA))
rowMeans(abs(errors_ARIMA111))
#rowMeans(abs(errors_ARFIMA111))
rowMeans(abs(errors_ETS))
rowMeans(abs(errors_Naive))

# MASE
rowMeans(abs(errors_ETS)) / rowMeans(abs(errors_Naive))

mape_autoARIMA <- rowMeans(abs(errors_autoARIMA) / c(fact))
mape_ARIMA111 <- rowMeans(abs(errors_ARIMA111) / c(fact))
mape_ETS <- rowMeans(abs(errors_ETS) / c(fact))
mape_Naive <- rowMeans(abs(errors_Naive) / c(fact))

# Calculate R-squared (R^2) for each model
# r_squared_autoARIMA <- 1 - (sum(errors_autoARIMA^2) / sum((c(fact) - mean(c(fact)))^2))
# r_squared_ARIMA111 <- 1 - (sum(errors_ARIMA111^2) / sum((c(fact) - mean(c(fact)))^2))
# r_squared_ETS <- 1 - (sum(errors_ETS^2) / sum((c(fact) - mean(c(fact)))^2))
# r_squared_Naive <- 1 - (sum(errors_Naive^2) / sum((c(fact) - mean(c(fact)))^2))

# Print R-squared results
# print(paste("AutoARIMA R-squared:", r_squared_autoARIMA))
# print(paste("ARIMA(1,1,1) R-squared:", r_squared_ARIMA111))
# print(paste("ETS R-squared:", r_squared_ETS))
# print(paste("Naive R-squared:", r_squared_Naive))


# Calculate MAE, MSE, and RMSE
mae_autoARIMA <- rowMeans(abs(errors_autoARIMA))
mae_ARIMA111 <- rowMeans(abs(errors_ARIMA111))
mae_ETS <- rowMeans(abs(errors_ETS))
mae_Naive <- rowMeans(abs(errors_Naive))

mse_autoARIMA <- rowMeans(errors_autoARIMA^2)
mse_ARIMA111 <- rowMeans(errors_ARIMA111^2)
mse_ETS <- rowMeans(errors_ETS^2)
mse_Naive <- rowMeans(errors_Naive^2)

rmse_autoARIMA <- sqrt(mse_autoARIMA)
rmse_ARIMA111 <- sqrt(mse_ARIMA111)
rmse_ETS <- sqrt(mse_ETS)
rmse_Naive <- sqrt(mse_Naive)

# MASE
rowMeans(abs(errors_ETS)) / rowMeans(abs(errors_Naive))

# MAPE results
print(paste("AutoARIMA MAPE:", mean(mape_autoARIMA)))
print(paste("ARIMA(1,1,1) MAPE:", mean(mape_ARIMA111)))
print(paste("ETS MAPE:", mean(mape_ETS)))
print(paste("Naive MAPE:", mean(mape_Naive)))

# MAE results
print(paste("AutoARIMA MAE:", mean(mae_autoARIMA)))
print(paste("ARIMA(1,1,1) MAE:", mean(mae_ARIMA111)))
print(paste("ETS MAE:", mean(mae_ETS)))
print(paste("Naive MAE:", mean(mae_Naive)))

# MSE results
print(paste("AutoARIMA MSE:", mean(mse_autoARIMA)))
print(paste("ARIMA(1,1,1) MSE:", mean(mse_ARIMA111)))
print(paste("ETS MSE:", mean(mse_ETS)))
print(paste("Naive MSE:", mean(mse_Naive)))

# RMSE results
print(paste("AutoARIMA RMSE:", mean(rmse_autoARIMA)))
print(paste("ARIMA(1,1,1) RMSE:", mean(rmse_ARIMA111)))
print(paste("ETS RMSE:", mean(rmse_ETS)))
print(paste("Naive RMSE:", mean(rmse_Naive)))

#  Diebold-Mariano Test 
dm_result <- dm.test(errors_autoARIMA[, 1], errors_ARIMA111[, 1], alternative = "two.sided") 
print(paste("Diebold-Mariano Test p-value:", dm_result$p.value))
