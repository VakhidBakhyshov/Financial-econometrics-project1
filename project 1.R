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
      open = first(price),
      high = max(price),
      low = min(price),
      close = last(price)
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
  
  return(candles)
}

candles_pepe <- create_candles(T = 1, min = TRUE, trades = trades_pepe)
print(n = 5, candles_pepe)

candles_pepe$volume <- c(candles_pepe$buy_volume+candles_pepe$sell_volume)
print(n = 5, candles_pepe)

plot(ts(candles_pepe$volume))

print(nrow(candles_pepe)) # кол-во строк в наших свечах

x <- ts(candles_pepe$volume, start=c(7000),end=c(9980), frequency=1)
plot(x)

adf.test(x, alternative = "stationary") # adf тест т.е тест Dickey-Fuller
#kpss.test(x)
#pp.test(x)
acf(x)
pacf(x)

checkresiduals(candles_pepe$volume)

m2 <- auto.arima(x, seasonal = TRUE)
summary(m2)
tsdisplay(residuals(m2))
checkresiduals(m2)
plot(forecast(m2, h = 100))

m3 <- arfima(x)
summary(m3)
tsdisplay(residuals(m3))
forecast_obj <- forecast(m3, h = 100)
plot(forecast_obj)
actual <- x[(length(x) - 99):length(x)] 

# Calculate performance metrics
predictions <- forecast_obj$mean

ME <- mean(actual - predictions)
RMSE <- sqrt(mean((actual - predictions)^2))
MAE <- mean(abs(actual - predictions))
MPE <- mean((actual - predictions) / actual) * 100
MAPE <- mean(abs((actual - predictions) / actual)) * 100
MASE <- mean(abs(actual - predictions)) / mean(abs(actual[2:length(actual)] - actual[1:(length(actual) - 1)]))
ACF1 <- acf(residuals(m3), plot = FALSE, lag.max = 1)$acf[2]

# Print metrics
print(paste0("ME: ", ME))
print(paste0("RMSE: ", RMSE))
print(paste0("MAE: ", MAE))
print(paste0("MPE: ", MPE))
print(paste0("MAPE: ", MAPE))
print(paste0("MASE: ", MASE))
print(paste0("ACF1: ", ACF1))

m <- auto.arima(candles_pepe$volume) 
summary(m) # оценила ARIMA
tsdisplay(residuals(m))
checkresiduals(m)
plot(forecast(m, h = 100))

# Normalize volume data using MinMax scaling
candles_pepe$volume_normalized <- (candles_pepe$volume - min(candles_pepe$volume)) / 
  (max(candles_pepe$volume) - min(candles_pepe$volume))

# Print normalized data
print(n = 5, candles_pepe[, c("volume", "volume_normalized")])

# Plot normalized data
plot(ts(candles_pepe$volume_normalized), main = "Normalized Volume")

#Cross-Validation
maxHorizon = 3
testLast = 48

errors_autoARIMA <- matrix(nrow = maxHorizon, 
                           ncol = testLast)
errors_autoARIMA -> errors_ARIMA111
#errors_autoARIMA -> errors_ARFIMA111
errors_autoARIMA -> errors_ETS
errors_autoARIMA -> errors_Naive

for (k in 1:testLast){
  tmpData <- ts(candles_pepe$volume_normalized[1:(nrow(candles_pepe) - k - maxHorizon + 1)]) # примерно у нас 100 точек мы прогнозируем горизонт до 5 точек вперед, нам нужно на 1-ой итерации взять первые 95 точек для оценки модели, а на точке 96, 97, 98, 99 и 100 спрогнозировать. если мы возьмем с 1-ой по 98-ую точку, то у нас не получится потому что мы прогнозировали в том числе для периодов 101, 102, 103 у нас для них нет фактических данных
  m_autoARIMA <- auto.arima(tmpData, allowdrift = F) # allowdrift = F (т.е False) - мы не будем оценивать модели с дрейвом (с помощью этого у нас будет быстрее считаться на таком цикле это важно)
  m_arima111 <- arima(tmpData, order = c(1,1,1))
  #m_arfima111 <- arfima(tmpData, nar = 1, nma = 1, d = 1, fit = TRUE)
  m_ETS <- ets(tmpData, model = "ZZZ", damped = TRUE) # команда для ETS это ets, и модель оставим по дефолту "ZZZ". модель ets это соотственно 3 буквы, соотвествующих ошибкам, трендам и сезонности (1-ая буква про ошибки, 2-ая про тренды и 3-я про сезонность)
  m_naive <- naive(tmpData)
  
  fact <- ts(candles_pepe$volume_normalized[(nrow(candles_pepe) - k - maxHorizon + 2):(nrow(candles_pepe) - k + 1)])
  
  errors_autoARIMA[,k] <- c(fact) - c(forecast(m_autoARIMA, h = 3)$mean) # errors_autoARIMA[,k] - все строчки и k-ый столбик
  errors_ARIMA111[,k] <- c(fact) - c(forecast(m_arima111, h = 3)$mean)
  #errors_ARFIMA111[,k] <- c(fact) - c(forecast(m_arfima111, h = 5)$mean)
  errors_ETS[,k] <- c(fact) - c(forecast(m_ETS, h = 3)$mean)
  errors_Naive[,k] <- c(fact) - c(forecast(m_naive, h = 3)$mean)
  print(k)
}

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

# Print MAPE results
print(paste("AutoARIMA MAPE:", mape_autoARIMA))
print(paste("ARIMA(1,1,1) MAPE:", mape_ARIMA111))
print(paste("ETS MAPE:", mape_ETS))
print(paste("Naive MAPE:", mape_Naive))
print(mean(mape_autoARIMA))
print(mean(mape_ARIMA111))
print(mean(mape_ETS))
print(mean(mape_Naive))

# Calculate R-squared (R^2) for each model
r_squared_autoARIMA <- 1 - (sum(errors_autoARIMA^2) / sum((c(fact) - mean(c(fact)))^2))
r_squared_ARIMA111 <- 1 - (sum(errors_ARIMA111^2) / sum((c(fact) - mean(c(fact)))^2))
r_squared_ETS <- 1 - (sum(errors_ETS^2) / sum((c(fact) - mean(c(fact)))^2))
r_squared_Naive <- 1 - (sum(errors_Naive^2) / sum((c(fact) - mean(c(fact)))^2))

# Print R-squared results
print(paste("AutoARIMA R-squared:", r_squared_autoARIMA))
print(paste("ARIMA(1,1,1) R-squared:", r_squared_ARIMA111))
print(paste("ETS R-squared:", r_squared_ETS))
print(paste("Naive R-squared:", r_squared_Naive))



# Ошибка в ets(tmpData, model = "MAM", damped = TRUE) : Nonseasonal data


candles_doge <- create_candles(T = 1, min = TRUE, trades = trades_doge)
candles_doge$volume <- c(candles_doge$buy_volume+candles_doge$sell_volume)
print(n = 5, candles_doge)

plot(ts(candles_doge$volume))

print(nrow(candles_doge)) # кол-во строк в наших свечах

m <- auto.arima(candles_doge$volume) 
summary(m) # оценила ARIMA
tsdisplay(residuals(m))
checkresiduals(m)
plot(forecast(m, h = 100))

#Cross-Validation
maxHorizon = 5
testLast = 100

errors_autoARIMA <- matrix(nrow = maxHorizon, 
                           ncol = testLast)
errors_autoARIMA -> errors_ARIMA111
#errors_autoARIMA -> errors_ARFIMA111
errors_autoARIMA -> errors_ETS
errors_autoARIMA -> errors_Naive

for (k in 1:testLast){
  tmpData <- ts(candles_doge$volume[1:(nrow(candles_doge) - k - maxHorizon + 1)]) # примерно у нас 100 точек мы прогнозируем горизонт до 5 точек вперед, нам нужно на 1-ой итерации взять первые 95 точек для оценки модели, а на точке 96, 97, 98, 99 и 100 спрогнозировать. если мы возьмем с 1-ой по 98-ую точку, то у нас не получится потому что мы прогнозировали в том числе для периодов 101, 102, 103 у нас для них нет фактических данных
  m_autoARIMA <- auto.arima(tmpData, allowdrift = F, seasonal = TRUE) # allowdrift = F (т.е False) - мы не будем оценивать модели с дрейвом (с помощью этого у нас будет быстрее считаться на таком цикле это важно)
  m_arima111 <- arima(tmpData, order = c(1,1,1))
  #m_arfima111 <- arfima(tmpData, nar = 1, nma = 1, d = 1, fit = TRUE)
  m_ETS <- ets(tmpData, model = "ZZZ", damped = TRUE) # команда для ETS это ets, и модель оставим по дефолту "ZZZ". модель ets это соотственно 3 буквы, соотвествующих ошибкам, трендам и сезонности (1-ая буква про ошибки, 2-ая про тренды и 3-я про сезонность)
  m_naive <- naive(tmpData)
  
  fact <- ts(candles_doge$volume[(nrow(candles_doge) - k - maxHorizon + 2):(nrow(candles_doge) - k + 1)])
  
  errors_autoARIMA[,k] <- c(fact) - c(forecast(m_autoARIMA, h = 5)$mean) # errors_autoARIMA[,k] - все строчки и k-ый столбик
  errors_ARIMA111[,k] <- c(fact) - c(forecast(m_arima111, h = 5)$mean)
  #errors_ARFIMA111[,k] <- c(fact) - c(forecast(m_arfima111, h = 5)$mean)
  errors_ETS[,k] <- c(fact) - c(forecast(m_ETS, h = 5)$mean)
  errors_Naive[,k] <- c(fact) - c(forecast(m_naive, h = 5)$mean)
  print(k)
}

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
#print(nrow(candles_doge))
#print(nrow(candles_pepe))

mape_autoARIMA <- rowMeans(abs(errors_autoARIMA) / c(fact))
mape_ARIMA111 <- rowMeans(abs(errors_ARIMA111) / c(fact))
mape_ETS <- rowMeans(abs(errors_ETS) / c(fact))
mape_Naive <- rowMeans(abs(errors_Naive) / c(fact))

# Print MAPE results
print(paste("AutoARIMA MAPE:", mape_autoARIMA))
print(paste("ARIMA(1,1,1) MAPE:", mape_ARIMA111))
print(paste("ETS MAPE:", mape_ETS))
print(paste("Naive MAPE:", mape_Naive))

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

# Print results
print(paste("AutoARIMA MAE:", mae_autoARIMA))
print(paste("ARIMA(1,1,1) MAE:", mae_ARIMA111))
print(paste("ETS MAE:", mae_ETS))
print(paste("Naive MAE:", mae_Naive))

print(paste("AutoARIMA MSE:", mse_autoARIMA))
print(paste("ARIMA(1,1,1) MSE:", mse_ARIMA111))
print(paste("ETS MSE:", mse_ETS))
print(paste("Naive MSE:", mse_Naive))

print(paste("AutoARIMA RMSE:", rmse_autoARIMA))
print(paste("ARIMA(1,1,1) RMSE:", rmse_ARIMA111))
print(paste("ETS RMSE:", rmse_ETS))
print(paste("Naive RMSE:", rmse_Naive))

k <- 36
#n <- nrow(candles_doge)
n <- 100
mae1 <- mae2 <- mae3 <- matrix(NA,n-k-12,12)
for(i in 1:(n-k-12))
{
  last_timestamp <- max(time(candles_doge$volume)) 
  
  # Calculate start and end correctly
  start <- last_timestamp + (5 + i)/12
  end <- last_timestamp + (5 + i + 12)/12  # Adjust to ensure start is before end
  
  xshort <- window(candles_doge$volume, start = start, end = end) 
  xnext <- window(candles_doge$volume, start = end, 
                  end = last_timestamp + 1996 + (5 + i + 12)/12)
  fit1 <- tslm(xshort ~ trend, lambda=0)
  fcast1 <- forecast(fit1,h=12)
  fit2 <- auto.arima(xshort,D=1, lambda=0)
  fcast2 <- forecast(fit2,h=12)
  fit3 <- ets(xshort)
  fcast3 <- forecast(fit3,h=12)
  mae1[i,] <- abs(fcast1[['mean']]-xnext)
  mae2[i,] <- abs(fcast2[['mean']]-xnext)
  mae3[i,] <- abs(fcast3[['mean']]-xnext)
  print(i)
}
plot(1:12,colMeans(mae1),type="l",col=2,xlab="horizon",ylab="MAE",
     ylim=c(0.58,1.0))
lines(1:12,colMeans(mae2),type="l",col=3)
lines(1:12,colMeans(mae3),type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)


library(ggfortify)
pred <- ts(rnorm(length(candles_doge$volume)), start=start(candles_doge$volume),
           frequency=frequency(candles_doge$volume))

# Now pass the whole time series and the corresponding predictors 
tsCV(candles_doge$volume, fc, xreg=pred)


candles_doge <- candles_doge[!is.na(candles_doge$volume), ]

candles_doge$volume <- ts(candles_doge$volume, start = c(1, 1), end = c(1, 10080), frequency = 1) 

errors_arima_cv <- tsCV(candles_doge$volume, auto.arima, h = 5,
                        allowdrift = FALSE, seasonal = TRUE) 

# Example with ETS model
errors_ets_cv <- tsCV(candles_doge$volume, ets, h = 5, model = "ZZZ", damped = TRUE)

tryCatch({
  errors_ets_cv <- tsCV(candles_doge$volume, ets, h = 5, model = "ZZZ", damped = TRUE)
  mae_ets_cv <- mean(abs(errors_ets_cv))
  rmse_ets_cv <- sqrt(mean(errors_ets_cv^2))
  print(paste("ETS MAE:", mae_ets_cv))
  print(paste("ETS RMSE:", rmse_ets_cv))
}, error = function(e) {
  print(paste("Error in ETS tsCV:", e))
})

# Example with Naive model
errors_naive_cv <- tsCV(candles_doge$volume, naive, h = 5)

# Calculate performance metrics (e.g., MAE, RMSE)
mae_arima_cv <- mean(abs(errors_arima_cv))
rmse_arima_cv <- sqrt(mean(errors_arima_cv^2))

mae_ets_cv <- mean(abs(errors_ets_cv))
rmse_ets_cv <- sqrt(mean(errors_ets_cv^2))

mae_naive_cv <- mean(abs(errors_naive_cv))
rmse_naive_cv <- sqrt(mean(errors_naive_cv^2))

print(paste("ARIMA MAE:", mae_arima_cv))
print(paste("ARIMA RMSE:", rmse_arima_cv))
print(paste("ETS MAE:", mae_ets_cv))
print(paste("ETS RMSE:", rmse_ets_cv))
print(paste("Naive MAE:", mae_naive_cv))
print(paste("Naive RMSE:", rmse_naive_cv))


tryCatch({
  dm_result <- dm.test(errors_autoARIMA[, 1], errors_ARIMA111[, 1], alternative = "two.sided")
  print(paste("Diebold-Mariano Test p-value:", dm_result$p.value))
}, error = function(e) {
  print(paste("Error in Diebold-Mariano Test:", e)) 
}) 

#  Diebold-Mariano Test 
dm_result <- dm.test(errors_autoARIMA[, 1], errors_ARIMA111[, 1], alternative = "two.sided") 
print(paste("Diebold-Mariano Test p-value:", dm_result$p.value))
