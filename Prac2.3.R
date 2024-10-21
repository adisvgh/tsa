# PRAC-1 : ARMA models

library(forecast)
library(tseries)

# Create the time series data
quarter <- 1:12
sales <- c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
sales_ts <- ts(sales, start = c(1, 1), frequency = 4)  # Frequency set to 4 for quarterly data
sales_ts
plot(sales_ts,main = 'Quarterly Sales data')

# Check Stationarity
adf.test(sales_ts)

# Plot the ACF and PACF of the original time series
acf(sales_ts, main = "ACF of Sales Data")
pacf(sales_ts, main = "PACF of Sales Data")

# Fit an ARMA(1,1) model to the data
arma_model <- Arima(sales_ts, order = c(1, 0, 1))

# Check the residual diagnostics of the fitted model
checkresiduals(arma_model)

# Forecast the next 12 values using the fitted model
forecast_values <- forecast(arma_model, h = 12)

# Plot the original time series, the fitted values, and the forecasted values
plot(forecast_values, main = "Sales Forecast", xlab = "Quarter", ylab = "Sales")
lines(sales_ts, col = "blue")  # Original time series
lines(fitted(arma_model), col = "red")  # Fitted values

