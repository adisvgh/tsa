# PRAC - SARIMA and ARIMA models

# Q1

library(forecast)
library(tseries)
sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330,
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350,
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370,
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390,
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)
sales_ts
plot(sales_ts, main = "Monthly Sales Data", ylab = "Sales", xlab = "Time", col = "blue")

# Differencing the series to remove trend and seasonality
sales_diff <- diff(sales_ts, differences = 1)
sales_seasonal_diff <- diff(sales_ts, lag = 12)
# Plot the differenced series
plot(sales_diff, main = "Differenced Sales Data", ylab = "Sales", xlab = "Time", col = "red")
plot(sales_seasonal_diff, main = "Seasonally Differenced Sales Data", ylab = "Sales", xlab = "Time", col = "green")

# Q2

# Augmented Dickey-Fuller Test for stationarity
adf.test(sales_ts)
# ACF and PACF plots to identify AR and MA orders
acf(sales_seasonal_diff, main = "ACF of Seasonally Differenced Data")
pacf(sales_seasonal_diff, main = "PACF of Seasonally Differenced Data")

# Q3

# Fit a SARIMA model, assuming (p,d,q) = (1,1,1) and (P,D,Q,m) = (1,1,1,12) for illustration
sarima_model <- auto.arima(sales_ts, seasonal = TRUE)
summary(sarima_model)

# Residual diagnostics
checkresiduals(sarima_model)
Box.test(residuals(sarima_model), lag=12, type="Ljung-Box")
qqnorm(residuals(sarima_model))  # Generate Q-Q plot
qqline(residuals(sarima_model))

# Q4

# Forecast the next 12 months
forecast_values <- forecast(sarima_model, h = 12)
print(forecast_values)
# Plot the forecasted values along with original data
plot(forecast_values, main = "SARIMA Forecast for Next 12 Months")

# Q5

# Fit a simple ARIMA model without seasonal terms
arima_model <- auto.arima(sales_ts, seasonal = FALSE)

# Compare SARIMA and ARIMA using AIC, BIC, and RMSE
sarima_aic <- AIC(sarima_model)
arima_aic <- AIC(arima_model)
sarima_bic <- BIC(sarima_model)
arima_bic <- BIC(arima_model)
sarima_metrics <- accuracy(sarima_model)
arima_metrics <- accuracy(arima_model)

cat("SARIMA AIC:", sarima_aic, "ARIMA AIC:", arima_aic, "\n")
cat("SARIMA BIC:", sarima_bic, "ARIMA BIC:", arima_bic, "\n")
sarima_metrics
arima_metrics
