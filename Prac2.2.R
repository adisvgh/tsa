# Load necessary libraries
library(forecast)

# Set seed for reproducibility
set.seed(123)

# Simulate AR(1) process with 100 observations
ar1_process <- arima.sim(model = list(ar = 0.5), n = 100)

# Display the first few values
head(ar1_process)
?arima.sim

# Plot the AR(1) time series
plot(ar1_process, main = "Simulated AR(1) Process", ylab = "X_t", xlab = "Time")

# Fit an AR(1) model to estimate the parameter
ar1_fit <- arima(ar1_process, order = c(1,0,0))

# Display the estimated parameters
ar1_fit

# Fit an AR(1) model to estimate the parameter
ar1_fit <- arima(ar1_process, order = c(1,0,0))

# Display the estimated parameters
ar1_fit

# Plot ACF and PACF of the AR(1) series
acf(ar1_process, main = "ACF of AR(1) Process")
pacf(ar1_process, main = "PACF of AR(1) Process")

# Fit AR(1) model
ar1_model <- arima(ar1_process, order = c(1,0,0))

# Optionally, fit AR(2) model
ar2_model <- arima(ar1_process, order = c(2,0,0))

# Display the model summary
ar1_model
ar2_model

# Forecast the next 10 observations using AR(1) model
ar1_forecast <- forecast(ar1_model, h = 10)

# Plot the forecast
plot(ar1_forecast, main = "AR(1) Model Forecast")

################################################################################
# Simulate MA(1) process with 100 observations
ma1_process <- arima.sim(model = list(ma = 0.5), n = 100)

# Display the first few values
head(ma1_process)

# Plot the MA(1) time series
plot(ma1_process, main = "Simulated MA(1) Process", ylab = "X_t", xlab = "Time")

# Fit an MA(1) model to estimate the parameter
ma1_fit <- arima(ma1_process, order = c(0,0,1))

# Display the estimated parameters
ma1_fit

# Plot ACF and PACF of the MA(1) series
acf(ma1_process, main = "ACF of MA(1) Process")
pacf(ma1_process, main = "PACF of MA(1) Process")

# Fit MA(1) model
ma1_model <- arima(ma1_process, order = c(0,0,1))

# Optionally, fit MA(2) model
ma2_model <- arima(ma1_process, order = c(0,0,2))

# Display the model summary
ma1_model
ma2_model

# Forecast the next 10 observations using MA(1) model
ma1_forecast <- forecast(ma1_model, h = 10)

# Plot the forecast
plot(ma1_forecast, main = "MA(1) Model Forecast")


# If suppose given Xt = 0.6 * X(t-1) - 0.3 * X(t-2) + εt + 0.4 * z(t-1) - 0.2 * z(t-1)
arima.sim(model = list(ar = c(0.6, -0.3), ma = c(0.4, -0.2)), n = 100)
