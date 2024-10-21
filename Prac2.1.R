## PRAC-1 : Holts Exponential Smoothing

library(forecast)
# Creating Time Series data from given table
observations = c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 153.5,
                  154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 157.8, 158.3,
                  158.6, 158.6, 159.1, 159.3)
year = 1996:2019
print(year)
ts_data = ts(data = observations,start = 1996, frequency = 1)
print(ts_data)

# Simple Exponential Smoothing
simple_exp_model = ses(y = ts_data, alpha = 0.3, initial = 'simple')
accuracy(simple_exp_model)
plot(ts_data,main = 'Simple Exponential Smoothing',col = 'black')
lines(fitted(simple_exp_model), col="red")

# Double Exponential Smoothing
double_exp_model = holt(y = ts_data,alpha = 0.3,beta = 0.2,initial = 'optimal')
accuracy(double_exp_model)
plot(ts_data,main = 'Holts or Double Exponential Smoothing',col = "black")
lines(fitted(double_exp_model),col = 'blue')

# Comparison
plot(ts_data,main = 'simple vs double',col = 'black')
lines(fitted(simple_exp_model),col = 'red')
lines(fitted(double_exp_model),col = 'blue')

# SES is a simpler model, which smooths the series based on a single smoothing parameter (alpha), so it reacts more slowly to changes.
# Holt's method incorporates trend (beta) in addition to level (alpha), allowing it to capture trends in the data better, which is expected to perform better if there is an underlying trend.

# PRAC-2 : Moving Average Smoothing

# Creating a Time Series data from given table
co2_concentration = c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 
                       368.3, 369.47, 371.03, 373.61, 357.61)
years = 1991:2003
co2_ts = ts(data = co2_concentration,start = 1991,frequency = 1)
co2_ts
plot(co2_ts,main = 'Co2 concentration',col = 'black')
mas_3yr = filter(co2_ts,rep(1/3, 3),sides = 2)
mas_3yr
forecast_2004 <- mean(tail(co2_concentration, 3))
forecast_2004

# PRAC-3 : Holt-Winters method for AirPassengers data

# importing Airpassengers 
data("AirPassengers")
# View the dataset
AirPassengers
# Plot the original time series
plot(AirPassengers, main = "AirPassengers Data",col = "blue")

# Apply Holt-Winters exponential smoothing with a multiplicative model
hw_model = HoltWinters(AirPassengers, seasonal = "multiplicative")

# Plot the fitted model
plot(hw_model, main = "AirPassengers Data",col = "black")

# Forecast the next 12 months
hw_forecast <- forecast(hw_model, h = 12)

# Plot the forecast
plot(hw_forecast, main = "12-Month Forecast Using Holt-Winters (Multiplicative Model)")

# Print the forecasted values
hw_forecast