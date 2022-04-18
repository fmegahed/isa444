# Class code written on April 18, 2022


# * Loading the required packages -----------------------------------------

if(require(pacman)==F) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot2, dplyr [pipe]
               astsa, # birth data
               fpp2) # forecast, auto.arima, check.residuals, ndiffs


# * Reading the data ------------------------------------------------------

birth_data = birth

# from printing the data, we see that we have a ts object
# frequency = 12 (i.e., 12 observations per year / monthly data)
# starting from Jan 1948 and ending in Jan 1979
birth_data 


autoplot(birth_data) # since it is a ts object, we can use forecast::autoplot to plot it

# * Let us see what happens when we fit an auto.arima model ---------------
auto_model = auto.arima(birth_data)

summary(auto_model)
checkresiduals(auto_model)

Feb1979 = forecast(auto_model, h = 1, level = 95)
Feb1979

forecasts = forecast(auto_model, h = 24, level = 95)

# to answer the question about combining the old and new data

# approach 1
total = c(birth_data, forecasts$mean)
total_ts = ts(total, start = c(1948, 1), frequency = 12)
total_ts

# approach 2
dates = seq.Date(from = lubridate::ymd('1948-01-01'), by = 'month', 
         to = lubridate::ymd('1981-01-01'))

df = data.frame(dates, total, type_data = c(rep('original', 373), rep('fct', 24) ) )
df


# * Intentionally ignoring the seasonal component -------------------------

# Step 1 -- plotted the data
autoplot(birth_data)

# Step 2 - ndiffs()
ndiffs(birth_data)

# step 3 - acf of the differenced ts (ndiffs = 0 --> original)
acf( diff(birth_data, differences = 1), lag.max = 60  )
pacf( diff(birth_data, differences = 1), lag.max = 60  )

# If we are consistent with the auto_arima model
nonseasonal = Arima(y = birth_data,
                    order = c(1,1,1))
summary(nonseasonal)
checkresiduals(nonseasonal)


# * Netflix Data ----------------------------------------------------------

netflix = read.csv('data/23 - Netflix_growth_pct_2000.csv')
netflix = ts(netflix, frequency = 4)

autoplot(netflix)

nflx_auto_arima = auto.arima(netflix)

summary(nflx_auto_arima)
checkresiduals(nflx_auto_arima)
