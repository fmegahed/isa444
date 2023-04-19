# Class 22: Review and Catch Up


# * In-class Activity -----------------------------------------------------

netflix = readr::read_csv('data/netflix_growth_pct_2000.csv')

netflix_ts = ts(data = netflix$`Quarterly Sales`,
                start = c(2000, 1), # you could keep this at one given the data is not very well described
                frequency = 4) # freq = 4 since col name is quarterly sales



forecast::autoplot(netflix_ts)


# Fitting an auto.arima()

netflix_model = forecast::auto.arima(y = netflix_ts )
netflix_model

# Nonseasonal ARIMA
# We took the second difference and then the first two error lags were used to predict y_t
# i.e., a MA(2) model on the second difference 
# no AR components since p = 0

# Seasonal ARIMA
# P = 2
# D = 0 (no seasonal differences)
# Q = 0 (no seasonal error lags)

# we are regressing y_t based on y(t-4) [1 yr ago data], and y(t-8) which is [2 yrs ago]


# Residuals
forecast::checkresiduals(netflix_model)




# * ACF - Example ---------------------------------------------------------

acf_list = acf(netflix_ts)

pacf(netflix_ts)

ar1 = forecast::Arima(y = netflix_ts, order = c(1,0,0))

forecast::ndiffs(netflix_ts)

# just showing you the residuals based on the code below
# i ran it first without the residuals to show that this was a worse model than the seasonal ARIMA
# you should run this in two steps
forecast::Arima(y = netflix_ts, order = c(1,2,0)) |> forecast::checkresiduals()
