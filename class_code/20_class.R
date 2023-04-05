# Class 20: auto.arima and lab 03

gnp = tidyquant::tq_get(x = 'GNP', get = 'economic.data', from = '1947-01-01')

gnp_ts = ts(data = gnp$price, frequency = 4, start = c(1947,01) )

forecast::ndiffs(gnp_ts) # from the successive KPSS test -> 2nd diff will make your ts stationary

gnp_ts_diff = diff(x = gnp_ts, differences = 2)
gnp_ts_diff

acf(gnp_ts_diff)

forecast::autoplot(gnp_ts)


auto_arima_model1 = forecast::auto.arima(
  y = gnp$price # numeric vector (I am ignoring seasonality)
)

auto_arima_model1 
forecast::checkresiduals(auto_arima_model1)


auto_arima_model1b = forecast::auto.arima(
  y = gnp_ts # numeric vector (I am ignoring seasonality)
)
auto_arima_model1b

forecast::accuracy(auto_arima_model1b)
forecast::accuracy(auto_arima_model1)

# * Model from Last Class -------------------------------------------------

gnp_model2 = forecast::Arima(
  y = gnp_ts, # because we want to forecast in the original units
  order = c(p = 1, # interpreting the ACF to have a sinsodiual pattern
            d = 2, # we took the second diff based on ndiffs
            q = 1 # the PACF is not cutting off, it dies down (exponential decay)
  )
)

gnp_model2
summary(gnp_model2) # good predictive accuracy
forecast::checkresiduals(gnp_model2) # reasonable fit
