# Class 19: ARIMA, AIC/BIC and auto.arima


# * Visc Example ----------------------------------------------------------

visc = readr::read_csv('data/viscosity.csv')

# Step 1: Plot the Data
visc_ts = ts(data = visc$y)
forecast::autoplot(visc_ts)

# Step 2: Stationary?
forecast::ndiffs(x = visc_ts)

# Step 3: Let us Look at the ACF and PACF plot
acf(x = visc_ts, plot = F) |> 
  forecast::autoplot() + 
  ggplot2::scale_x_continuous(
    breaks = scales::pretty_breaks(n = 25)
  )
pacf(x = visc_ts, plot = F) |> 
  forecast::autoplot() + 
  ggplot2::scale_x_continuous(
    breaks = scales::pretty_breaks(n = 25)
  )


# Step 4a: Fitting an AR(3)
ar3_model = forecast::Arima(
  y = visc_ts,
  order = c(3,0,0)
    )
ar3_model

forecast::checkresiduals(ar3_model) # model fits well to the data and the residuals seem to be uncorrelated

# plot the model (ts and forecasted values)
forecast::forecast(ar3_model, h = 5)
forecast::forecast(ar3_model, h = 5) |> forecast::autoplot()
# note that if you were to apply the autoplot function on the model, you will not get this plot
forecast::autoplot(ar3_model)


# Step4b: Comparing with the AR(2)
ar2_model = forecast::Arima(
  y = visc_ts,
  order = c(2,0,0)
)
ar2_model



# * Kahoot ----------------------------------------------------------------

acf_results = acf(AirPassengers)
pacf_results = pacf(AirPassengers)


acf_results$acf[-1]
pacf_results$acf[1:21]



# * ARIMA Model -----------------------------------------------------------

gnp = tidyquant::tq_get(x = 'GNP', get = 'economic.data', from = '1947-01-01')

gnp_ts = ts(data = gnp$price, frequency = 4, start = c(1947,01) )

# Step 1
forecast::autoplot(gnp_ts)

# Step 2: ndiffs()
forecast::ndiffs(x = gnp_ts) # 2 differences are needed to bring the data to stationarity

# diff [ts] vs dplyr::diff [with a numeric vector]
gnp_diff2 = diff(x = gnp_ts, differences = 2) # would provide me with the second diff
# as a note second diff = first diff of the first diff
gnp_diff2 # is still a ts


# step 3

acf(gnp_diff2) |> forecast::autoplot() + 
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 25))

pacf(gnp_diff2) |> forecast::autoplot() + 
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 25))


# Heervansh seems to think that the ACF cuts off at Lag 1 (Lets test this)

# step 4

gnp_model1 = forecast::Arima(
  y = gnp_ts, # because we want to forecast in the original units
  order = c(p = 0, # we concluded that the ACF cuts off
            d = 2, # we took the second diff based on ndiffs
            q = 1 # we conluded that the ACF cuts of at lag 1
            )
)

gnp_model1
summary(gnp_model1)

# we cannot conclude that this is a good model since the residuals are correlated
forecast::checkresiduals(gnp_model1)



# Now what should we do next?
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

forecast::forecast(gnp_model2, h = 4) # forecast Q1-Q4 of 2023
forecast::forecast(gnp_model2, h = 4) |> forecast::autoplot()
