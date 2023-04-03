# Class 18 - ARMA Models


# * Reading the Viscosity Data --------------------------------------------

# you got a data frame with 1 variable (y) and 95 observations
visc = readr::read_csv('data/viscosity.csv')


# Step 1: Plot the Data

visc = visc |> dplyr::mutate(
  obs_num = 1:nrow(visc)
)

visc$obs_no = 1:nrow(visc)

# approach 1 for plotting it
visc |> ggplot2::ggplot(
  ggplot2::aes(x = obs_no, y = y)
) +
  ggplot2::geom_point(size = 1) +
  ggplot2::geom_line()

# approach 2: plot it as a ts_object

visc_ts = ts(data = visc$y)
plot(visc_ts) # 2a
forecast::autoplot(visc_ts) # 2b (has the advantage that this is a ggplot object)


# Step 2: Stationary?
forecast::ndiffs(x = visc_ts) # 0 differences are needed to make it stationary (i.e., it is stationary)


# Step 3: Let us Look at the ACF plot
acf(x = visc_ts) # will plot the acf
acf(x = visc_ts, plot = F) |> forecast::autoplot() + 
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 25))
acf_values = acf(x = visc_ts)
acf_values$acf


# * Model 1 ---------------------------------------------------------------

# Using Heervansh's interpretation of dying down
# Then look at the PACF
pacf(x = visc_ts, plot = F) |> forecast::autoplot() + 
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 25))


# if you conclude that the PACF cuts off at lag 2, then you would fit an AR(2)

ar2 = forecast::Arima(
  y = visc_ts,
  order = c(2, # p which is the number of autoregressive lags
            0, # d = 0 (because we are not differencing)
            0 # q which is the number of error lags used in the MA mode
            )
)

ar2 # print the output
class(ar2) # forecast_ARIMA / arima object
summary(ar2) # because it is a forecast object, we get the forecast::accuracy() output in the printout

# check the residuals for this model
forecast::checkresiduals(ar2)

# check residuals null hypothesis: residuals are iid
# i.e., large p-values; we do not have enough evidence to reject the null hypothesis
# p < 0.05; we will conclude that we have enough evidence to say the residuals are autocorrelated

acf(ar2$residuals, plot = F) |> forecast::autoplot()
pacf(ar2$residuals, plot = F) |> forecast::autoplot()


# conclusions
# The AR(2) provides a "good" fit for this data, with good predictive performance and 
# the residuals are iid as indicated by the checkresiduals() or the plots of the ACF

# The question is whether we need to model the data differently?



# * Model 2 ---------------------------------------------------------------

# conclude that the ACF cuts off at lag 4

ma4 = forecast::Arima(
  y = visc_ts,
  order = c(0, # p which is the number of autoregressive lags
            0, # d = 0 (because we are not differencing)
            4 # q which is the number of error lags used in the MA mode
  )
)

summary(ma4)

forecast::checkresiduals(ma4)



