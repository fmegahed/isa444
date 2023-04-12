# Seasonal ARIMA Models


# * Lab Solution ----------------------------------------------------------

macros = tidyquant::tq_get(
  x = c('GNP', 'TOTALSA'),
  get = 'economic.data',
  from = '1947-01-01'
)

macros


# to nest a dataset by symbol we need to first group the data
macros_nested = macros |> dplyr::group_by(symbol) |> tidyr::nest()

macros_nested$data[[2]]

macros_nested_rolled = 
  macros_nested |> 
  # automatically obtaining the initial length of each training set and then rolling it
  dplyr::mutate(
    initial_length = (purrr::map_dbl(.x = data, .f = nrow) * 0.95) |>  ceiling(),    
    rolled = purrr::map2(
      .x = data, .y = initial_length,  .f = rsample::rolling_origin,  
      assess = 1, cumulative = TRUE
    ) ) |> # unnest rolled so that the length equals splits * symbols
  tidyr::unnest(rolled)


macros_nested_rolled

macros_nested_rolled = 
  macros_nested_rolled |> 
  dplyr::mutate(
    data = purrr::map(.x = splits, .f = rsample::analysis),
    # getting the training data
    train_dates = purrr::map(.x = data, .f = `[[`, 'date' ), # `[[` same as magrittr::extract2
    train_price = purrr::map(.x = data, .f = magrittr::extract2, 'price'),
    # testing data
    target_data = purrr::map(.x = splits, .f = rsample::assessment),
    target_date = purrr::map_dbl(.x = target_data, .f = magrittr::extract2, 'date') |>  lubridate::as_date(),
    target_price = purrr::map_dbl(.x = target_data, .f = magrittr::extract2, 'price')
  )


macros_df = macros_nested_rolled |>  
  dplyr::select(-c(data, train_dates, target_data))


macros_df

macros_df = 
  macros_df |> 
  dplyr::mutate(
    naive_fct = purrr::map(.x = train_price, .f = forecast::naive, h = 1) |> 
      purrr::map_dbl(.f = magrittr::extract2, 'mean'),
    holt_fct = purrr::map(.x = train_price, .f = forecast::holt, h = 1) |> 
      purrr::map_dbl(.f = magrittr::extract2, 'mean'),
    auto_arima_fct = purrr::map(.x = train_price, .f = forecast::auto.arima) |>  
      purrr::map(.f = forecast::forecast, h = 1) |> purrr::map_dbl(.f = magrittr::extract2, 'mean')
  )


macros_df

macros_results = 
  macros_df |> 
  dplyr::select(symbol, target_price:auto_arima_fct) |> 
  # this longer pivot will facilitate the computation of the metrics after grouping by method and symbol
  tidyr::pivot_longer(
    cols = dplyr::ends_with('_fct'),
    names_to = 'method',
    values_to = 'forecast'
  ) |> 
  dplyr::ungroup() |>
  dplyr::group_by( symbol, method ) |>
  dplyr::summarise(
    me = forecast::accuracy(object = forecast, x = target_price) |> magrittr::extract2(1),
    rmse = forecast::accuracy(object = forecast, x = target_price) |> magrittr::extract2(2),
    mae = forecast::accuracy(object = forecast, x = target_price) |> magrittr::extract2(3),
    mpe = forecast::accuracy(object = forecast, x = target_price) |> magrittr::extract2(4),
    mape = forecast::accuracy(object = forecast, x = target_price) |> magrittr::extract2(5)
  )




# * Birth Data (auto.arima) -----------------------------------------------

install.packages('astsa') # install the package once

us_births = astsa::birth
us_births

# given that it is a seasonal time-series
forecast::autoplot(us_births)

# let us use the auto.arima() and talk about the output model
# since the data is seasonal, make sure that the input to the auto.arima is a ts object
# with the correct frequency
auto_model = forecast::auto.arima(y = us_births)
auto_model
summary(auto_model)
# this is a reasonable model (uncorrelated residuals and other two plots look fine)
#and it has the lowest AICc from the models evaluated by auto.arima()
forecast::checkresiduals(auto_model)
forecast::forecast(auto_model, h = 12)
forecast::forecast(auto_model, h = 12) |> forecast::autoplot()



# Could we have fitted a different model? 
forecast::ndiffs(us_births)

births_diff = diff(x = us_births, differences = 1)
acf(births_diff, lag.max = 60)
pacf(births_diff, lag.max = 60)

# I will fit a bad model not accounting for seasonality
# ARIMA(p = 3, d = 1, q = 0) 
# based on the wrong interpretation that the PACF cuts of at lag 3 with no seasonality evidence

model1 = forecast::Arima(y = us_births, 
                         order = c(3, 1, 0) )
summary(model1)
forecast::checkresiduals(model1)
acf(model1$residuals, lag.max = 60)
pacf(model1$residuals, lag.max = 60)


model2 = forecast::Arima(y = us_births,
                         order = c(0,1,2),
                         seasonal = c(1,1,1))

# BASED ON THE ACF and PACF of the residuals from Model 1
model3 = forecast::Arima(y = us_births,
                         order = c(3,1,0),
                         seasonal = c(2,1,0))

forecast::checkresiduals(model3)
summary(model3)
summary(auto_model)

# * Tangent (auto.arima with no seasonality) ------------------------------

gnp = tidyquant::tq_get(
  x = c('GNP'),
  get = 'economic.data',
  from = '1947-01-01'
)

gnp_ts = ts(data = gnp$price, start = c(1947,01))

auto_arima_gnp = forecast::auto.arima(y = gnp_ts)
auto_arima_gnp # note there were no seasonal arima components in the model