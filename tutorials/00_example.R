
# This is a file where I am testing some code

df = tidyquant::tq_get(
  x = c('BTC-USD', 'ADA-USD'),
  from = '2013-01-01'
) |> 
  dplyr::rename(
    demand = adjusted
  ) |> 
  dplyr::select(symbol, date, demand)

nested_data = 
  df |> 
  dplyr::group_by(symbol) |> 
  tidyr::nest() |> 
  # create a rolling origin dataset, where the test is 30 days into the future
  dplyr::mutate(
    min_date = purrr::map_dbl(.x = data, .f = function(x){ x$date[1]} ) |> lubridate::as_date(),
    initial_size = 250, #as.numeric(lubridate::ymd('2022-01-01') - min_date),
    rolled = purrr::map2(
      .x = data, .y = initial_size,  .f = rsample::rolling_origin,  
      assess = 30, cumulative = TRUE
    ) ) |> # unnest rolled so that the length equals splits * symbols
  tidyr::unnest(rolled)

nested_data = 
  nested_data |> 
  # extracting the training and test data
  dplyr::mutate(
    train_data = purrr::map(.x = splits, .f = rsample::analysis),
    # getting the training data
    train_dates = purrr::map(.x = data, .f = `[[`, 'date' ), # `[[` same as `[[`
    train_demand = purrr::map(.x = data, .f = `[[`, 'demand'),
    # testing data
    test_data = purrr::map(.x = splits, .f = rsample::assessment),
    test_dates = purrr::map(.x = test_data, .f = `[[`, 'date'),
    test_demand = purrr::map(.x = test_data, .f = `[[`, 'demand')
  ) |> 
  dplyr::select(-c(splits, train_data, test_data))

snaive_fun = function(x, y){
  x_msts = forecast::msts(data = x, seasonal.periods = 7, start = lubridate::year(y))
  snaive_fct = forecast::snaive(y = x_msts, h = 30) |> magrittr::extract2('mean')
  return(snaive_fct)
}

nested_data = 
  nested_data |> 
  dplyr::mutate(
    tbats_fct = purrr::map(.x = train_demand, .f = forecast::tbats)
  )

nested_data = 
  nested_data |> 
  dplyr::mutate(
    mape_tbats = purrr::map2(.x = snaive_fct, .y = test_demand, .f = forecast::accuracy) |>
      purrr::map_dbl(.f = `[[`, c(5))
  )

overall_mape = nested_data |> dplyr::group_by(symbol) |> dplyr::summarise(mean_mape = mean(mape_snaive))

overall_mape
