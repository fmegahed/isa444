



# * Generating Fake Occupancy Data for Hotels -----------------------------

df = tidyquant::tq_get(
  # using the ts of crypto in lieu of actual hotel demands
  x = c('BTC-USD', 'ADA-USD'),
  from = '2018-01-01'
) |> 
  dplyr::rename(
    demand = adjusted  # renaming the adjusted column to demand
  ) |> 
  dplyr::select(symbol, date, demand) |> # keeping only the three columns of interest
  # creating new variables for day, month, name (random names from hotels.com), country, state
  # changing the symbol to correspond to correspond to an abbreviation of the hotel names
  dplyr::mutate(
    day = lubridate::wday(x = date, label = TRUE),
    month = lubridate::month(x = date, label = TRUE),
    name = ifelse(symbol == 'BTC-USD', 'Hampton Inn Oxford/Miami University Area', 'The St. Regis New York'),
    country = 'US',
    state = ifelse(symbol == 'BTC-USD', 'Ohio', 'New York'),
    symbol = ifelse(symbol == 'BTC-USD', 'Hampton', 'Regis')
  ) |> 
  dplyr::group_by(symbol) |> 
  dplyr::mutate(
    demand = demand/max(demand) # mimicking the pct of the hotel rooms occupied
  ) |> 
  dplyr::relocate(demand, .after = state)


dplyr::glimpse(df)

# nesting with parallel processing
future::plan(strategy = 'multisession', workers = 4)

nested_data = 
  df |> 
  dplyr::group_by(symbol) |> 
  tidyr::nest() |> 
  # create a rolling origin dataset, where the test is 30 days into the future
  dplyr::mutate(
    min_date = furrr::future_map_dbl(.x = data, .f = function(x){ x$date[1]} ) |> lubridate::as_date(),
    initial_size = 250, #as.numeric(lubridate::ymd('2022-01-01') - min_date),
    rolled = furrr::future_map2(
      .x = data, .y = initial_size,  .f = rsample::rolling_origin,  
      assess = 30, cumulative = TRUE
    ) ) |> # unnest rolled so that the length equals splits * symbols
  tidyr::unnest(rolled)


nested_data = 
  nested_data |> 
  # extracting the training and test data
  dplyr::mutate(
    train_data = furrr::future_map(.x = splits, .f = rsample::analysis),
    # getting the training data
    train_dates = furrr::future_map(.x = data, .f = `[[`, 'date' ), # `[[` same as `[[`
    train_demand = furrr::future_map(.x = data, .f = `[[`, 'demand'),
    # testing data
    test_data = furrr::future_map(.x = splits, .f = rsample::assessment),
    test_dates = furrr::future_map(.x = test_data, .f = `[[`, 'date'),
    test_demand = furrr::future_map(.x = test_data, .f = `[[`, 'demand')
  ) |> 
  dplyr::select(-c(splits, train_data, test_data))

snaive_fun = function(x, y){
  x_ts = ts(data = x, frequency = 7, start = c(lubridate::year(y), lubridate::wday(y)))
  snaive_fct = forecast::snaive(y = x_ts, h = 30) |> magrittr::extract2('mean')
  return(snaive_fct)
}

nested_data = 
  nested_data |> 
  dplyr::mutate(
    snaive_fct = furrr::future_map2(.x = train_demand, .y = train_dates, .f = snaive_fun)
  )

nested_data = 
  nested_data |> 
  dplyr::mutate(
    mape_snaive = furrr::future_map2(.x = snaive_fct, .y = test_demand, .f = forecast::accuracy) |>
      furrr::future_map_dbl(.f = `[[`, c(5))
  )

overall_mape = nested_data |> dplyr::group_by(symbol) |> dplyr::summarise(mean_mape = mean(mape_snaive))

overall_mape


future::plan(strategy = 'sequential')
