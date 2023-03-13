

# * Getting bitcoin data --------------------------------------------------

btc = tidyquant::tq_get(x = 'BTC-USD', from = '2020-12-31', to = '2023-03-01') |> 
  dplyr::select(symbol, date, adjusted) |> 
  # we will look at the perc_diff; this will make our mean and variance more stable
  dplyr::mutate(perc_diff = 100*(adjusted - dplyr::lag(adjusted))/dplyr::lag(adjusted) )

btc |> ggplot2::ggplot(ggplot2::aes(x = date, y = perc_diff)) + ggplot2::geom_line()

plotly::ggplotly() # interactive

# remove the first value since perc_diff cannot be computed
btc = btc[-1, ]

0.8*790


# * Rolling Origin Intro ------------------------------------------------

# nesting will help with understanding the rolling origin
rolled_data = 
  rsample::rolling_origin(
    data = btc, 
    initial = 632, # 80% of my data (there are no single correct choices for this)
    assess = 1 # we want to predict only one period ahead
  )

# printing the data to get a sense of the structure
rolled_data
rolled_data$splits[[2]]

# analysis and assessment that can be helpful in extracting the data
rsample::analysis(rolled_data$splits[[1]]) # this will show me the data in my first split
# not surprising that we have n = 632 (# based on initial = 632)
rsample::assessment(rolled_data$splits[[1]])



# * Extracting the columns of interest ------------------------------------

rolled_data = rolled_data |> 
  dplyr::mutate(
    # extract your training data
    train_data = purrr::map(.x = splits, .f = rsample::analysis),
    # extract my training dates and response of interest
    train_dates = purrr::map(.x = train_data, .f = magrittr::extract2, 'date'),
    train_resp = purrr::map(.x = train_data, .f = magrittr::extract2, 'perc_diff'),
    
    # extract our test/assessment/evaluation data
    test_data = purrr::map(.x = splits, .f = rsample::assessment),
    test_dates = purrr::map(.x = test_data, .f = magrittr::extract2, 'date') |> 
      purrr::map_dbl(.f = magrittr::extract2, 1) |> lubridate::as_date(),
    test_resp = purrr::map_dbl(.x = test_data, .f = magrittr::extract2, 'perc_diff')
  )


# saving the rolled_data into a smaller object
results_df = rolled_data |> 
  dplyr::select(-c(splits, train_data, test_data))


# let us fit a naive forecast, ma7, and a SES_optimized for each split

last_lag = function(x){
  tail(x, n = 1)
}

results_df = results_df |> 
  dplyr::mutate(
    naive_fct = purrr::map_dbl(.x = train_resp, .f = last_lag),
    naive_fct2 = purrr::map(.x = train_resp, .f = forecast::naive, h = 1) |> 
      purrr::map_dbl(.f = magrittr::extract2, 'mean'),
    
    # initial = 'simple' if you are worried about overfitting
    # given that I did not provide an alpha, I will be optimizing for it
    ses_fct = purrr::map(.x = train_resp, .f = forecast::ses, h = 1, initial = 'simple') |> 
      purrr::map_dbl(.f = magrittr::extract2, 'mean')
  )

# do not do this externally (we are doing it in class since the ses took some time to run)


ma7_fct = function(x){
  tail(x, n = 7) |> mean() 
}

results_df = results_df |> 
  dplyr::mutate(
    ma7_fct = purrr::map_dbl(.x = train_resp, .f = ma7_fct) 
  )

# for a single symbol, the choice of method is easy
forecast::accuracy(object = results_df$naive_fct, x = results_df$test_resp)
forecast::accuracy(object = results_df$ses_fct, x = results_df$test_resp)
forecast::accuracy(object = results_df$ma7_fct, x = results_df$test_resp)


summary_df = results_df |> 
  tidyr::pivot_longer(cols = 6:9, names_to = 'model', values_to = 'forecast') |> 
  dplyr::select(test_dates, test_resp, model, forecast) |> 
  dplyr::group_by(model) |> 
  dplyr::summarise(
    acc_metrics = forecast::accuracy(object = forecast, x = test_resp)
  )
