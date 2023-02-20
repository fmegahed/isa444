# nonseasonal smoothing


# * Getting the Data ------------------------------------------------------

usdc = tidyquant::tq_get(
  x = 'usdc-USD', from = '2019-01-01', to = '2023-02-01',
  periodicity = 'monthly'
)

usdc = 
  usdc |> 
  dplyr::mutate(
    overall_avg = mean(adjusted),
    
    # smoothing_based methods
    c_mean_s = dplyr::cummean(adjusted),
    # k = 3 for using the last three obs, fill = NA assigns NA for the first few obs
    ma3_s = zoo::rollmeanr(x = adjusted, k = 3, fill = NA),
    ma7_s = zoo::rollmeanr(x = adjusted, k = 7, fill = NA),
    
    # forecast
    naive_f = dplyr::lag(adjusted),
    c_mean_f = dplyr::lag(c_mean_s),
    ma3_f = dplyr::lag(ma3_s),
    ma7_f = dplyr::lag(ma7_s)
  )


forecast_metrics = rbind(
  forecast::accuracy(object = usdc$naive_f, x = usdc$adjusted),
  forecast::accuracy(object = usdc$c_mean_f, x = usdc$adjusted),
  forecast::accuracy(object = usdc$ma3_f, x = usdc$adjusted),
  forecast::accuracy(object = usdc$ma7_f, x = usdc$adjusted)
)

row.names(forecast_metrics) = c('Naive', 'Cumulative Mean', 'MA3', 'MA7')
forecast_metrics
