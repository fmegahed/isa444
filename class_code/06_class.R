# Class 06

# I will pull crypto data for 2023

crypto = tidyquant::tq_get(
  x = c('BTC-USD', 'ADA-USD'),
  from = '2023-01-01',
  to = '2023-02-07'
) |> 
  dplyr::select(symbol, date, adjusted)

# The ask to create a naive forecast for each symbol

crypto_df = 
  crypto |> 
  dplyr::group_by(symbol) |> 
  dplyr::mutate(
    naive_fct = dplyr::lag(adjusted, n = 1)
  )

crypto_df |> dplyr::slice_head(n = 3)
