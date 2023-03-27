
last_lag = function(x){
  tail(x, n = 1)
}

ma7_fct = function(x){
  tail(x, n = 7) |> mean() 
}

# * Assignment 11 Solution ------------------------------------------------

crypto = c('BTC-USD', 'ETH-USD')

ticker = 'BTC-USD'
train_size = 0.8
  
rolling_origin_many = function(
    ticker = 'BTC-USD', train_size = 0.8, response = c('adjusted', 'perc_diff', 'log')){
  
  ticker_df = tidyquant::tq_get(x = ticker, from = '2020-12-31', to = '2023-03-01') |> 
    dplyr::select(symbol, date, adjusted) |> 
    # we will look at the perc_diff; this will make our mean and variance more stable
    dplyr::mutate(
      log_adj = log(adjusted),
      perc_diff = 100*(adjusted - dplyr::lag(adjusted))/dplyr::lag(adjusted) 
      )
  
  ticker_df = ticker_df[-1, ]
  
  rolled_data = 
    rsample::rolling_origin(
      data = ticker_df, 
      initial = ceiling(train_size*nrow(ticker_df)), # 80% of my data (there are no single correct choices for this)
      assess = 1 # we want to predict only one period ahead
    )
  
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
  
  results_df = rolled_data |> 
    dplyr::select(-c(splits, train_data, test_data))
  
  results_df = results_df |> 
    dplyr::mutate(
      naive_fct = purrr::map_dbl(.x = train_resp, .f = last_lag),
      
      # initial = 'simple' if you are worried about overfitting
      # given that I did not provide an alpha, I will be optimizing for it
      ses_fct = purrr::map(.x = train_resp, .f = forecast::ses, h = 1, initial = 'simple') |> 
        purrr::map_dbl(.f = magrittr::extract2, 'mean')
    )
  
  results_df = results_df |> 
    dplyr::mutate(
      ma7_fct = purrr::map_dbl(.x = train_resp, .f = ma7_fct) 
    )
  
  summary_df = results_df |> 
    tidyr::pivot_longer(cols = 6:8, names_to = 'model', values_to = 'forecast') |> 
    dplyr::select(test_dates, test_resp, model, forecast) |> 
    dplyr::group_by(model) |> 
    dplyr::summarise(
      symbol = ticker,
      mape = forecast::accuracy(object = forecast, x = test_resp) |> magrittr::extract(5)
    )
  
  return(summary_df)
}

mult_summaries = purrr::map_df(
  .x = crypto,
  .f = rolling_origin_many,
  train_size = 0.99
)

summaries_wide = tidyr::pivot_wider(data = mult_summaries, names_from = model, values_from = mape)

# avg mape
summaries_wide |> 
  dplyr::summarise(
    ma7_avg = mean(ma7_fct),
    naive_avg = mean(naive_fct),
    ses_avg = mean(ses_fct)
  )

# count the number of wins
summaries_wide = summaries_wide |> 
  dplyr::group_by(symbol) |> 
  dplyr::mutate(
    winner = which.min( dplyr::c_across(ma7_fct:ses_fct) )
  )

table(summaries_wide$winner) |> prop.table()




# * Graduate School Stats Hack --------------------------------------------
set.seed(2023)
df = data.frame(wt = rnorm(100000))

df = df |> 
  dplyr::mutate(
    yt = zoo::rollmean(wt, k = 3, fill = NA),
    y1= dplyr::lag(yt, n = 1),
    y2= dplyr::lag(yt, n = 2),
    y3= dplyr::lag(yt, n = 3),
    y4= dplyr::lag(yt, n = 4)
  )

df_full = na.omit(df)

cor(df_full$yt, df_full$y1)
cor(df_full$yt, df_full$y2)
cor(df_full$yt, df_full$y3)

var(df$yt, na.rm = T)
mean(df$yt, na.rm = T)



# * ACF -------------------------------------------------------------------

WFJ = readxl::read_excel(
  "data/WFJ_sales.xlsx") |> 
  dplyr::select(1,2)

acf_results = acf(x = WFJ$`WFJ Sales`)

names(acf_results)
acf_results$acf


WFJ$Lag1 = dplyr::lag(WFJ$`WFJ Sales`, n =1)
# traditional lm model
model = lm(data = WFJ, formula = `WFJ Sales` ~ Lag1 )

summary(model)


acf_results$acf[2]^2 # approximately equal to your r2 which is an expected result



