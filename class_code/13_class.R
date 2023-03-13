

# * Comparing Multiple Methods for a Single TS ----------------------------

macro = 'GNP' # this is the macro indicator of interest, we can change it

data = tidyquant::tq_get(
  x = macro,
  from = '1947-01-01',
  get = 'economic.data'
)

# let us add year, quarter, and month to the dataset

data = 
  data |> 
  dplyr::mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date),
    month = lubridate::month(date, label = F)
  )

# We will try to automatically divide our data into a training and validation
# sample; we will also try to automatically get the frequency

length_ts = nrow(data)
train_size = ceiling(0.8*length_ts)

years = unique(data$year) # a vector with one value per year

# has an assumption that I have at least 2 years of worth of data
# and 3 years to ensure that the freq is computed correctly
auto_freq = data |> dplyr::filter(year == years[2]) |> nrow()

# lets make a ts object that has the correct starting point, frequency, etc

# Challenge (if I want to have an accurate rep of the data)
# if your freq = 1, starting value is the first year
# if your freq = 4, starting value is year, quarter
# if your freq = 12, starting value is year and month

if(auto_freq == 1) starting_value = data$year[1]
if(auto_freq == 4) starting_value = c(data$year[1], data$quarter[1])
if(auto_freq == 12) starting_value = c(data$year[1], data$month[1])


train_ts = ts(
  data = data$price[1:train_size],
  start = starting_value,
  frequency = auto_freq
)

all_ts = ts(
  data = data$price, # note that we dropped the index to use all observations
  start = starting_value,
  frequency = auto_freq
)


# fit holt (or any other model you want to get opt paramaters)
train_holt = forecast::holt(
  y = train_ts, initial = 'simple'
)

alpha_opt1 =  train_holt$model$par['alpha']
beta_opt1 =  train_holt$model$par['beta']

# use optimal parameters and fit all non-optimized models to your entire dataset

data = data |> 
  dplyr::mutate(
    naive_fct = dplyr::lag(price, n = 1),
    seasonal_naive = dplyr::lag(price, n = auto_freq),
    
    ma7_f = zoo::rollmeanr(x = price, k =7, fill = NA) |> dplyr::lag(),
    
    holt_opt = forecast::holt(y = all_ts, h = 3*auto_freq, 
                              alpha = alpha_opt1, beta = beta_opt1,
                              initial = 'simple') |> 
      magrittr::extract2('fitted'),
    
    hw_predef = forecast::hw(y = all_ts, h = 3*auto_freq, 
                             alpha = 0.2, beta = 0.1, gamma = 0.1,
                             initial = 'simple') |>  
      magrittr::extract2('fitted')
  )

# let us get only the validation data
valid_data = data[(train_size+1):nrow(data), ] # , empty after comma means use all cols 

# validation metrics
acc_metrics = rbind(
  forecast::accuracy(object = valid_data$naive_fct, x = valid_data$price),
  forecast::accuracy(object = valid_data$seasonal_naive, x = valid_data$price),
  forecast::accuracy(object = valid_data$ma7_f, x = valid_data$price),
  forecast::accuracy(object = valid_data$holt_opt, x = valid_data$price),
  forecast::accuracy(object = valid_data$hw_predef, x = valid_data$price)
)
row.names(acc_metrics) = c('Naive', 'Seasonal Naive', 'MA7', 'Holt', 'HW')

acc_metrics

# tidy acc metrics such that the model is a row, add symbol
acc_metrics = data.frame(acc_metrics) |> 
  tibble::rownames_to_column(var = 'model') |> 
  dplyr::mutate( symbol = unique(data$symbol) ) |> 
  dplyr::select(symbol, dplyr::everything())





# * Extrapolating to Many TS ----------------------------------------------

fixed_window_many_ts = function(macro_input){
  
  data = tidyquant::tq_get(
    x = macro_input,
    from = '1947-01-01',
    get = 'economic.data'
  )
  
  # let us add year, quarter, and month to the dataset
  
  data = 
    data |> 
    dplyr::mutate(
      year = lubridate::year(date),
      quarter = lubridate::quarter(date),
      month = lubridate::month(date, label = F)
    )
  
  # We will try to automatically divide our data into a training and validation
  # sample; we will also try to automatically get the frequency
  
  length_ts = nrow(data)
  train_size = ceiling(0.8*length_ts)
  
  years = unique(data$year) # a vector with one value per year
  
  # has an assumption that I have at least 2 years of worth of data
  # and 3 years to ensure that the freq is computed correctly
  auto_freq = data |> dplyr::filter(year == years[2]) |> nrow()
  
  # lets make a ts object that has the correct starting point, frequency, etc
  
  # Challenge (if I want to have an accurate rep of the data)
  # if your freq = 1, starting value is the first year
  # if your freq = 4, starting value is year, quarter
  # if your freq = 12, starting value is year and month
  
  if(auto_freq == 1) starting_value = data$year[1]
  if(auto_freq == 4) starting_value = c(data$year[1], data$quarter[1])
  if(auto_freq == 12) starting_value = c(data$year[1], data$month[1])
  
  
  train_ts = ts(
    data = data$price[1:train_size],
    start = starting_value,
    frequency = auto_freq
  )
  
  all_ts = ts(
    data = data$price, # note that we dropped the index to use all observations
    start = starting_value,
    frequency = auto_freq
  )
  
  
  # fit holt (or any other model you want to get opt paramaters)
  train_holt = forecast::holt(
    y = train_ts, initial = 'simple'
  )
  
  alpha_opt1 =  train_holt$model$par['alpha']
  beta_opt1 =  train_holt$model$par['beta']
  
  # use optimal parameters and fit all non-optimized models to your entire dataset
  
  data = data |> 
    dplyr::mutate(
      naive_fct = dplyr::lag(price, n = 1),
      seasonal_naive = dplyr::lag(price, n = auto_freq),
      
      ma7_f = zoo::rollmeanr(x = price, k =7, fill = NA) |> dplyr::lag(),
      
      holt_opt = forecast::holt(y = all_ts, h = 3*auto_freq, 
                                alpha = alpha_opt1, beta = beta_opt1,
                                initial = 'simple') |> 
        magrittr::extract2('fitted'),
      
      hw_predef = forecast::hw(y = all_ts, h = 3*auto_freq, 
                               alpha = 0.2, beta = 0.1, gamma = 0.1,
                               initial = 'simple') |>  
        magrittr::extract2('fitted')
    )
  
  # let us get only the validation data
  valid_data = data[(train_size+1):nrow(data), ] # , empty after comma means use all cols 
  
  # validation metrics
  acc_metrics = rbind(
    forecast::accuracy(object = valid_data$naive_fct, x = valid_data$price),
    forecast::accuracy(object = valid_data$seasonal_naive, x = valid_data$price),
    forecast::accuracy(object = valid_data$ma7_f, x = valid_data$price),
    forecast::accuracy(object = valid_data$holt_opt, x = valid_data$price),
    forecast::accuracy(object = valid_data$hw_predef, x = valid_data$price)
  )
  row.names(acc_metrics) = c('Naive', 'Seasonal Naive', 'MA7', 'Holt', 'HW')
  
  acc_metrics
  
  # tidy acc metrics such that the model is a row, add symbol
  acc_metrics = data.frame(acc_metrics) |> 
    tibble::rownames_to_column(var = 'model') |> 
    dplyr::mutate( symbol = unique(data$symbol) ) |> 
    dplyr::select(symbol, dplyr::everything())
  
  return(acc_metrics)
  
}

macros = c('GNP', 'TOTALSA')

results_many_ts = purrr::map_df(
  .x = macros,
  .f = fixed_window_many_ts
)


# * Map 2 vs Map ----------------------------------------------------------


fixed_window_many_ts_either = function(input, type){
  
  data = tidyquant::tq_get(
    x = input,
    from = '1947-01-01',
    get = type
  )
  
  # let us add year, quarter, and month to the dataset
  
  data = 
    data |> 
    dplyr::mutate(
      year = lubridate::year(date),
      quarter = lubridate::quarter(date),
      month = lubridate::month(date, label = F)
    )
  
  # We will try to automatically divide our data into a training and validation
  # sample; we will also try to automatically get the frequency
  
  length_ts = nrow(data)
  train_size = ceiling(0.8*length_ts)
  
  years = unique(data$year) # a vector with one value per year
  
  # has an assumption that I have at least 2 years of worth of data
  # and 3 years to ensure that the freq is computed correctly
  auto_freq = data |> dplyr::filter(year == years[2]) |> nrow()
  
  # lets make a ts object that has the correct starting point, frequency, etc
  
  # Challenge (if I want to have an accurate rep of the data)
  # if your freq = 1, starting value is the first year
  # if your freq = 4, starting value is year, quarter
  # if your freq = 12, starting value is year and month
  
  if(auto_freq == 1) starting_value = data$year[1]
  if(auto_freq == 4) starting_value = c(data$year[1], data$quarter[1])
  if(auto_freq == 12) starting_value = c(data$year[1], data$month[1])
  
  
  train_ts = ts(
    data = data$price[1:train_size],
    start = starting_value,
    frequency = auto_freq
  )
  
  all_ts = ts(
    data = data$price, # note that we dropped the index to use all observations
    start = starting_value,
    frequency = auto_freq
  )
  
  
  # fit holt (or any other model you want to get opt paramaters)
  train_holt = forecast::holt(
    y = train_ts, initial = 'simple'
  )
  
  alpha_opt1 =  train_holt$model$par['alpha']
  beta_opt1 =  train_holt$model$par['beta']
  
  # use optimal parameters and fit all non-optimized models to your entire dataset
  
  data = data |> 
    dplyr::mutate(
      naive_fct = dplyr::lag(price, n = 1),
      seasonal_naive = dplyr::lag(price, n = auto_freq),
      
      ma7_f = zoo::rollmeanr(x = price, k =7, fill = NA) |> dplyr::lag(),
      
      holt_opt = forecast::holt(y = all_ts, h = 3*auto_freq, 
                                alpha = alpha_opt1, beta = beta_opt1,
                                initial = 'simple') |> 
        magrittr::extract2('fitted'),
      
      hw_predef = forecast::hw(y = all_ts, h = 3*auto_freq, 
                               alpha = 0.2, beta = 0.1, gamma = 0.1,
                               initial = 'simple') |>  
        magrittr::extract2('fitted')
    )
  
  # let us get only the validation data
  valid_data = data[(train_size+1):nrow(data), ] # , empty after comma means use all cols 
  
  # validation metrics
  acc_metrics = rbind(
    forecast::accuracy(object = valid_data$naive_fct, x = valid_data$price),
    forecast::accuracy(object = valid_data$seasonal_naive, x = valid_data$price),
    forecast::accuracy(object = valid_data$ma7_f, x = valid_data$price),
    forecast::accuracy(object = valid_data$holt_opt, x = valid_data$price),
    forecast::accuracy(object = valid_data$hw_predef, x = valid_data$price)
  )
  row.names(acc_metrics) = c('Naive', 'Seasonal Naive', 'MA7', 'Holt', 'HW')
  
  acc_metrics
  
  # tidy acc metrics such that the model is a row, add symbol
  acc_metrics = data.frame(acc_metrics) |> 
    tibble::rownames_to_column(var = 'model') |> 
    dplyr::mutate( symbol = unique(data$symbol) ) |> 
    dplyr::select(symbol, dplyr::everything())
  
  return(acc_metrics)
  
}



results_many_ts = purrr::map2_df(
  .x = c('GNP'),
  .y = 'economic.data',
  .f = fixed_window_many_ts_either
)
