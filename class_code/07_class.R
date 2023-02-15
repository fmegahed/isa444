

# * Step 0 ----------------------------------------------------------------

# using the tq_get function from tidyquant to get stock data
# note that in the class example (I unintentionally did not include the crypto
# you can add them here)
stocks_crypto =
  tidyquant::tq_get(x = c('DIS', 'HMC', 'GOOGL', 'NFLX', 'GE'),
                    from = '2018-01-01',
                    to = '2023-02-12') 

# showing you that the pipe is equivalent of passing the output from the previous
# function to dplyr and we selected the three columns in line 16
stocks_crypto = dplyr::select(stocks_crypto,
                              symbol, date, adjusted)

# get the macroeconomic data and note that the get argument was changed from its default
# to economic data since the FRED data is from the FRED API
macro = tidyquant::tq_get(x = c('GNP', 'UNRATE', 'RHORUSQ156N'),
                          from = '2018-01-01',
                          to = '2023-02-12',
                          get = 'economic.data')

# change the third column (used to be adjusted) to price
colnames(stocks_crypto)[3] = 'price'

# options to put them on top of each other
# rbind() --> base R (no pkgs are needed)
# dplyr::bind_rows() --> it gives me a tibble (a nicer df in printing)

all_data = dplyr::bind_rows(stocks_crypto, macro) # they have to same num cols and same colnames




# * Step 1 ----------------------------------------------------------------

# a custom function (possibly more generalized) from the one we introduced in Class 05
MAD = function(x){
  mean( abs( x - mean(x, na.rm = T) ), na.rm = T) # mean abs deviation
  # ( x - mean(x, na.rm = T) ) |> abs() |> mean(na.rm = T)
}

# How many observations do we have per each symbol? 
# What conclusions can you make pertaining to the number of observations per each symbol? 
# (e.g., freq, missing data, etc.)

# Approach 1: Completely lazy (and inefficient approach) -- use filter for each symbol

# Approach 2: Try to count the number of observations per group (group first, count later)

# grouping is needed so that we do not count across the different symbols
all_data_grouped = dplyr::group_by(all_data, symbol)

dplyr::count(all_data_grouped)


#Compute the mean absolute deviation for each time-series.

mad_result = all_data_grouped |> 
  dplyr::summarise(
    mad_price = MAD(x = price)
  ) # summary tables for grouped data

mad_result

# Plot all the different time-series.

all_data |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = price, color = symbol, group = symbol)) +
  ggplot2::geom_line() +
  # free_y allows different y scales and ncol = 2 can be changed depending on the number of stocks
  ggplot2::facet_wrap(facets = ~symbol, scales = 'free_y', ncol = 2) +
  # removing the legend since we have every symbol in its own panel
  ggplot2::theme(legend.position = 'none') 


# * Transformations -------------------------------------------------------

all_data_grouped = 
  all_data_grouped |> 
  dplyr::mutate(price_chng_perc = 100*(price - dplyr::lag(price))/dplyr::lag(price) )



# * Naive Forecast --------------------------------------------------------

# my prediction for today is what happened yesterday (nonseasonal)

all_data_grouped = 
  dplyr::mutate(
    all_data_grouped,
    # naive forecast for o and t series
    naive_o =  dplyr::lag(price),
    naive_t = dplyr::lag(price_chng_perc),
    
    # errors
    e_o = price - naive_o, # actual - forecast 
    pe_o = 100*e_o/price # percent error = error/actual
    )

all_data_grouped |> 
  dplyr::summarise(
    me_o = mean(e_o, na.rm = T),
    mad_o = mean( abs(e_o), na.rm = T),
    mape_o = mean( abs(pe_o), na.rm = T)
  )
