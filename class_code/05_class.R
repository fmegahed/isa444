


# * Can you delete date? --------------------------------------------------

# Yes, you do not have to.

gme_get = 
  tidyquant::tq_get(x = 'GME', from = '2020-01-01') |> 
  dplyr::select(date, symbol, adjusted) |> 
  dplyr::mutate(
    year = lubridate::year(date), 
    month = lubridate::month(date, label = T)
  ) |> # if you wanted to delete date
  dplyr::select(-c(date))

  

# printing out what our data would look like without year
gme_get |> 
  dplyr::group_by(symbol, month) |>
  dplyr::summarise( 
    ajusted_avg = mean(adjusted), 
    adjusted_med = median(adjusted), 
    adjusted_var = var(adjusted), 
    adjusted_sd = sd(adjusted) 
  )



# * Our Own Mad Function --------------------------------------------------

# our own mean absolute deviation function

# in R, each function can return only one object
# that can be vector, list, data.frame
# return it by printing it, or by using the return
MAD = function(x){
  mean( abs(x - mean(x) ) )
}



# * Slide 21 --------------------------------------------------------------

q1 = (225 - 325) / 325

q2 = 225 * (1-q1)




# * Live Demo -------------------------------------------------------------

crypto = tidyquant::tq_get(
  x = c('ADA-USD', 'LINK-USD', 'ZIL-USD'),
  from = '2023-01-01',
  to = '2023-02-05'
)

# we will compute the growth rate, natural log, log diff, scale

# our first change would be to group the data such that differences are 
# based on a specific coin

scale01 = function(x){
 (x - min(x) ) / ( max(x) - min(x) )
}

scale01(1:10) # a quick check where we can do mental math

crypto = 
  crypto |> 
  dplyr::group_by(symbol) |> 
  dplyr::mutate(
    # natural log
    log_adj = log(adjusted),
    # scales the adjusted for each coin
    scaled_adj = scale01(adjusted),
    # log diff
    log_diff = log_adj - log( dplyr::lag(adjusted) ),
    # growth rate
    growth = (adjusted - dplyr::lag(adjusted) ) / dplyr::lag(adjusted)
  ) 