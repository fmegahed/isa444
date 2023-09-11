

# * Highlight ts objects in R ---------------------------------------------

air_p = AirPassengers
air_p

year_1 = air_p[1:12] # using traditional indexing will convert the ts into a num vec
year_1_ts = window(air_p, start = c(1949, 1), end = c(1949,12))


# ggplot (I need to have an x and a y)
x_vec = time(air_p) # convert the index for the time-series to a numeric variable that you can use for plotting

proper_date = seq.Date(
  from = lubridate::ymd('1949-01-01'), to = lubridate::ymd('1960-12-01'), 
  by = '1 month'
  )

airpass_df = data.frame(date = x_vec, proper_date, num_pass = air_p)

# to leverage the fact that we have a proper date in case you
# did not like the decimals
# the decimals represent (period_num - 1)/num_periods_per_year
ggplot2::ggplot(data = airpass_df,
                ggplot2::aes(x = proper_date, y = num_pass)) +
  ggplot2::geom_line() +
  ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 12))


