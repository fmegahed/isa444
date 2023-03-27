# getting the gnp data for our example
# slightly different from the data shown in the slide (since the data from the slide 
# started in 2013-01-01)
gnp = tidyquant::tq_get(x = 'GNP', get = 'economic.data',
                        from = '2000-01-01')

# line chart showing that the data has an increasing trend
gnp |> ggplot2::ggplot(ggplot2::aes(x = date, y = price)) + ggplot2::geom_line()

# acf showing that the acf does not die down and does not cut off
# it shows a random walk pattern
acf(x = gnp$price) # will print acf at lag 0 (always 1)
acf(x = gnp$price) |> forecast::autoplot() +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 25))# will not print the acf at lag 0

acf_results = acf(x = gnp$price) # will print acf at lag 0 (always 1)
acf_results$acf

# adf test has a large p-value, i.e., we will not reject the null of nonstationary ts
tseries::adf.test(x = gnp$price)

# kpss test
# reject the null hypothesis (stationary ts) and conclude that our time-series is nonstationary
# since the p-value is 0.01 (defaults to comparing it to 0.05)
tseries::kpss.test(x = gnp$price)


# successive implementations of the kpss test
forecast::ndiffs(gnp$price)
# 1 difference is needed to bring the gnp$price to stationarity