log_jj = log(astsa::jj)
# extracting time and quarter
t = time(log_jj) 
q = cycle(log_jj) |> factor() 
model3 = lm(log_jj ~ t + q)
summary(model3)

model3$fitted.values
model3$coefficients

forecast::checkresiduals(model3)


# * Regression with ARIMA errors ------------------------------------------

install.packages('fpp2') # only needed once

uschange = fpp2::uschange
class(uschange)

# how to subset the mts object
uschange[, c('Consumption', 'Income')]

# plotting the data
forecast::autoplot(
  uschange[, c('Consumption', 'Income')], facets = TRUE
) +
  ggplot2::labs(y = "Pct Change by Quarter") +
  ggplot2::theme_bw()

# line 33 will only work if you store it in lectures > 27_arima_reg > resPlotTS.R
source('https://raw.githubusercontent.com/fmegahed/isa444/main/lectures/27_arima_reg/resPlotTS.R')
source('lectures/27_arima_reg/resPlotTS.R')


