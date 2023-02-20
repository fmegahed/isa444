

# * Crypto Today ----------------------------------------------------------

cardano = 
  tidyquant::tq_get(
    'ADA-USD', from = '2020-01-01', to = '2023-02-20') |> 
  dplyr::select(date, adjusted) |> 
  dplyr::mutate(
    ma7 = zoo::rollmeanr(adjusted, k = 7, fill = NA),
    ma7_f = dplyr::lag(ma7)
  )
tail(cardano, 10)



# * Table 3.3 Example -----------------------------------------------------

# data from columns 1 and 2
sales_tbl = tibble::tibble(
  time = 1:12, 
  sales = c(5, 6, 7, 8, 7, 6, 5, 6, 7, 8, 7, 6)
)

# a quick plot to visualize the data
sales_tbl |>  
  # setting the canvas
  ggplot2::ggplot(ggplot2::aes(x= time, y = sales)) + 
  # lines with dots shown
  ggplot2::geom_line() + 
  ggplot2::geom_point() + 
  # cleaning up the chart (x_axis pretty and black & white theme)
  ggplot2::scale_x_continuous( breaks = scales::pretty_breaks(12) ) + 
  ggplot2::theme_bw() 


ses_fit = forecast::ses(
  # a time-series or a numeric vector of interest
  # I used the $ to extract a numeric column from my tibble
  # I converted to a ts using the base/stats R function ts()
   y = ts(sales_tbl$sales, start = 1, frequency = 1),
   # h is the number of time periods that you want to forecast
   h = 3,
   level = 95, # I want to have a 95% PI
   alpha = 0.3, # set to 0.3 to replicate the example in the book
   # initial takes one of two values:
   # simple: if specifying alpha, then this would lead to l0 = your first obs
   # optimal (default): optimize the l0 to reduce your forecasting error
   initial = 'simple'
)

# similar to the meanf() output --> ses_fit contains a forecast object that has 10 sublists
names(ses_fit)
ses_fit$fitted  # fitted/smoothed data whose length = length of your time series
ses_fit$mean # should have 3 values since I specified h = 3

summary(ses_fit)
forecast::accuracy(ses_fit)

# Exclude the first observation from the calculation
# this will have some impact on short time series data
# for longer time-series the impact will be minimal
forecast::accuracy(
  object = ses_fit$fitted[2:12],
  x = ses_fit$x[2:12]
)


# * Training and Validation Samples ---------------------------------------

wfj_sales = 
  readxl::read_excel('data/WFJ_sales.xlsx') |>
  dplyr::select(c(1,2))

# Step 1: Training Data (n = 26 based on the book example)
train_data = wfj_sales$`WFJ Sales`[1:26]

ses_fit_wfj = forecast::ses(
  y = ts(train_data, start = 1, frequency = 1),
  h = 10, # does not matter because all we need is the optimal value for alpha
  level = 95,
  initial = 'optimal' # optimize for alpha and l0 (given that I did not provide them)
)

# let us get the optimal alpha

# approach 1: approximate
summary(ses_fit_wfj) # prints optimized values, accuracy metrics, and your forecasts
alpha_opt1 = 0.727 # up to the third decimal point

# approach 2: we will pull it from the model object directly
alpha_opt2 = ses_fit_wfj$model$par['alpha']
alpha_opt3 = ses_fit_wfj$model$par[1]

train_metrics = forecast::accuracy(ses_fit_wfj)

# Step 2: Let us fit the ses over the entire dataset
wfj_sales =
  wfj_sales |> 
  dplyr::mutate(
    # the ses function returns a list
    # we want the fitted values so we use the extract2 [['']]
    # to convert the fitted sublist into a numeric column
    ses_fitted = forecast::ses(
      y = `WFJ Sales`, h = 10, level = 95, 
      initial = 'simple',
      alpha = alpha_opt2
    ) |> magrittr::extract2('fitted')
  )

# Step 3: We will focus on the validation data
# in plain English, we cannot use the first 26 observations since we used them to opt alpha
# we want to have a fair idea of how our model works

valid_data = wfj_sales[27:62, ]

valid_results = forecast::accuracy(
  object = valid_data$ses_fitted,
  x = valid_data$`WFJ Sales`
)

train_metrics

