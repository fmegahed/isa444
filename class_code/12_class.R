

# * Decomposition ---------------------------------------------------------

bike = readxl::read_excel("data/bike_sales_R.xlsx")

bike_ts = ts(bike$`Bike Sales`, start = 1, frequency = 4)
# input to the stats decompose function should be a ts

bike_decomposed = decompose(
  x = bike_ts, type = 'additive'
)

names(bike_decomposed)

forecast::autoplot(bike_decomposed) + 
  ggplot2::theme_bw()


# * * Some Insights into the Decomposition Process ------------------------

# Step 1: Estimate the trend based on the CMA
bike_decomposed$trend

# Step 2: De-Trend the TS (Extracting the Trend from the TS [ Additive] )
detrend = bike_ts - bike_decomposed$trend

# Step 3: Estimate the initial values for your seasonal factor
qtr1 = c(-15.000, -14.625, -14.375) |> mean()

# lets do this in 1-2 steps
detrend_matrix = matrix(detrend, nrow = 4) |> t() # to make it look like the detrend ts
initial_seasonal_factors = colMeans(detrend_matrix, na.rm = T)

# Step 4: Standardize these values
st_seasonal_factor = initial_seasonal_factors  - mean(initial_seasonal_factors)
bike_decomposed$figure # this is equal to the st_seasonal_factor above

# Step 5: Errors
errors = detrend - st_seasonal_factor
bike_decomposed$random # this equal to the errors above




# * HW --------------------------------------------------------------------

# with seasonal data, you have to have a ts object (bec this is the only
# way where you can define the frequency of your data)

# from our previous example, we have the bike_ts as a time series object

hw_add_fit = forecast::hw(y = bike_ts,
                          seasonal = 'additive',
                          level = c(80,95),
                          alpha = 0.2,
                          beta = 0.1,
                          gamma = 0.1,
                          initial = 'simple')

hw_mult_fit = forecast::hw(y = bike_ts,
                          seasonal = 'multiplicative',
                          level = 95,
                          alpha = 0.2,
                          beta = 0.1,
                          gamma = 0.1,
                          initial = 'simple')
# plot
forecast::autoplot(hw_add_fit) + forecast::autolayer(hw_add_fit$fitted)

# you could be wrong, thinking that you have an additive seasonality
# when it is multiplicative

results = rbind(
  forecast::accuracy(hw_add_fit),
  forecast::accuracy(hw_mult_fit)
)

row.names(results) = c('Add', 'Mult')  

results








