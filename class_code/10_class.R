

# * LES on WFJ Sales ------------------------------------------------------

wfj_sales = 
  readxl::read_excel('data/WFJ_sales.xlsx') |> 
  dplyr::select( c(1,2) ) # keeps only the first two columns

train_data = wfj_sales[1:26, ] # per the book example we keep the first 26 rows for training

# if we will create a new column of fitted values -- we need to have a numeric vector out of
# our holt()

# Step 2 extract alpha, beta and training metrics from the training model
holt_fit = forecast::holt(
  y = train_data$`WFJ Sales` |> ts(start = 1, frequency = 1),
  h = 1, # does not matter for training
  level = 95, #95% PI
  initial = 'optimal' # optimize also for the starting values
)

names(holt_fit)

alpha_opt = holt_fit$model$par['alpha']
beta_opt = holt_fit$model$par['beta']

summary(holt_fit) # will print  alpha, beta, starting values, accuracy metrics as your forecasts


# Step 3 apply it to your entire dataset

wfj_sales = 
  wfj_sales |> 
  dplyr::mutate(
    holt_f_opt = forecast::holt(
      y = ts(`WFJ Sales`, start = 1, frequency = 1),
      h = 10, 
      level = 95,
      # optimized smoothing parameters from training
      alpha = alpha_opt, beta = beta_opt,
      initial = 'simple' # we will not reoptimize for your starting values
    ) |> magrittr::extract2('fitted'),
    
    ma8_f = zoo::rollmeanr(`WFJ Sales`, k = 8, fill = NA) |> dplyr::lag()
  )

valid_data = wfj_sales[27:62, ]

valid_metrics = rbind(
  forecast::accuracy(object = valid_data$holt_f_opt, x = valid_data$`WFJ Sales`),
  forecast::accuracy(object = valid_data$ma8_f, x = valid_data$`WFJ Sales`)
)

valid_metrics

row.names(valid_metrics) = c('Holt', 'MA8')

valid_metrics
