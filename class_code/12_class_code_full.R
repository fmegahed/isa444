


if(require(pacman) == FALSE) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, fpp2, magrittr, scales)




# * The LES Example -------------------------------------------------------
pacman::p_load(readxl)
download.file(url = 'https://github.com/fmegahed/businessForecasting/blob/master/assignments/WFJ_sales.xlsx?raw=true', destfile = 'WFJ_sales.xlsx', mode = 'wb')
WFJ = read_excel("WFJ_sales.xlsx") %>% select(c(1,2))

WFJ %>% ggplot(aes(x = Obs, y = `WFJ Sales`)) +
  geom_line() + geom_point(size = 2)

# The goal here is to figure out an optimal alpha based on a training sample
# of 26 observations

train_data = WFJ[1:26, 1:2]
train_data


ses_fit_opt = ses(y = train_data$`WFJ Sales`, level = 95, initial = 'optimal')
summary(ses_fit_opt) # I can extract my alpha (from the printout 0.727)
alpha_opt = ses_fit_opt$model$par['alpha']


# to use it for the validation data
## our approach would be to first use the alpha with 'simple' for the entire data
## and then we will cut the data for the validation sample (reduce the impact
## of your initial values on your test data)

WFJ %<>% mutate(fitted = ses(y = `WFJ Sales`, h = 10, level = 95,
                             initial = 'simple', alpha = alpha_opt) %>% 
                  .[['fitted']] ) 

valid_data = WFJ[27:62, ]
valid_data

results = rbind(
  accuracy(ses_fit_opt), # the accuracy metrics from my training data
  c( accuracy(object = valid_data$fitted, x = valid_data$`WFJ Sales`),
     NA, NA)
)
# or alternatively just extracting the first 5 values from the accuracy
results_short = rbind(
  accuracy(ses_fit_opt)[1:5], # the accuracy metrics from my training data
  accuracy(object = valid_data$fitted, x = valid_data$`WFJ Sales`)
)
# making the output easier to read (optional)
row.names(results_short) = c('Training', 'Validation')
results_short



# * The Weekly Therm Sales Example ----------------------------------------

weekly_therm = read_excel(path = "data/Weekly_Therm_Sales.xlsx")

weekly_therm %>% ggplot(aes(x = Time, y = WeeklyThermSales)) + geom_line()

les_model = holt(y = weekly_therm$WeeklyThermSales, h = 10, level = 95,
                 initial = "optimal", alpha = 0.2, beta = 0.1)

weekly_therm$fitted_values = les_model$fitted

# Slide 14 from the notes with the addition of the fitted values
autoplot(les_model) + autolayer(fitted(les_model))

# Slide 15
weekly_therm

# Slide 16
accuracy(les_model)


# Along the same lines
ses_fit = ses(y = weekly_therm$WeeklyThermSales, h = 10, level = 95,
           initial = "optimal", alpha = 0.2)
accuracy(ses_fit)
autoplot(ses_fit) + autolayer(fitted(ses_fit))



# * LES Practice  ---------------------------------------------------------

les_wfj_fit = holt(train_data$`WFJ Sales`)
accuracy(les_wfj_fit)

alpha = les_wfj_fit$model$par['alpha']
beta = les_wfj_fit$model$par['beta']

les_total = holt(y = WFJ$`WFJ Sales`, alpha = alpha, beta = beta)

WFJ$fitted_holt = les_total$fitted

valid_data = WFJ[27:62, ]

accuracy(object = valid_data$fitted_holt, x = valid_data$`WFJ Sales`)
