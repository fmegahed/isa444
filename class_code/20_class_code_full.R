# Code written in class on April 06, 2022
# Exam II Recap


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (for ndiffs)
               tidyquant, # for getting GNP data
               ggpubr)


# * Question 4 ------------------------------------------------------------

inflation = read.csv('https://tinyurl.com/y56hzetf')

inflation$DATE %<>% ymd() # to convert from char to date + ymd() from lubridate and two-way pipe from magrittr

# we are fitting this intentionally in its own object
# and not within the mutate pipeline because we only care about the 
# alpha_optimal value
# note that this is solely based on the training data of 24 obs
ses_fit = ses(inflation$FPCPITOTLZGUSA[1:24])

inflation %<>% mutate(
  ses1 = ses(y = FPCPITOTLZGUSA, 
             alpha = ses_fit$model$par['alpha']) %>% .[['fitted']],
  ses2 = ses(y = FPCPITOTLZGUSA, 
             alpha = ses_fit$model$par['alpha'], 
             initial = 'simple') %>% extract2('fitted') )

valid_data = inflation[25:28, ]

accuracy(object = valid_data$ses1, x = valid_data$FPCPITOTLZGUSA)
accuracy(object = valid_data$ses2, x = valid_data$FPCPITOTLZGUSA)



# * Question 7 ------------------------------------------------------------

gdp = tidyquant::tq_get('GDP', from = '1947-01-01', to = '2021-12-01', get = 'economic.data')

# from the data, my forecast for Q1 of 2022
# MA(3) -> avg(Q2, Q3, Q4 of 2021)
mean( tail(x = gdp$price, 3) )


# Another way of doing this
gdp %<>% mutate(
  ma3 = rollmeanr(x = price, na.pad = T, k =3),
  fma3 = lag(ma3)
) 

gdp$ma3[300]



# * Question 9 ------------------------------------------------------------
gdp = tidyquant::tq_get('GDP', from = '1947-01-01', to = '2021-12-01', get = 'economic.data')

holt_model = holt(
  y = gdp$price,
  alpha =0.2, beta = 0.1, initial = 'optimal'
)

# common errors that I saw was you did this in mutate and extracted the
# fitted values (fitted values are based on the historical data that you
# used to fit/train your model)
holt_model
holt_model$mean
summary(holt_model)



# * Question 11 -----------------------------------------------------------
pacman::p_load(tidyverse, magrittr, lubridate, zoo, fpp2)

alcohol_sales = tidyquant::tq_get('S4248SM144NCEN', from = '1992-01-01', to = '2021-12-01', get = 'economic.data')

alchohol_sales_ts = ts(data =  alcohol_sales$price,
                       frequency = 12,
                       start = c(1992, 01))

decompose(x = alchohol_sales_ts, type = 'multiplicative') %>% autoplot()


# * Question 12 -----------------------------------------------------------

hw_add = hw(y = alchohol_sales_ts, seasonal = 'additive')
hw_mult = hw(y = alchohol_sales_ts, seasonal = 'multiplicative')

accuracy(hw_add)
accuracy(hw_mult)



# * Question 13 -----------------------------------------------------------

pacman::p_load(tidyverse, magrittr, fpp2)

aut = austourists

autoplot(austourists)

aut
train_data = aut[1:(68-16)] %>% ts(frequency = 4, start = c(1999, 01))
train_data

hw_fit_aut_add = hw(train_data, seasonal = 'additive')
hw_fit_aut_mult = hw(train_data, seasonal = 'multiplicative')

accuracy(hw_fit_aut_add)
accuracy(hw_fit_aut_mult)

# if you were to follow-up with using the multiplicative,
hw_fit_opt = hw(aut, seasonal = 'multiplicative',
                alpha = hw_fit_aut_mult$model$par['alpha'],
                beta = hw_fit_aut_mult$model$par['beta'],
                gamma = hw_fit_aut_mult$model$par['gamma'])

aut_df = data.frame(ts = aut,
                    fitted = hw_fit_opt$fitted)

valid_data_aut = aut_df[53:68, ]
accuracy(object = valid_data_aut$fitted, x = valid_data_aut$ts)



# * Question 17 -----------------------------------------------------------
pacman::p_load(tseries)
?kpss.test

x <- rnorm(1000)  # is level stationary
kpss.test(x)



# * Question 19 -----------------------------------------------------------

beerprod = read.csv("http://course1.winona.edu/bdeppa/FIN%20335/Datasets/AusBeer.csv")

df = ts(beerprod$Beer.Production, frequency = 4)

# approach 1
decomposed1 = decompose(df, type = "additive") %>% autoplot() + theme_bw()

plot_data = decomposed1$data

# approach 2 -- similar to what we did in class
decomposed2 = decompose(df, type = "additive")
decomposed2$figure

# approach 3 - making a ggplot object interactive
# which we also did in class (maybe in a different setting)
plotly::ggplotly()
