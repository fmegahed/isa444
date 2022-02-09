# Code written in class on February 09, 2022
# Introduction to TS Data Visualization and Summarizing TS Data



# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (accuracy fun)
               tidyquant) # for getting the stock data


# * Transformations -------------------------------------------------------

coins = tq_get(x = c('ADA-USD', 'LINK-USD', 'ZIL-USD'),
               from = '2020-11-01',
               to = '2021-02-06') 
glimpse(coins) # a glimpse of the tibble

# two-way pipe -> pass coins to the select function and then overwrite coins
coins %<>% select(symbol, date, adjusted)

coins %<>%
  # group_by ensures that the calculations are by group
  # (e.g., the min is for a given group)
  group_by(symbol) %>%
  # mutate allows you to add calculated columns
  mutate(
    lag_adjusted = lag(adjusted),
    growth_rate =  (adjusted - lag_adjusted) / lag_adjusted,
    log_adjusted = log(adjusted),
    log_diff = log_adjusted - log(lag_adjusted),
    min = min(adjusted),
    max = max(adjusted),
    scaled_adjusted = (adjusted - min)/ (max-min),
    diff1 = adjusted - lag_adjusted,
    diff2 = c(NA, diff(adjusted))
  )



# * Measures of Forecast Accuracy -----------------------------------------

cardano = tq_get("ADA-USD",
                 from = "2022-02-01",
                 to = "2022-02-07") %>% 
  select(date, adjusted)

cardano %<>% mutate(
  naiveFC = lag(adjusted),
  e = adjusted - naiveFC,
  pe = e/adjusted,
  ae = abs(e),
  ape = ae/adjusted
  )

cardano


# * By Hand (in R) Calculations -------------------------------------------
ME = mean(cardano$e, na.rm = T) # I will get NA
MPE = 100*mean(cardano$pe, na.rm = T)
MAE = mean(cardano$ae, na.rm = T)
MAPE = 100*mean(cardano$ape, na.rm = T)
RMSE = mean(cardano$e^2, na.rm = T) %>% sqrt()



# * Using the forecast package to calculate the errors ---------------------
pacman::p_load(fpp2)


forecast::accuracy(object = cardano$naiveFC, # forecasted values go into obj
         x = cardano$adjusted) # actuals go into x

RMSE2 = forecast::accuracy(object = cardano$naiveFC, # forecasted values go into obj
                           x = cardano$adjusted)[2]



# * Multiple TS -----------------------------------------------------------


stocks = tq_get(x = c('AAPL', 'TSLA', 'DAL'),
                from = '2020-01-01',
                to = '2022-02-09')

stocks %>% 
  # keeping only the cols of interest
  select(symbol, date, adjusted) %>% 
  # so we can do calculations for each stock separately
  group_by(symbol) %>% 
  # do the naive forecast
  mutate(naiveFC = lag(adjusted)) %>% 
  # we are using summarise instead of mutate because we want a
  # single row per symbol
  summarise(ME = accuracy(object = naiveFC, x = adjusted)[1],
            RMSE = accuracy(object = naiveFC, x = adjusted)[2],
            MAE = accuracy(object = naiveFC, x = adjusted)[3],
            MPE = accuracy(object = naiveFC, x = adjusted)[4],
            MAPE = accuracy(object = naiveFC, x = adjusted)[5]) -> 
  stock_summary

