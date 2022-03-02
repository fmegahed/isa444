# Code written in class on February 23, 2022
# Exam 01 Review and Simple Exponential Smoothing


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (accuracy fun, meanf, etc)
               tidyquant) # for getting the stock data + loads pkg zoo

options(options = 4) # show four decimal places when printing



# * Exam 01 Review --------------------------------------------------------


# * * Baseball question ---------------------------------------------------

baseball = read_csv('data/baseball.csv')
glimpse(baseball)
GGally::ggpairs(baseball)



# * COVID Questions -------------------------------------------------------

covid = read_csv('data/butler_county_vaccination.csv')
covid %<>% 
  mutate(pct_chg_series_complete = Series_Complete_Pop_Pct - lag(Series_Complete_Pop_Pct)) 

mean(covid$pct_chg_series_complete, na.rm = T)
AVERAGE(covid$pct_chg_series_complete)

# Naive Forecast
covid %<>% mutate(naiveFC = lag(Series_Complete_Pop_Pct)) 



# * RMSE Calculation ------------------------------------------------------
accuracy(object = covid$naiveFC,
          x = covid$Series_Complete_Pop_Pct)

# just putting them next to each other
covid %<>% relocate(Date, Series_Complete_Pop_Pct, naiveFC) %>% 
  mutate(error = Series_Complete_Pop_Pct - naiveFC)

summary(covid$error)
hist(covid$error)
covid$error



# * Solano Questions ------------------------------------------------------
sol = tq_get(x = 'SOL1-USD', from = '2021-01-01', to = '2022-02-17')

# whether it is based on 2021-2022
# or based on solely 2022 (both are reasonable intentions)
pct_chg_correct_based_onQ = 100*(sol$adjusted[48] - sol$adjusted[1])/sol$adjusted[1]


sp500 = tq_get(x = '^GSPC', from = '2021-01-01', to = '2022-02-18')


# * I want to get data from the begining to Feb 17 ------------------------
if(require(pacman)==FALSE) install.packages("pacman")

pacman::p_load(tidyverse, tidyquant) # you will need to upload more packages for your analysis

coins = c('SOL1-USD', 'ADA-USD') # cryptocurrencies of interest

df = tq_get(x = coins, from = '2021-01-01', to = '2022-02-17') %>% 
  select(symbol, date, adjusted ) # data.frame for analysis

df %>% group_by(symbol) %>% summarise(maxDate = max(date))

sp500 = tq_get(x = '^GSPC', from = '2021-01-01', to = '2022-02-18') %>% 
  mutate(naiveFC = lag(adjusted))
max(sp500$date)

naiveFC = naive(y = sp500$adjusted, h = 1, level = 90)
naiveFC$upper

accuracy(object = sp500$naiveFC, x = sp500$adjusted)

upper = 4380.26 + (38.12635* qnorm(0.95))


# * Answering the question about making a plot by salary ------------------

DataExplorer::plot_scatterplot(data = baseball,
                                by = "salary")

colnames(baseball)[1] = 'salary'
