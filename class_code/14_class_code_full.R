# Code written in class on March 9, 2022
# Seasonal Decomposition and Holt-Winters Method


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (accuracy fun, meanf, etc)
               tidyquant, # loads pkg zoo
               readxl, # for reading excel data
               plotly) # for interactive graphs



# * Bike Sharing Data -----------------------------------------------------

bike = read_xlsx(path = 'data/BikeSalesR.xlsx')
bike

# [1] First Plot the data
bike %>% mutate(obs_number = 1:16) %>% # because we have 16 observations 
  ggplot(aes(x = obs_number, y = `Bike Sales`) ) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = 'bottom')

ggplotly()

# [2] some quick calculations for the difference between peaks and troughs
# for a large timeseries sample a year at the begining, middle and end
yr1 = 43 - 10
yr2 = 45 - 11
yr3 = 50 - 14
yr4 = 55 - 19


# [3] Convert the numeric data into a ts
bike_ts = ts(data = bike$`Bike Sales`,
             # assuming that the data starts in 2018 (we do not need this
             # but it looks good in plots)
             # Q1 is solely based on our data
             start = c(2018, 1), 
             # freq = 4 is because our data is quarterly (4 obs per year)
             frequency = 4) 

# [4] Decompose the ts (assuming thats your end goal)

decomposed = decompose(x = bike_ts, 
                # we picked additive based on calculations in [2] and plot in [1]
                       type = "additive")
decomposed # list of six items

autoplot(decomposed) # returns a ggplot chart
plot(decomposed) # returns a base R chart

# [5] Interpretation
decomposed$figure # returns the four constant seasonal patterns
# in Q1, I am on average selling 14.6 less bikes than what I do in a given year
decomposed$figure[1]


# [6] The Math [Only for explanation - not for testing]
detrended = decomposed$x - decomposed$trend # removing the trend
# I removed both the trend and seasonality -- thats why remainder is E
errors = detrended - decomposed$seasonal # calculation of resiudals/errors

q1_initial = mean(c(-15, -14.625, -14.375))
q2_initial = mean(c(6.625, 6.25, 6.5))
q3_initial = mean(c(17.875, 18.125, 19.125))
q4_initial = mean(c(-9.5, -10.625, -11.125))

# once we adjust the values above, based on the avg
# we get our final seasonal factors stored in decomposed$figure
q1_sf = q1_initial - mean(c(q1_initial, q2_initial, q3_initial, q4_initial))




# * Holt-winters ----------------------------------------------------------

# Again, we need to have a time-series

holt_add = hw(y = bike_ts, level = 95, 
              alpha = 0.2, beta = 0.1, gamma = 0.1,
              initial = "optimal")
summary(holt_add)

holt_multi = hw(y = bike_ts, level = 95, seasonal = 'multiplicative',
              alpha = 0.2, beta = 0.1, gamma = 0.1,
              initial = "optimal")
summary(holt_multi)

autoplot(bike_ts) +
  autolayer(holt_add, PI = F, color = 'red') +
  autolayer(holt_multi, PI = F)
