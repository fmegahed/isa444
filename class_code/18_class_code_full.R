# Code written in class on March 30, 2022
# ARMA Models


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (for ndiffs)
               tidyquant, # for getting GNP data
               ggpubr)


visc = read_csv('data/18_viscosity.csv')

# * Step1: Plot the data --------------------------------------------------

# Approach 1 - add time to the data
visc$x = 1:nrow(visc)

visc %>% 
  ggplot( aes(x = x, y = y) ) +
  geom_line() +
  geom_point(size = 1)

# Approach 2 - convert the visc to a TS and use autoplot
visc_ts = ts(visc$y)
autoplot(visc_ts)



# * Step 2 - Checking the Assumptions Behind Stationarity -----------------

# From the plot, it is likely that the ts is stationary
# but we can also use what we learned last class to check that

ndiffs(x = visc_ts) # this returned 0 (which means that it is stationary)



# * Step 3: Plotting the ACF and potentially the PACF ---------------------
acf(x = visc_ts) # default acf plot (which has lag0)
acf(x = visc_ts) %>% autoplot() + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=25))

# from the ACF, I would probably start by examining an MA(4) since
# it would be reasonable to assume that the acf cuts-off at lag 4


# For the purpose of today, I would intentionally pick the wrong model
# to see how it impacts my model fit -- so lets pick an MA(1)

ma1 = Arima(y = visc_ts,
            order = c(p =0, # p = 0 (not an AR model) 
                     d = 0, # 0 differences are needed to make ts stationary
                     q = 1) # 1 is the order of the MA model
            )
class(ma1) # an object of the type forecast
names(ma1) # to get the names of the sublists in that object
summary(ma1) # similar to what we did with other forecast models
accuracy(ma1)

checkresiduals(ma1)
