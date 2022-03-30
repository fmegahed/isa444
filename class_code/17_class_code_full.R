# Code written in class on March 28, 2022
# Stationarity


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (for ndiffs)
               tidyquant, # for getting GNP data
               ggpubr)


# * GNP Example for Computing Differences ---------------------------------

gnp = tq_get(x = 'GNP', get = "economic.data")
gnp

gnp %>% ggplot(aes(x = date, y = price)) + geom_line() + geom_point()

# computing the first and second differences
gnp %<>% mutate(
  diff1 = c(NA, diff(x = price, differences = 1) ),
  # two equivalent ways to compute the second difference (your choice)
  diff2a = c(NA, diff(x = diff1, differences = 1)),
  diff2b = c(NA, NA, diff(x = price, differences = 2) )
)

gnp %>% ggplot(aes(x = date, y = price)) + geom_line() + geom_point() -> p1
gnp %>% ggplot(aes(x = date, y = diff1)) + geom_line() + geom_point() -> p2
gnp %>% ggplot(aes(x = date, y = diff2a)) + geom_line() + geom_point() -> p3

ggarrange(p1, p2, p3, ncol = 1)
ggarrange(p3, p2, p1, ncol = 1) # to make it similar to notes



# * Formal Tests for Stationarity -----------------------------------------
pacman::p_load(tseries)


# * ADF Test --------------------------------------------------------------

adf.test(x = gnp$price)
# if we are using the 0.05 threshold 
# (to be consistent with the threshold used in R for acf plots)
# from this output, we will conclude that we do Not have enough evidence
# to reject the null hypothesis of non-stationary ts
# since the reported p-value of 0.06147 > 0.05
# i.e., the gnp series is likely to be non-stationary

adf.test(x = na.omit(gnp$diff1) )
# if we are using the 0.05 threshold 
# (to be consistent with the threshold used in R for acf plots)
# from this output, we will conclude that we do Not have enough evidence
# to reject the null hypothesis of non-stationary ts
# since the reported p-value of 0.08 > 0.05
# i.e., the gnp series is likely to be non-stationary

adf.test(x = na.omit(gnp$diff2a) )
# we would reject the null hypothesis since the reported p-value is 0.01 <
# your threshold of 0.05
# i.e., we would conclude that the time series is stationary
# in plain English, by taking the second differences for the GNP, we can now
# conclude that we have evidence for a constant mean and an acf that
# does not depend on t

mean(gnp$diff2a, na.rm = T)



# * * KPSS Test -----------------------------------------------------------

kpss.test(gnp$price)
# we reject the null hypothesis of stationary ts
# and conclude that the raw GNP data is non-stationary 
# since the p-value of 0.01 is less than our 0.05 threshold

kpss.test(gnp$diff1) 
# disagrees with ADF conclusion since we do not reject
# the null of stationary ts here
# the reported p-value is 0.1 > which is greater than the 0.05 threshold
# hence, we do not reject the null of stationary ts
# i.e, we will conclude that the ts is likely to be stationary



kpss.test(gnp$diff2a) # AGREES with ADF conclusion since we do not reject the
# null hypothesis of stationary ts


ndiffs(gnp$price) # ndiffs with the default of kpss implementations
