# Class code written on April 20, 2022
# ts regression


# * Loading the required packages -----------------------------------------

pacman::p_load(tidyverse, astsa, fpp2)

jj = jj # loading the data and storing it in an object called jj

# Based on printing this out, we can observe the following:
# (1) jj is a time-series
# (2) frequency = 4, i.e., we have quarterly data
# (3) start at 1960 Q1, and end at 1980 Q4
jj

# Always start by plotting the data
autoplot(jj) + theme_bw() + geom_point(size = 2)

# from the plot,
# (1) not stationary, with an increasing trend [exponentially increasing trend]
# (2) there is some evidence for seasonality (we will ignore this today)
# (3) multiplicative seasonality -- i.e., variance is not constant over time

# A Question 
# Based on this plot, how do you recommend to fit a linear regression line?

# Proving that changing our x has no impact
jj_df = data.frame(eps = as.numeric(jj), x = 1:84)
jj_df %>% ggplot(aes(x = x, y = eps)) +
  geom_line() + geom_point(size = 2)

# Solution do a log transform on the data
log_jj = log(jj)

autoplot(log_jj) + theme_bw() + geom_point(size = 2)
# this should allow us to fit a linear model to the output based on the plot
# however, note that we did NOT fix the issue with seasonality.
# we made the variance more stable, but it may not be constant over time
# (beginning and end of the ts may have a larger variance than the middle part)


# * fit the regression model ----------------------------------------------
year = time(log_jj)

year # this returns a ts where the integer reflects the year and the decimal
# reflects (period - 1)/frequency

options(scipen = 999) # makes it in non-scientific notation
summary(reg_model)

# took the fitted values and I converted them to a ts
# to easily identify the fitted value for Q3 of 1972
ts(reg_model$fitted.values, start = c(1960, 1), frequency = 4)

ts(reg_model$residuals, start = c(1960, 1), frequency = 4)
anova(reg_model)
