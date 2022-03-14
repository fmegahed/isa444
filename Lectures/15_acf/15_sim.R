library(tidyverse)
library(tidyquant)
library(magrittr)
library(MASS)

sim_data = rnorm(100000) %>% tibble()
colnames(sim_data) = 'w_n'

# plotting the simulated data
sim_data %>% ggplot(aes(x = w_n)) + 
  geom_histogram(color = 'black', fill = 'white') + 
  theme_bw()

# creating the centered moving average and the leading (or lagging data)
sim_data %<>% mutate(cma = rollmean(w_n, k = 3, na.pad = T),
                 `w_t+1` = lead(cma),
                 `w_t+2` = lead(cma, n = 2),
                 `w_t+3` = lead(cma, n = 3),
                 `w_t+4` = lead(cma, n = 4))

# variance
var(sim_data$cma, na.rm = T) %>% fractions(max.denominator = 10)

# computing the covariance
cov(x = sim_data$cma, sim_data$`w_t+1`, use = 'complete.obs') %>% fractions(max.denominator = 10)
cov(x = sim_data$cma, sim_data$`w_t+2`, use = 'complete.obs') %>% fractions(max.denominator = 10)
cov(x = sim_data$cma, sim_data$`w_t+3`, use = 'complete.obs') %>% fractions(max.denominator = 10)

# computing the correlations
cor(x = sim_data$cma, sim_data$`w_t+1`, use = 'complete.obs') %>% fractions(max.denominator = 10)
cor(x = sim_data$cma, sim_data$`w_t+2`, use = 'complete.obs') %>% fractions(max.denominator = 10)
cor(x = sim_data$cma, sim_data$`w_t+3`, use = 'complete.obs') %>% fractions(max.denominator = 10)