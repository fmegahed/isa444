# Code written in class on February 14, 2022
# Nonseasonal Smoothing



# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (accuracy fun)
               tidyquant) # for getting the stock data

cardano = tq_get(x = 'ADA-USD',
                 from = '2021-02-01',
                 to = '2021-02-08') %>% 
  select(date, adjusted) 

cardano %<>% mutate(naiveFC = lag(adjusted))

# Alternatively, I could have used the naive function for forecasting
cardano_pred = naive(cardano$adjusted,
                     h = 5, # h = 5 (predict 5 days ahead)
                     level = 95) # 95% PI

cardano_pred_tibble = tibble(
  date = seq.Date(from = ymd('2021-02-09'), # start on the 9th of Feb
                  by = 1, # increment by 1 day
                  # make it a vector of 5 (because we set h to 5) days
                  length.out = 5),
  # for the sake of a simple plot, we will put predictions in adjusted as well
  adjusted = cardano_pred$mean, # not correct but for sake of easy plotting
  naiveFC = cardano_pred$mean,
  lower = as.numeric(cardano_pred$lower), # lower limit of 95% (level input) pred interval
  upper = as.numeric(cardano_pred$upper)
)

# we will now add two variables (lower and upper to cardano)
# so that we can add cardano_pred to the bottom of the 8 obs in cardano

cardano %<>% mutate(lower = NA, upper = NA) 

cardano_total = rbind(cardano, cardano_pred_tibble)

# Now we will plot
cardano_total %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  geom_point(size = 2) +
  # adding a ribbon to capture the lower and upper limits of the PI
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2) + # make it 80% transparent 
  theme_bw() +
  scale_x_date(breaks = scales::pretty_breaks(n = 13))
  


# * * Alternate Version with 2 Lines --------------------------------------
cardano_pred = naive(cardano$adjusted,
                     h = 5, # h = 5 (predict 5 days ahead)
                     level = 95) # 95% PI

cardano_pred_tibble = tibble(
  date = seq.Date(from = ymd('2021-02-09'), # start on the 9th of Feb
                  by = 1, # increment by 1 day
                  # make it a vector of 5 (because we set h to 5) days
                  length.out = 5),
  # NA
  adjusted = cardano_pred$mean, # NA
  naiveFC = cardano_pred$mean,
  lower = as.numeric(cardano_pred$lower), # lower limit of 95% (level input) pred interval
  upper = as.numeric(cardano_pred$upper)
)

cardano_total = rbind(cardano, cardano_pred_tibble)

cardano_total %>% 
  ggplot(aes(x=date))+
  geom_line(aes(y = naiveFC, color = 'forecast'))+
  geom_point(aes(y = naiveFC, color = 'forecast'), size= 2)+
  geom_line(aes(y = adjusted, color = 'actual'))+
  geom_point(aes(y = adjusted, color = 'actual'), size= 2)+
  geom_ribbon(aes(ymin= lower, ymax= upper),
              alpha= 0.2) +
  scale_color_manual(values = c('forecast' = 'red', 'actual' = 'black')) +
  theme(legend.position = 'bottom')


