# Code written in class on February 16, 2022
# Nonseasonal Smoothing



# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (accuracy fun, meanf, )
               tidyquant) # for getting the stock data + loads pkg zoo

options(options = 4) # show four decimal places when printing



# * Corrected Plot from Last Class ----------------------------------------

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
  adjusted = NA, # NA becuase we do not know this for future values
  naiveFC = cardano_pred$mean,
  lower = as.numeric(cardano_pred$lower), # lower limit of 95% (level input) pred interval
  upper = as.numeric(cardano_pred$upper)
)

# we will now add two variables (lower and upper to cardano)
# so that we can add cardano_pred to the bottom of the 8 obs in cardano

cardano %<>% mutate(lower = NA, upper = NA) 

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
  theme_bw() +
  theme(legend.position = 'bottom') 



# * Smoothing and Forecasting ---------------------------------------------

ziliqa = tq_get('ZIL-USD',
                from = '2022-01-12',
                to = '2022-01-23') %>% select(date, adjusted)

# computing the smoothed values for our ziliqa ts

# showing you that meanf from the forecast pkg returns a list
# forecast pkg is loaded with fpp2 (which we have in the p_load())
# the point forecasts are stored in the sublist named 'mean'
# we learned that by ?meanf()
temp1 = meanf(y = ziliqa$adjusted)$mean
temp2 = meanf(y = ziliqa$adjusted) %>% .[['mean']]


ziliqa %<>%
  # mutate function is from dplyr (allows you to generate new cols)
  # it also respects grouping if you have multiple ts
  mutate(
    # overall/global average smoothing
    globalAVG_1 = mean(adjusted),
    globalAVG_2 = meanf(y = adjusted, h = 1, level = 95)$mean,
    
    # cumulative mean
    # we will use a function called cummean from dplyr
    cumulative_mean_1 = cummean(x = adjusted),
    cumulative_mean_2 = CUMULATIVE_MEAN(x = adjusted), # from tidyquant
    
    # moving average (we will use functions from the zoo pkg, which are loaded
    # with tidyquant)
    ma3 = rollmeanr(x = adjusted, k = 3, na.pad = TRUE),
    ma7 = rollmean(x = adjusted, k = 7, na.pad = TRUE, align = 'right'),
    ma10 = rollmean(x = adjusted, k = 10, na.pad = TRUE, align = 'right'),
    
    # forecasting instead of smoothing
    fCM = lag(cumulative_mean_1),
    fma3 = lag(ma3),
    fma7 = lag(ma7)
  )

# explaining the na.pad 
temp3 = rollmeanr(x = ziliqa$adjusted, k = 3)
temp3
temp4 = rollmeanr(x = ziliqa$adjusted, k = 3, na.pad = TRUE)


# Prediction Accuracy

acc_table = rbind(
  cumul_mean_fc = accuracy(object = ziliqa$fCM, x = ziliqa$adjusted),
  ma3_fc = accuracy(object = ziliqa$fma3, x = ziliqa$adjusted),
  ma7_fc = accuracy(object = ziliqa$fma7, x = ziliqa$adjusted)
)

row.names(acc_table) = c('Cumulative Mean', 'MA3', 'MA7')

