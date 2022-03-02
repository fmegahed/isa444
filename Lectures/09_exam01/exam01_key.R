library(tidyverse)
library(scales)

df = read_csv('https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD')

butler_county = df %>% 
  filter(Recip_State == 'OH' & Recip_County == 'Butler County') %>% 
  select(Date, FIPS, MMWR_week, Recip_County, Recip_State, 
         Series_Complete_Pop_Pct, Booster_Doses_Vax_Pct) %>% 
  mutate(Date = lubridate::mdy(Date),
         new_boosted_pct = Booster_Doses_Vax_Pct - lead(Booster_Doses_Vax_Pct))


# * Q7 --------------------------------------------------------------------

butler_county %>% 
  ggplot(aes(x = Date, y = 0.01*Series_Complete_Pop_Pct)) + 
  geom_line() +
  theme_bw() + 
  scale_x_date(breaks = scales::breaks_pretty(12)) +
  scale_y_continuous(breaks = scales::breaks_pretty(10), 
                     labels = scales::percent, limits = c(0,1)) +
  labs(x = 'Date',
       y = 'Percent Population Vaccinated', 
       title = 'COVID-19 Vaccinations in Butler County')


# * Q8 --------------------------------------------------------------------


butler_county %>% 
  filter(Date >= '2022-01-01') %>% 
  ggplot(aes(x = Date, y = new_boosted_pct)) + 
  geom_line() +
  geom_point(size = 2) +
  theme_bw() + 
  scale_x_date(breaks = scales::breaks_pretty(18)) +
  scale_y_continuous(breaks = scales::breaks_pretty(10)) +
  labs(x = '2022',
       y = 'Day-over-Day Percent Change in Boosted Population PCT', 
       title = 'COVID-19 Boosted Population in Butler County')



# * Q9 --------------------------------------------------------------------

butler_county = df %>% 
  filter(Recip_State == 'OH' & Recip_County == 'Butler County') %>% 
  select(Date, FIPS, MMWR_week, Recip_County, Recip_State, 
         Series_Complete_Pop_Pct, Booster_Doses_Vax_Pct) %>% 
  mutate(Date = lubridate::mdy(Date)) %>% 
  filter(Date >= '2022-01-01' & Date <= '2022-02-17') %>% 
  arrange(Date) %>% 
  mutate(new_boosted_pct = Booster_Doses_Vax_Pct - lag(Booster_Doses_Vax_Pct))

write_csv(x = butler_county %>% select(-new_boosted_pct), 
          file = 'Lectures/09_exam01/butler_county_vaccination.csv')

q9 = round(mean(butler_county$new_boosted_pct, na.rm = T), 4)




# * Q10 -------------------------------------------------------------------
forecast::accuracy(object = lag(butler_county$Series_Complete_Pop_Pct),
                   x = butler_county$Series_Complete_Pop_Pct)




# * Crypto ----------------------------------------------------------------

if(require(pacman)==FALSE) install.packages("pacman")

pacman::p_load(tidyverse, tidyquant) # you will need to upload more packages for your analysis

coins = c('SOL1-USD', 'ADA-USD') # cryptocurrencies of interest

df = tq_get(x = coins, from = '2021-01-01', to = '2022-02-17') %>% 
  select(symbol, date, adjusted ) # data.frame for analysis



df %>% ggplot(aes(x = date, y = adjusted)) + geom_line() +
  facet_wrap(~symbol, scales = 'free_y', ncol = 1) +
  scale_y_continuous(breaks = pretty_breaks(n=10), labels = comma) +
  theme_bw()


df$day = lubridate::wday(df$date, label = T) %>% as_factor()
df$week = lubridate::week(df$date) %>% as.factor()

df %>% filter(date >= '2022-01-02' & date <= '2022-02-17') %>%
  ggplot(aes(x = day, y = adjusted, group = week, color = week)) +
  geom_line() +
  geom_point() +
  facet_wrap(~symbol, scales = 'free_y', ncol = 1) +
  scale_y_continuous(breaks = pretty_breaks(n=10), labels = comma) +
  theme_bw()


df %>% filter(symbol == 'SOL1-USD') %>% 
  mutate(pct_change = (tail(adjusted, 1) - head(adjusted,1) )/head(adjusted, 1) )



# * SP500 -----------------------------------------------------------------
sp500 = tq_get(x = '^GSPC', from = '2021-01-01', to = '2022-02-18') %>% 
  select(symbol, date, adjusted ) # data.frame for analysis

forecast::naive(sp500$adjusted, h = 1, level = 90) # approach 2

sp500$naiveFC = lag(sp500$adjusted)

forecast::accuracy(sp500$naiveFC, x = sp500$adjusted)

sp500_upperLimit1 = sp500$adjusted[nrow(sp500)] + 38.12635*qnorm(0.95) # approach 1
