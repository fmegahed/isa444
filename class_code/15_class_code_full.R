# Code written in class on March 14, 2022
# ACF


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (accuracy fun, meanf, etc)
               tidyquant, # loads pkg zoo
               readxl, # for reading excel data
               plotly, # for interactive graphs
               MASS) # generating fractions


# * ACF Simulation --------------------------------------------------------

set.seed(2022) # ensures that we are generating the same random data

w_n = rnorm(n= 100000) # generating 100,000 IID N ~ (0, 1)

w_df = data.frame(w_n)

w_df %<>% mutate(Y_t = rollmean(x = w_n, k = 3, na.pad = T),
                 `Y_t+1` = lead(Y_t, n = 1),
                 `Y_t+2` = lead(Y_t, n = 2),
                 `Y_t+3` = lead(Y_t, n = 3),
                 `Y_t+4` = lead(Y_t, n = 4)) 

# estimate of the population mean
expected_value_y_t = mean(w_df$Y_t, na.rm = T) # close to 0

# estimate of the variance
var(w_df$Y_t, na.rm = T) %>% MASS::fractions(max.denominator = 100)

# autocovariance
var(x = w_df$Y_t, y = w_df$`Y_t+1`, na.rm = T) %>% 
  MASS::fractions(max.denominator = 10)

# autocorrelations
cor(x = w_df$Y_t, y = w_df$`Y_t+1`, use = 'complete.obs') %>% MASS::fractions(max.denominator = 100)
cor(x = w_df$Y_t, y = w_df$`Y_t+2`, use = 'complete.obs') %>% MASS::fractions(max.denominator = 100)
cor(x = w_df$Y_t, y = w_df$`Y_t+3`, use = 'complete.obs') %>% MASS::fractions(max.denominator = 100)
cor(x = w_df$Y_t, y = w_df$`Y_t+4`, use = 'complete.obs') %>% MASS::fractions(max.denominator = 100)



# * Example 1 - White Noise and CMA - ACF ---------------------------------
set.seed(2022) # different seed for generating the data

white_noise = rnorm(n = 500)
white_noise_ts = ts(white_noise) # to be able to use the acf function

white_noise_acf = acf(x = white_noise_ts, lag.max = 15)

white_noise_acf$acf # result in 16 (lag.max + 1) acf computations [y_axis in the plot]
white_noise_acf$lag # [x_axis in the plot]
white_noise_acf$acf > 1.96/sqrt(500) # only the lag 0 acf is sign (bec everything else is FALSE)


acf(x = white_noise_ts, lag.max = 15, plot = F) %>% autoplot() + theme_bw()
ggplotly() # another way of figuring out the acf at a given lag (requires a ggplot to be produced)


cma = rollmean(white_noise, k = 3, na.pad = T)
cma_ts = ts(cma) %>% na.omit()

acf(cma_ts, lag.max = 15, plot = F) %>% autoplot() + theme_bw()
cma_acf = acf(cma_ts, lag.max = 15, plot = F)

which(cma_acf$acf > 1.96/sqrt(498) ) # produces 16 values where your first value is lag 0
cma_acf$acf




# * Example 2 -------------------------------------------------------------
wfj = read_excel('data/WFJ_sales.xlsx') %>% 
  pull(`WFJ Sales`) %>% 
  ts()

wfj_df = read_excel('data/WFJ_sales.xlsx') %>% 
  dplyr::select(c(1, 2)) %>% 
  mutate(lag1 = lag(x = `WFJ Sales`))

# plot in Slide 19
autoplot(wfj ) + theme_bw()
wfj_df %>% ggplot(aes(x = Obs, y = `WFJ Sales`)) + geom_line() + theme_bw()

# Slide 20
wfj_acf = acf(x = wfj, lag.max = 15, plot = F) # saved in a seperate object
wfj_acf %>% autoplot() + theme_bw() # plot not saved -- just shown
wfj_acf$acf >= 1.96/sqrt(62) # which we used here

# Slide 21
model_fit = lm(`WFJ Sales` ~ lag1, data = wfj_df)
summary(model_fit) # model summary
stargazer(model_fit, type = 'html') # prints nicely in Markdown
