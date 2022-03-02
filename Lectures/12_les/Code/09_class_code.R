setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, magrittr, tidyquant, fpp2, ggpubr, scales, readxl)

# -------------------------------------------------------------------------------------------------
# GDP Example for SES
# -------------------------------------------------------------------------------------------------
  # Slides 9-11
  gdp = tq_get('GDP', get = 'economic.data', from = '1960-01-01') %>% 
    select(-symbol)
  gdp %<>% mutate(logGDP = log(price), DeltaLogGDP = logGDP - lag(logGDP))
  
  # Create the three plots
  p1 = gdp %>% ggplot(aes(x = date, y = price)) + 
    geom_line() + theme_bw(base_size = 6)
  p2 = gdp %>% ggplot(aes(x = date, y = logGDP)) + 
    geom_line() + theme_bw(base_size = 6)
  p3 = gdp %>% ggplot(aes(x = date, y = DeltaLogGDP)) + 
    geom_line() + theme_bw(base_size = 6)
  
  # Combining them using the ggpubr package (will need to be installed)
  ggpubr::ggarrange(p1, p2, p3, ncol = 1, nrow = 3)
  
  # In this quick live coding session, let us compare the forecasting performance of the cumulative average 
  # (dplyr::cummean()), moving average (zoo::rollmeanr()), and the SES (forecast::ses()) on the log differenced series. 
  # For the purpose of the example, let us use a window size (k = 4), and alpha = 0.2.
  
  

  
# -------------------------------------------------------------------------------------------------
# WFJ Example for SES (to obtain optimal alpha; we are using the first 26 observations for training)
# -------------------------------------------------------------------------------------------------
  download.file("https://github.com/fmegahed/businessForecasting/blob/master/assignments/WFJ_sales.xlsx?raw=true",
                destfile = "../Data/WFJ_sales.xlsx", mode = "wb")
  WFJ = read_excel("../Data/WFJ_sales.xlsx") %>% select(c(1,2))
  
  
  
  
  
  
# -------------------------------------------------------------------------------------------------
# Weekly Therm Sales for LES (to obtain optimal alpha; we are using the first 26 observations for training)
  # alpha = 0.2, beta = 0.1
  # Fit LES model, obtain errors, and plot the fitted, forecast and prediction interval
# -------------------------------------------------------------------------------------------------  