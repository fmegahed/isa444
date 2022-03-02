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
  gdp %<>% na.omit()
  
  sesFit = ses(gdp$DeltaLogGDP, h = 50, initial = 'simple', alpha = 0.2)
  names(sesFit) # see the sublist names
  
  
  temp2 = gdp %>% .[['DeltaLogGDP']]
  
  gdp %<>% mutate(Fcm = cummean(DeltaLogGDP) %>% lag(), # do not use the obs in forecasting the same obse
                  Fma = rollmeanr(DeltaLogGDP, k = 4, na.pad = TRUE) %>% lag(),
                  Fses = ses(DeltaLogGDP, h = 10, level = 95, initial = 'simple', alpha = 0.2) %>% .[['fitted']])
                   
  results = rbind(accuracy(object = gdp$Fcm, x = gdp$DeltaLogGDP), # accuracy metrics for your cumulative mean
                  accuracy(object = gdp$Fma, x = gdp$DeltaLogGDP),
                  accuracy(object = gdp$Fses, x = gdp$DeltaLogGDP) )              
  
  rownames(results) = c('Cumulative Mean', 'MA (4)', 'Ses (alpha =0.2)')
  results
  
  autoplot(sesFit)
  
# -------------------------------------------------------------------------------------------------
# WFJ Example for SES (to obtain optimal alpha; we are using the first 26 observations for training)
# -------------------------------------------------------------------------------------------------
  download.file("https://github.com/fmegahed/businessForecasting/blob/master/assignments/WFJ_sales.xlsx?raw=true",
                destfile = "../Data/WFJ_sales.xlsx", mode = "wb")
  WFJ = read_excel("../Data/WFJ_sales.xlsx") %>% select(c(1,2))
  
  WFJ %>% ggplot(aes(x = Obs, y = `WFJ Sales`)) + geom_point() + geom_line()
  
  trainData = WFJ[1:26, ]
  
  sesFitOptimal = ses(trainData$`WFJ Sales`)
  modelSES = sesFitOptimal$model$par
  optimalAlpha = sesFitOptimal$model$par['alpha']
  
  sesFitOptimal$model$par['alpha']
  sesFitOptimal %>% .[['model']] %>% .[['par']] %>% .[[1]]
  
  optimalAlpha
  
  # We will apply the ses model with alpha = 0.727 on my entire series (so that we do not initialize the smoother twice)
  
  WFJ %<>% mutate(sesOpt = ses(`WFJ Sales`, h =10, level = 0.95, initial = 'simple',
                               alpha = optimalAlpha) %>% .[['fitted']] ) 
  
  accuracy(object = WFJ$sesOpt, x = WFJ$`WFJ Sales`) # for the entire time series [DO NOT REPORT THIS]
  
  accuracy(sesFitOptimal) # training data accuracy
  
  validData = WFJ[-c(1:26), ] # excluded the training data
  accuracy(object = validData$sesOpt, x = validData$`WFJ Sales`) # reported accuracy on the validation data only
  
  # Equivalent Approach for what did in Lines 69-70
  sesFit2 = ses(WFJ$`WFJ Sales`, h =10, level = 0.95, initial = 'simple', alpha = optimalAlpha)
  WFJ$sesOpt2 = sesFit2$fitted
  
# -------------------------------------------------------------------------------------------------
# Weekly Therm Sales for LES (to obtain optimal alpha; we are using the first 26 observations for training)
  # alpha = 0.2, beta = 0.1
  # Fit LES model, obtain errors, and plot the fitted, forecast and prediction interval
# -------------------------------------------------------------------------------------------------  
