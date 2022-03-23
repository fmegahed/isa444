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
               MASS, # generating fractions
               stargazer)



# * Stargazer Output -------------------------------------------------------------
wfj = read_excel('data/WFJ_sales.xlsx') %>% 
  pull(`WFJ Sales`) %>% 
  ts()

wfj_df = read_excel('data/WFJ_sales.xlsx') %>% 
  dplyr::select(c(1, 2)) %>% 
  mutate(lag1 = lag(x = `WFJ Sales`))


# Slide 21
model_fit = lm(`WFJ Sales` ~ lag1, data = wfj_df)
summary(model_fit) # model summary
stargazer(model_fit, type = 'html', out = 'sample_output.html') # prints nicely in Markdown



# * Assignment ------------------------------------------------------------

therm_sales = read_excel(path = 'data/Weekly_Therm_Sales.xlsx')
therm_sales

# solution approach 1
acf_therm_sales = acf(x = therm_sales$WeeklyThermSales)
acf_therm_sales$acf

# use plotly
acf(x = therm_sales$WeeklyThermSales) %>% autoplot()
plotly::ggplotly() # look at the value based on the plot
# Note we used autoplot to generate a ggplot2 object
# which we can then convert into an interactive chart via the plotly pkg


# * RSCCAS Data -----------------------------------------------------------
retail = tq_get(x = 'RSCCAS', get = 'economic.data', 
                from = '1992-01-01', to = Sys.Date())

retail %<>% mutate(diff = c(NA, diff(price)) ) 

retail_acf = acf(retail$price)
diff_acf = na.omit(retail$diff) %>% acf()

upper_limit_ts = 1.96/sqrt(362)
upper_limit_diff = 1.96/sqrt(361)


# * pacf in R -------------------------------------------------------------
retail_pacf = pacf(retail$price) 
# partial autocorrelation values
retail_pacf$acf
retail_acf$acf