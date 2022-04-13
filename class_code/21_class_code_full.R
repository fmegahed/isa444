# Code written in class on April 11, 2022
# ARMA Models


# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               fpp2, # loads the forecast pkg (for ndiffs)
               tidyquant, # for getting GNP data
               ggpubr)


visc = read_csv('data/18_viscosity.csv')

# * Step1: Plot the data --------------------------------------------------

# Approach 1 - add time to the data
visc$x = 1:nrow(visc)

visc %>% 
  ggplot( aes(x = x, y = y) ) +
  geom_line() +
  geom_point(size = 1)

# Approach 2 - convert the visc to a TS and use autoplot
visc_ts = ts(visc$y)
autoplot(visc_ts)



# * Step 2 - Checking the Assumptions Behind Stationarity -----------------

# From the plot, it is likely that the ts is stationary
# but we can also use what we learned last class to check that

ndiffs(x = visc_ts) # this returned 0 (which means that it is stationary)



# * Step 3: Plotting the ACF and potentially the PACF ---------------------
acf(x = visc_ts) # default acf plot (which has lag0)
acf(x = visc_ts) %>% autoplot() + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=25))

# from the ACF, I would probably start by examining an MA(4) since
# it would be reasonable to assume that the acf cuts-off at lag 4


# For the purpose of today, I would intentionally pick the wrong model
# to see how it impacts my model fit -- so lets pick an MA(1)

ma1 = Arima(y = visc_ts,
            order = c(p =0, # p = 0 (not an AR model) 
                      d = 0, # 0 differences are needed to make ts stationary
                      q = 1) # 1 is the order of the MA model
)

class(ma1) # an object of the type forecast
names(ma1) # to get the names of the sublists in that object
summary(ma1) # similar to what we did with other forecast models
accuracy(ma1)

checkresiduals(ma1)
# the p-value of 0.0284 < our typical cut-off of 0.05
# we have enough evidence to reject the null hypothesis of
# uncorrelated residuals 
# this is supported by our ACF plot, which showed that 
# the ts is correlated with past values at lag(3)

# The implication would be that we need to fit a different model
# so re-examine the ACF and/or the PACF for clues



# * Today's New Material --------------------------------------------------


# * Rationale for trying an AR(2) -----------------------------------------

# reason 1 --> some possibility for interpreting the chart
# as a sinsodiual pattern that dies down 
acf(x = visc_ts) %>% autoplot() + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=25))

# reason 2 -->
pacf(x = visc_ts) %>% autoplot() + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=25))


ar2 = Arima(y = visc_ts,
            order = c(p =2, # p = 2 
                      d = 0, # 0 differences are needed to make ts stationary
                      q = 0) # 0 since we are fitting an AR2 model
)

summary(ar2) # similar to what we did with other forecast models
checkresiduals(ar2)
# conclude that I have uncorrelated residuals since my p-value of
# 0.6443 > 0.05 
# (i.e., we are not violating the assumptions behind the ARIMA model)
# it seems that this is a good model

# A good model means that the AR(2) is a reasonable fit for the ts
# based on its past data
# [1] this may not hold for future data
# [2] there can be better models based on the same past data
# e.g., compare AR(2) to MA(4) and see which one is better



# * GNP Data --------------------------------------------------------------
gnp = read_csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GNP&scale=left&cosd=1947-01-01&coed=2021-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2022-04-11&revision_date=2022-04-11&nd=1947-01-01')
gnp

gnp_ts = ts(data = gnp$GNP,
            start = c(1947, 01),
            frequency = 4)

# Step 2 - ndiffs()
ndiffs(gnp_ts) # 2 differences are needed to bring the data to stationarity


## Side Note - plot the differenced data
differenced_gdp = diff(gnp_ts, differences = 2)
autoplot( differenced_gdp )

acf(differenced_gdp) %>% autoplot()

pacf(differenced_gdp) %>% autoplot()

# We will start by examining a MA(1) type model

gnp_ma1 = Arima(y = gnp_ts, # original data bec we would like to forecast in these units
                order = c(p = 0, # not an AR model
                          d = 2, # 2 differences were needed to bring ts to stationarity
                          q = 1) # q = 1 based on how we interpreted the ACF chart
                )

summary(gnp_ma1)
checkresiduals(gnp_ma1)



# * Checking for the Impact of COVID-19 -----------------------------------

gnp_pre_covid = gnp %>% filter(DATE <= '2019-12-31')

gnp_ts = ts(data = gnp_pre_covid$GNP,
            start = c(1947, 01),
            frequency = 4)

ndiffs(gnp_ts)

differenced_gdp = diff(gnp_ts, differences = 2)
autoplot( differenced_gdp )

acf(differenced_gdp) %>% autoplot()

pacf(differenced_gdp) %>% autoplot()

arima121 = Arima(y = gnp_ts,
                 order = c(1,2,1) ) 
summary(arima121)

checkresiduals(arima121)



# * GNP from astsa package ------------------------------------------------

pacman::p_load(astsa)

gnp_data = astsa::gnp

# step 1
autoplot(gnp_data)

# step 2
ndiffs(gnp_data)

# step3 look at the acf and pacf of diff data 
gnp_data_diff = diff(gnp_data, differences = 2)

acf(gnp_data_diff)
pacf(gnp_data_diff)

# step 4 is fitting a/the model
auto_arima_model = auto.arima(y = gnp_data)

summary(auto_arima_model)
checkresiduals(auto_arima_model)

# step 5 - once you have a model that fits well and you want
# to use to forecast future values 
# here is how to do it
forecast_2002_and_2003 = forecast(object = auto_arima_model,
                                  h = 5)
forecast_2002_and_2003
