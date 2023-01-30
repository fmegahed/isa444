
# class 03


# * Reading TS Data from Your PC ------------------------------------------

# check if that package is installed if not install it
if( require(tidyverse)==F ) install.packages('tidyverse')
library(tidyverse)

# go into the data folder and read the file that I want
cincy_weather = read.csv('data/cincy_monthly_weather.csv')
cincy_weather2 = read.csv(file.choose())

str(cincy_weather) # internal structure of the object
dplyr::glimpse(cincy_weather) # an alternative for str

cincy_weather3 = readr::read_csv('data/cincy_monthly_weather.csv')
cincy_weather3 # prints that object (tibble)

cincy_weather2 # diff between data.frame and tibble (df does not print nicely)



# * Getting Stock and Macroeconomic Data ----------------------------------
if(require(tidyquant)==F) install.packages("tidyquant")


stocks = c('NFLX', 'AAPL', "META")

stocks_tbl = tidyquant::tq_get(
  x = stocks, from = '2010-01-01', to = '2023-01-27'
) # when getting stock data, the to is not included

stocks_tbl


# * Macroeconomic Data ----------------------------------------------------

gdp_df = read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2022-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-01-30&revision_date=2023-01-30&nd=1947-01-01") |> 
  dplyr::mutate(DATE = lubridate::ymd(DATE), # we will convert date from string to a Date format
                QUARTER = lubridate::quarter(DATE), # we extract the quarter number (1 if Jan, 4 if Oct)
                MONTH = lubridate::month(DATE, label = T)) # this extracts the month

dplyr::glimpse(gdp_df)

gdp_tbl = readr::read_csv('https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=GDP&scale=left&cosd=1947-01-01&coed=2022-10-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Quarterly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-01-30&revision_date=2023-01-30&nd=1947-01-01')

dplyr::glimpse(gdp_tbl) # checking the classes of each variable

# another benefit from the readr functions is that it attempts to guess the data types
# from the output, I do not need to change DATE to a date.

gdp_tq = tidyquant::tq_get(x = 'GDP', get = 'economic.data',
                           from = '1947-01-01')

dplyr::glimpse(gdp_tq)



# * Converting the data into a time-series --------------------------------

# Assume that you are interested in a single TS (we will relax this assumption later)

# lets extract the AAPL data from stocks_tbl

aapl_tbl = stocks_tbl |> # passing the left of the pipe operator (|>) as 1st arg to next fun
  dplyr::filter(symbol == 'AAPL' )

unique(aapl_tbl$symbol) # two checks to make sure that the filter function worked
# unique would return only symbol and the number of observations is about 1/3

unique(stocks_tbl$symbol) # originally three

# we want to make a ts of the adjusted closing price
aapl_ts = ts(data = aapl_tbl$adjusted)

gdp_ts = ts(data = gdp_tbl$GDP, 
            frequency = 4, # 4 obs per year
            start = c(1947, 1) ) # 1947 is the year and 1 is for q1

gdp_ts



# * Subsetting Data Frames ------------------------------------------------

# if you have a date column, you can use filter to pick dates of interest
stocks_tbl |> 
  dplyr::filter(
    date >= lubridate::ymd('2023-01-01') &
      date <= lubridate::ymd('2023-01-15')) -> stocks_tbl_2023
