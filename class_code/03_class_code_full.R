# Code written in class on January 31, 2022
# Introduction to TS Data Visualization


# Assignment 01 -----------------------------------------------------------



install.packages("pacman")
pacman::p_load(tidyquant, tidyverse)

delta = tq_get("DAL",
               from = "2020-02-03",
               to = "2020-05-01")
first_price = delta$adjusted[1]

index_max_date = which( delta$date == max(delta$date) )
last_price = delta$adjusted[index_max_date]

pct_change = 100*(last_price - first_price)/first_price

round(pct_change, digits = 2)


delta[delta$date == max(delta$date), "adjusted"] %>% as.numeric()



# Assignment 02 -----------------------------------------------------------




# RSCASSN Plots -----------------------------------------------------------



# # Static Plot -----------------------------------------------------------

# I prefer the read_csv() from readr (loaded with tidyverse)
retail_sales = read_csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=RSCCASN&scale=left&cosd=1992-01-01&coed=2021-12-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2022-01-31&revision_date=2022-01-31&nd=1992-01-01")
retail_sales

# From base r , returns a data frame and 
# dates are always going to be a character vector
retail_sales_df = read.csv(file = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=RSCCASN&scale=left&cosd=1992-01-01&coed=2021-12-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2022-01-31&revision_date=2022-01-31&nd=1992-01-01")
retail_sales_df

# to convert date to an actual date in R
pacman::p_load(lubridate) # for fixing dates

# the issue the DATE column is character
class(retail_sales_df$DATE)
# we cannot use it directly for ts analysis

retail_sales_df$DATE = ymd(retail_sales_df$DATE)
class(retail_sales_df$DATE)


# Based on the first request in class, we will use retail_sales
# we could have used the other data frame as well

# data being used
retail_sales %>% 
  # creating the aesthetics (i.e., what goes on x and y-axes)
  ggplot(aes(x = DATE, y = RSCCASN)) + # layers in ggplot are added
  # specifying that its a line plot
  geom_line() +
  # adding more breaks on the x-axis (i.e., showing more years)
  scale_x_date(breaks = scales::pretty_breaks(n= 12) ) +
  # same for y (notice difference in extension as var types are diff)
  # also adding the comma to the labels so we can read them easier
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n= 10)) +
  # adding one recession as an example
  # put in aes to specify the color later (current color = label)
  geom_vline(aes(xintercept = ymd("2001-03-01"),  color = 'recession_1')) +
  geom_vline(aes(xintercept = ymd("2001-11-01"),  color = 'recession_1')) +
  # the later part of specifying color
  # did not have use the combine fun c() but used it to tell you what 
  # to do when you want to add the other recessions
  scale_color_manual(values = c('recession_1' = 'blue')) +
  # I do not like the default gray theme
  theme_classic() +
  # I like the legend position to be bottom and not right
  theme(legend.position = "bottom") +
  labs(x = 'Year', caption = paste('Data from Fred, captured on', 
                                   format(Sys.Date(), 
                                          format = '%B %d, %Y')) )

retail_sales_tq = tq_get(x = 'RSCCASN',
                         get = 'economic.data',
                         from = '1992-01-01')

df= read_csv("RSCCASN.csv")

download.file(url = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=RSCCASN&scale=left&cosd=1992-01-01&coed=2021-12-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2022-01-31&revision_date=2022-01-31&nd=1992-01-01",
              destfile = 'test.csv')

df2 = read_csv("test.csv")
