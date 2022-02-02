# Code written in class on February 02, 2022
# Introduction to TS Data Visualization



# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               dygraphs, plotly, # for interactive visuals
               tidyquant) # for getting the FRED data


# * Continuing with the FRED AutoSales Plot -------------------------------

retail_sales = tidyquant::tq_get(x = "RSCCASN",
                      get = "economic.data",
                      from = "1992-01-01")
glimpse(retail_sales)

# data being used
retail_sales %>% 
  # creating the aesthetics (i.e., what goes on x and y-axes)
  ggplot(aes(x = date, y = price)) + # layers in ggplot are added
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
  theme(legend.position = "none") +
  labs(x = 'Year', caption = paste('Data from Fred, captured on', 
                                   format(Sys.Date(), 
                                          format = '%B %d, %Y')) ) -> p1

# New part -- making the previous plot interactive

## Approach 1: Plotly
ggplotly(p1) # from plotly and it takes the last ggplot and convert it to interactive


## Approach 2

# Note that we had to remove the symbol column per the instructions in 
# ?dygraph() -- see argument titled data
# the data frame approach did not work
retail_sales$symbol = NULL # approach for removing that first column
retail_sales = retail_sales %>% select(date, price) # another way for doing the step above

retail_sales = data.frame(retail_sales)
str(retail_sales)

# so we resorted to the time_series 
retail_sales_ts = ts(data = retail_sales$price, # numeric vector of length 360
                     start = c(1992, 01), # year, month from start (see first row)
                     end = c(2021, 12), # year, month from end (see last row)
                     frequency = 12) # 12 observations per year

dygraph(data = retail_sales_ts) %>% 
  dyRangeSelector()


# Adding a trend line to the data

## Approach 1 (if you have the plot saved to an object)
p1 + geom_smooth(method = "lm")

# Approach 2 (if you did not have the plot saved)

# data being used
retail_sales %>% 
  # creating the aesthetics (i.e., what goes on x and y-axes)
  ggplot(aes(x = date, y = price)) + # layers in ggplot are added
  # specifying that its a line plot
  geom_line() +
  # adding more breaks on the x-axis (i.e., showing more years)
  scale_x_date(breaks = scales::pretty_breaks(n= 12) ) +
  # same for y (notice difference in extension as var types are diff)
  # also adding the comma to the labels so we can read them easier
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n= 10)) +
  # adding the trend line
  geom_smooth(method = "lm") +
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
  theme(legend.position = "none") +
  labs(x = 'Year', caption = paste('Data from Fred, captured on', 
                                   format(Sys.Date(), 
                                          format = '%B %d, %Y')) )

# * Seasonal Plots ----------------------------------------------------------

## Plot 1

### Needs
#### year variable and a variable for month
#### lubridate package to the rescue (we have it loaded with tidyquant)

# using the $ to create a new column (if the name "year" is not in your data)
retail_sales$year = year(retail_sales$date) %>% as_factor() # as_factor makes it prettier in plotting
retail_sales$month = month(x= retail_sales$date, label = T)

retail_sales %>% 
  filter(date >= "2015-01-01" & date <= "2019-12-31") %>%  # filter keeps rows meeting a certain condition in the data
  # we had to add year as a grouping variable so it does not connect the diff years
  # in a given month
  ggplot(aes(x = month, y = price, color = year, group = year)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  # changing the colors from the default values -- "Dark 2" works for factors and
  # is color blind friendly
  scale_color_brewer(palette = "Dark2") +
  # optional changing the y-axis ticks, and setting the limit from 0 to 40,000
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000),
                     breaks = scales::pretty_breaks(n=10))



# Plot 2
retail_sales %>% 
  filter(date >= "2015-01-01" & date <= "2019-12-31") %>%  # filter keeps rows meeting a certain condition in the data
  # we had to add year as a grouping variable so it does not connect the diff years
  # in a given month
  ggplot(aes(x = date, y = price, color = year, group = year)) +
  geom_line() +
  geom_point(size = 2) +
  theme_bw() +
  theme(legend.position = "bottom") +
  # changing the colors from the default values -- "Dark 2" works for factors and
  # is color blind friendly
  scale_color_brewer(palette = "Dark2") +
  # optional changing the y-axis ticks, and setting the limit from 0 to 40,000
  scale_y_continuous(labels = scales::comma, limits = c(0, 40000),
                     breaks = scales::pretty_breaks(n=10)) +
  scale_x_date(breaks = scales::pretty_breaks(n=10))



# * Decomposing the TS ----------------------------------------------------
?decompose(0) # x is the only input and it has to be a ts

decompose(x = retail_sales_ts) %>% plot()

