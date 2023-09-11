
# before the :: is the package name
# after the :: is the function name
# I did not need to specify a file location bec
# I am using the Rproject environment

# In R, there are two common ways to read csvs


# * Reading the Data ------------------------------------------------------

# Option 1: using the read_csv function from readr which I prefer
# [a] guesses column types, i.e., data is being read as date
# [b] once you read it, it gives you a nice printout
df = readr::read_csv("RSCCASN.csv")

# Option 2: using the read.csv() from base R (so no packages are needed)
df_fred = read.csv("RSCCASN.csv")
str(df_fred) # shows 389 obs and 2 variables but our date column is recognized as string/chr
# extra processing is needed if I were to plot this



# * Extracting some Date-based Features -----------------------------------

# to extract date-based information, we will use functions from dplyr and lubridate
# lubridate is a popular pkg in R for handling dates
# dplyr allows you to manipulate data
# should have been installed when we did install.packages('tidyverse')

# we will create a month and year variable from that data
# they will be helpful when we plot the data

df |> 
  # allows me to create new variables in R
  dplyr::mutate(
    year = lubridate::year(DATE),
    # label = T will allow me to have "Jan", "Feb", ... instead of 1, 2, .., 12
    month = lubridate::month(DATE, label = T)
  ) -> # assignment can be either at the begin or at the end 
  df # re-saves what we did in the object called df



# * Plots -----------------------------------------------------------------



# * * Line-Chart ----------------------------------------------------------

# we will be using ggplot2 for making charts in R
# but you can use any other approach if you want to 

df |> 
  # this step will create an empty canvas with the right variables on the axes
  # and the right scales
  ggplot2::ggplot(mapping = ggplot2::aes(x = DATE, y = RSCCASN)) +
  # we added lines and points
  # aes passed in the ggplot function is global (i.e., for all geoms)
  # we can also pass aes to specific functions (color only applies to the points)
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes(color = month)) +
  # adds a trend line based on Ashley's request
  ggplot2::geom_smooth(method = 'lm', formula = 'y~x') +
  # optional (beautification)
  ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 6)) +
  ggplot2::scale_y_continuous(labels = scales::dollar, breaks = scales::pretty_breaks(n = 8)) +
  ggplot2::theme_bw() +
  # this line I am zooming over the last few years
  # this is not changing the data (so your lm fit is based on the entire data)
  ggplot2::coord_cartesian(
    xlim = c(lubridate::ymd('2010-12-31'), lubridate::ymd('2016-12-02'))
    ) -> chart1

chart1


# * * Line Chart with Month on the X-axis ---------------------------------

# good for seeing seasonality, not great if you have many years

df |> 
  dplyr::filter(year > 2018) |> 
  # this step will create an empty canvas with the right variables on the axes
  # and the right scales
  ggplot2::ggplot(mapping = ggplot2::aes(x = month, y = RSCCASN, group = year,
                                         color = as.factor(year))) +
  # we added lines and points
  # aes passed in the ggplot function is global (i.e., for all geoms)
  # we can also pass aes to specific functions (color only applies to the points)
  ggplot2::geom_line() +
  ggplot2::geom_point(ggplot2::aes()) +
  # adds a trend line based on Ashley's request
  # ggplot2::geom_smooth(method = 'lm', formula = 'y~x') +
  # optional (beautification)
  ggplot2::scale_y_continuous(labels = scales::dollar, breaks = scales::pretty_breaks(n = 8)) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom') -> chart2

chart2



# * Charts Interactively --------------------------------------------------

plotly::ggplotly(p = chart1 + ggplot2::theme(legend.position = 'none') )
