# In this file, I provide solutions for the four questions in Assignment # 03


# * Package Installation and Setup ------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # setting working director

if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, readxl, scales)



# * Question 1 --------------------------------------------------------------

dfBoulder = read_excel("Boulder_2.xlsx")

dfBoulder$YearMonth = paste(dfBoulder$Year, dfBoulder$Month, 01, sep = "-") %>%
  ymd()

# * * Plot 1 --------------------------------------------------------------

download.file("https://www.wessexlearning.org/pobf2e/dsa/Boulder_2.xlsx",
              destfile = "Boulder_2.xlsx", mode = "wb")

dfBoulder %>% 
  ggplot(aes(x = YearMonth, y = Temperature)) +
  geom_line() + 
  geom_point() +
  labs(title = "Scatterplot of Temperature vs. Time",
       caption = "Data from Ord et al. (2017)") + 
  theme_bw()


# * * Plot 2 --------------------------------------------------------------
dfBoulder$Year = as.factor(dfBoulder$Year)

dfBoulder %>% 
  filter(Year >= 2012) %>%
  ggplot(aes(x = month(YearMonth), y = Temperature, group = Year, color = Year)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = pretty_breaks(n = 12)) +
  labs(title = "Seasonal Plot of Temperature vs. Time",
       caption = "Data from Ord et al. (2017)",
       x = 'Month') +
  theme_bw() +
  theme(legend.position = "bottom")


# The resulting graphs show a consistent pattern, and thus, it is considered seasonal.



# * Question 3 ------------------------------------------------------------

# Using a shortcut to specify that the first column is a Date
# read_csv is from readr (which gets loaded with tidyverse)
# obviously you can recode the column to a date using the mdy() from lubridate
pageviews <- read_csv("Data/pageviews-20210530-20210731.csv",
                      col_types = cols(Date = col_date(format = "%m/%d/%Y")))
pageviews$Week = week(pageviews$Date) %>% as.factor()
pageviews$Day = wday(pageviews$Date, label = T)

pageviews %>% 
  ggplot(aes(x = Day, y = `Miami University`, group = Week, color = Week)) +
  geom_line() + 
  geom_point() +
  labs(title = "Plot of Wikipedia Page Views for 'Miami University'",
       x = "Day of Week",
       caption = "Data from https://pageviews.toolforge.org/?project=en.wikipedia.org | Miami University Pageviews") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Paired") 



# * Question 4 ------------------------------------------------------------

pageviews %>% 
  ggplot(aes(x = Date, y = `Miami University`)) +
  geom_line() + 
  geom_point() +
  labs(title = "TS Plot of Wikipedia Page Views for 'Miami University'",
       caption = "Data from https://pageviews.toolforge.org/?project=en.wikipedia.org | Miami University Pageviews") +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_smooth(method = "lm")
  # everything below this line is extra to illustrate the graph for you
  geom_hline(yintercept = mean(pageviews$`Miami University`), color = 'darkorange', size = 1.5) +
  annotate("text", x = ymd("2021-06-24"), y = max(pageviews$`Miami University`) + 10, label = "Mean of Miami University Wikipedia Pageviews",
           color = 'darkorange', size = 4) +
  annotate("segment", x = ymd("2021-06-21"), xend = ymd("2021-06-15"), 
           y = max(pageviews$`Miami University`) ,
           yend = mean(pageviews$`Miami University`), color = 'darkorange',  arrow = arrow(), size = 1.5 ) +
  geom_smooth(method = "lm")
