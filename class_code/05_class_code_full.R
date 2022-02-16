# Code written in class on February 07, 2022
# Introduction to TS Data Visualization and Summarizing TS Data



# * R Packages ------------------------------------------------------------
if(require(pacman)==FALSE) install.packages("pacman")
pacman::p_load(tidyverse, # for ggplot and for read_csv + pipe
               magrittr, # for two-way pipe
               readxl, # for reading xl data
               GGally, # for the scatter plot matrix
               tidyquant) # for getting the stock data



# * Scatter Plots ----------------------------------------------------------
download.file(url = "https://www.wessexlearning.org/pobf2e/dsa/German_forecasts.xlsx",
              destfile = "data/german_forecasts.xlsx",
              mode = "wb")

german = read_xlsx("data/german_forecasts.xlsx")
glimpse(german) # take a look at the information in the file


# * * A Singular Scatter Plot ---------------------------------------------
german %>% 
  ggplot(aes(x = Govsurp, y = GDP)) +
  geom_point() +
  theme_bw()


# * * Scatterplot Matrix --------------------------------------------------
?ggpairs
ggpairs(data = german,
        columns = c(2, 4, 7, 9)) +
  theme_bw()

# with names
ggpairs(data = german,
        columns = c('GDP', 'GFCF', 'Govsurp', "Unemp")) +
  theme_bw()



# * Numerical Summaries ---------------------------------------------------

MAD = function(x){
  (x - mean(x)) %>% # the definition of a deviation
    abs() %>%  # taking its absolute value
    mean() # taking the mean of the abs of the deviations
}

1:3 %>% MAD() # we expect to return 0.667



# * * GME example ---------------------------------------------------------

gme = tq_get(x = "GME",
             from = "2020-09-01",
             to = "2021-01-31") %>% 
  select(date, adjusted)

mean(gme$adjusted) # for a single calculation that makes sense


# * * Overall summaries ---------------------------------------------------
gme %>% summarise(
  # summarise function from dplyr (loaded with tidyverse)
  # allows you to calculate multiple summaries
  # you can either print them or save them to R obj
  meanACP = mean(adjusted),
  medianACP = median(adjusted),
  madACP = MAD(adjusted),
  varACP = var(adjusted),
  sdACP = sd(adjusted)
) -> gmeSummary


glimpse(gme)

gme %<>% mutate(
  year = year(date),
  month = month(date, label = T),
  month_year = paste(month, year, sep = " ")
)

gme %>% 
  group_by(year, month) %>% 
  summarise(
    # summarise function from dplyr (loaded with tidyverse)
    # allows you to calculate multiple summaries
    # you can either print them or save them to R obj
    meanACP = mean(adjusted),
    medianACP = median(adjusted),
    madACP = MAD(adjusted),
    varACP = var(adjusted),
    sdACP = sd(adjusted)
  ) -> gme_summary_month_year



# * * Growth Rate Calc ----------------------------------------------------

gr_gme = 100*(225-325)/325
gr_gme

225*1.3077  
  