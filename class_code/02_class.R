
# * Data types ------------------------------------------------------------

vec_int = c(123L, 456L)


# * Data Structures -------------------------------------------------------

chr_vec = c('ISA', 444L, 1.15)



# * Demo 1 ----------------------------------------------------------------

# to check built-in datasets in R
data()

nile = Nile
typeof(nile) # basic/underlying structure
class(nile) # returns higher-level class that we use interpretation

length(nile) # 100 observations in that object
time(nile)

# snippet/snapshot of time (I prefer the window() )
nile_subset = window(
  x = nile, start = 1950, end = 1959
) 

# time(nile)==1950 -> checking for when the time period for the
# nile object is equal to 1950
# which function allows me to not have T/F and have an index for T
start_index = which( time(nile)==1950 )
# the above calculation is the equivalent of 
# 1950-1871+1

end_index = which( time(nile)==1959 )



# subset using typical non-ts methods
nile_subset2 = nile[start_index:end_index]
nile_subset2_ts = ts(nile_subset2, start = 1950, frequency = 1)



# * Reading TS Data from Your PC ------------------------------------------

# check if that package is installed if not install it
if( require(tidyverse)==F ) install.packages('tidyverse')
library(tidyverse)

cincy_weather = read.csv('data/cincy_monthly_weather.csv')
cincy_weather2 = read.csv(file.choose())



















