# this means that any function from pandas can be called using pd.function_name
# the . is equivalent to the R :: (in this case)
import pandas as pd 
import os
import datetime as dt

from plotnine import *

os.getcwd() # show me where is my working dir

df = pd.read_csv('RSCCASN.csv')

# prints out the variable types and I can see that date is being recognized 
# as a object/string/chr
df.info()

# this will convert the column named DATE to a datetime object
df['DATE'] = pd.to_datetime(df['DATE'])

df.describe() # for numeric-like variables, it will show you summary stats

# Add the month and year to our data frame
df['month'] = df['DATE'].dt.month
df['year'] = df['DATE'].dt.year



# Plot 1
# parantheses here means do not worry about indentation
(
  ggplot(df, aes(x = 'DATE', y = 'RSCCASN')) +
  geom_line() +
  geom_smooth() +
  geom_point(aes(color = 'month')) +
  theme_bw()
)
