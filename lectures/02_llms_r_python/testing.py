from pandas_datareader import data
from plotnine import *

# Extract OHUR data from FRED
OHUR = data.DataReader('OHUR', 'fred')

# Plot the data using plotnine
(ggplot(OHUR.reset_index(), aes(x='DATE', y='OHUR')) 
 + geom_line() 
 + labs(x='Date', y='OHUR', title='OHUR Data from FRED'))
