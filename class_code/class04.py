import yfinance as yf
import datetime as dt
import matplotlib.pyplot as plt # add this

aapl = yf.download(tickers=['AAPL'], start=dt.datetime(2021, 1, 1), end=dt.datetime(2023, 9, 6))

aapl = aapl.reset_index()
aapl.columns

aapl.plot(x = 'Date', y = 'Adj Close')

# add this
plt.show()
