
fred_tbl = tidyquant::tq_get(
  x = c('GDP', 'UNRATE'), # the fred indicators we are interested in
  get = 'economic.data', # denotes that the data is from FRED
  from = '2010-01-01' # start at Jan 01, 2010
)

fred_tbl |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = price, group = symbol)) + # with the pipe I passing data = fred_tbl
  ggplot2::geom_line() +
  ggplot2::facet_wrap(facets = 'symbol', scales = 'free_y', ncol =  1)
