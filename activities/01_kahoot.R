

# * Cincy Weather Plot ----------------------------------------------------

cincy_weather = readr::read_csv('data/cincy_monthly_weather.csv')

cincy_long = cincy_weather |> 
  tidyr::pivot_longer(cols = 2:13, names_to = 'month', values_to = 'temp') |> 
  dplyr::mutate(
    date = lubridate::ymd( paste(year, month, '01', sep = '-') ),
    month = factor(month, levels = tolower( month.abb) )
    )

monthly_plot = 
  cincy_long |> 
  dplyr::filter(month %in% c('jan', 'apr','jul', 'oct')) |> 
  ggplot2::ggplot(ggplot2::aes(x = date, y = temp)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 1.5, ggplot2::aes(color = month)) +
  ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = 20)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  ggplot2::scale_color_brewer(palette = 'Dark2') +
  ggplot2::labs(x = 'Year', y = 'Degrees (F)', title = 'Cincy Monthly Temp (Limited to 1 Month Per Quarter)', color = 'Quarter') +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = 'bottom') +
  ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) 

ggplot2::ggsave(filename = 'figures/cincy_monthly_weather.png', plot = monthly_plot,
                width = 8.5, height = 4)

annual_plot = 
  cincy_long |> 
  ggplot2::ggplot(ggplot2::aes(x = year, y = annual)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  ggplot2::labs(x = 'Year', y = 'Degrees (F)', title = 'Cincy Annual Temp') +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid = ggplot2::element_blank()) +
  ggplot2::geom_smooth(method = "lm", se = FALSE)

ggplot2::ggsave(filename = 'figures/cincy_annual_weather.png', plot = annual_plot,
                width = 8.5, height = 4)


ggpubr::ggarrange(monthly_plot, annual_plot, nrow = 2)
