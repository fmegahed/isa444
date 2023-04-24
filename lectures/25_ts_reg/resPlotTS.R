# A little helpful function for diagnosing the residuals for your TS Regression Model

resplot <- function(res, fit, ncol = 2, nrow = 3, freq = NULL){
  ggplot2::theme_set(ggplot2::theme_bw()) # setting the theme for all ggplots to be black and white
  
  df = data.frame(res = res, fit = fit) # creating a df of residuals and fitted values
  
  # Plot 1: QQ Plot for the residuals
  qq = ggplot2::ggplot(df, ggplot2::aes(sample = res)) + 
    ggplot2::stat_qq() +  ggplot2::stat_qq_line() +  ggplot2::labs(x = "Theoretical Values", y = "Sample")
  
  # Plot 2: Scatter Plot of Residuals Vs. Fitted Values
  scatter = ggplot2::ggplot(df, ggplot2::aes(x = fit, y = res)) + 
    ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::labs(x = "Fitted Values", y = "Residuals")
  
  # Plot 3: Histogram of Residuals with Density Plot
  histogram = ggplot2::ggplot(df, aes(x = res)) + 
    ggplot2::geom_histogram(bins = 15) + ggplot2::geom_density(alpha = 0.2, fill = "red") +
    ggplot2::geom_vline(ggplot2::aes(xintercept= mean(res)), color="red", linetype="dashed", size=1) + ggplot2::labs(x = "Residuals", y = "Count")
  
  # Plot 4: Time Order Plot of Residuals
  if(is.numeric(freq)){
    colorFactor = as.factor( as.numeric(row.names(df)) %% freq ) # coloring based on the modulus operation
    timeOrder = ggplot2::ggplot(df, ggplot2::aes(x = as.numeric(row.names(df)), y = res) ) +
      ggplot2::geom_line() + ggplot2::geom_point(aes(color = colorFactor)) + ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::labs(x = "Observation Order", y = "Residuals") + ggplot2::theme(legend.position = "none")
  }else{
    timeOrder = ggplot2::ggplot(df, ggplot2::aes(x = as.numeric(row.names(df)), y = res)) +
      ggplot2::geom_line() + ggplot2::geom_point() + ggplot2::geom_hline(yintercept = 0) + 
      ggplot2::labs(x = "Observation Order", y = "Residuals") 
  }
  
  # Plot 5: ACF Plot of Residuals
  acfPlot = forecast::ggAcf(res) + ggplot2::labs(title = "")
  
  # Plot 6: ACF Plot of Residuals
  pacfPlot = forecast::ggPacf(res) + ggplot2::labs(title = "")
  
  # Putting all 6 figures together
  ggpubr::ggarrange(qq, scatter, histogram, timeOrder, acfPlot, pacfPlot, ncol = ncol, nrow = nrow)
}