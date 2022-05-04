# A little helpful function for diagnosing the residuals for your TS Regression Model

resplot <- function(res, fit, ncol = 2, nrow = 3, freq = NULL){
  
  # check to see if packages are installed (and install them if needed)
  if(require(ggplot2) == FALSE) install.packages(ggplot2)
  if(require(fpp2) == FALSE) install.packages(fpp2)
  if(require(ggpubr) == FALSE) install.packages(ggpubr)
  
  library(ggplot2)
  library(fpp2)
  library(ggpubr)
  
  
  theme_set(theme_bw()) # setting the theme for all ggplots to be black and white
  
  df = data.frame(res = res, fit = fit) # creating a df of residuals and fitted values
  
  # Plot 1: QQ Plot for the residuals
  qq = ggplot(df, aes(sample = res)) + 
    stat_qq() + stat_qq_line() + labs(x = "Theoretical Values", y = "Sample")
  
  # Plot 2: Scatter Plot of Residuals Vs. Fitted Values
  scatter = ggplot(df, aes(x = fit, y = res)) + 
    geom_point() + geom_hline(yintercept = 0) +
    labs(x = "Fitted Values", y = "Residuals")
  
  # Plot 3: Histogram of Residuals with Density Plot
  histogram = ggplot(df, aes(x = res)) + geom_histogram(bins = 15) +
    geom_density(alpha = 0.2, fill = "red") +
    geom_vline(aes(xintercept= mean(res)),
               color="red", linetype="dashed", size=1) +
    labs(x = "Residuals", y = "Count")
  
  # Plot 4: Time Order Plot of Residuals
  if(is.numeric(freq)){
    colorFactor = as.factor( as.numeric(row.names(df)) %% freq ) # coloring based on the modulus operation
    timeOrder = ggplot(df, aes(x = as.numeric(row.names(df)), y = res) ) +
      geom_line() + geom_point(aes(color = colorFactor)) + geom_hline(yintercept = 0) + 
      labs(x = "Observation Order", y = "Residuals") + theme(legend.position = "none")
    
  }else{
    timeOrder = ggplot(df, aes(x = as.numeric(row.names(df)), y = res)) +
      geom_line() + geom_point() + geom_hline(yintercept = 0) + 
      labs(x = "Observation Order", y = "Residuals") 
  }
  
  # Plot 5: ACF Plot of Residuals
  acfPlot = ggAcf(res) + labs(title = "")
  
  # Plot 6: ACF Plot of Residuals
  pacfPlot = ggPacf(res) + labs(title = "")
 
  # Putting all 6 figures together
  ggarrange(qq, scatter, histogram, timeOrder, acfPlot, pacfPlot,
            ncol = ncol, nrow = nrow)
}
