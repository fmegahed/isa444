
#  Linear Regression and its application to TS Data (A Review)

jj = astsa::jj

jj

# Step 1: Plot the ts data (saved to object to not print)
p = forecast::autoplot(jj) + 
  ggplot2::theme_bw() + 
  ggplot2::geom_point(size = 1) 

# stabilizing the variance
log_jj = log(jj) 


# Step 1b: Our updated plot
p2 = forecast::autoplot(log_jj) +
  ggplot2::theme_bw() + 
  ggplot2::geom_point(size = 1)

# Step 2: Extract time
year = time(log_jj) # time is a base R function (stats pkg)
year # notice the values over time

# Step 3: Fit the regression model
reg_model = lm(log_jj ~ year)

options(scipen = 999) # to see the data without the 10^ of something notation

summary(reg_model)
