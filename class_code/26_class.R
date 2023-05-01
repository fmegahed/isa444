

# * Slide 7 ---------------------------------------------------------------

q1 = -327.5 + 0.1668*1981.00
q2 = -327.5 + 0.1668*1981.25
q3 = -327.5 + 0.1668*1981.50
q4 = -327.5 + 0.1668*1981.75


jj = astsa::jj
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
year = time(log_jj)
# Step 3: Fit the regression model
reg_model = lm(log_jj ~ year) 
summary(reg_model) # prints top right  
anova(reg_model) # prints bottom right

# Approach (a):
pred1981a = predict(
  object = reg_model,
  newdata =
    data.frame(year = c(1981, 1981.25, 1981.5, 1981.75))
) 

pred1981a

# Approach (b): I prefer this approach
pred1981b = forecast::forecast(
  object = reg_model,
  # newdata has to have a data.frame as an input with 
  # the variable(s) corresponding to the predictor(s) used in your lm model
  newdata =
    data.frame(year = c(1981, 1981.25, 1981.5, 1981.75)),
  level = 90
)

pred1981b
names(pred1981b)
forecast::autoplot(pred1981b)


# * tslm() for trend adjustment -------------------------------------------

# the approach to tslm is similar to lm
# we use y ~ x
# trend is predefined in the forecast::tslm function
# and extracted automatically from the ts() captured in log_jj
model_tslm =  forecast::tslm(log_jj ~ trend)

summary(model_tslm)

model_tslm |> names()

# lets talk about the data data.frame (trend and season extracted from the log_jj ts)
model_tslm$data # trend is coded such as a unit increase in the trend corresponds to one quarter/period


# * Sourcing the function for resplot -------------------------------------

source('lectures/26_ts_reg/resPlotTS.R')

residuals_tslm = model_tslm$residuals
fitted_tslm = model_tslm$fitted.values

resplot(res = residuals_tslm, fit = fitted_tslm, freq = 4)
forecast::checkresiduals(model_tslm)
