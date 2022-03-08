# Code written on March, 7 2022

pacman::p_load(tidyverse, zoo, fpp2, readxl, magrittr)

wfj = read_excel(path = 'data/WFJ_sales.xlsx') %>% select(c(1,2))

wfj %<>% mutate(
  # overall mean, where I did not loose any data
  mean_f = meanf(y = `WFJ Sales`) %>% .[['fitted']],
  # moving average of 3
  ma3_f = rollmeanr(x = `WFJ Sales`, k =3, na.pad = T) %>% lag(),
  # moving average of 7
  ma7_f = rollmeanr(x = `WFJ Sales`, k =7, na.pad = T) %>% lag(),
  # ses with alpha = 0.2
  ses_0.2 = ses(y = `WFJ Sales`, alpha = 0.2, initial = 'simple') %>%
    .[['fitted']],
  ses_0.2b = ses(y = `WFJ Sales`, alpha = 0.2, initial = 'simple') %>%
    extract2('fitted')
) %>% as_tibble() # as tibble just makes it a better printed df

wfj

# What does the word fitted do here?
ses_model = ses(y = wfj$`WFJ Sales`, alpha = 0.2, initial = 'simple')
# based on ? fitted = Fitted values (one-step forecasts)
# the .[['fitted]] means that we are extracting the sublist titled "fitted"
# from out data --> . is used in a pipe to do that
fitted_ses = ses_model$fitted


# Scenario 1: ignoring the fact that we have different number of observations
scenario1_acc = rbind(
  accuracy(object = wfj$mean_f, x = wfj$`WFJ Sales`),
  accuracy(object = wfj$ma3_f, x = wfj$`WFJ Sales`),
  accuracy(object = wfj$ma7_f, x = wfj$`WFJ Sales`),
  accuracy(object = wfj$ses_0.2, x = wfj$`WFJ Sales`)
)
row.names(scenario1_acc) = c('overall_mean', 'ma3', 'ma7', 'ses')
scenario1_acc


# scenario 2: accounting for the number of different obs
wfj_complete = na.omit(wfj) # a total of 62 - 7 obs

scenario2_acc = rbind(
  accuracy(object = wfj_complete$mean_f, x = wfj_complete$`WFJ Sales`),
  accuracy(object = wfj_complete$ma3_f, x = wfj_complete$`WFJ Sales`),
  accuracy(object = wfj_complete$ma7_f, x = wfj_complete$`WFJ Sales`),
  accuracy(object = wfj_complete$ses_0.2, x = wfj_complete$`WFJ Sales`)
)
row.names(scenario2_acc) = c('overall_mean', 'ma3', 'ma7', 'ses')
scenario2_acc

scenario1_acc  # printing it again for comparison



# * Methods that require validation ---------------------------------------

wfj %>% ggplot(aes(x = Obs, y = `WFJ Sales`)) + geom_line() + geom_point()


train_data = wfj[1:26, 1:2] # keep only the first two cols 

ses_opt_model = ses(y = train_data$`WFJ Sales`) 
summary(ses_opt_model)

# fit that model over the entire data

wfj %<>% mutate(ses_opt = ses(y = `WFJ Sales`, 
                              alpha = ses_opt_model$model$par[1], 
                              initial = 'simple') %>% extract2('fitted'),
                ses_opt_w_opt_init = ses(y = `WFJ Sales`, 
                                         alpha = ses_opt_model$model$par[1], 
                                         initial = 'optimal') %>% 
                  extract2('fitted')) 
wfj

valid_data = wfj[27:62, ]
# one issue is that mean_f should only be based on the training data
# mean of the first 26 observations
valid_data

# you can calculate all the accuracy metrics on the validation data
scenario_op = rbind(
  accuracy(object = valid_data$mean_f, x = valid_data$`WFJ Sales`),
  accuracy(object = valid_data$ma3_f, x = valid_data$`WFJ Sales`),
  accuracy(object = valid_data$ma7_f, x = valid_data$`WFJ Sales`),
  accuracy(object = valid_data$ses_0.2, x = valid_data$`WFJ Sales`),
  accuracy(object = valid_data$ses_opt, x = valid_data$`WFJ Sales`)
)
scenario_op
