#####Calculate Correlation between rain and temperature on BCI#####

library(brms)
library(rstan)
library(cmdstanr)

#make a new column raising every datapoints  by .001
temp_data$rain2=temp_data$rain+.001

####First Correlation model ####
weather_model <- brm(rain2 ~ raw,
                     data = temp_data[complete.cases(temp_data[,c("rain","raw")]),], 
                     save_pars = save_pars(all = TRUE),
                     prior = c(prior(gamma(2, .1), class = shape),
                               prior(normal(0, 10), class = Intercept),
                               prior(normal(0, 10), class = b)),
                     family = Gamma(link = "log"),
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .999))
#show the summary of the model
summary(weather_model)
#add a criterion to later compare the model 
weather_model=add_criterion(weather_model, c("loo", "loo_R2"), moment_match = TRUE,
                            backend = "cmdstanr", 
                            control = list(max_treedepth = 10, adapt_delta = .999))

#plot the correlation 
conditional_effects(weather_model, spaghetti = TRUE)
#plot the correlation with every single datapoint
plot(conditional_effects(weather_model, spaghetti = TRUE),points = TRUE)


####Second correlation model####
weather_model2 <- brm(rain ~ raw,
                      data = temp_data[complete.cases(temp_data[,c("rain","raw")]),],
                      save_pars = save_pars(all = TRUE),
                      prior = c(prior(gamma(2, .1), class = shape),
                                prior(normal(0, 10), class = Intercept),
                                prior(normal(0, 10), class = b)),
                      family = hurdle_gamma(),
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .999))

#show the result of the second model 
summary(weather_model2)

#add criterion to later compare the models 
weather_model2=add_criterion(weather_model2, c("loo", "loo_R2"), moment_match = TRUE,
                             backend = "cmdstanr", 
                             control = list(max_treedepth = 10, adapt_delta = .999))
#plot the correlation 
conditional_effects(weather_model2, spaghetti = TRUE)

####Comparing the two models####
loo_compare(weather_model,weather_model2)
loo_R2(weather_model)
loo_R2(weather_model2)
