######### Exhaustion ~ sleep hypothesis ###########

library(rstan)
library(brms)
library(cmdstanr)
library(sf)

options(mc.cores = parallel::detectCores()) 

# prev_day_ave_vedba = the average vedba (movement during the day)

##### Correlation between prev_ave_vedba and sleep efficency of the following night ####
vedba_sleep_eff_model <- brm(bf(sleep_eff ~ prev_day_ave_vedba + (prev_day_ave_vedba | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                               data = sleep_per_nona[complete.cases(sleep_per_nona[,c("prev_day_ave_vedba")]),],
                               save_pars = save_pars(all = TRUE),
                               iter = 2000,
                               prior = c(
                                 prior(normal(0, 1), class = Intercept),
                                 prior(exponential(2), class = sd ),
                                 prior(normal(0, 1), class = b )
                               ),
                               family = Beta (link = "logit"), #because of the distribution of the rain and temp data
                               backend = "cmdstanr",
                               control = list(max_treedepth = 10, adapt_delta = .999))

summary(vedba_sleep_eff_model)
pp_check(vedba_sleep_eff_model)
#posterior_interval(vedba_sleep_eff_model)

#plot the model
conditional_effects(vedba_sleep_eff_model, spaghetti = TRUE)
plot(conditional_effects(vedba_sleep_eff_model, spaghetti = TRUE),points = TRUE) 


################################################################################
####Correlation between TST and sleep sites in overlap home ranges#### 
vedba_TST_model <- brm(bf(TST ~ prev_day_ave_vedba + (prev_day_ave_vedba | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                         data = sleep_per_nona[complete.cases(sleep_per_nona[,c("prev_day_ave_vedba")]),],
                         save_pars = save_pars(all = TRUE),
                         iter = 2000,
                         prior = c(
                           prior(student_t(3, 482, 50), class = Intercept),
                           #prior(exponential(2), class = sd ),
                           prior(normal(0, 10), class = b )
                         ),
                         family = skew_normal, #because of the distribution of the rain and temp data
                         backend = "cmdstanr",
                         control = list(max_treedepth = 10, adapt_delta = .999))
summary(vedba_TST_model)
pp_check(vedba_TST_model)

#plot the model
conditional_effects(vedba_TST_model, spaghetti = TRUE)
plot(conditional_effects(vedba_TST_model, spaghetti = TRUE),points = TRUE) 
