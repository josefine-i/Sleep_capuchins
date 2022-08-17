####Effect of Weather (rain and temp) on sleep ####

library(rstan)
library(brms)
library(cmdstanr)
library(sf)

options(mc.cores = parallel::detectCores()) 

################################################################################
####sleep efficency####

####Correlation between sleep efficency and rain####
sleep_model <- brm(bf(sleep_eff ~ rain + (rain | tag)), 
                   data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain")]),],
                   save_pars = save_pars(all = TRUE),
                   iter = 2000,
                   init = 0, #because log it function needs to start sampling above 0
                   prior = c(
                     prior(normal(0, .5), class = Intercept),
                     prior(exponential(2), class = sd ),
                     prior(normal(0, .5), class = b )
                   ),
                   family = Beta (link = "logit"), #because of the distribution of the rain and temp data
                   backend = "cmdstanr",
                   control = list(max_treedepth = 10, adapt_delta = .999))

summary(sleep_model)
pp_check(sleep_model)
#adding criterion for comparison later 
sleep_model=add_criterion(sleep_model, c("loo", "loo_R2"), moment_match = TRUE,
                            backend = "cmdstanr", 
                            control = list(max_treedepth = 10, adapt_delta = .999))
#plot the model
conditional_effects(sleep_model, spaghetti = TRUE)
plot(conditional_effects(sleep_model, spaghetti = TRUE),points = TRUE) #with all datapoints

####second model: rain + temp####
#with temp -> to see if it has an effect 
sleep_model_2 <- brm(bf(sleep_eff ~ rain + temp + (rain + temp | tag),decomp = "QR"), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                     data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain","temp")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     init = 0,
                     prior = c(
                       prior(normal(0, .5), class = Intercept),
                       prior(exponential(2), class = sd ),
                       prior(normal(0, .5), class = b )
                     ),
                     family = Beta(link = "logit"), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .999999999))

summary(sleep_model_2)
posterior_interval(sleep_model_2)
pp_check(sleep_model_2)
#adding criterion for later comparison 
sleep_model_2 =  add_criterion(sleep_model_2, c("loo", "loo_R2"), moment_match = TRUE,
                            backend = "cmdstanr", 
                            control = list(max_treedepth = 10, adapt_delta = .999999999))

loo(sleep_model_2)
#plot model 2
rain_eff_plot <- conditional_effects(sleep_model_2, spaghetti = TRUE)
plot(conditional_effects(sleep_model_2, spaghetti = TRUE),points = TRUE) #with all datapoints


#test to see how much of the interval is above or below 0
h1 = hypothesis(sleep_model_2,c("b_rain>0","b_rain<0",
                               "b_temp>0","b_temp<0"),class="")
print(h1,digits=3)


####Third model: temp####
sleep_model_3 <- brm(bf(sleep_eff ~ temp + (temp | tag)), 
                   data = sleep_per_nona[complete.cases(sleep_per_nona[,c("temp")]),],
                   save_pars = save_pars(all = TRUE),
                   iter = 2000,
                   init = 0, #because log it function needs to start sampling above 0
                   prior = c(
                     prior(normal(0, .5), class = Intercept),
                     prior(exponential(2), class = sd ),
                     prior(normal(0, .5), class = b )
                   ),
                   family = Beta (link = "logit"), #because of the distribution of the rain and temp data
                   backend = "cmdstanr",
                   control = list(max_treedepth = 10, adapt_delta = .99999999))

summary(sleep_model_3)
pp_check(sleep_model_3)
posterior_interval(sleep_model_3)
#adding criterion for comparison later 
sleep_model_3 = add_criterion(sleep_model_3, c("loo", "loo_R2"), moment_match = TRUE,
                          backend = "cmdstanr", 
                          control = list(max_treedepth = 10, adapt_delta = .99999999))
#plot the model and save the plot 
temp_eff <- conditional_effects(sleep_model_3, spaghetti = TRUE)
plot(conditional_effects(sleep_model_3, spaghetti = TRUE),points = TRUE) #with all datapoints

#compare the temp models#
loo_compare(sleep_model_3,sleep_model_2)
loo_R2(sleep_model_3)
loo_R2(sleep_model_2)
bayes_R2(sleep_model_2)

#test the interval of temp in sleep model 3
h2 = hypothesis(sleep_model_3,c("b_temp>0","b_temp<0"),class="")
print(h2,digits=3)

#####Fourth model: rain and temp and together####
sleep_model_all <- brm(bf(sleep_eff ~ rain + temp + rain:temp + (rain + temp | tag), decomp = "QR"),
                       data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain","temp")]),],
                       save_pars = save_pars(all = TRUE),
                       iter = 3000,
                       init = 0,
                       prior = c(
                         prior(normal(0, .5), class = Intercept),
                         prior(exponential(2), class = sd ),
                         prior(normal(0, .5), class = b )
                       ),
                       family = Beta(link = "logit"), #because of the distribution of the rain and temp data
                       backend = "cmdstanr",
                       control = list(max_treedepth = 10, adapt_delta = .99999999))
summary(sleep_model_all)
pp_check(sleep_model_all)  

#adding criterion for comparison later 
sleep_model_all = add_criterion(sleep_model_all, c("loo", "loo_R2"), moment_match = TRUE,
                              backend = "cmdstanr", 
                              control = list(max_treedepth = 10, adapt_delta = .99999999))
#plot the model
conditional_effects(sleep_model_all, spaghetti = TRUE)
plot(conditional_effects(sleep_model_all, spaghetti = TRUE),points = TRUE) #with all datapoints

####compare all sleep_eff models####
#rain models
####compare the rain models####
loo_compare(sleep_model,sleep_model_2)
loo_compare(sleep_model_all,sleep_model, sleep_model_2)
#temp models
loo_compare(sleep_model_3,sleep_model_2)
loo_compare(sleep_model_all,sleep_model_3, sleep_model_2)
#comparative models 
loo_compare(sleep_model_all,sleep_model_2)

loo_R2(sleep_model)
loo_R2(sleep_model_2)
loo_R2(sleep_model_3)
loo_R2(sleep_model_all)


################################################################################

####Looking at TST####
hist(sleep_per_nona$TST)

####Effect of rain on TST####
TST_rain_model <- brm(bf(TST ~ rain + (rain | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                   data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain")]),],
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
summary(TST_rain_model)
pp_check(TST_rain_model)
#check actual interval
h3 = hypothesis(TST_rain_model,c("b_rain>0","b_rain<0"
                                ),class="")
print(h3,digits=3)
#plot the model 
conditional_effects(TST_rain_model, spaghetti = TRUE)
plot(conditional_effects(TST_rain_model, spaghetti = TRUE),points = TRUE) 
#adding criterion for comparison later 
TST_rain_model = add_criterion(TST_rain_model, c("loo", "loo_R2"), moment_match = TRUE,
                              backend = "cmdstanr", 
                              control = list(max_treedepth = 10, adapt_delta = .999))


#### Effect of rain and temp on TST####
mu_intercept_TST = mean(sleep_per_nona$TST)
TST_model_2 <- brm(bf(TST ~ rain + temp + (rain + temp | tag),decomp = "QR"), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                     data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain","temp")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     prior = c(
                       prior(student_t(3, 482, 50), class = Intercept),
                       #prior(exponential(2), class = sd ),
                      prior(normal(0, 10), class = b )),
                     family = skew_normal(), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .99999))
prior_summary(TST_model_2)
summary(TST_model_2)
bayes_R2(TST_model_2)
pp_check(TST_model_2)
#plot the model and save the plot 
rain_TST_plot <-conditional_effects(TST_model_2, spaghetti = TRUE)
temp_TST <-conditional_effects(TST_model_2, spaghetti = TRUE)
plot(conditional_effects(TST_model_2, spaghetti = TRUE),points = TRUE) 
#adding criterion for comparison later 
TST_model_2 = add_criterion(TST_model_2, c("loo", "loo_R2"), reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .99999))


####Effect of temp on TST####
TST_temp_model <- brm(bf(TST ~ temp + (temp | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_per_nona[complete.cases(sleep_per_nona[,c("temp")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      prior = c(
                        prior(student_t(3, 482, 50), class = Intercept),
                        #prior(exponential(2), class = sd ),
                        prior(normal(0, 10), class = b )
                      ),
                      family = skew_normal, #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .99999999))
summary(TST_temp_model)
pp_check(TST_temp_model)

#plot the model 
conditional_effects(TST_temp_model, spaghetti = TRUE)
plot(conditional_effects(TST_temp_model, spaghetti = TRUE),points = TRUE) 
#adding criterion for comparison later 
TST_temp_model = add_criterion(TST_temp_model, c("loo", "loo_R2"), moment_match = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .99999999))


####Effect of temp and rain together in TST####
TST_model_all <- brm(bf(TST ~ rain + temp + rain:temp + (rain + temp | tag ), decomp = "QR"),
                     data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain","temp")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 2000,
                     prior = c(
                       prior(student_t(3, 482, 50), class = Intercept),
                       #prior(exponential(2), class = sd ),
                       prior(normal(0, 10), class = b )),
                     family = skew_normal(), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .999))
summary(TST_model_all)
pp_check(TST_model_all)

#plot the model 
conditional_effects(TST_model_all, spaghetti = TRUE)
plot(conditional_effects(TST_model_all, spaghetti = TRUE),points = TRUE) 
#adding criterion for comparison later 
TST_model_all = add_criterion(TST_model_all, c("loo", "loo_R2"), moment_match = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999))


####compare all TST models####
#rain models 
loo_compare(TST_rain_model,TST_model_2)
loo_compare(TST_model_all,TST_rain_model, TST_model_2)
#temp models 
loo_compare(TST_model_all,TST_temp_model, TST_model_2)
loo_compare(TST_temp_model, TST_model_2)
#comparative models 
loo_compare(TST_model_all,TST_model_2)

loo_R2(TST_rain_model)
loo_R2(TST_model_2)
loo_R2(TST_temp_model)
loo_R2(TST_model_all)


                     

#################################################################################
####SPT####
hist(sleep_per_nona$SPT)
mean(sleep_per_nona$SPT)
####Effect of rain on SPT####
SPT_rain_model <- brm(bf(SPT ~ rain + (rain | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      prior = c(
                        prior(student_t(3, 629, 70), class = Intercept),
                        #prior(exponential(2), class = sd ),
                        prior(normal(0, 20), class = b )
                      ),
                      family = skew_normal(), #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .9999))
summary(SPT_rain_model)
prior_summary(SPT_rain_model)
pp_check(SPT_rain_model)

#plot the model
conditional_effects(SPT_rain_model, spaghetti = TRUE)
plot(conditional_effects(SPT_rain_model, spaghetti = TRUE),points = TRUE) 

#add criterion for later comparison 
SPT_rain_model = add_criterion(SPT_rain_model, c("loo", "loo_R2"), reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .9999))


####Effect of rain and temp  on SPT####
SPT_model_2 <- brm(bf(SPT ~ rain + temp + (rain + temp | tag),decomp = "QR"), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                   data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain","temp")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 3000,
                      prior = c(
                        prior(student_t(3, 629, 70), class = Intercept),
                        #prior(exponential(2), class = sd ),
                        prior(normal(0, 20), class = b )
                      ),
                      family = skew_normal(), #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .99999))
summary(SPT_model_2)
pp_check(SPT_model_2)
prior_summary(SPT_model_2)
#look at intervals
h4 = hypothesis(SPT_model_2,c("b_rain>0","b_rain<0",
                                "b_temp>0","b_temp<0"),class="")
print(h4,digits=3)

#plot the model and save the plot 
rain_SPT_plot <-conditional_effects(SPT_model_2, spaghetti = TRUE)
temp_SPT <-conditional_effects(SPT_model_2, spaghetti = TRUE)
plot(conditional_effects(SPT_model_2, spaghetti = TRUE),points = TRUE) 

#add criterion for later comparison 
SPT_model_2 = add_criterion(SPT_model_2, c("loo", "loo_R2"), moment_match = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .99999))




####Effect of temp ion SPT####
SPT_temp_model <- brm(bf(SPT ~ temp + (temp | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                      data = sleep_per_nona[complete.cases(sleep_per_nona[,c("temp")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      prior = c(
                        prior(student_t(3, 629, 70), class = Intercept),
                        #prior(exponential(2), class = sd ),
                        prior(normal(0, 20), class = b )
                      ),
                      family = skew_normal(), #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .999999))
summary(SPT_temp_model)
pp_check(SPT_temp_model)

#plot the model
conditional_effects(SPT_temp_model, spaghetti = TRUE)
plot(conditional_effects(SPT_temp_model, spaghetti = TRUE),points = TRUE) 

#add criterion for later comparison 
SPT_temp_model = add_criterion(SPT_temp_model, c("loo", "loo_R2"), moment_match = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999999))


#### Effect of rain and temp together on SPT####
SPT_model_all <- brm(bf(SPT ~ rain + temp + rain:temp + (rain + temp | tag ), decomp = "QR"),
                     data = sleep_per_nona[complete.cases(sleep_per_nona[,c("rain","temp")]),],
                     save_pars = save_pars(all = TRUE),
                     iter = 4000,
                     prior = c(
                       prior(student_t(3, 629, 70), class = Intercept),
                       #prior(exponential(2), class = sd ),
                       prior(normal(0, 20), class = b )
                     ),
                     family = skew_normal(), #because of the distribution of the rain and temp data
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .999))
summary(SPT_model_all)
pp_check(SPT_model_all)

#plot the model
conditional_effects(SPT_model_all, spaghetti = TRUE)
plot(conditional_effects(SPT_model_all, spaghetti = TRUE),points = TRUE) 

#add criterion for later comparison 
SPT_model_all = add_criterion(SPT_model_all, c("loo", "loo_R2"), reloo = TRUE,
                               backend = "cmdstanr", 
                               control = list(max_treedepth = 10, adapt_delta = .999))

####compare all SPT models####
#rain models
loo_compare(SPT_model_all,SPT_rain_model, SPT_model_2)
loo_compare(SPT_rain_model,SPT_model_2)
#temp models
loo_compare(SPT_model_all,SPT_temp_model, SPT_model_2)
loo_compare(SPT_temp_model,SPT_model_2)
#comparative models 
loo_compare(SPT_model_all,SPT_model_2)

loo_R2(SPT_rain_model)
loo_R2(SPT_temp_model)
loo_R2(SPT_model_2)
loo_R2(SPT_model_all)

################################################################################

library(ggplot2)
library(RColorBrewer)
library(ggpubr)

#### Visualize the rain plots ####

#sleep_eff
#design gg plot
rain_eff_plot_gg <- as.data.frame(rain_eff_plot[[1]]) 
eff_rain_plot = ggplot()+
  geom_point (aes(rain, sleep_eff), sleep_per_nona, size = 1)+
  geom_linerange(aes(rain, estimate__, ymin = lower__, ymax = upper__, color = "#9ECAE1"), rain_eff_plot_gg, show.legend = FALSE)+
  geom_line(aes(rain, estimate__), rain_eff_plot_gg, size = 2.5, color = "#08306B")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep efficency', x = 'rainfall (mm)')


#TST
rain_TST_plot_gg <- as.data.frame(rain_TST_plot[[1]])
TST_rain_plot = ggplot()+
  geom_point (aes(rain, TST), sleep_per_nona, size = 1)+
  geom_linerange(aes(rain, estimate__, ymin = lower__, ymax = upper__, color = "#9ECAE1"),rain_TST_plot_gg, show.legend = FALSE)+
  geom_line(aes(rain, estimate__), rain_TST_plot_gg, size = 2.5, color = "#08306B")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'total sleep time (min)', x = 'rainfall (mm)')

#SPT
#design gg plot
rain_SPT_plot_gg <- as.data.frame(rain_SPT_plot[[1]])
SPT_rain_plot = ggplot()+
  geom_point (aes(rain, SPT), sleep_per_nona, size = 1)+
  geom_linerange(aes(rain, estimate__, ymin = lower__, ymax = upper__, color = "#9ECAE1"),rain_SPT_plot_gg, show.legend = FALSE)+
  geom_line(aes(rain, estimate__), rain_SPT_plot_gg, size = 2.5, color = "#08306B")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep period time (min)', x = 'rainfall (mm)')

#arrange model plots together
ggarrange(eff_rain_plot, TST_rain_plot, SPT_rain_plot, labels = c('a', 'b', 'c') )

#### Visualize the temp plots ####
#sleep_eff
#design gg plot
temp_eff_plot_gg <- as.data.frame(temp_eff[[1]]) 
eff_temp_plot = ggplot()+
  geom_point (aes(temp, sleep_eff), sleep_per_nona, size = 1)+
  geom_linerange(aes(temp, estimate__, ymin = lower__, ymax = upper__, color = "#C6DBEF"), temp_eff_plot_gg, show.legend = FALSE)+
  geom_line(aes(temp, estimate__), temp_eff_plot_gg, size = 2.5, color = "#2171B5")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep efficency', x = 'temperature (°C)')


#TST
temp_TST_plot_gg <- as.data.frame(temp_TST[[2]])
TST_temp_plot = ggplot()+
  geom_point (aes(temp, TST), sleep_per_nona, size = 1)+
  geom_linerange(aes(temp, estimate__, ymin = lower__, ymax = upper__, color = "#C6DBEF"),temp_TST_plot_gg, show.legend = FALSE)+
  geom_line(aes(temp, estimate__), temp_TST_plot_gg, size = 2.5, color = "#2171B5")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'total sleep time (min)', x = 'temperature (°C)')

#SPT
#design gg plot
temp_SPT_plot_gg <- as.data.frame(temp_SPT[[2]])
SPT_temp_plot = ggplot()+
  geom_point (aes(temp, SPT), sleep_per_nona, size = 1)+
  geom_linerange(aes(temp, estimate__, ymin = lower__, ymax = upper__, color = "#C6DBEF"),temp_SPT_plot_gg, show.legend = FALSE)+
  geom_line(aes(temp, estimate__), temp_SPT_plot_gg, size = 2.5, color = "#2171B5")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep period time (min)', x = 'temperature (°C)')

#arrange model plots together
ggarrange(eff_temp_plot, TST_temp_plot, SPT_temp_plot, labels = c('a', 'b', 'c') )

