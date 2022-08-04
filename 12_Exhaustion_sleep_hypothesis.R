######### Exhaustion ~ sleep hypothesis ###########

library(rstan)
library(brms)
library(cmdstanr)
library(sf)

options(mc.cores = parallel::detectCores()) 

#### Calculating the traveled distance from GPS data ####
unique(GPSdata$day)
GPSdata$loopID = paste(GPSdata$individual.local.identifier, GPSdata$day, sep = "_")
GPSsplit = split(GPSdata, as.factor(GPSdata$loopID))
traveldistance = c() #create dataframe 

#calculating distance in meters
for (i in 1:length(GPSsplit)){
  ID = unique(GPSsplit[[i]]$individual.local.identifier)
  Date = as.character(min(unique(lubridate::date(GPSsplit[[i]]$timestamp))))
  Z = GPSsplit[[i]]$location.long.2+1i*GPSsplit[[i]]$location.lat.2
  distance = sum(Mod(diff(Z)), na.rm = TRUE) #distance is the sum of distances between GPS points 
  res = data.frame(t(c(ID, Date, distance)))
  traveldistance[i] = list(res)
}
traveldistance = do.call(rbind, traveldistance)

#changing column names
colnames(traveldistance) = c("tag", "date", "distance")
traveldistance$date = as.Date(traveldistance$date)
traveldistance$distance = as.numeric(traveldistance$distance)

# merging the dataframes into one for the models 
exhaustion = merge(sleep_per_nona, traveldistance, by.x = c( "tag", "night" ), by.y = c("tag", "date"), all.x = TRUE)
save(exhaustion, file= "Sleep_per_nona_with_distance&sites")

################################################################################

#### Correlation between distance and ave_vedba ####
#prev_day_ave_vedba = the average vedba (movement during the day) 
#correlation checks if the average vedba can be predicted by the travelled distance 

#check distributions of the variables 
hist(exhaustion$distance, breaks = 100)
hist(exhaustion$prev_day_ave_vedba, breaks = 100)

#drop an outlier in distance 
exhaustion = exhaustion[-which(exhaustion$distance == max(exhaustion$distance)),]

#calculate the mean for the priors 
mean(exhaustion$prev_day_ave_vedba)

#correlation model 
dis_vedba_model <- brm(bf(prev_day_ave_vedba ~ distance + (distance | tag)), 
                             data = exhaustion[complete.cases(exhaustion[,c("prev_day_ave_vedba", "distance")]),],
                             save_pars = save_pars(all = TRUE),
                             iter = 10000,
                             prior = c(
                               prior(student_t(3, 4, 50), class = Intercept),
                               prior(student_t(3, 0, 20), class = sd ),
                               prior(student_t(3, 0, 10), class = b )
                             ),
                             family = student, 
                             backend = "cmdstanr",
                             control = list(max_treedepth = 10, adapt_delta = .999))
summary(dis_vedba_model)
pp_check(dis_vedba_model)
#look closely at the intervals 
posterior_interval(dis_vedba_model, prob = .9)
#plot the model 
conditional_effects(dis_vedba_model, spaghetti = TRUE)
plot(conditional_effects(dis_vedba_model, spaghetti = FALSE),points = TRUE) 
#see how much vedba can be predicted by travelled distance
loo_R2(dis_vedba_model)

#distance was taken as a measurement of physical exhaustion during the day, since most vedba is biologically explained otherwise 

################################################################################

##### Correlation between distance  and sleep efficency of the following night ####
dis_sleep_eff_model <- brm(bf(sleep_eff ~ distance + (distance | tag)), 
                               data = exhaustion[complete.cases(exhaustion[,c("distance")]),],
                               save_pars = save_pars(all = TRUE),
                               iter = 10000,
                               init = 0,
                               prior = c(
                                 prior(normal(0, .5), class = Intercept),
                                 prior(exponential(2), class = sd ),
                                 prior(normal(0, .5), class = b )
                               ),
                               family = Beta (link = "logit"),
                               backend = "cmdstanr",
                               control = list(max_treedepth = 10, adapt_delta = .999))

summary(dis_sleep_eff_model)
pp_check(dis_sleep_eff_model)
#posterior_interval(dis_sleep_eff_model)

#plot the model
conditional_effects(vedba_sleep_eff_model, spaghetti = TRUE)
plot(conditional_effects(vedba_sleep_eff_model, spaghetti = TRUE),points = TRUE) 


################################################################################
####Correlation between TST and sleep sites in overlap home ranges#### 
dis_TST_model <- brm(bf(TST ~ distance + (distance | tag)), 
                      data = exhaustion[complete.cases(exhaustion[,c("distance", "TST")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 2000,
                      prior = c(
                        prior(student_t(3, 482, 50), class = Intercept),
                        prior(normal(0, 10), class = b )
                      ),
                      family = skew_normal, 
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .999))
summary(dis_TST_model)
pp_check(dis_TST_model)

#plot the model
conditional_effects(dis_TST_model, spaghetti = TRUE)
plot(conditional_effects(dis_TST_model, spaghetti = TRUE),points = TRUE) 
