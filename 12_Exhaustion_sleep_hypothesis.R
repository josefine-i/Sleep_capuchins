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

exhaustion$SPT = as.integer(exhaustion$SPT)
save(exhaustion, file= "Exhaustion")
write.csv(exhaustion, '/Volumes/INTENSO/Bachelorarbeit/exhaustion.csv')
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

dis_sleep_eff_model2 <- brm(bf(sleep_eff ~ distance + (distance | tag)), 
                            data = exhaustion[complete.cases(exhaustion[,c("distance", "sleep_eff")]),],
                            save_pars = save_pars(all = TRUE),
                            iter = 30000,
                            init = 0,
                            prior = c(
                              prior(normal(0, 1), class = Intercept),
                              prior(exponential(.8), class = sd, ),
                              prior(exponential(.8), class = sd, group = tag ),
                              prior(exponential(.8), class = sd, group = tag, coef = distance ),
                              prior(exponential(.8), class = sd, group = tag, coef = Intercept ),
                              prior(normal(0, .5), class = b )
                            ),
                            family = Beta(link = "logit"),
                            backend = "cmdstanr",
                            control = list(max_treedepth = 10, adapt_delta = .999999999999999))

summary(dis_sleep_eff_model2)
pp_check(dis_sleep_eff_model2)
posterior_interval(dis_sleep_eff_model2)

h1 = hypothesis(dis_sleep_eff_model2,c("b_distance>0","b_distance<0"
),class="")
print(h1,digits=3)


#plot the model and save the plot 
dis_eff <- conditional_effects(dis_sleep_eff_model2, spaghetti = TRUE)
plot(conditional_effects(dis_sleep_eff_model2, spaghetti = TRUE),points = TRUE) 


################################################################################

####Correlation between TST and travelled distance during the day #### 

mean(exhaustion$TST)

dis_TST_model <- brm(bf(TST ~ distance + (distance | tag)), 
                      data = exhaustion[complete.cases(exhaustion[,c("distance", "TST")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 10000,
                      init = 0, 
                      prior = c(
                        prior(student_t(3, 482, 30), class = Intercept),
                        prior(student_t(3, 0, 20), class = b ),
                        prior(normal(0, 10), class = sd)
                      ),
                      family = skew_normal, 
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta = .999))
summary(dis_TST_model)
pp_check(dis_TST_model)

h2 = hypothesis(dis_TST_model,c("b_distance>0","b_distance<0"
),class="")
print(h2,digits=3)

#plot the model and save the plot
dis_TST <- conditional_effects(dis_TST_model, spaghetti = TRUE)
plot(conditional_effects(dis_TST_model, spaghetti = TRUE),points = TRUE) 

################################################################################

hist(exhaustion$SPT, breaks = 100)

dis_SPT_model <- brm(bf(SPT ~ distance + (distance | tag)), 
                      data = exhaustion[complete.cases(exhaustion[,c("distance")]),],
                      save_pars = save_pars(all = TRUE),
                      iter = 10000,
                      init = 0,
                      prior = c(
                        prior(student_t(3, 629, 70), class = Intercept),
                        prior(student_t(3, 0, 10), class = alpha ),
                        prior(normal(0, 20), class = b )
                      ),
                      family = skew_normal(), #because of the distribution of the rain and temp data
                      backend = "cmdstanr",
                      control = list(max_treedepth = 10, adapt_delta =  .9999999))
summary(dis_SPT_model)
pp_check(dis_SPT_model)

h3 = hypothesis(dis_SPT_model,c("b_distance>0","b_distance<0"
),class="")
print(h3,digits=3)

#plot the model and save the plot
dis_SPT <- conditional_effects(dis_SPT_model, spaghetti = TRUE)
plot(conditional_effects(dis_SPT_model, spaghetti = TRUE),points = TRUE) 


################################################################################

#### Visualize the correlation models ####
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

#sleep_eff
#design gg plot
dis_eff_plot_gg <- as.data.frame(dis_eff[[1]]) 
eff_dis_plot = ggplot()+
  geom_point (aes(distance, sleep_eff), exhaustion, size = 1)+
  geom_linerange(aes(distance, estimate__, ymin = lower__, ymax = upper__, color = "#C6DBEF"), dis_eff_plot_gg, show.legend = FALSE)+
  geom_line(aes(distance, estimate__), dis_eff_plot_gg, size = 2, color = "#2171B5")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep efficency', x = 'travelled distance')


#TST
dis_TST_plot_gg <- as.data.frame(dis_TST[[1]])
TST_dis_plot = ggplot()+
  geom_point (aes(distance, TST), exhaustion, size = 1)+
  geom_linerange(aes(distance, estimate__, ymin = lower__, ymax = upper__, color = "#C6DBEF"),dis_TST_plot_gg, show.legend = FALSE)+
  geom_line(aes(distance, estimate__), dis_TST_plot_gg, size = 2, color = "#2171B5")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'total sleep time', x = 'travelled distance')

#SPT
dis_SPT_plot_gg <- as.data.frame(dis_SPT[[1]])
SPT_dis_plot = ggplot()+
  geom_point (aes(distance, SPT), exhaustion, size = 1)+
  geom_linerange(aes(distance, estimate__, ymin = lower__, ymax = upper__, color = "#C6DBEF"),dis_SPT_plot_gg, show.legend = FALSE)+
  geom_line(aes(distance, estimate__), dis_SPT_plot_gg, size = 2, color = "#2171B5")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep period time', x = 'travelled distance')


#arrange model plots together
ggarrange(eff_dis_plot, TST_dis_plot, nrow = 1, labels = c('a', 'b') )
ggarrange(eff_dis_plot, TST_dis_plot, SPT_dis_plot, nrow = 1, labels = c('a', 'b', 'c') )
