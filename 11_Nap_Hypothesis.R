#### Nap Hypothesis ####

#### Create dataframe with naps on the next day in the row of the night ####

#split the dataframe by tags to assign naps per individual
splitdata=split(sleep_per_nona,as.factor(sleep_per_nona$tag))

#prev_day_sleep_lim = Minutes of sleep during the day, befor the night -> between 7.30 am and 5.30 pm 
#shifting the prev_day_sleep_lim by one day adds it as a next day sleep to the nights
napdata=c()
counts=1
for(i in 1:length(splitdata)){
  naps=c()
  temp=splitdata[[i]]
  for(j in 1:nrow(temp)){
    tempnaps=temp$night[j]+lubridate::days(1)
    if(length(which(temp$night==tempnaps))==0){
      futurenaps=NA
    }else{
      futurenaps=temp$prev_day_sleep_lim[which(temp$night==tempnaps)]
    }
    naps[j]=list(futurenaps)
  }
  naps=unlist(naps)
  temp$futurenaps=naps
  napdata[i]=list(temp)
}
#bind all individuals in one dataframe
napdata=do.call(rbind,napdata)


##### Correlation between sleep_eff and futurenaps ####

#futurenaps = minutes slept during the next day

#plot histogramm of futurenaps
hist(napdata$futurenaps, breaks=100)

#shift the entire furturenaps column by .00001 to eliminate 0
napdata$futurenaps=napdata$futurenaps+0.00001

#look for the average minutes of futurenaps
mean(napdata$futurenaps, na.rm = TRUE)

#Correlation model
nap_model <- brm(futurenaps ~ sleep_eff + (sleep_eff | tag),
                     data = napdata[complete.cases(napdata[,c("futurenaps","sleep_eff")]),], 
                     save_pars = save_pars(all = TRUE),
                 prior = c(prior(gamma(2, .1), class = shape),
                           prior(student_t(3, 4, 1.5), class = Intercept),
                           prior(student_t(3, 0, 1.5), class = sd),
                           prior(student_t(3, 0, 1.5), class = b)),
                     family = Gamma(link = "log"),
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .99999))

summary(nap_model)
pp_check(nap_model)

#look cloesly at the interval
h3 = hypothesis(nap_model,c("b_sleep_eff>0","b_sleep_eff<0"
),class="")
print(h3,digits=3)
posterior_interval(nap_model, prob = .89)

#plot the correlation model 
conditional_effects(nap_model, spaghetti = TRUE)
plot(conditional_effects(nap_model, spaghetti = TRUE),points = TRUE)
#save plot
napplot=plot(conditional_effects(nap_model, spaghetti = TRUE),points = TRUE)[[1]] ##save brms plot as ggplot object
napplot+theme_classic()


#### Correlation between TST and futurenaps ####

nap_TST_model <- brm(futurenaps ~ TST + (TST | tag),
                 data = napdata[complete.cases(napdata[,c("futurenaps","TST")]),], 
                 save_pars = save_pars(all = TRUE),
                 iter = 10000, init = 0,
                 prior = c(prior(gamma(2, .1), class = shape),
                           prior(student_t(3, 4, 1.5), class = Intercept),
                           prior(student_t(3, 0, 1.5), class = sd),
                           prior(student_t(3, 0, 1.5), class = b)),
                 family = Gamma(link = "log"),
                 backend = "cmdstanr",
                 control = list(max_treedepth = 10, adapt_delta = .9999999))
summary(nap_TST_model)
prior_summary(nap_TST_model)

#plot the correlation model 
conditional_effects(nap_TST_model, spaghetti = TRUE)
plot(conditional_effects(nap_TST_model, spaghetti = TRUE),points = TRUE)
#save plot 
nap_TST_plot=plot(conditional_effects(nap_TST_model, spaghetti = TRUE),points = TRUE)[[1]] ##save brms plot as ggplot object
nap_TST_plot+theme_classic()

