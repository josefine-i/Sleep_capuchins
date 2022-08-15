#### Nap Hypothesis ####
library(rstan)
library(brms)
library(cmdstanr)
library(sf)

options(mc.cores = parallel::detectCores()) 

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
hist(napdata$futurenaps, breaks = 100)
#delete one outlier 
napdata = napdata[-which(napdata$futurenaps > 100),]

#shift the entire furturenaps column by .00001 to eliminate 0
napdata$futurenaps=napdata$futurenaps+0.00001

#look for the average minutes of futurenaps
mean(napdata$futurenaps, na.rm = TRUE)

#Correlation model
nap_eff_model <- brm(futurenaps ~ sleep_eff + (sleep_eff | tag),
                     data = napdata[complete.cases(napdata[,c("futurenaps","sleep_eff")]),], 
                     save_pars = save_pars(all = TRUE),
                 prior = c(prior(gamma(2, .1), class = shape),
                           prior(student_t(3, 4, 1.5), class = Intercept),
                           prior(student_t(3, 0, 1.5), class = sd),
                           prior(student_t(3, 0, 1.5), class = b)),
                     family = Gamma(link = "log"),
                     backend = "cmdstanr",
                     control = list(max_treedepth = 10, adapt_delta = .99999))

summary(nap_eff_model)
pp_check(nap_eff_model)
loo_R2(nap_eff_model)

#look cloesly at the interval
h3 = hypothesis(nap_eff_model,c("b_sleep_eff>0","b_sleep_eff<0"
),class="")
print(h3,digits=3)
posterior_interval(nap_eff_model, prob = .89)

#plot the correlation model ans save plot
nap_eff <- conditional_effects(nap_eff_model, spaghetti = TRUE)
plot(conditional_effects(nap_eff_model, spaghetti = TRUE),points = TRUE)


#### Correlation between TST and futurenaps ####

nap_TST_model <- brm(futurenaps ~ TST + (TST | tag),
                 data = napdata[complete.cases(napdata[,c("futurenaps","TST")]),], 
                 save_pars = save_pars(all = TRUE),
                 iter = 2000, 
                 init = 0,
                 prior = c(prior(gamma(2, .1), class = shape),
                           prior(student_t(3, 4, 1.5), class = Intercept),
                           prior(student_t(3, 0, 1.5), class = sd),
                           prior(student_t(3, 0, 1.5), class = b)),
                 family = Gamma(link = "log"),
                 backend = "cmdstanr",
                 control = list(max_treedepth = 10, adapt_delta = .9999))
summary(nap_TST_model)
pp_check(nap_TST_model)
h1 = hypothesis(nap_TST_model,c("b_TST>0","b_TST<0"
),class="")
print(h1,digits=3)
posterior_interval(nap_TST_model, prob = .82)
loo_R2(nap_TST_model)

#plot the correlation model and save plot
nap_TST <- conditional_effects(nap_TST_model, spaghetti = TRUE)
plot(conditional_effects(nap_TST_model, spaghetti = TRUE),points = TRUE)


#### Visualize the nap plots ####

library(ggplot2)
library(RColorBrewer)
library(ggpubr)

#sleep_eff
#design gg plot
nap_eff_plot_gg <- as.data.frame(nap_eff[[1]]) 
eff_nap_plot = ggplot()+
  geom_point (aes(sleep_eff, futurenaps), napdata, size = 1)+
  geom_linerange(aes(sleep_eff, estimate__, ymin = lower__, ymax = upper__, color = "#6BAED6"), nap_eff_plot_gg, show.legend = FALSE)+
  geom_line(aes(sleep_eff, estimate__), nap_eff_plot_gg, size = 2, color = "#08519C")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep during the next day', x = 'sleep efficency')


#TST
nap_TST_plot_gg <- as.data.frame(nap_TST[[1]])
TST_nap_plot = ggplot()+
  geom_point (aes(TST, futurenaps), napdata, size = 1)+
  geom_linerange(aes(TST, estimate__, ymin = lower__, ymax = upper__, color = "#6BAED6"),nap_TST_plot_gg, show.legend = FALSE)+
  geom_line(aes(TST, estimate__), nap_TST_plot_gg, size = 2, color = "#08519C")+
  scale_color_brewer(palette = "Paired")+
  theme_classic() + labs(y = 'sleep during the next day', x = 'total sleep time')


#arrange model plots together
ggarrange(eff_nap_plot, TST_nap_plot, nrow = 1, labels = c('a', 'b') )


