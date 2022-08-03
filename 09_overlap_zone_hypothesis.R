####OVERLAP ZONES HYPOTHESIS

##### Correlation between home range and sleep efficency ####
#make overlap column a factor 
overlap_sleep_eff_model <- brm(bf(sleep_eff ~ overlap + (overlap | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                           data = Sleep_overlap_clean[complete.cases(Sleep_overlap_clean[,c("overlap")]),],
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

summary(overlap_sleep_eff_model)
pp_check(overlap_sleep_eff_model)
posterior_interval(overlap_sleep_eff_model)

#plot the model
conditional_effects(overlap_sleep_eff_model, spaghetti = TRUE)
overlap_eff_plot = plot(conditional_effects(overlap_sleep_eff_model, spaghetti = TRUE),points = TRUE)[[1]] 
#design gg plot
overlap_eff_plot_gg = overlap_eff_plot + theme_classic() + labs(y = 'sleep efficency', x = 'inside overlapping home ranges')
plot(overlap_eff_plot_gg)

################################################################################
####Correlation between TST and sleep sites in overlap home ranges#### 
overlap_TST_model <- brm(bf(TST ~ overlap + (overlap | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                   data = Sleep_overlap_clean[complete.cases(Sleep_overlap_clean[,c("overlap")]),],
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
summary(overlap_TST_model)
pp_check(overlap_TST_model)

#plot the model
conditional_effects(overlap_TST_model, spaghetti = TRUE)
TST_overlap_plot = plot(conditional_effects(overlap_TST_model, spaghetti = TRUE),points = TRUE) [[1]]
#design gg plot
TST_overlap_plot_gg = TST_overlap_plot + theme_classic() + labs(y = 'total sleep time', x = 'inside overlapping home ranges')


################################################################################
####Correlation between SPT and sleep sites in overlap home ranges#### 
overlap_SPT_model <- brm(bf(SPT ~ overlap + (overlap | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                         data = Sleep_overlap_clean[complete.cases(Sleep_overlap_clean[,c("overlap")]),],
                         save_pars = save_pars(all = TRUE),
                         iter = 2000,
                         prior = c(
                           prior(student_t(3, 629, 50), class = Intercept),
                           #prior(exponential(2), class = sd ),
                           prior(normal(0, 10), class = b )
                         ),
                         family = skew_normal, #because of the distribution of the rain and temp data
                         backend = "cmdstanr",
                         control = list(max_treedepth = 10, adapt_delta = .999))
summary(overlap_SPT_model)
pp_check(overlap_SPT_model)

#plot the model
conditional_effects(overlap_SPT_model, spaghetti = TRUE)
SPT_overlap_plot = plot(conditional_effects(overlap_SPT_model, spaghetti = TRUE),points = TRUE) [[1]]
#design gg plot
SPT_overlap_plot_gg = SPT_overlap_plot + theme_classic() + labs(y = 'sleep period time', x = 'inside overlapping home ranges')

#arrange model plots together
ggarrange(overlap_eff_plot_gg, TST_overlap_plot_gg, SPT_overlap_plot_gg, nrow = 1, labels = c('a', 'b', 'c') )


####Visualize the overlap####
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

#make new dataframe
#overlap = NA
overlap <- data.frame ( 'YES' = nrow(Sleep_overlap_clean[which(Sleep_overlap_clean$overlap == 'YES'),]), 'NO' = nrow(Sleep_overlap_clean[which(Sleep_overlap_clean$overlap == 'NO'),]) )

#boxplot TST
TST_overlap_box = ggplot(Sleep_overlap_clean,
       aes(x = overlap, y = TST, fill = overlap) ) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Paired")+
  theme_classic()+
  labs(x = "sleep in overlap zone",
       y = "total sleep time")

#boxplot sleep_eff
eff_overlap_box = ggplot(Sleep_overlap_clean,
                         aes(x = overlap, y = sleep_eff, fill = overlap) ) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Paired")+
  theme_classic()+
  labs(x = "sleep in overlap zone",
       y = "sleep efficency")

plot(overlap_bar)

#barplot
overlap_bar = ggplot(Sleep_overlap_clean, aes(x = overlap, fill = overlap) ) + 
  geom_bar(width = 0.5)+
  scale_fill_brewer(palette = "Paired")+
  theme_classic() +
  labs(x = "sleep in overlap zone", 
       y = "number of sleep nights")

# arrange barplot and boxplots together
ggarrange(overlap_bar, eff_overlap_box, TST_overlap_box, nrow = 1, heights = c(2,1,1), labels = c('a', 'b', 'c'), common.legend = TRUE)  

