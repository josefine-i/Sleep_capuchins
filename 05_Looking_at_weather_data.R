#####LOOKING AT THE WEATHER DATA 

#Reading in the data 
rain_data = read.csv("/Users/Josie/Documents/DATA/bci_cl_ra_elect.csv")
temp_data = read.csv("/Users/Josie/Documents/DATA/bci_cl_at_elect.csv")


###Exploring the Rain data###
#converting datetime
rain_data$datetime <- as.POSIXct(x= rain_data$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz='America/Panama' )
temp_data$datetime <- as.POSIXct(x= temp_data$datetime, format=c("%d/%m/%Y %H:%M:%S"), tz='America/Panama' )

##trimming the data 
#check timerange of our vedba data: 
range(lubridate::date(d1$local_timestamp))

#some dates were in the future and needed to be deleted: 
d1=d1[which(as.character(lubridate::year(d1$local_timestamp))<="2018"),]
range(lubridate::date(d1$local_timestamp)) #checking it worked 


#applying the date range of vedba data to rain data:
rain_data = rain_data[which(rain_data$datetime>= range(lubridate::date(d1$local_timestamp))[1] & rain_data$datetime<= range(lubridate::date(d1$local_timestamp))[2]),]
range(rain_data$datetime) #checking it worked
#same for temp date
temp_data = temp_data[which(temp_data$datetime>= range(lubridate::date(d1$local_timestamp))[1] & temp_data$datetime<= range(lubridate::date(d1$local_timestamp))[2]),]
range(temp_data$datetime)

#check range of rain data 
range(rain_data$raw) #needs to be trimmed, there cannot be negative rain fall
rain_data = rain_data[which(rain_data$raw >= 0),]
range(rain_data$raw) #check

#create column date and year, compressed years and hours
rain_data$date = lubridate::date(rain_data$datetime)
rain_data$year = as.character(lubridate::year(rain_data$datetime))

rain_data$year_com = NA

#create column with just the study year 
#pb = txtProgressBar(max = nrow(rain_data, min = 0), style = 3, char = "=") #creates a progress bar 
for (i in 1:nrow(rain_data)){
  if (rain_data$year [i] == "2015"|rain_data$year[i] == "2016"){
    rain_data$year_com[i] = 1}
  else {rain_data$year_com[i] = 2}
  #setTxtProgressBar(pb,i)
}
#close(pb)

rain_data$year_com = as.factor(rain_data$year_com) #combines the years 

rain_data$hour = as.factor(as.character(lubridate::hour(rain_data$datetime)))


#Looking at the data of every day
library(ggplot2)
ggplot(rain_data, aes(x=as.factor(as.character(date)), y=raw)) + geom_boxplot()+facet_wrap(~year_com)
library (dplyr)
?summarize

#creates means for the rainfall per hour 
rain_data2 = rain_data %>% 
  group_by(hour) %>%
  summarise(mean = mean (raw))

#plot rain againt hour of the day 
rain_data2$hour=as.numeric(as.character(rain_data2$hour))
ggplot(rain_data2, aes(x=hour, y=mean)) + geom_line()+theme_classic()

#do a histogram for rain amount 
hist(rain_data$raw, breaks = 100) #is a very skewed data 
hist(rain_data$raw[rain_data$raw>0], breaks = 100) #for rain above 0 ml 

####Look at temperature and rain together####
#check range of temperature data and trim wrong data points
range(temp_data$raw)
temp_data = temp_data[which(temp_data$raw > 0),]
range(temp_data$raw) #check

#set rain data to total rainfall per hour 
#rain_data$date = as.factor(as.character(rain_data$date)) #check that date and hour are a factor
#rain_data_hour = rain_data %>%
  #group_by(date, hour) %>%
  #summarise(sum = sum(raw))
#rain_data_hour = data.frame(rain_data_hour)

#create date, hour and month column in temp data 
temp_data$date = lubridate::date(temp_data$datetime)
temp_data$month = lubridate::month(temp_data$datetime)
temp_data$hour = lubridate::hour(temp_data$datetime)

#create a column of the sum of rain every 15 min in temp_data 
temp_data$rain=NA
for(i in 2:nrow(temp_data)){
  if(length(which(rain_data$datetime<=temp_data$datetime[i] & rain_data$datetime>=temp_data$datetime[i-1]))==0){
    next
  }
  rain=rain_data$raw[which(rain_data$datetime<=temp_data$datetime[i] & rain_data$datetime>=temp_data$datetime[i-1])]
  rain=sum(rain)
  temp_data$rain[i]=rain
}

#plot the temperature and amount of rain
ggplot(temp_data, aes(x=rain, y=raw)) + geom_point()+theme_classic()#rain and temp needs to be num

#save everything 
save(d1, file = "d1.3")
save(rain_data, file = "BCI_rain_cleaned")
save(temp_data, file = "BCI_temp_cleaned")



##### Correlation #####
library(brms)
library(rstan)
library(cmdstanr)

options(mc.cores = parallel::detectCores()) ##parallelize the chains

#new column named temperature 
temp_data$temperature = temp_data$raw

check_cmdstan_toolchain()
install_cmdstan(cores = 2)

cmdstan_path()

#correlation model for rain and temp data 
#that did not work 
correlation_model <- brm(bf(mvbind(rain, temperature) ~ 1)+set_rescor(rescor = TRUE),
                         data = temp_data[complete.cases(temp_data[,c("rain","temperature")]),],
                         iter = 10000,
                         prior = c(prior(gamma(2, .1), class = nu),
                                   prior(normal(0, 50), class = Intercept, resp = rain),
                                   prior(normal(0, 50), class = Intercept, resp = temperature),
                                   prior(normal(0, 50), class = sigma, resp = rain),
                                   prior(normal(0, 50), class = sigma, resp = temperature),
                                   prior(lkj(1), class = rescor)),
                         family = student,
                         backend = "cmdstanr",
                         control = list(max_treedepth = 10, adapt_delta = .9999))
#show results of the model
summary(correlation_model)


####test sleep efficency against rain#### 
sleep_model <- brm(bf(sleep_eff ~ rain  (rain | tag)), #see if rain and temp affect sleep eff (if temp becomes a factor take it out of the ())
                           data = temp_data[complete.cases(temp_data[,c("rain")]),],
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

#plot the model 
conditional_effects(weather_model, spaghetti = TRUE)
plot(conditional_effects(weather_model, spaghetti = TRUE),points = TRUE)


