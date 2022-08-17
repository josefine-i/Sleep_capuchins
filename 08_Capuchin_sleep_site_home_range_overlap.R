####Calculating sleep sites and adding in home ranges####

#read in GPS data 
GPSdata=lonlat_to_utm(GPSdata)

GPSdata$timestamp=as.POSIXct( GPSdata$timestamp, tz = 'UTC' )
GPSdata$localtime=lubridate::with_tz(GPSdata$timestamp,tzone = "America/Panama")

GPSdata=GPSdata[which(GPSdata$individual.taxon.canonical.name=="Cebus capucinus"),]
GPSdata=GPSdata[-which(GPSdata$individual.local.identifier=="Da Vinci 5764"),]

GPSdata$individual.local.identifier= gsub(" ", "_", GPSdata$individual.local.identifier) ###Change space to underscore in tag names

##create new column names to store after loop
sleep_per_nona$rain=NA
sleep_per_nona$temp=NA
sleep_per_nona$Sleep_centroid=NA
sleep_per_nona$Sleep_centroid_spread=NA
sleep_per_nona$overnight_GPS_error=NA
sleep_per_nona$Number_GPS_Points=NA




##Loop to merge weather and GPS data to sleep data
for(i in 1:nrow(sleep_per_nona)){
  
  #Isolate weather data within the sleep bout times
  temp=temp_data[which(temp_data$datetime>=sleep_per_nona$onset[i] & temp_data$datetime<sleep_per_nona$waking[i] ),]
  sleep_per_nona$rain[i]=sum(temp$rain, na.rm = TRUE) #Add total rain to sleep data for this sleep bout
  sleep_per_nona$temp[i]=mean(temp$raw, na.rm = TRUE) #Add mean temp to sleep data for this sleep bout

  #If statement checks if there is GPS data during the night
  if(length(which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i]& GPSdata$individual.local.identifier==sleep_per_nona$tag[i]))==0){
    
    ##If no GPS was recorded during the night, take the last point of the day the first point of the next day 
    tempgps=GPSdata[which(lubridate::date(GPSdata$localtime)==sleep_per_nona$night[i] & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]),] #last point of day
    
    tempgps2 = GPSdata[which(GPSdata$day==(unique(tempgps$day)+1) & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]),] #first point of next day
    
    start=tempgps[which(tempgps$localtime==max(tempgps$localtime, na.rm = TRUE)),]
    end=tempgps2[which(tempgps2$localtime==min(tempgps2$localtime, na.rm = TRUE)),]
    
    coords=rbind(start, end)
    coords$Z=coords$location.long.2+1i*coords$location.lat.2
    
    sleep_per_nona$Sleep_centroid[i]=mean(coords$Z) ##mean GPS location
    sleep_per_nona$Sleep_centroid_spread[i]=sqrt(var(coords$location.long.2)+var(coords$location.lat.2)) #GPS drift accross all GPS points 
    sleep_per_nona$overnight_GPS_error[i]=Mod(diff(coords$Z)) ##Distance between start and end if no GPS overnight
    sleep_per_nona$Number_GPS_Points[i]=length(which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i]& GPSdata$individual.local.identifier==sleep_per_nona$tag[i])) #How many GPS points were there if any
    
    #inside the if, Number_GPS_Points should be 0, in else should be above 0
    #IF only one overnight GPS point, Sleep_centroid_spread will be NA

    
  } else{
    #overnight_GPS_error will be NA if there are overnight GPS points
    tempgps=GPSdata[which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i] & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]),]
    
    tempgps$Z=tempgps$location.long.2+1i*tempgps$location.lat.2
    
    sleep_per_nona$Sleep_centroid[i]=mean(tempgps$Z)
    sleep_per_nona$Sleep_centroid_spread[i]=sqrt(var(tempgps$location.long.2)+var(tempgps$location.lat.2))
    sleep_per_nona$Number_GPS_Points[i]=length(which(GPSdata$localtime>=sleep_per_nona$onset[i] & GPSdata$localtime<sleep_per_nona$waking[i] & GPSdata$individual.local.identifier==sleep_per_nona$tag[i]))

    
  }
  

}
  
#exploratory plots
hist(sleep_per_nona$Sleep_centroid_spread, breaks = 100)
hist(sleep_per_nona$rain, breaks = 100)
hist(sleep_per_nona$temp, breaks = 100)
##Should do a model with these rain and temp values


#How many points have error above 50 if there were no overnight GPS points (should probably be excluded)
length(which(sleep_per_nona$overnight_GPS_error>50 & sleep_per_nona$Number_GPS_Points==0))
nrow(sleep_per_nona) #Total number of data points

##Create columns identifying the study years 
sleep_per_nona$Year=NA
sleep_per_nona$Year[which(lubridate::year(sleep_per_nona$night)<2017)]="2015"
sleep_per_nona$Year[which(lubridate::year(sleep_per_nona$night)>2016)]="2017"

##Plot the sleep sites (including bad points)
library(ggplot2)
ggplot(sleep_per_nona)+geom_point(aes(x=Re(Sleep_centroid), y=Im(Sleep_centroid), color= tag))+coord_equal()
ggplot(sleep_per_nona)+geom_point(aes(x=Re(Sleep_centroid), y=Im(Sleep_centroid), color= tag))+coord_equal()+
  facet_wrap(~Year)


##Read in home range files (shapefiles)
BobUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Bob 4661_UD_CDF.shp")
IbethUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Ibeth 4654_UD_CDF.shp")
MartinelliUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Martinelli 5763_UD_CDF.shp")
MimiUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Mimi 4660_UD_CDF.shp")
NorahUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Norah 4655_UD_CDF.shp")
OlgaUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Olga 4657_UD_CDF.shp")
ValoyUD=readOGR("C:/Users/salavi/Documents/Shapefiles/Valoy 5766_UD_CDF.shp")


##project them to the correct UTM zone
BobUD=spTransform(BobUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
IbethUD=spTransform(IbethUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
MartinelliUD=spTransform(MartinelliUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
MimiUD=spTransform(MimiUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
NorahUD=spTransform(NorahUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
OlgaUD=spTransform(OlgaUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
ValoyUD=spTransform(ValoyUD, CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))


##Separate the X and Y values for the sleep site locations
sleep_per_nona$Sleep_centroid_X=Re(sleep_per_nona$Sleep_centroid)
sleep_per_nona$Sleep_centroid_Y=Im(sleep_per_nona$Sleep_centroid)

#Split the dataframe into a list so we can work with one individual at a time
sleepdata_split=split(sleep_per_nona,as.factor(sleep_per_nona$tag)) #to interact with homerange file
sleepdata_split2=split(sleep_per_nona,as.factor(sleep_per_nona$tag)) #to save overlap data later

##Loop to convert individual data to a spatial data frame so it can interact with the homerange shapefile
for(i in 1:length(sleepdata_split)){
  sleepdata_split[[i]]=SpatialPointsDataFrame(coords = sleepdata_split[[i]][, c("Sleep_centroid_X", "Sleep_centroid_Y")], proj4string = CRS("+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), data = sleepdata_split[[i]])
  
  
}

### Manually check for each individual if sleep sites are in overlap zones

#2017
#Bob
Bob_Mart=over(sleepdata_split$Bob_4661,MartinelliUD)
Bob_Norah=over(sleepdata_split$Bob_4661,NorahUD)
Bob_Valoy=over(sleepdata_split$Bob_4661,ValoyUD)
Bob_overlap=data.frame(cbind(Bob_Mart,Bob_Norah,Bob_Valoy))
sleepdata_split2$Bob_4661$overlap="NO"
sleepdata_split2$Bob_4661$overlap[which(is.na(Bob_overlap$name)==FALSE|is.na(Bob_overlap$name.1)==FALSE|is.na(Bob_overlap$name.2)==FALSE)]="YES"


#Martinelli
Mart_Bob=over(sleepdata_split$Martinelli_5763,BobUD)
Mart_Norah=over(sleepdata_split$Martinelli_5763,NorahUD)
Mart_Valoy=over(sleepdata_split$Martinelli_5763,ValoyUD)
Mart_overlap=data.frame(cbind(Mart_Bob,Mart_Norah,Mart_Valoy))
sleepdata_split2$Martinelli_5763$overlap="NO"
sleepdata_split2$Martinelli_5763$overlap[which(is.na(Mart_overlap$name)==FALSE|is.na(Mart_overlap$name.1)==FALSE|is.na(Mart_overlap$name.2)==FALSE)]="YES"

#Norah
Norah_Bob=over(sleepdata_split$Norah_4655,BobUD)
Norah_Mart=over(sleepdata_split$Norah_4655,MartinelliUD)
Norah_Valoy=over(sleepdata_split$Norah_4655,ValoyUD)
Norah_overlap=data.frame(cbind(Norah_Bob,Norah_Mart,Norah_Valoy))
sleepdata_split2$Norah_4655$overlap="NO"
sleepdata_split2$Norah_4655$overlap[which(is.na(Norah_overlap$name)==FALSE|is.na(Norah_overlap$name.1)==FALSE|is.na(Norah_overlap$name.2)==FALSE)]="YES"


#Valoy
Valoy_Bob=over(sleepdata_split$Valoy_5766,BobUD)
Valoy_Mart=over(sleepdata_split$Valoy_5766,MartinelliUD)
Valoy_Norah=over(sleepdata_split$Valoy_5766,NorahUD)
Valoy_overlap=data.frame(cbind(Valoy_Bob,Valoy_Mart,Valoy_Norah))
sleepdata_split2$Valoy_5766$overlap="NO"
sleepdata_split2$Valoy_5766$overlap[which(is.na(Valoy_overlap$name)==FALSE|is.na(Valoy_overlap$name.1)==FALSE|is.na(Valoy_overlap$name.2)==FALSE)]="YES"

#2015
#Ibeth
Ibeth_Mimi=over(sleepdata_split$Ibeth_4654,MimiUD)
Ibeth_Olga=over(sleepdata_split$Ibeth_4654,OlgaUD)
Ibeth_overlap=data.frame(cbind(Ibeth_Mimi,Ibeth_Olga))
sleepdata_split2$Ibeth_4654$overlap="NO"
sleepdata_split2$Ibeth_4654$overlap[which(is.na(Ibeth_overlap$name)==FALSE|is.na(Ibeth_overlap$name.1)==FALSE|is.na(Ibeth_overlap$name.2)==FALSE)]="YES"


#Mimi
Mimi_Ibeth=over(sleepdata_split$Mimi_4660,IbethUD)
Mimi_Olga=over(sleepdata_split$Mimi_4660,OlgaUD)
Mimi_overlap=data.frame(cbind(Mimi_Ibeth,Mimi_Olga))
sleepdata_split2$Mimi_4660$overlap="NO"
sleepdata_split2$Mimi_4660$overlap[which(is.na(Mimi_overlap$name)==FALSE|is.na(Mimi_overlap$name.1)==FALSE|is.na(Mimi_overlap$name.2)==FALSE)]="YES"


#Olga
Olga_Ibeth=over(sleepdata_split$Olga_4657,IbethUD)
Olga_Mimi=over(sleepdata_split$Olga_4657,MimiUD)
Olga_overlap=data.frame(cbind(Olga_Ibeth,Olga_Mimi))
sleepdata_split2$Olga_4657$overlap="NO"
sleepdata_split2$Olga_4657$overlap[which(is.na(Olga_overlap$name)==FALSE|is.na(Olga_overlap$name.1)==FALSE|is.na(Olga_overlap$name.2)==FALSE)]="YES"






Sleep_overlap=do.call(rbind,sleepdata_split2) ## recombine all of the individuals back into one dataframe with the overlap data
############################################################################################################
##Filter out bad sleep sites 
Sleep_overlap_clean=Sleep_overlap[-which(Sleep_overlap$Number_GPS_Points==0 &Sleep_overlap$overnight_GPS_error>=50),]


##Plot sleep sites over home ranges

##use sf package to convert from spatial objects to sf objects so ggplot can plot them faster
#sf is a tidyverse alternative to sp 
library(sf)
BobUD=st_as_sf(BobUD)
IbethUD=st_as_sf(IbethUD)
MartinelliUD=st_as_sf(MartinelliUD)
MimiUD=st_as_sf(MimiUD)
NorahUD=st_as_sf(NorahUD)
OlgaUD=st_as_sf(OlgaUD)
ValoyUD=st_as_sf(ValoyUD)

#split the data per study year so we can plot them separately 


#with bad 
sleep_per_YEAR=split(sleep_per_nona,as.factor(sleep_per_nona$Year))
firstyear=ggplot()+
  # ggmap(studysite)+
  # coord_sf(xlim = c(30, 32.5),
  #          crs = st_crs(4326)) +
  geom_sf(data = IbethUD,fill = NA) +
  geom_sf(data = OlgaUD, fill = NA) +
  geom_sf(data = MimiUD, fill = NA) +
  geom_point(data=sleep_per_YEAR$`2015`, aes(x=Re(Sleep_centroid), y=Im(Sleep_centroid), color= tag))+
  theme_classic()



secondyear=ggplot()+
  # ggmap(studysite)+
  # coord_sf(xlim = c(30, 32.5),
  #          crs = st_crs(4326)) +
  geom_sf(data = BobUD,fill = NA) +
  geom_sf(data = MartinelliUD, fill = NA) +
  geom_sf(data = NorahUD, fill = NA) +
  geom_sf(data = ValoyUD, fill = NA) +
  geom_point(data=sleep_per_YEAR$`2017`, aes(x=Re(Sleep_centroid), y=Im(Sleep_centroid), color= tag))+
  theme_classic()


#without bad
firstyear_clean=ggplot()+
  # ggmap(studysite)+
  # coord_sf(xlim = c(30, 32.5),
  #          crs = st_crs(4326)) +
  geom_sf(data = IbethUD,fill = NA) +
  geom_sf(data = OlgaUD, fill = NA) +
  geom_sf(data = MimiUD, fill = NA) +
  geom_point(data=sleep_per_YEAR$`2015`[-which(sleep_per_YEAR$`2015`$Number_GPS_Points==0 &sleep_per_YEAR$`2015`$overnight_GPS_error>=50),], aes(x=Re(Sleep_centroid), y=Im(Sleep_centroid), color= tag))+
  scale_color_brewer(palette = "Paired",labels = c("Ibeth", "Mimi", "Olga"))+
  theme_classic()+
  labs(x = " ",
       y = "",
       color = "individual") 
  


secondyear_clean=ggplot()+
  #ggmap(studysite)+
  #coord_sf(xlim = c(30, 32.5),
    #       crs = st_crs(4326)) +
  geom_sf(data = BobUD,fill = NA) +
  geom_sf(data = MartinelliUD, fill = NA) +
  geom_sf(data = NorahUD, fill = NA) +
  geom_sf(data = ValoyUD, fill = NA) +
  geom_point(data=sleep_per_YEAR$`2017`[-which(sleep_per_YEAR$`2017`$Number_GPS_Points==0 &sleep_per_YEAR$`2017`$overnight_GPS_error>=50),], aes(x=Re(Sleep_centroid), y=Im(Sleep_centroid), color= tag))+
  scale_color_brewer(palette = "Paired", labels = c("Bob", "Martinelli", "Norah", "Valoy"))+
  theme_classic()+
  labs(x = " ",
       y = "",
       color = "individual")
  

library(ggpubr)
##ggarrange lets us make a multipanel plot
ggarrange(firstyear,secondyear) #with bad
ggarrange(firstyear_clean,secondyear_clean, labels = c('2015', '2017')) #without bad


#saving the overlap data 
save(Sleep_overlap, file = "Sleep_overlap")
save(Sleep_overlap_clean, file = "Sleep_overlap_clean")

class(Sleep_overlap_clean$overlap)
Sleep_overlap_clean$overlap <- as.factor(Sleep_overlap_clean$overlap)
