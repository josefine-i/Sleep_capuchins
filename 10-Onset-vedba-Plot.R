############ Onset/offset ~ sunset/sunrise Figure ##########

library(lubridate)
library(hms)
library(plotrix)

sleep_trim <- sleep_per[ !is.na( sleep_per$SPT ), ]

## function for plotting times from noon to noon. It will make correct 12:00 - 24:00 to be 00:00 - 12:00 and 00:00 - 12:00 to be 12:00 to 24:00
ts_func <- function( time_vec ){
  
  num_time <- as.numeric( as_hms( time_vec ) )
  
  corr_time <- ifelse( num_time < 12*60*60, num_time + 12*60*60, num_time - 12*60*60 )
  
  return( corr_time )
  
}

## function for setting transparency of a color while plotting
transp <- function(col, alpha=.5){
  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))
  return(res)
}


### Fig 1 ###

sun_trim <- sun_dat[ sun_dat$date %in% unique( full_dat$night ), ]

ave_sunset <- as_hms( mean( as.numeric(as_hms( sun_trim$sunset ) ) ) )
ave_sunset <- ts_func( ave_sunset )

ave_sunrise <- as_hms( mean( as.numeric( as_hms( sun_trim$sunrise ) ) ) )
ave_sunrise <- ts_func( ave_sunrise )

ave_night_start <- as_hms( mean( as.numeric( as_hms( sun_trim$night_start ) ) ) )
ave_night_start <- ts_func( ave_night_start )

ave_night_end <- as_hms( mean( as.numeric( as_hms( sun_trim$night_end ) ) ) )
ave_night_end <- ts_func( ave_night_end )

par(mar=c(6,6,4,4))
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(2,2,2.5), respect = FALSE)


## Fig 1A


par(mar = c(0, 5, 0, 1))

ave_ved <- aggregate( d1$log_vedba, by = list( d1$local_time ), FUN = mean, na.rm = T ) 

names( ave_ved ) <- c( 'local_time', 'ave_VeDBA')

ave_ved <- ave_ved[ order( ts_func( ave_ved$local_time ) ), ]


se_ved <- aggregate( d1$log_vedba, by = list( d1$local_time ), FUN = std.error, na.rm = T ) 
names( se_ved ) <- c( 'local_time', 'se_VeDBA')

se_ved <- se_ved[ order( ts_func( se_ved$local_time ) ), ]

se_ved$se_VeDBA[ is.na( se_ved$se_VeDBA) ] <- 0

x_vals <- ts_func( ave_ved$local_time )

plot( x_vals , ave_ved$ave_VeDBA, type = 'l', xaxt = 'n', xlab = 'Time', ylab = '', las = 1, lwd = 0.5  )

title( ylab = "log VeDBA", line = 3.9 )


low_se <- ave_ved$ave_VeDBA - se_ved$se_VeDBA
high_se <- ave_ved$ave_VeDBA + se_ved$se_VeDBA


polygon( x = c( ( x_vals ) ,rev( x_vals ), x_vals[ 1 ] ), y = c( low_se, rev( high_se ), low_se[ 1 ] ), col=transp( "#08519C" , 0.5 ), border=NA ) # fills area between the curves

polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('darkgrey', .25), border = NA )


## Fig 1B

par(mar = c(4.1, 5, 0, 1))

## make a density plot of the onset time and waking time
onset_dens <- density( ts_func( sleep_trim$onset_time ) )

wake_dens <- density(  ts_func( sleep_trim$waking_time ) )

waso_times <- full_dat[ !is.na( full_dat$sleep_per ) & full_dat$sleep_per == 1 & full_dat$sleep_bouts == 0, 'local_time' ]

waso_dens <- density( ts_func( waso_times ) )

x_range <- c( 0, 24*60*60 )

y_range <- c( min( onset_dens$y, wake_dens$y ), max( onset_dens$y, wake_dens$y ) )

plot( onset_dens$x, onset_dens$y, type = 'l', ylab = '', xlab = 'Time', xaxt = 'n', yaxt = "n", xlim = x_range, ylim = y_range, lty = 2, las = 1, col = c( '#08306B') )

#axis( 2, at = seq( 0, 0.0004, by = 0.0002 ), las = 1 )

title( ylab = "Probability Density", line = 3.9 )

lines( wake_dens$x, wake_dens$y, type = 'l', ylab = 'Density', xlab = 'Waking Time', xaxt = 'n', lty = 3, col = c('#2171B5' ) )


legend( x = 'topright', cex = 0.9, col = c( '#2171B5', '#4292C6' ), lty = c( 1, 1 ), legend = c( 'Sleep onset time', 'Waking time' ), bty = 'n' )

polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('darkgrey', .25), border = NA )

axis( 1, at = seq( 0, 60*24*60, 3*60*60), labels = c( as_hms( seq( 12*60*60, 60*23*60, 3*60*60) ), as_hms( seq( 0, 60*12*60, 3*60*60) ) ) ) 

axis( 1, at = seq( 0, 60*24*60, 1*60*60), labels = F ) 

axis( 2, at = c( 0.0, 0.0002, 0.0004 ), labels = c( '0.0', '0.0002', '0.0004' ), las = 2 )

#### End of Fig
