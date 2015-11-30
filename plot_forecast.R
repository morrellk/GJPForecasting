##   plot_forecast.R
##   Plotting forecast and Brier score over time.  Uses base graphics
##
##   Generic version
##
##   Will work for multiple categories or binary
##   For binary, shows Brier score over forecast period for both
##   "yes" and "no" results.
##
##   Packages: lubridate
##   Inputs:
##        f_dates: list of dates for each forecast entered, as strings in mdy format,
##             eg: mm/dd/yy  Will be processed with lubridate function mdy().
##        f_vals: list or array of forecast values arranged with forecast
##             options in rows, forecast value for each date in columns.  
##             Expects numeric values in (0:1)
##        f_names: list of names for each forecast option, to use in legends
##             Default is c("yes","no")
##        filename: name of file to put plots intoe, expects png at present.
##             Default is "forecast_plot.png"
##
##   Outputs:
##        PNG file containing 2 plots
##   Returns: filename for plot file
##
##   30-NOV-2015  K. Morrell 
##

plot_forecast <- function(f_dates, f_vals, f_names=c("Yes","No"), 
                          filename="forecast_plot.png"){
     library(lubridate)
     
     ##   Process the dates, generate lists will need below and in plot
     f_dates <- mdy(f_dates)
     how_long <- sapply(c(1:(length(f_dates)-1)), 
                        function(x) as.numeric(difftime(f_dates[x+1],
                                                        f_dates[x], days))) 
     total_days <- as.numeric(difftime(f_dates[length(f_dates)], 
                                       f_dates[1], days)) 
     all_f_dates <- seq(f_dates[1], f_dates[length(f_dates)], by="days")
     all_f_dates <- all_f_dates[1:(length(all_f_dates)-1)]
     
     ## For each vector of forecast values, want to calculate the Brier 
     ## score in the event that forecast is correct
     if (is.null(nrow(f_vals))) {
          ## Needs to calculate scores for Yes and No, so part has 2 rows
          both_f_vals <- rbind(f_vals,(1-f_vals))
          part <- matrix(data=sapply(both_f_vals, function(x) ((1-x)^2 + (0-(1-x))^2)),
                         nrow=2)
          ## set this up here, useful in plotting forecasts
          all_f_vals <- rep(f_vals, how_long)
     }
     else{
          part <- matrix(data=sapply(f_vals, function(x) ((1-x)^2 + (0-(1-x))^2)), 
                         nrow=nrow(f_vals)) 
          ## This ends up with forecasts expanded in columns rather than rows
          all_f_vals <- sapply(c(1:nrow(f_vals)), 
                               function(x) rep(f_vals[x,],how_long))
          all_f_vals <- t(all_f_vals)   
     }
     
     ## daily is the daily contribution at each forecast
     daily <- part/total_days  
     
     ## each_day is daily expanded for correct # of days for each forecast
     if (is.null(nrow(daily))){
          each_day <- rep(daily,how_long)
          sum_each_day <- cumsum(each_day)
     }
     else{
          ## each_day ends up with colums corresponding to particular condition
          each_day <- sapply(c(1:nrow(daily)), 
                             function(x) rep(daily[x,,drop=FALSE], how_long))
          each_day <- t(each_day)
          sum_each_day <- sapply(c(1:nrow(daily)), 
                                 function(x) cumsum(each_day[x,]))
     }
     
     forecast <- data.frame(all_f_dates, t(all_f_vals))
     scores <- data.frame(all_f_dates, t(each_day), sum_each_day)
     
     ## Send the plots to a file (png? pdf?)
     png(filename, width=500, height=500, res=72)
     par(mfrow=c(2,1))
     
     ## color palette
     if (!is.null(nrow(f_vals))) palette(rainbow(nrow(f_vals)))
     
     ## forecast plot
     with(forecast, plot(all_f_dates, seq(0,1,1/(length(all_f_dates)-1)), 
                       main="Forecasts and Brier Scores", xlab="Date", 
                       ylab="Forecast",type='n'))
     with(forecast, par(yaxp = c(0,1,1)))
     if (is.null(nrow(f_vals))){
          with(forecast, lines(all_f_dates, all_f_vals))          
     }
     else{
          sapply(c(1:nrow(f_vals)), 
                 function(x) with(forecast, lines(all_f_dates, all_f_vals[x,],
                                                  col=x)))
          legend("topleft", lty=1, col=c(1:nrow(f_vals)), legend=f_names)    
     }
     
     ## Brier score effect plot
     with(scores, plot(all_f_dates, seq(0,2,2/(length(all_f_dates)-1)),
                       xlab="Date",ylab="Brier Score", type="n"))
     if (is.null(ncol(sum_each_day))){
          ## Left this in here, but shouldn't ever end up here
          with(scores, lines(all_f_dates, sum_each_day))          
     }
     else{
          sapply(c(1:ncol(sum_each_day)), 
                 function(x) with(scores, lines(all_f_dates, sum_each_day[,x],
                                                  col=x)))
          legend("topleft", lty=1, col=c(1:ncol(sum_each_day)), legend=f_names)    
     }
     dev.off()  
     palette("default")
     return(filename)
}
