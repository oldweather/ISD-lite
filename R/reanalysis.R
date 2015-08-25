# Optional extras for the ISDlite package - functions for
#  comparison with 20CR.
# Requires the GSDF.TWCR package.
#
# For reanalysis comparison, we want to do 2 things:
#  1) Get reanalysis data for the location and time range of the ISD data.
#  2) Interpolate the ISD data to get hourly values.
#

#' Extracts reanalysis output to match a set of station data.
#'
#' Takes a station list from ReadObs, and extracts, for each station,
#'  reanalysis actuals and normals at 0,6,12 & 18 hours for the whole
#'  timespan of the obs data.
#'
#' Currently only does air temperature.
#'
#' @export
#' @param obs - list of station data from \code{\link{ReadObs}}.
#' @param var - Which reanalysis variable (default 'air.2m').
#' @param type - What reanalysis data is required? 'actual' (default),
#'                'spread', or 'normal'
#' @param version - Which version of 20CR (ensda run number), defaults
#'                   to '3.5.1' (v2c).
#' @return list - as input, but for each station a new component
#'                 'reanalysis.actual' (or '.normal' or '.spread'), 
#'                 a data frame with columns 'chron' and var.
AddReanalysis<-function(obs,var='air.2m',type='actual',version='3.5.1') {

  lons<-rep(NA,length(obs))
  lons<-rep(NA,length(obs))
  start.date<-NA
  end.date<-NA
  for(i in seq_along(obs)) {
    lons[i]<-obs[[i]]$LON
    lats[i]<-obs[[i]]$LAT
    start.date<-min(c(start.date,obs[[i]]$chron),na.rm=TRUE)
    end.date<-max(c(end.date,obs[[i]]$chron),na.rm=TRUE)
  }
  RData<-array(data=NA,dim=c(length(obs),length(seq(start.date,end.date))*4))
  chrn<-rep(NA,length(seq(start.date,end.date))*4)
  count<-1
  for(dy in seq(start.date,end.date)) {
    year<-as.integer(as.character(years(dy)))
    month<-as.integer(months(dy))
    day<-as.integer(days(dy))
    for(hour in c(0,6,12,18)) {

      chrn[count]<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
                    times=sprintf("%02d:00:00",hour),
                    format = c(dates = "y/m/d", times = "h:m:s"))
      
      Field<-GSDF.TWCR::TWCR.get.slice.at.hour(var,year,month,day,hour,
                                  type=type,
				  version=version)
      RData[,count]<-GSDF::GSDF.interpolate.ll(Field,lats,lons)
      count<-count+1
    }
  }
  # Pack the data into each station component
  for(i in seq_along(obs)) {
     title<-sprintf('reanalysis.%s',type)
     obs[[i]][[title]]<-data.frame(chron=chrn,var=RData[i,])
     names(obs[[i]][[title]])[names(obs[[i]][[title]])=='var']<-var
   }

  return(obs)
}            

#' Get ISD data interpolated to a given hour.
#'
#' First load the station obs with \code{\link{ReadObs}}, then add the
#'  reanalysis equivalents as required with calls to \code{\link{AddReanalysis}},
#'  then call this function to get interpolated values at any hour.
#'
#' Currently only does air temperature.
#'
#' @export
#' @param obs - list of station data from \code{\link{ReadObs}}.
#' @param var - Which variable: 'AT' (default),'DPT','SLP','WD',
#'               'WS','TCC','LP1H','LP6H', 'air.2m', ...
#' @param type - 'actual' (default, and only option for observed values),
#'                'spread', or 'normal'
#' @return values - for each station in the input, selected value interpolated
#'                   to specified hour
GetVarAtHour<-function(obs,year,month,day,hour,var='AT',type='actual') {
  
  chrn<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
                    times=sprintf("%02d:00:00",hour),
                    format = c(dates = "y/m/d", times = "h:m:s"))

  Result<-rep(NA,length(obs))

  for(i in seq_along(obs)) {
  
    if(var %in% names(obs[[1]]$data)) { # observed value
       if(type != 'actual') stop('Only actuals available for observed values')
       w<-which(abs(obs[[i]]$data$chron-chrn)<1/48 & !is.na(obs[[i]]$V5))
       if(length(w)>0) { # Within an hour of an ob - use directly
          Result[i]<-obs[[i]]$data[[var]][w[1]]
       } else { # interpolate if 2 nearby values are available
	w<-which(abs(obs[[i]]$data$chron-chrn)<0.6 & !is.na(obs[[i]]$data[[var]]))
	if(length(w)>1) {
	  Results[i]<-(approx(x=obs[[i]]$data$chron[w],
				  y=obs[[i]]$data[[var]][w],
				  xout=chrn)$y)
	}
      }
    } else {
       if(var != 'air.2m') stop('Only reanalysis variable "air.2m" currently supported')
       title<-sprintf('reanalysis.%s',type)
	w<-which(abs(obs[[i]][[title]]$chron-chrn)<0.6 & !is.na(obs[[i]][[title]][[var]]))
	if(length(w)>1) {
	  Results[i]<-approx(x=obs[[i]][[title]]$chron[w],
				  y=obs[[i]][[title]][[var]][w],
				  xout=chrn)
          if(type!='spread') Results[i]<-Results[i]-273.15
	}
      }
  }
  return(Results)
}
