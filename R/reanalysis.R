# Get the temperatures interpolated to a point in time
#  (For comparison with a reanalysis slice)
ISDGetVarAtHour<-function(var,year,month,day,hour,obs,md) {
  if(!var=='air.2m') stop("air.2m is only supported variable")
  chrn<-chron(dates=sprintf("%04d/%02d/%02d",year,month,day),
                    times=sprintf("%02d:00:00",hour),
                    format = c(dates = "y/m/d", times = "h:m:s"))
  slice<-list(
           station=rep('',length(obs)),
           lat=rep(NA,length(obs)),
           lon=rep(NA,length(obs)),
           values=rep(NA,length(obs)),
           var=var,
           chron=chrn)
  for(i in seq_along(obs)) {
    slice$station[i]<-names(obs)[i]
    w<-which(sprintf("%06d-%05d",md$V1,md$V2)==names(obs)[i])
    slice$lat[i]<-md[w,6]
    slice$lon[i]<-md[w,7]
    w<-which(abs(obs[[i]]$chron-chrn)<1/48 & !is.na(obs[[i]]$V5))
    if(length(w)>0) { # Within an hour of an ob - use directly
      slice$values[i]<-obs[[i]]$V5[w[1]]/10
    } else { # interpolate if 2 nearby values are available
	w<-which(abs(obs[[i]]$chron-chrn)<0.6 & !is.na(obs[[i]]$V5))
	if(length(w)>1) {
	  slice$values[i]<-(approx(x=obs[[i]]$chron[w],
				  y=obs[[i]]$V5[w],
				  xout=chrn)$y)/10
	}
    }
  }
  return(slice)
}
