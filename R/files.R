#' Read in the station metadata: Numbers, name, lat, lon, height for
#' all stations. (Reads the isd-history.txt file).
#'
#' There's a problem with some of the latitude's in the file - R
#'  does not think they are valid numbers. These are forced to NA
#'  but mean the function will throw some warnings.
#'
#' Returns a data frame containing the metadata for each station
#'
#' @export
#' @param 
#' @param dir - directory containing ISD-lite
#' @return data frame - 1 row per station, columns as in the ISD-lite
#'  documentation.
ReadMetaData<-function(dir) {
  fname<-sprintf("%s/isd-history.txt",dir)
  if(!file.exists(fname)) {
    stop(sprintf("No file isd-history.txt in %s",dir))
  }
  md<-read.fwf(fname,
               c(6,6,30,3,10,9,9,8,9,9),
               skip=22,stringsAsFactors=FALSE,
               col.names=c('USAF','WBAN','CTRY','ST',
                           'ICAO','LAT','LON','ELEV',
                           'BEGIN','END'),
               comment.char="")
  md$LAT<-as.numeric(md$LAT)
  return(md)
}

#' Read in the hourly observations from one station file.
#'
#' Reads all the observations from one file (1 year, 1 station). 
#'
#' Most users will want to call ReadStation instead.
#'
#' @export
#' @param 
#' @param fname - name of file to be read
#' @return data frame - Observations: 1 row per hour, columns as in the ISD-lite
#'  documentation, except that AT, DPT, and WS are scaled to degC and m/s,
#'    and there is an added 'chron' column with the date-times.
ReadStationFile<-function(fname) {
    sd<-read.fwf(fname,
               c(4,3,3,3,6,6,6,6,6,6,6,6),
               col.names=c('YR','MO','DY','HR',
                           'AT','DPT','SLP','WD',
                           'WS','TCC','LP1H','LP6H'),
               stringsAsFactors=FALSE,
               na.strings=' -9999')
    sd$chron<-chron(dates=sprintf("%04d/%02d/%02d",sd$V1,sd$V2,sd$V3),
                    times=sprintf("%02d:00:00",sd$V4),
                    format = c(dates = "y/m/d", times = "h:m:s"))
    sd$AT<-sd$AT/10 # Scale to degreesC
    sd$DPT<-sd$DPT/10 # Scale to degreesC
    sd$WS<-sd$WS/10 # Scale to m/s
    return(sd)
}

#' Read in the hourly observations from one station for a given period.
#'
#' Identifies the station by combination of USAF and WBAN numbers
#'   (need both).
#'
#' Combines data from more than one file, where necessary.
#'
#' @export
#' @param 
#' @param dir - directory containing ISD-lite
#' @param usaf - USAF number of station.
#' @param wban - WBAN number of station.
#' @param date.range - start and end dates in format
#'    c(YYYY-MM-DD:HH,YYYY-MM-DD:HH)
#' @param meta - Metadata from \code{\link{ReadMetaData}} (calculated if NULL (default).
#' @return list - 'data' - observations as from \code{\link{ReadStationFile}},
#'     also 'usaf', 'wban', 'lat', 'lon', & 'elev'.
ReadStation<-function(dir,usaf,wban,date.range,meta=NULL) {
    
    if(is.null(meta)) meta<-ReadISDmeta(sprintf("%s/isd-history.txt",dir))
    chrn.range<-chron(dates=sprintf("%04d/%02d/%02d",
                        substr(date.range,1,4),
                        substr(date.range,5,6),
                        substr(date.range,8,9)),
                      times=sprintf("%02d:00:00",
                        substr(date.range,11,12)),
                      format = c(dates = "y/m/d", times = "h:m:s"))
    w<-which(meta$USAF==sprintf("%06d",as.integer(usaf)) &
             meta$WBAN==sprintf("%05d",as.integer(wban)))
    if(length(w)<1) stop(sprintf("No station with USAF==%06d & WBAN=%05d",
                           as.integer(usaf),as.integer(wban)))
     station<-data.frame()
     for(year in seq(start.year,end.year)) {
        fn<-sprintf("%s/%04d/%06d-%05d-%04d.gz",dir,year,md[stn,]$V1,md[stn,]$V2,year)
    
    return(sd)
}



# Get a block of data for a region and period
ReadObs<-function(dir,md=NULL,lat=c(-90,90),lon=c(-180,180),date) {

  if(is.null(md)) md<-ReadISDmeta(sprintf("%s/isd-history.txt",dir))
  # Find the set of stations matching region and period
  sd<-as.integer(sprintf("%04d%02d%02d", as.integer(as.character(years(min(date)))),
                                        as.integer(months(min(date))),
                                        as.integer(days(min(date)))))
  ed<-as.integer(sprintf("%04d%02d%02d",as.integer(as.character(years(max(date)))),
                                        as.integer(months(max(date))),
                                        as.integer(days(max(date)))))
  w<-which(md$V6>min(lat) & md$V6<max(lat) &
           md$V7>min(lon) & md$V7<max(lon) &
           md$V9<=ed & md$V10>=sd)
  if(length(w)==0) return(NULL)
  md<-md[w,]

  # Read in the obs.
  start.year<-min(as.numeric(as.character(years(date))))
  end.year<-max(as.numeric(as.character(years(date))))
  station.obs<-list()
  for(stn in seq_along(md$V1)) {
     station<-data.frame()
     for(year in seq(start.year,end.year)) {
        fn<-sprintf("%s/%04d/%06d-%05d-%04d.gz",dir,year,md[stn,]$V1,md[stn,]$V2,year)
        if(!file.exists(fn)) next
        sd<-ReadISDStationFile(fn)
        w<-which(sd$chron>=min(date) & sd$chron<=max(date))
        if(length(w)==0) next
        station<-rbind(station,sd[w,])
      }
     if(length(station)==0) next # No data
     station.obs[[sprintf("%06d-%05d",md[stn,]$V1,md[stn,]$V2)]]<-station
   }

  return(station.obs)
}
