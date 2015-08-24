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
    sd$chron<-chron(dates=sprintf("%04d/%02d/%02d",sd$YR,sd$MO,sd$DY),
                    times=sprintf("%02d:00:00",sd$HR),
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
#' @param dir - directory containing ISD-lite
#' @param usaf - USAF number of station.
#' @param wban - WBAN number of station.
#' @param date.range - start and end dates in format
#'    c(YYYY-MM-DD:HH,YYYY-MM-DD:HH)
#' @param meta - Metadata from \code{\link{ReadMetaData}} (calculated if NULL (default)).
#' @return list - 'data' - observations as from \code{\link{ReadStationFile}},
#'     also 'USAF', 'WBAN', 'LAT', 'LON', & 'ELEV'.
ReadStation<-function(dir,usaf,wban,date.range,meta=NULL) {
    
    if(is.null(meta)) meta<-ReadMetaData(dir)
    chrn.range<-chron(dates=sprintf("%s/%s/%s",
                        substr(date.range,1,4),
                        substr(date.range,6,7),
                        substr(date.range,9,10)),
                      times=sprintf("%s:00:00",
                        substr(date.range,12,13)),
                      format = c(dates = "y/m/d", times = "h:m:s"))
    w<-which(meta$USAF==sprintf("%06d",as.integer(usaf)) &
             meta$WBAN==sprintf("%05d",as.integer(wban)))
    if(length(w)<1) stop(sprintf("No station with USAF==%06d & WBAN=%05d",
                           as.integer(usaf),as.integer(wban)))
    station<-data.frame()
    start.year<-as.integer(substr(date.range[1],1,4))
    end.year<-as.integer(substr(date.range[1],1,4))
    for(year in seq(start.year,end.year)) {
        fn<-sprintf("%s/%04d/%06d-%05d-%04d.gz",dir,year,meta[w,]$USAF,meta[w,]$WBAN,year)
        if(!file.exists(fn)) {
          warning(sprintf("No file for station with USAF==%06d & WBAN=%05d for %04d",
                           as.integer(usaf),as.integer(wban),year))
          next
        }
        sd<-ReadStationFile(fn)
        w2<-which(sd$chron>=min(chrn.range) & sd$chron<=max(chrn.range))
        if(length(w2)==0) next
        if(length(station)==0) station<-sd[w2,]
        else station<-rbind(station,sd[w2,])
      }
    
    return(list(USAF=meta$USAF[w],WBAN=meta$WBAN[w],LAT=meta$LAT[w],
                LON=meta$LON[w],ELEV=meta$ELEV[w],data=station))
}

#' Find stations with data in a given region and date range.
#'
#' Locates stations by lat.range, lon.range, and date.range.
#'
#' Finds stations in the metadata file, not available on the local system.
#'
#' @export
#' @param dir - directory containing ISD-lite
#' @param lat.range - Selected latitudes, defaults to c(-90.90).
#' @param lon.range - Selected longitudes, defaults to c(-180,180).
#' @param date.range - start and end dates in format
#'    c(YYYY-MM-DD:HH,YYYY-MM-DD:HH) - defaults to all years.
#' @param meta - Metadata from \code{\link{ReadMetaData}} (calculated if NULL (default)).
#' @return list - 'USAF', 'WBAN', 'LAT', 'LON', & 'ELEV' (subset of metadata).
FindStations<-function(dir,lat.range=c(-90,90),lon.range=c(-180,180),
                       date.range=c('1000-01-01:00','3000-12-31:23'),meta=NULL) {
  
  if(is.null(meta)) meta<-ReadMetaData(dir)
  # Find the set of stations matching region and period
  sed<-as.integer(sprintf("%s%s%s",substr(date.range,1,4),
                                   substr(date.range,6,7),
                                   substr(date.range,9,10)))
  w<-which(meta$LAT>=min(lat.range) & meta$LAT<=max(lat.range) &
           meta$LON>=min(lon.range) & meta$LON<=max(lon.range) &
           meta$BEGIN<=sed[2] & meta$END>=sed[1])
  if(length(w)==0) return(NULL)
  meta<-meta[w,]
  return(meta)
}

#' Reads all observations in a given region and date range.
#'
#' Locates stations by \code{\link{ReadMetaData}}, reads each one with \code{\link{ReadStation}}.
#'
#' Finds stations in the metadata file, not available on the local system.
#'
#' @export
#' @param dir - directory containing ISD-lite
#' @param lat.range - Selected latitudes, defaults to c(-90.90).
#' @param lon.range - Selected longitudes, defaults to c(-180,180).
#' @param date.range - start and end dates in format
#'    c(YYYY-MM-DD:HH,YYYY-MM-DD:HH)
#' @param meta - Metadata from \code{\link{ReadMetaData}} (calculated if NULL (default)).
#' @return list - Each element 1 station list from \code{\link{ReadStation}}.
ReadObs<-function(dir,lat.range,lon.range,date.range,meta=NULL) {

  if(is.null(meta)) meta<-ReadMetaData(dir)
  md<-FindStations(dir,lat.range,lon.range,date.range,meta)
  if(is.null(md)) return(NULL)
  Result<-list()
  for(i in seq_along(md$USAF)) {
    Result[[i]]<-ReadStation(dir,md$USAF[i],md$WBAN[i],date.range,meta=md)
  }
  # Clean out any stations with no data
  miss.d<-integer(0)
  for(i in seq_along(md$USAF)) {
     if(length(Result[[i]]$data)==0) miss.d<-c(miss.d,i)
   }
  Result[miss.d]<-NULL
  return(Result)
}
  
