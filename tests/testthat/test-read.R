context("Read")

# Can't do anything if we don't have the data files and environment variable.
dir<-Sys.getenv('ISD_dir',unset=NA)
if(is.na(dir)) stop("Can't run tests: Environment variable 'ISD.dir' must be set to directory containing ISD-lite data.")
if(!file.exists(dir)) stop(sprintf("Can't run tests:ISD data directory %s does not exist.",dir))
if(!file.exists(sprintf("%s/isd-history.txt",dir))) {
  stop(sprintf("Can't run tests: ISD data directory %s does not contain a station data file.",dir))
}

md=NULL # Give metadata cache global scope

test_that("Read metadata", {

# Check access to data directory
  Sys.unsetenv('ISD_dir')
  expect_error(GetDataDir(),'Environment variable "ISD.dir" must be set to directory containing ISD-lite data.')
  Sys.setenv(ISD_dir="/Surely/non/existent/directory!%]")
  expect_error(GetDataDir(),'ISD data directory /Surely/non/existent/directory!%] does not exist.')
  Sys.setenv(ISD_dir="/")
  expect_error(GetDataDir(),"ISD data directory / does not contain a station data file.")
  Sys.setenv(ISD_dir=dir)
  expect_equal(GetDataDir(),dir)

# Check metadata reads OK
  md<<-ReadMetaData()
  expect_equal(length(md),10)
  # Test some sample expected values - might change if using updated data
  expect_more_than(length(md$USAF),29000)
  expect_equal(md$LAT[29000],32.855)
  expect_equal(md$USAF[29],8408)
  
# Find stations from metadata
  m2<-FindStations(meta=md)
  expect_more_than(length(m2$USAF),28000)
  expect_equal(length(which(is.na(m2$LAT))),0)
  m3<-FindStations(lat.range=c(51,52),lon.range=c(-1,1),
                   date.range=c('1936-06-01:00','1936-06-30:23'),meta=md)
  expect_equal(length(m3$WBAN),4)
})

if(file.exists(sprintf("%s/1936",dir)) &
   length(list.files(sprintf("%s/1936",dir))>950)) {
  
    test_that("Read obs", {

     rsf<-ReadStationFile(sprintf("%s/1936/012620-99999-1936.gz",dir))
     expect_equal(length(rsf),13)
     expect_more_than(length(rsf$YR),1000)

     expect_error(ReadStation(usaf=92620,wban=99989,meta=md),
                  "No station with USAF==092620 & WBAN=99989")
     rs<-ReadStation(usaf=12620,wban=99999,
                     date.range=c('1936-01-01:00','1936-12-31:23'),meta=md)
     expect_equal(rsf,rs$data)
     expect_equal(rs$LAT,64.8)

     ro<-ReadObs(lat.range=c(51,52),lon.range=c(-1,1),
                 date.range=c('1936-06-01:00','1936-06-30:23'),meta=md)
     expect_equal(length(ro),4)
     expect_equal(ro[[2]]$data$MO[2],6)
     
    })

} else {
  print("Skipping obs read tests as data for 1936 not present")
}

