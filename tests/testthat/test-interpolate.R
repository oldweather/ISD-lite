context("Interpolate")

dir<-GetDataDir()
md<-ReadMetaData()

if(file.exists(sprintf("%s/1936",dir)) &
   length(list.files(sprintf("%s/1936",dir))>950)) {
  
    test_that("Interpolate obs to hourly", {

     ro<-ReadObs(lat.range=c(51,52),lon.range=c(-1,1),
                 date.range=c('1936-06-01:00','1936-06-30:23'),meta=md)

     expect_equal(GetVarAtHour(ro,1936,6,28,13)[1],20.0)
     expect_equal(GetVarAtHour(ro,1936,6,28,15)[1],18.88)
     expect_equal(GetVarAtHour(ro,1936,6,28,13,var='SLP')[1],10184)
     expect_equal(GetVarAtHour(ro,1936,6,28,15,var='SLP')[1],10173.6)
     
     
    })

} else {
  print("Skipping obs interpolation tests as data for 1936 not present")
}

