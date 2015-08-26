context("Reanalysis")

dir<-GetDataDir()
md<-ReadMetaData()
r4<-NULL # pass data between tests

if (requireNamespace("GSDF.TWCR", quietly = TRUE)) {

  if(file.exists(sprintf("%s/1936",dir)) &
    length(list.files(sprintf("%s/1936",dir))>950)) {
  
       test_that("Add reanalysis temperatures", {

        ro<-ReadObs(lat.range=c(51,52),lon.range=c(-1,1),
                 date.range=c('1936-06-01:00','1936-06-03:23'),meta=md)

        r2<-AddReanalysis(ro,var='air.2m',type='mean',version='3.5.1')
        expect_equal(length(r2[[4]]$reanalysis.mean),2)
        expect_equal(length(r2[[4]]$reanalysis.mean$air.2m),12)
        r3<-AddReanalysis(r2,var='air.2m',type='spread',version='3.5.1')
        expect_equal(length(r3[[4]]$reanalysis.spread),2)
        expect_equal(length(r3[[4]]$reanalysis.spread$air.2m),12)
        r4<<-AddReanalysis(r3,var='air.2m',type='normal',version='3.4.1')
        expect_equal(length(r4[[4]]$reanalysis.normal),2)
        expect_equal(length(r4[[4]]$reanalysis.normal$air.2m),12)

      })
  
        test_that("Interpolate reanalysis to hourly", {

         expect_error(GetVarAtHour(r4, 1936, 6, 2, 12, var = "prate"),
                      'Only reanalysis variable "air.2m" currently supported')
         expect_error(GetVarAtHour(r4, 1936, 6, 2, 12,
                                   var = "air.2m", type='actual'),
                      'reanalysis.actual data not present')
         expect_equal(GetVarAtHour(r4,1936,6,2,12,
                                   var = "air.2m", type='mean')[1],
                      285.3101,tolerance=0.00001)
         expect_equal(GetVarAtHour(r4,1936,6,2,15,
                                   var = "air.2m", type='mean')[1],
                      285.3752,tolerance=0.00001)
         expect_equal(GetVarAtHour(r4,1936,6,2,15,
                                   var = "air.2m", type='spread')[1],
                      0.5231857,tolerance=0.00001)
         expect_equal(GetVarAtHour(r4,1936,6,2,15,
                                   var = "air.2m", type='normal')[1],
                      289.6734,tolerance=0.00001)
   
     
    })
   } else {
     print("Skipping reanalysis tests as data for 1936 not present")
   }
} else {
  print("Skipping reanalysis tests as package GSDF.TWCR not available")
}

