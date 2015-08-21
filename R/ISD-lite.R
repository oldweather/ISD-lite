#' ISD-lite: Basic access to data from NOAA's Integrated Surface Database
#'  Download the data first (Online access too slow & limited):
#'  requires a directory to work from (could be a URL rather than a local
#'   directory of course).
#' 
#'  ISD (https://www.ncdc.noaa.gov/isd).
#'  This packace supports only ISD-lite
#'  (http://www1.ncdc.noaa.gov/pub/data/noaa/isd-lite/isd-lite-technical-document.pdf).
#'  This package is really for comparing ISD and 20CR, so it contains
#'   functions to suport that, but the 20CR components are optional.
#' 
#' @section functions:
#' \itemize{
#'   \item \code{\link{ReadObs}} - read a selected set of observations.
#'   \item \code{\link{ReadStationFile}} - read the obs in one file: one
#'         year for one station.
#'   \item \code{\link{ReadMetaData}} - read the metadata (station names and
#'         locations.
#' }
#'
#' @section data structure:
#' \itemize{
#'  \item Metadata - a data frame of station numbers, names, positions
#'      and dates.
#'  \item Obs - list of stations, each a list of lat, lon, name, numbers,
#'     and data.
#' }
#'
#' @docType package
#' @name ISD-lite
NULL

