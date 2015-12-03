#' retrieve_NCEP_pressure_daily
#'
#' @description Retrieve daily climatic data from ESRL  \url {http://www.esrl.noaa.gov/psd/thredds/catalog.html}
#'
#' @param  dataset Character Data source to achieve data . Default: ncep.reanalysis2.dailyavgs \url {http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/}
#' @param  var  Character Variable of reanalisys
#' @param  level  Numeric Level of reanalisys model.
#' @param  year   Numeric Year of data 
#' @param  minLon Numeric Minimum of Longitude. Default is -51.25
#' @param  maxLon Minimum of Longitude. Default is 76.25
#' @param  minLat Minimum of Longitude. Default is 18.75
#' @param  maxLat Minimum of Longitude. Default is 81.25
#' @return Return Data brick of data
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  Ranalisys , climatology, Climate
#'
#'
#'
#' @export
#'
#'
#'
#'

retrieve_NCEP_pressure_daily=function(dataset="ncep.reanalysis2.dailyavgs",var="air",level=1000,year=1979,minLon=-51.25,maxLon=76.25,minLat=18.75,maxLat=81.25 ) {
  require(raster)
  require(ncdf)
  e <- extent(minLon,maxLon,minLat,maxLat)
  levels_NCEP_pressure=as.list(c(1:17))
  names(levels_NCEP_pressure)=as.character(c(1000, 925, 850, 700, 600, 500, 400, 300, 250, 200, 150, 100, 70, 50, 30, 20, 10))
  nlevel=as.numeric(levels_NCEP_pressure[as.character(level)])
  databrick= brick(paste0("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/",dataset,"/pressure/",var,".",as.character(year),".nc"),level=nlevel)
  proj4string(dataset) <- CRS("+init=epsg:4326")
  extent(databrick)=extent(0,360,-90,90)
  proj4string(databrick) <- CRS("+init=epsg:4326")
  databrick <- rotate(databrick)
  databrick_crop=crop(databrick,e)
  ncep=rts(databrick_crop,as.Date(as.character( databrick_crop@z$time)))
  return(ncep)
}
