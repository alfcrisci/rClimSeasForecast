#' create_composite
#'
#' @description Create and save composite of weather type 
#'
#' @param  wt  Numerical Vector with weather type ( weather type 6 to be avoided)
#' @param  mese Numerical Month of analisys
#' @param  weighted  If using a vector of frequency referred to weather type 1,2,3,4,5,7,8. Default is FALSE
#' @param  weight   Numerical Vector of frequency related 
#' @param  save_files Integer indicate the maximum number for top statistics
#' @return Return a raster layer of composites
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  weather type,climatology
#'
#'
#'
#' @export
#'
#'
#'
#'

create_composite=function(wt=c(1,2,3,4,5,7,8),mese=1,weighted=FALSE,weight=c(1,1,1,1,1,1,1),save_files=T) {
         require(raster)
         require(ncdf)
 
          if ((length(weight) !=7) && (weighted==TRUE) )  {stop("Some error occur. Remember to exclude circulation type 6. Ma x length of vector is seven!")}
         
         data(hgt500_anom_wt)
         data(rr_eobs_anom_wt)
         data(tg_eobs_anom_wt)
         data(tx_eobs_anom_wt)
         data(tn_eobs_anom_wt)
  
         index_hgt500=as.numeric(sapply(as.character(wt),FUN=function(x) grep(paste0("_",mese,"_",x,"_"),names(hgt500_anom_wt))))
         index_rr_eobs=as.numeric(sapply(as.character(wt),FUN=function(x) grep(paste0("_",mese,"_",x,"_"),names(rr_eobs_anom_wt))))
         index_tg_eobs=as.numeric(sapply(as.character(wt),FUN=function(x) grep(paste0("_",mese,"_",x,"_"),names(tg_eobs_anom_wt))))
         index_tx_eobs=as.numeric(sapply(as.character(wt),FUN=function(x) grep(paste0("_",mese,"_",x,"_"),names(tx_eobs_anom_wt))))
         index_tn_eobs=as.numeric(sapply(as.character(wt),FUN=function(x) grep(paste0("_",mese,"_",x,"_"),names(tn_eobs_anom_wt))))
         
         if ( weighted==FALSE) {
         
         hgt500_composite=raster::calc(hgt500_anom_wt[[index_hgt500]],weight,na.rm=T)
         rr_eobs_composite=raster::calc(rr_eobs_anom_wt[[index_rr_eobs]],weight,na.rm=T)
         tg_eobs_composite=raster::calc(tg_eobs_anom_wt[[index_tg_eobs]],weight,na.rm=T)
         tx_eobs_composite=raster::calc(tx_eobs_anom_wt[[index_tx_eobs]],weight,na.rm=T)
         tn_eobs_composite=raster::calc(tx_eobs_anom_wt[[index_tn_eobs]],weight,na.rm=T)
         
         }
         
         if ( weighted == TRUE) 
           {
           
           hgt500_composite=raster::weighted.mean(hgt500_anom_wt[[index_hgt500]],mean,na.rm=T)
           rr_eobs_composite=raster::weighted.mean(rr_eobs_anom_wt[[index_rr_eobs]],mean,na.rm=T)
           tg_eobs_composite=raster::weighted.mean(tg_eobs_anom_wt[[index_tg_eobs]],mean,na.rm=T)
           tx_eobs_composite=raster::weighted.mean(tx_eobs_anom_wt[[index_tx_eobs]],mean,na.rm=T)
           tn_eobs_composite=raster::weighted.mean(tx_eobs_anom_wt[[index_tn_eobs]],mean,na.rm=T)
           
           
          }
           
         if (save_files==TRUE) 
           
           {
           raster::writeRaster(rr_eobs_composite,paste0("rr_eobs_composite_",mese,"_wt_obs.nc", "CDF", overwrite=TRUE,varname="anom_rr",varunit="centimeter",longname="anom_rr",xname="lon",yname="lat"))
           raster::writeRaster(hgt500_composite,paste0("hgt500_composite_",mese,"_wt_obs.nc", "CDF", overwrite=TRUE,varname="anom_hgt500",varunit="meters",longname="anom_hgt500",xname="lon",yname="lat"))
           raster::writeRaster(tg_eobs_composite,paste0("tg_eobs_composite_",mese,"_wt_obs.nc", "CDF", overwrite=TRUE,varname="anom_tg_eobs",varunit="degrees",longname="anom_tg_eobs",xname="lon",yname="lat"))
           raster::writeRaster(tx_eobs_composite,paste0("tx_eobs_composite_",mese,"_wt_obs.nc", "CDF", overwrite=TRUE,varname="anom_tx_eobs",varunit="degrees",longname="anom_tx_eobs",xname="lon",yname="lat"))
           raster::writeRaster(tn_eobs_composite,paste0("tn_eobs_composite_",mese,"_wt_obs.nc", "CDF", overwrite=TRUE,varname="anom_tn_eobs",varunit="degrees",longname="anom_tn_eobs",xname="lon",yname="lat"))
            }
         
         res=list(hgt500_composite=hgt500_composite,
                  tg_eobs_composite=tg_eobs_composite,
                  tx_eobs_composite=tx_eobs_composite,
                  tn_eobs_composite=tn_eobs_composite)
                  
         return(res)
}
