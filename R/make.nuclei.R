##Function----
#' make.nuclei()
#' 
#' Requires sf, terra, and classInt
#' 
#' The function derives population nuclei, i.e. "noyaux d'habitat" or morphological 
#'  agglomeration clusters based on 200m resolution raster (terra SpatRaster)
#'  with population counts. A 200m gap is often agreed as a threshold to define
#'  a spatial disconituity in the urban fabric. As a proxy to using buildings,
#'  the function computes patches from a 200 m resolution grid.
#'  Not all populated cells are considered. The default is to disconsider first
#'  all cells with less than 6 people (urban_th), which means a 150 inh/sq km threshold.
#'  Then only patches summing at least 150 (pop_th) inhabitants in total are kept as
#'   nuclei (noyaux).
#'  The function output a list of two elements: a sf of the nuclei polygons 
#'  and a summary table with population counts per groups of population size (cl_brks)
#'  The Dispersed population is the total population not matching the urban_th at
#'  the cell scale the population in patches smaller than pop_th.
#'
#'
#' @param r SpatRaster at resolution 200 x 200 with numeric values. Name of variable is disconsidered
#' @param urban_th Population threshold under which a cell is not considered as urbanised
#'  and cannot be part of a nucleus (noyaux). Default is 6, which given the reslution is 150 inh/sq km 
#' @param pop_th Population threshold under which a patch is not considered a nucleus (noyaux).
#'  Defaulted to 150 inhabitants. 
#' @param neighb 4 or 8. Depending on whether we accept cells to be connected by
#' their diagonal to make a patch. Preference is 4 (default), no diagonal, given
#' the process based on 200m can already lead to longer gaps than if one would
#' consider building distances
#' @param cl_brks Vector of population sizes used to categorize the noyaux in
#'  population classes. Upper limit is total population in the case everyone would be in a single noyaux.
#' @param sfplot Logical to return the polygons sf. False means only the summary table
#'
#' @return A list with 2 named objects: "sf" and "classes", respectively the
#'  polygons of the noyaux and the summary table (data.frame) by population classes (cl_brks)
#' @export
#'
#' @examples
make.nuclei<-function(r, urban_th=6, pop_th=150, neighb=4,
                      cl_brks=c(150,500,1000,2500,5000,10000,25000,50000,200000,sum(terra::values(r),na.rm=TRUE)),
                      sfplot=TRUE, fillHoles=TRUE
){
  TOTPOP<-sum(terra::values(r),na.rm=TRUE)
  message(paste("Total population in raster:",TOTPOP))
  
  r2<-r>= urban_th
  #urban_th is limit at which a 200m cell can potentially be attached to a nucleus.
  #4 people means 100 inhb/sq km.
  #6 is 150 inhb/sq km used as default
  #too small then dispersed pop at the outskirt of a nucleus will be included. If too high nuclei might get smaller than needed
  rp <- terra::patches(r2, directions=neighb, zeroAsNA=TRUE) #4neighb consider, hence max distance 200
  freq_rp<-terra::freq(rp)
  message(paste("Number of patches:",length(unique(freq_rp$value))))
  
  #rp is any patch, and many are of size 1 cell.
  #Following Belgian case in 2001 a noyau must be at least 150 inhab
  #We thus zonal stats original pop raster r to rp then limit rp's by pop_th>=150
  zonal_sum<-terra::zonal(r,rp, fun="sum")
  message(paste("Population within cells below the urban threshold:",
                TOTPOP-sum(zonal_sum[,2])))
  #population considered dispersed whatever the definition of nuclei
  # before applying pop threshold to nuclei
  nucleus_id<-zonal_sum$patches[zonal_sum[,2]>=pop_th]
  message(paste("Number of population clusters (Noyaux)",length(nucleus_id)))
  rnuclei <- terra::mask(rp, rp, nucleus_id, inverse=TRUE) #this set NA's in the raster of patches where patches do not reach pop limit
  
  #convert raster to spatVector 
  vnuclei0<-terra::as.polygons(rnuclei) #spatvector

  #get rid of isolated cells within a polygon, i.e. include them in noyaux
  if (fillHoles==TRUE) {
    vnuclei<-terra::fillHoles(vnuclei0, inverse=FALSE)
  } else {
    vnuclei<-vnuclei0
  }
  
  #Create sf (thus df)
  nuclei_sf<-sf::st_as_sf(vnuclei)
  
  #recompute pop of noyaux after filling the holes, i.e. reintegrate those cells
  # excluded by urban_th (see r2 from r) but being within filled hole of a noyaux
  nuclei_sf$population<-terra::zonal(r,vnuclei, fun="sum", na.rm=TRUE)[,1]
  # Also recompute number of cells per noyaux (i.e surfaces)
  #  I use sum of those being true >1 since count is not a zonal function
  nuclei_sf$cellcount<-terra::zonal(r>=1,vnuclei, fun="sum", na.rm=TRUE)[,1] 
  # Add surface and density
  nuclei_sf$km2<-sf::st_area(nuclei_sf)*10^(-6)
  nuclei_sf$density_km2<-nuclei_sf$population/nuclei_sf$km2
  
  #Assemble nuclei summary table
  cl.int.nuclei<-classInt::classIntervals(nuclei_sf$population,
                                          style = "fixed",
                                          fixedBreaks=cl_brks)
  nuclei_sf$cl.nucleus<-classInt::findCols(cl.int.nuclei, factor=TRUE)
  
  group_n <- aggregate(nuclei_sf$population, list(nuclei_sf$cl.nucleus), length)
  group_sum <- aggregate(nuclei_sf$population, list(nuclei_sf$cl.nucleus), sum)
  
  nuclei_table0<-cbind(group_n,group_sum[,2])
  levels(nuclei_table0$Group.1)<-c(levels(nuclei_table0$Group.1),"Dispersed")
  nuclei_table<-rbind(c("Dispersed",NA,TOTPOP-sum(nuclei_sf$population)),nuclei_table0)
  
  names(nuclei_table)<-c("cl.nuclei","n","pop")
  nuclei_table$pop<-as.numeric(nuclei_table$pop)
  nuclei_table$n<-as.numeric(nuclei_table$n)
  
  nuclei_table$cumul.pop<-cumsum(nuclei_table$pop)
  nuclei_table$pc.pop<-round(100*nuclei_table$pop/TOTPOP,digits=2)
  nuclei_table$cumul.pc.pop<-cumsum(nuclei_table$pc.pop)
  
  #Map
  if(sfplot==TRUE){plot(nuclei_sf)}
  
  return(list(sf=nuclei_sf, classes=nuclei_table))
  
}
