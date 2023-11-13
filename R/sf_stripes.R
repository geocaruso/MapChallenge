#geoffrey.caruso@uni.lu 

#Function to create striped maps without ggpattern.
#Result can be plotted with other means.
#Idea: create a series of stripes based on a ordinal variable
# (factor for which levels make sense) present in a sf
# For each level, the bounding box of the sf is covered by stripes
# of different intensity based on the level.
# Default, minimum 10 and max 100 stripes over the entire bounding box
# 
# The stripes are in forms of linestrings. They are intersected by the orignial sf
# and those where the linestring and the sf levels correspond are kept
# 
# There might be more parcimonious approaches

#Given a factor fname within a sf, covers the bbox with
# lines with an angle 30 at regular interval between 10 (first level)
# and 100 (last level)
sf_stripes<-function(sf,fname,angle=30,n=c(10,100),reverse=TRUE){
  bb<-unname(st_bbox(sf))
  y.range<-bb[4]-bb[2]
  x.range<-bb[3]-bb[1]
  sf2<-st_drop_geometry(sf)
  stopifnot(is.factor(sf2[,fname]))
  sf.levels<-levels(sf2[,fname])
  f<-length(sf.levels)
  delta.y<-sin(angle*(pi/180))*x.range
  rns<-round(seq(from=n[1],to=n[2],by=(n[2]-n[1])/(f-1)))
  ns<-rev(rns)
  if(reverse==FALSE){ns<-rns}
  
  line.coords<-lapply(ns,function(i){
    y.intval<-y.range/i
    fy=c(seq(bb[2],by=y.intval,length.out=i),
         seq(bb[2],by=-y.intval,length.out=i)[-1])
    data.frame(fx=rep(bb[1],2*i-1),
               fy=fy,
               ex=rep(bb[3],2*i-1),
               ey=fy+delta.y)
  })
  
  splitted<-lapply(line.coords,
                   function(lc){split(lc,seq(nrow(lc)))})
  
  #Need A function to transform a df with coordinates
  # of origin and destination as a linestring 
  # assume following order of columns: from x, from y, end x, endy
  #
  lstring<-function(s){ 
    st_linestring(matrix(
      as.numeric(c(s[1:2],s[3:4])),
      c(2,2),
      byrow=TRUE)
    )}
  
  #Now for each feature, make multinestring from each row (list) of linestrings (using funtion lstring)
  multis.lst<-lapply(splitted, function(alist){st_multilinestring((lapply(alist,lstring)))})
  
  levels.sfc<-st_sfc(multis.lst)
  st_crs(levels.sfc)<-st_crs(sf)
  df<-data.frame(sf.levels)
  st_geometry(df)<-levels.sfc
  sf3<-st_intersection(sf,df)
  keep<-st_drop_geometry(sf3)[,fname]==st_drop_geometry(sf3)[,"sf.levels"]

  Hachures<-sf3[keep==TRUE,]
  return(Hachures)
}
