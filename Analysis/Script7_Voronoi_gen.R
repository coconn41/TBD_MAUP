library(readxl)
library(sf)
library(dplyr)
library(sp)
library(tmap)
library(dismo)
source(paste(getwd(),"/Data_prep/Script1_Shapefiles.R",sep=""))

Gen_V_polys = function(n2,p){
  p2 = as(p,Class='Spatial')
  p3 <<- spsample(p2,n=n2,type='random')
  p4 = dismo::voronoi(x = p3,ext=extent(NYS))
  p5 = st_as_sf(p4)
  Voronoi_polygons <<- st_intersection(p5,p)
}

probsel<-function(rast, n){
  x<-getValues(rast)
  #set NA cells in raster to zero
  x[is.na(x)]<-0
  samp<-sample(nrow(rast)*ncol(rast), size=n, prob=x)
  samprast<-raster(rast)
  samprast[samp]<-1 #set value of sampled squares to 1
  #convert to SpatialPoints
  points<-rasterToPoints(samprast, fun=function(x){x>0})
  points = data.frame(points) 
  points = st_as_sf(points,coords=c("x", "y"),crs=st_crs(rast)) 
  return(points)
}

Gen_Weighted_V_polys = function(rast,n,polygon){
  samppoints=probsel(rast,n)
  spsamppoints=as(samppoints,Class="Spatial")
  weighted_v1 = dismo::voronoi(x = spsamppoints,ext=extent(polygon))
  weighted_v2 = st_as_sf(weighted_v1) 
  Weighted_Voronoi_polygons <<- st_intersection(weighted_v2,polygon)
}

