library(dismo)
Gen_V_polys = function(n2,p){
  p2 = as(p,Class='Spatial')
  p3 = spsample(p2,n=n2,type='random')
  p4 = dismo::voronoi(x = p3,ext=extent(NYS)) # extent can be changed to any other sf shapefile
  p5 = st_as_sf(p4)
  Voronoi_polygons <<- st_intersection(p5,p)
}