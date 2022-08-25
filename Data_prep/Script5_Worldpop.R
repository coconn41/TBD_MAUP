library(raster)

# Gridded world population count can be downloaded here:
# https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download
# Login required, but is available for free
# When downloaded set file path within your working directory below:

filepath = ""
popdat = raster::raster(x=paste(getwd(),filepath,sep=""))
NYS_4326 = st_transform(NYS,crs=4326)

crop_popdat = crop(popdat,NYS_4326)
NYS_pop = mask(crop_popdat,extent(NYS_4326))
names(NYS_pop)[1]="population"

NYS_pop = raster::projectRaster(NYS_pop,crs="+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
NYS_reduced_rast = raster::mask(NYS_pop,NYS_ex_NYC)
