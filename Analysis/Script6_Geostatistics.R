library(gstat)
library(automap)
library(raster)

Density_df = Kriging_df %>%
  st_as_sf(.,coords=c('x','y')) %>%
  filter(is.na(.$Tick_density)==F)
coordinates(Kriging_df)=~x+y
gridded(Kriging_df)=TRUE
Density_df = as(Density_df,Class="Spatial")

Prevalence_df = Kriging_df %>%
  st_as_sf(.,coords=c('x','y')) %>%
  filter(is.na(.$ANA_prevalence)==F)
Prevalence_df = as(Prevalence_df,Class="Spatial")

# Ordinary Kriging --------------------------------------------------------
Density_variogram=autofitVariogram(Tick_density~1,
                                   Density_df)
Density_kriging=krige(Tick_density~1,
                      locations=Density_df,
                      newdata=Kriging_df,
                      model=Density_variogram$var_model)
Density_predictions=raster(Density_kriging)
proj4string(Density_predictions)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(Density_predictions)=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")

Prevalence_variogram=autofitVariogram(ANA_prevalence~1,
                                      Prevalence_df)
Prevalence_kriging=krige(ANA_prevalence~1,
                         locations=Prevalence_df,
                         newdata=Kriging_df,
                         model=Prevalence_variogram$var_model)
Prevalence_predictions=raster(Prevalence_kriging)
proj4string(Prevalence_predictions)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(Prevalence_predictions)=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")

OK_ERI = Density_predictions*Prevalence_predictions

# Universal Kriging models ------------------------------------------------
UK_Density_variogram=autofitVariogram(Tick_density~layer,
                                      Density_df)
UK_Density_kriging=krige(Tick_density~layer,
                         locations=Density_df,
                         newdata=Kriging_df,
                         model=UK_Density_variogram$var_model)
UK_Density_predictions=raster(UK_Density_kriging)
proj4string(UK_Density_predictions)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(UK_Density_predictions)=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")
# predictions=raster::crop(predictions,extent(NYS))
# predictions=raster::mask(predictions,NYS)

UK_Prevalence_variogram=autofitVariogram(ANA_prevalence~layer,
                                         Prevalence_df)
UK_Prevalence_kriging=krige(ANA_prevalence~layer,
                locations=Prevalence_df,
                newdata=Kriging_df,
                model=UK_Prevalence_variogram$var_model)
UK_Prevalence_predictions=raster(UK_Prevalence_kriging)
proj4string(UK_Prevalence_predictions)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(UK_Prevalence_predictions)=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")

UK_ERI = UK_Density_predictions*UK_Prevalence_predictions