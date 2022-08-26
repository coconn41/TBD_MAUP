library(elevatr)
library(sp)
library(raster)

# Download and reproject DEM ------------------------------------------------------------
DEM = get_elev_raster(locations = st_transform(NYS,crs=4326),z = 10,neg_to_na = T,) 
DEMproj = projectRaster(DEM, crs="+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


# Convert Tick Data to sp -------------------------------------------------
Tick_data_kriging_sp = as(Tick_data_kriging %>%
            dplyr::select(geometry,
                          Tick_density,
                          ERI) %>%
              mutate(ERI=ifelse(is.na(ERI==F),0,ERI)),
          Class = "Spatial")
Tick_data_kriging_sp@bbox = as.matrix(extent(NYS))

Tick_data_kriging_sp_ANA = as(Tick_data_kriging %>%
                            dplyr::select(geometry,
                                          ANA_prevalence) %>%
                              filter(is.na(ANA_prevalence)==F),
                          Class = "Spatial")
Tick_data_kriging_sp_ANA@bbox = as.matrix(extent(NYS))
# Resample DEM ------------------------------------------------------------
resamp_rast = raster(Tick_data_kriging_sp,nrow=238,ncol=302)
DEM2 = raster::resample(DEMproj,resamp_rast,method='bilinear')




# Rasterize Tick Data and merge with DEM ----------------------------------

DEMdf = as.data.frame(DEM2,xy=T)

Tick_density_raster = rasterize(x=Tick_data_kriging_sp,
                                 y=DEM2,
                                 field='Tick_density')
Tick_density_df = as.data.frame(Tick_density_raster,
                                xy=T)
names(Tick_density_df)[3]="Tick_density"

ANA_prevalence_raster = rasterize(x=Tick_data_kriging_sp_ANA,
                                  y=DEM2,
                                  field='ANA_prevalence')
ANA_prevalence_df = as.data.frame(ANA_prevalence_raster,
                                  xy=T)
names(ANA_prevalence_df)[3]="ANA_prevalence"

ERI_raster = rasterize(x=Tick_data_kriging_sp,
                       y=DEM2,
                       field="ERI")
ERI_raster_df = as.data.frame(ERI_raster,
                              xy=T)
names(ERI_raster_df)[3]="ERI"


Kriging_df = left_join(DEMdf,Tick_density_df) %>%
  left_join(.,ERI_raster_df) %>%
  left_join(.,ANA_prevalence_df)

