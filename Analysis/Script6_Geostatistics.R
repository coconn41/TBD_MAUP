library(gstat)
library(automap)
library(raster)
library(cowplot)

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

hist(OK_ERI)
tm_shape(OK_ERI)+tm_raster(breaks=c(-1,0,.002,.004,
                                    .006,.008,.01,
                                    .012,.014,.016,1))+
  tm_shape(NYS)+tm_borders()




# Universal Kriging models

variogram=autofitVariogram(Density~Elevation,spdf1)
krigtest=krige(Density~Elevation,locations=spdf1,newdata=Kriging_df,model=variogram$var_model)
predictions=raster(krigtest)
proj4string(predictions)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(predictions)=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")
# predictions=raster::crop(predictions,extent(NYS))
# predictions=raster::mask(predictions,NYS)

variogram2=autofitVariogram(Prevalence~Elevation,spdf1)
krigtest2=krige(Prevalence~Elevation,locations=spdf1,newdata=Kriging_df,model=variogram2$var_model)
predictions2=raster(krigtest2)
proj4string(predictions2)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(predictions2)=CRS("+proj=utm +zone=18 +datum=NAD83 +units=m +no_defs")

UK_predfin = predictions2*predictions

# Clip NLCD
NLCD = raster(paste(getwd(),"/MAUP/Raster_files/Forest_NLCD_Land_Cover_2016.tif",sep=""))
crs(NLCD)=raster::crs(UK_predfin)
extent(NLCD)=extent(UK_predfin)
NLCD2 = raster::resample(NLCD,UK_predfin,method='ngb')


extent(NLCD2)=extent(UK_predfin)
NLCD3 = crop(NLCD2,extent(UK_predfin))
NLCD4 = mask(NLCD3,UK_predfin)
nlcdleg = c(11,12,24)
values(UK_predfin)[values(NLCD4)%in%nlcdleg]=0

# Save Universal Kriging Variograms
# Found here https://gis.stackexchange.com/questions/234221/adjust-text-font-size-in-plotting-autofitvariogram-in-r

# dgt <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2
# 
# mdl <- variogram$var_model
# cls <- as.character(mdl[2, "model"])
# ngt <- sum(mdl[1, "psill"])
# sll <- sum(mdl[, "psill"])
# rng <- sum(mdl[, "range"])
# lbl <- paste("Model:", cls,
#              "\nNugget:", round(ngt,5),
#              "\nSill:", round(sll,4),
#              "\nRange:", round(rng,2))
# 
# if (cls %in% c("Mat", "Ste")) {
#   kpp <- mdl[2, "kappa"]
#   lbl <- paste(lbl, "\nKappa:", round(kpp, dgt(kpp)), "")
# }
# 
# 
# Density_variogram=xyplot(gamma ~ dist, data=variogram$exp_var,
#                          main = "Density variogram",
#                          xlab = "Distance (m)",
#                          ylab = "Semi-variance",
#                          panel = function(x, y, ...) {
#                            gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
#                            ltext(max(x), 0.2 * max(y), lbl, font = 2, cex = .9, adj = c(1, 0), 
#                                  col = "grey30")},
#                          labels=NULL,mode='direct',model=mdl,
#                          direction=c(variogram$exp_var$dir.hor[1],variogram$exp_var$dir.ver[1]))
# 
# DV_grob=as_grob(Density_variogram)
# ggdraw()+draw_plot(DV_grob)
# 
# 
# dgt2 <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2
# 
# mdl2 <- variogram2$var_model
# cls2 <- as.character(mdl2[2, "model"])
# ngt2 <- sum(mdl2[1, "psill"])
# sll2 <- sum(mdl2[, "psill"])
# rng2 <- sum(mdl2[, "range"])
# lbl2 <- paste("Model:", cls2,
#               "\nNugget:", round(ngt2, 5),
#               "\nSill:", round(sll2, 4),
#               "\nRange:", round(rng2,4))
# 
# if (cls2 %in% c("Mat", "Ste")) {
#   kpp2 <- mdl2[2, "kappa"]
#   lbl2 <- paste(lbl2, "\nKappa:", round(kpp2, dgt2(kpp2)), "")
# }
# 
# #compare 
# Prevalence_variogram=xyplot(gamma ~ dist, data=variogram2$exp_var,
#                             main = "Prevalence variogram",
#                             xlab = "Distance (m)",
#                             ylab = "Semi-variance",
#                             panel = function(x, y, ...) {
#                               gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
#                               ltext(max(x), 0.2 * max(y), lbl2, font = 2, cex = .9, adj = c(1, 0), 
#                                     col = "grey30")},
#                             labels=NULL,mode='direct',model=mdl2,
#                             direction=c(variogram2$exp_var$dir.hor[1],variogram2$exp_var$dir.ver[1]))
# 
# P_grob=as_grob(Prevalence_variogram)
# 
# variogram_grobs=ggdraw()+
#   draw_plot(DV_grob,x=0,y=0,width=.5,height = 1)+
#   draw_plot(P_grob,x=.5,y=0,width=.5,height=1);variogram_grobs

# ggsave(variogram_grobs,filename = paste(getwd(),"UK_Variogram_plots.pdf",sep=""),
#        height=5,width=8,units='in',dpi=300)






#OK Variograms

# # Save Variograms
# dgt <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2
# 
# mdl <- variogram$var_model
# cls <- as.character(mdl[2, "model"])
# ngt <- sum(mdl[1, "psill"])
# sll <- sum(mdl[, "psill"])
# rng <- sum(mdl[, "range"])
# lbl <- paste("Model:", cls,
#              "\nNugget:", round(ngt,5),
#              "\nSill:", round(sll,4),
#              "\nRange:", round(rng,2))
# 
# if (cls %in% c("Mat", "Ste")) {
#   kpp <- mdl[2, "kappa"]
#   lbl <- paste(lbl, "\nKappa:", round(kpp, dgt(kpp)), "")
# }
# Density_variogram=xyplot(gamma ~ dist, data=variogram$exp_var,
#                          main = "Density variogram",
#                          xlab = "Distance (m)",
#                          ylab = "Semi-variance",
#                          panel = function(x, y, ...) {
#                            gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
#                            ltext(max(x), 0.2 * max(y), lbl, font = 2, cex = .9, adj = c(1, 0), 
#                                  col = "grey30")},
#                          labels=NULL,mode='direct',model=mdl,
#                          direction=c(variogram$exp_var$dir.hor[1],variogram$exp_var$dir.ver[1]))
# 
# DV_grob=as_grob(Density_variogram)
# ggdraw()+draw_plot(DV_grob)
# 
# 
# dgt2 <- function(x) if (x >= 10) 0 else if (x >= 1) 1 else 2
# 
# mdl2 <- variogram2$var_model
# cls2 <- as.character(mdl2[2, "model"])
# ngt2 <- sum(mdl2[1, "psill"])
# sll2 <- sum(mdl2[, "psill"])
# rng2 <- sum(mdl2[, "range"])
# lbl2 <- paste("Model:", cls2,
#               "\nNugget:", round(ngt2, 5),
#               "\nSill:", round(sll2, 4),
#               "\nRange:", round(rng2,4))
# 
# if (cls2 %in% c("Mat", "Ste")) {
#   kpp2 <- mdl2[2, "kappa"]
#   lbl2 <- paste(lbl2, "\nKappa:", round(kpp2, dgt2(kpp2)), "")
# }
# 
# #compare 
# Prevalence_variogram=xyplot(gamma ~ dist, data=variogram2$exp_var,
#                             main = "Prevalence variogram",
#                             xlab = "Distance (m)",
#                             ylab = "Semi-variance",
#                             panel = function(x, y, ...) {
#                               gstat::vgm.panel.xyplot(x, y, cex = 1.2, ...)
#                               ltext(max(x), 0.2 * max(y), lbl2, font = 2, cex = .9, adj = c(1, 0), 
#                                     col = "grey30")},
#                             labels=NULL,mode='direct',model=mdl2,
#                             direction=c(variogram2$exp_var$dir.hor[1],variogram2$exp_var$dir.ver[1]))
# 
# P_grob=as_grob(Prevalence_variogram)
# 
# variogram_grobs=ggdraw()+
#   draw_plot(DV_grob,x=0,y=0,width=.5,height = 1)+
#   draw_plot(P_grob,x=.5,y=0,width=.5,height=1);variogram_grobs
# 
# ggsave(variogram_grobs,filename = paste(getwd(),"/OK_Variogram_plots.pdf",sep=""),
#        height=5,width=8,units='in',dpi=300)