library(tmap)
library(viridisLite)

source(paste(getwd(),"/Data_prep/Script6_Geostatistics.R",sep=""))

OKgstatmap = tm_shape(OK_ERI)+
  tm_raster(breaks = c(0,0.001,
                       0.002,
                       0.004,0.006,
                       0.008,0.076),
            palette = viridisLite::viridis(n=6,option='plasma'),
            title="Predicted ERI")+
 # tm_add_legend(type='fill',col='white',labels='No prediction')+
  tm_layout(legend.position=c('left','top'),panel.labels = "Ordinary Kriging",panel.label.bg.color = 'white')+
  tm_compass(position = c('left','bottom'),type='4star')+
  tm_scale_bar(position=c('left','bottom'));OKgstatmap


UKgstatmap= tm_shape(UK_ERI)+
  tm_raster(breaks = c(0,0.001,
                       0.002,
                       0.004,0.006,
                       0.008,1),
            palette = viridisLite::viridis(n=6,option='plasma'),
            title="Predicted ERI")+
  tm_add_legend(type='fill',col='white',labels='No prediction')+
  tm_layout(legend.show=F,panel.labels = "Universal Kriging",panel.label.bg.color = 'white')+
  tm_compass(position = c('left','bottom'),type='4star')+
  tm_scale_bar(position=c('left','bottom'));UKgstatmap
  
bothmaps= tmap_arrange(OKgstatmap,UKgstatmap,ncol=2);bothmaps
    # tmap_save(bothmaps,filename=paste(getwd(),"/Figures/Gstat_maps.pdf",sep=""),
    #           height=5,width=8,units='in',dpi = 300)
   