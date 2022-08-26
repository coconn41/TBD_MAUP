library(sf)
library(tmap)
source(paste(getwd(),"/Data_prep/Script1_Shapefiles.R",sep=""))
source(paste(getwd(),"/Data_prep/Script2_Cases.R",sep=""))

sample_cases # loaded from Script2 in Data_prep

sample_case_map = tm_shape(NYS_Counties_ex_NYC)+
  tm_borders()+
tm_shape(NYC_Counties)+
  tm_polygons(col='grey85')+
tm_shape(sample_cases)+
  tm_dots(size=.075)+
tm_compass(position=c('left','bottom'),
           type='4star')+
tm_add_legend(title = "Counties",
              type = "fill",
              labels = c("New York State",
                         "New York City"),
              col=c("white","grey85"))+
tm_add_legend(type = 'symbol',
              labels = "Anaplasmosis case",
              shape = 16,
              col = 'black')

# tmap_save(sample_case_map,
#           filename = paste(getwd(),"/Figures/Sample_case_map.pdf",sep = ""))
