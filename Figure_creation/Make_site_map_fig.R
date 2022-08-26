library(tmap)
library(tmaptools)

source(paste(getwd(),"/Data_prep/Script3_ERI.R",sep=""))


sitemap=tm_shape(NYS_Counties_ex_NYC)+
  tm_borders()+
  tm_shape(NYC_Counties)+
  tm_polygons(col='grey85')+
  tm_shape(Tick_data_kriging)+
  tm_dots(col='ERI',title = "Ap-ha ERI",
          size=0.35,shape = 21,
          breaks=c(0,0.00015,0.004,0.006,0.01,0.02),
          labels=c("0","> 0 - 0.004",
                   "> 0.004 - 0.006",
                   "> 0.006 - 0.01",
                   "> 0.01"),
         palette= get_brewer_pal("Reds", n = 5, contrast = c(0, 0.8)))+
  tm_compass(position=c('left','bottom'),type='4star')+
  tm_scale_bar(position=c('left','bottom'))+
tm_add_legend(title = "Counties",
              type="fill",
              labels=c("New York State","New York City"),
              col=c('white','grey85'))+
  tm_credits("Counties with no circle were not sampled in 2017",
             position=c('left','bottom'));sitemap

 # tmap_save(sitemap,filename=paste(getwd(),"/Figures/Sitemap.pdf",sep=""),
 #                      width=7,height=6.5,units='in')
         