library(readr)
library(dplyr)
library(ggplot2)
library(tmaptools)


Moran_sim <- read_csv(paste(getwd(),"/Results/Moran_simulation.csv",sep=""))

sds1=Moran_sim %>%
  group_by(no_units,Field,Voronoi_type) %>%
  summarize(mean=mean(Moran),
            stdev=sd(Moran))
sds1$ymax = sds1$mean+sds1$stdev
sds1$ymin = sds1$mean-sds1$stdev


Moran_sim$`P-value`=ifelse(Moran_sim$adjusted<0.05,"< 0.05","\u2265 0.05")
Moran_sim$Field2=ifelse(Moran_sim$Field=="OK_ERI","Ordinary Kriging",
                        ifelse(Moran_sim$Field=="UK_ERI","Universal Kriging","Incidence"))
sds1$Field2=ifelse(sds1$Field=="OK_ERI","Ordinary Kriging",
                        ifelse(sds1$Field=="UK_ERI","Universal Kriging","Incidence"))

Moran_plot=ggplot(data=Moran_sim,aes(x=no_units,y=Moran))+
  geom_point()+
  geom_smooth(data=sds1,aes(y=mean),method='loess',formula=y~x,se=F)+
  geom_smooth(data=sds1,aes(y=ymax),color='red',method='loess',formula=y~x,se=F)+
  geom_smooth(data=sds1,aes(y=ymin),color='red',method='loess',formula=y~x,se=F)+
  facet_grid(Voronoi_type~Field2)+
  xlab("Number of polygons")+
  ylab("Moran's Index")+
  theme_classic()+
  theme(panel.background = element_rect(colour = 'grey70', size = 0.4));Moran_plot

 #  ggsave(Moran_plot,filename = paste(getwd(),"/Figures/Morans_I_plot.tiff",sep=""),
 #           height=4,width=7,units='in',dpi=300)
