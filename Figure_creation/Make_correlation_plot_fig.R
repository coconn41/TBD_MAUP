library(readr)
library(dplyr)
library(ggplot2)
library(tmaptools)

Full_sim <- read_csv(paste(getwd(),"/Results/Corr_simulation.csv",sep=""))
Trues <- read_csv(paste(getwd(),"/Results/True_polygons.csv",sep="")) %>%
  mutate(`P-value`=ifelse(.$P.val<0.05,"< 0.05","\u2265 0.05"),
         Voronoi_type="Random")
Trues1 <- read_csv(paste(getwd(),"/Results/True_polygons.csv",sep="")) %>%
  mutate(`P-value`=ifelse(.$P.val<0.05,"< 0.05","\u2265 0.05"),
         Voronoi_type="Weighted")
Trues <- rbind(Trues,Trues1)
Trues$`Polygon type`=ifelse(Trues$`Polygon type`=="ZCTAs","ZIP code\ntabulation areas",Trues$`Polygon type`)


sds1 = Full_sim %>%
  group_by(no_units,Geostat,Voronoi_type) %>%
  summarize(mean=mean(correlation),
            stdev=sd(correlation))

sds1$ymax = sds1$mean+sds1$stdev
sds1$ymin = sds1$mean-sds1$stdev
sds1$Geostat = ifelse(sds1$Geostat=="Ordinary","Ordinary Kriging","Universal Kriging")






full_plot_dat2 = Full_sim %>%
  mutate(`P-value` =factor(ifelse(.$adjusted<0.05,"< 0.05","\u2265 0.05"))) %>%
  mutate(`Polygon type` = "Simulated") %>%
  bind_rows(.,Trues) %>%
  mutate(Geostat = ifelse(Geostat=="Ordinary","Ordinary Kriging",'Universal Kriging')) 

full_plot_dat2$`Polygon type`=factor(full_plot_dat2$`Polygon type`,
                                     levels=c("Simulated","Congressional\ndistricts",
                                              "Counties","State legislative\ndistricts (upper)",
                                              "State legislative\ndistricts (lower)","County subdivisions",
                                              "ZIP code\ntabulation areas","Census tracts"))
full_plot_dat2$`P-value`=factor(full_plot_dat2$`P-value`,levels=c("< 0.05","\u2265 0.05"))
palette1 =c("black",get_brewer_pal("Accent",n=8));palette1
palette2 = c("black",'#7FC97F','#BEAED4','#FDC086','#FFFF99','#386CB0','#F0027F','#BF5B17',"#666666")

arrange_data = full_plot_dat2 %>%
  arrange(`P-value`,`Polygon type`)

corr_plot=ggplot(data=arrange_data,
                 aes(x=no_units,y=correlation))+
  geom_point(aes(fill=`Polygon type`,
                 size=`Polygon type`,
                 color=`P-value`,
                 alpha=`Polygon type`),
              shape=21,stroke=.75)+
  scale_fill_manual(values=palette2)+
  scale_size_manual(values=c(1,4,4,4,4,4,4,4,4,4))+
  scale_color_manual(values=c('black','grey80'))+
  scale_alpha_manual(values=c(1,.9,.9,.9,.9,.9,.9,.9,.9,.9))+
  geom_smooth(data=sds1,aes(y=mean),method='loess',formula=y~x,se=F)+
  geom_smooth(data=sds1,aes(y=ymax),color='red',method='loess',formula=y~x,se=F)+
  geom_smooth(data=sds1,aes(y=ymin),color='red',method='loess',formula=y~x,se=F)+
  xlab("Number of polygons")+
  ylab("Correlation coefficient")+
  facet_grid(Voronoi_type~Geostat,scales='free_y')+
  theme_classic()+
  theme(legend.key.height = unit(.95,'cm'),
        #panel.grid.major = element_line(colour = "grey70", size = 0.2),
        panel.background = element_rect(colour = 'grey70', size = 0.4));corr_plot



 #  ggsave(corr_plot,filename = paste(getwd(),"/Figures/Corr_plot.pdf",sep=""),
  #          height=7,width=10,units='in',dpi=300)
