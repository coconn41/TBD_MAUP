library(tmap)
library(raster)
library(tmaptools)
library(cowplot)
library(ggplot2)
library(exactextractr)
source(paste(getwd(),"/Analysis/Script7_Voronoi_gen.R",sep=""))
# Make sure your NYS_pop object is loaded 
# source(paste(getwd(),"/Data_prep/Script5_Worldpop.R",sep="")) 
NYS_reduced_rast = raster::mask(NYS_pop,NYS_ex_NYC)
source(paste(getwd(),"/Data_prep/Script2_Cases.R",sep=""))

Gen_V_polys(n2 = 30,p = NYS_ex_NYC)
v2 <- exact_extract(NYS_pop,
                      Voronoi_polygons,
                      fun='sum')
v5 = data.frame(v2,lengths(st_intersects(Voronoi_polygons, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(Voronoi_polygons,v5)

map1=tm_shape(mapdf)+
  tm_polygons(col="inc_per_hund",border.alpha = .5,
              breaks=c(-1,0.00000001,seq(10,70,by=20),400),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_credits("30 polygons",position = c('left','bottom'))+
  tm_layout(legend.show = F);map1

## map 2


Gen_V_polys(300,NYS_ex_NYC)
v2 <- exact_extract(NYS_pop,
                      Voronoi_polygons,
                      fun='sum')
v5 = data.frame(v2,lengths(st_intersects(Voronoi_polygons, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(Voronoi_polygons,v5)

map2=tm_shape(mapdf)+
  tm_polygons(col="inc_per_hund",border.alpha = .5,
              breaks=c(-1,0.00000001,seq(10,70,by=20),420),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_credits("300 polygons",position=c('left','bottom'))+
  tm_layout(legend.show=F);map2


Gen_V_polys(3000,NYS_ex_NYC)
v2 <- exact_extract(NYS_pop,
                      Voronoi_polygons,
                      fun='sum')
v5 = data.frame(v2,lengths(st_intersects(Voronoi_polygons, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(Voronoi_polygons,v5)

map3=tm_shape(mapdf %>% filter(inc_per_hund>0),bbox=st_bbox(NYS))+
  tm_polygons(col="inc_per_hund",border.alpha = 0,
              breaks=c(-1,0.00000001,seq(10,70,by=20),4000),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_shape(mapdf %>% filter(inc_per_hund==0))+
  tm_borders(alpha=.15)+
  tm_credits("3,000 polygons",position=c('left','bottom'))+
  tm_layout(legend.show = F);map3

map4=tm_shape(mapdf)+
  tm_polygons(col="inc_per_hund",
              breaks=c(-1,0.00000001,seq(10,70,by=20),400),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_layout(legend.only = T)



v2 <- exact_extract(NYS_pop,
                    NYS_Counties_ex_NYC,
                      fun='sum')
v5 = data.frame(v2,lengths(st_intersects(NYS_Counties_ex_NYC, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(NYS_Counties_ex_NYC,v5)

map5=tm_shape(mapdf)+
  tm_polygons(col="inc_per_hund",border.alpha = .5,
              breaks=c(-1,0.00000001,seq(10,70,by=20),400),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_credits("New York State Counties\n(57 polygons)",position=c('left','bottom'))+
  tm_layout(legend.show = F);map5

v2 <- exact_extract(NYS_pop,
                    NYS_ZCTAs_ex_NYC,
                      fun='sum')
v5 = data.frame(v2,lengths(st_intersects(NYS_ZCTAs_ex_NYC, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(NYS_ZCTAs_ex_NYC,v5)

map6=tm_shape(mapdf %>% filter(inc_per_hund>0),bbox=st_bbox(NYS))+
  tm_polygons(col="inc_per_hund",border.alpha = 0,
              breaks=c(-1,0.00000001,seq(10,70,by=20),3500),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_shape(mapdf %>% filter(inc_per_hund==0))+
  tm_borders(alpha=.15)+
  tm_credits("New York State ZCTAs\n(1,583 polygons)",position=c('left','bottom'))+
  tm_layout(legend.show = F);map6


Gen_Weighted_V_polys(NYS_pop,n=30,polygon = NYS_ex_NYC)
v2 <- exact_extract(NYS_pop,
                    Weighted_Voronoi_polygons,
                    fun='sum')
v5 = data.frame(v2,lengths(st_intersects(Weighted_Voronoi_polygons, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(Weighted_Voronoi_polygons,v5)

map7=tm_shape(mapdf)+
  tm_polygons(col="inc_per_hund",border.alpha = .5,
              breaks=c(-1,0.00000001,seq(10,70,by=20),400),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_credits("30 polygons",position = c('left','bottom'))+
  tm_layout(legend.show = F);map7

Gen_Weighted_V_polys(NYS_pop,n=300,polygon = NYS_ex_NYC)
v2 <- exact_extract(NYS_pop,
                    Weighted_Voronoi_polygons,
                    fun='sum')
v5 = data.frame(v2,lengths(st_intersects(Weighted_Voronoi_polygons, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(Weighted_Voronoi_polygons,v5)

map8=tm_shape(mapdf)+
  tm_polygons(col="inc_per_hund",border.alpha = .5,
              breaks=c(-1,0.00000001,seq(10,70,by=20),400),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_credits("300 polygons",position = c('left','bottom'))+
  tm_layout(legend.show = F);map8

Gen_Weighted_V_polys(NYS_pop,n=3000,polygon = NYS_ex_NYC)
v2 <- exact_extract(NYS_pop,
                    Weighted_Voronoi_polygons,
                    fun='sum')
v5 = data.frame(v2,lengths(st_intersects(Weighted_Voronoi_polygons, sample_cases)))
names(v5)=c("Population","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
mapdf = cbind(Weighted_Voronoi_polygons,v5)



map9=tm_shape(mapdf %>% filter(inc_per_hund>0),bbox=st_bbox(NYS))+
  tm_polygons(col="inc_per_hund",border.alpha = 0,
              breaks=c(-1,0.00000001,seq(10,70,by=20),3500),
              palette=c('white',get_brewer_pal("Reds",n=5)),
              title="Incidence per 100K",
              labels=c("0",
                       ">0 - 10",
                       ">10 - 30",
                       ">30 - 50",
                       ">50 - 70",
                       ">70"))+
  tm_shape(mapdf %>% filter(inc_per_hund==0))+
  tm_borders(alpha=.15)+
  tm_credits("3,000 polygons",position=c('left','bottom'))+
  tm_layout(legend.show = F);map9





map1grob=tmap_grob(map1)
map2grob=tmap_grob(map2)
map3grob=tmap_grob(map3)
leggrob=tmap_grob(map4)
map5grob=tmap_grob(map5)
map6grob=tmap_grob(map6)
map7grob=tmap_grob(map7)
map8grob=tmap_grob(map8)
map9grob=tmap_grob(map9)

fullmap=ggdraw()+
  draw_plot(map1grob,x=0.03,y=.67,width = .3,height=.3)+
  draw_plot(map2grob,x=.35,y=.67,width=.3,height=.3)+
  draw_plot(map3grob,x=.67,y=.67,width=.3,height=.3)+
  draw_plot(map7grob,x=0.03,y=.35,width=.3,height=.3)+
  draw_plot(map8grob,x=.35,y=.35,width=.3,height=.3)+
  draw_plot(map9grob,x=.67,y=.35,width=.3,height=.3)+
  draw_plot(map5grob,x=.18,y=0.03,width=.3,height=.3)+
  draw_plot(map6grob,x=.52,y=0.03,width=.3,height=.3)+
  draw_plot(leggrob,x=-.08,y=-.435,height = .8)+
  draw_plot_label(c("Random Voronoi","Weighted Voronoi","True-border"),
                  x=c(1,1,.85),
                  y=c(1.01,.71,.33),
                  size=c(14,14,14),
                  angle=-90)#;fullmap

  # ggsave(fullmap,filename = paste(getwd(),"/Figures/MAUP_example_map.pdf",sep=""),
   #       width=10,height=8,units='in')
