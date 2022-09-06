library(readr)
library(dplyr)
library(ggplot2)
library(tmaptools)

Var_sim <- read_csv(paste(getwd(),"/Results/Var_simulation.csv",sep=""))
                        
sds1=Var_sim %>%
  group_by(no_units,Variable,Voronoi_type) %>%
  summarize(mean=mean(variance,na.rm=T),
            stdev=sd(variance,na.rm=T)) %>%
  mutate(mean = ifelse(mean=="NaN",NA,mean),
         stdev = ifelse(stdev=="NaN",NA,stdev),
         ymax = ifelse(is.na(stdev)==T&is.na(mean)==F,mean,
                       ifelse(is.na(mean)==T,NA,mean+stdev)),
         ymin = ifelse(is.na(stdev)==T&is.na(mean)==F,mean,
                       ifelse(is.na(mean)==T,NA,mean-stdev)))

Var_sim$Field2 = ifelse(Var_sim$Variable=="OK_ERI","Ordinary Kriging ERI",
                        ifelse(Var_sim$Variable=="UK_ERI","Universal Kriging ERI",Var_sim$Variable))
sds1$Field2 = ifelse(sds1$Variable=="OK_ERI","Ordinary Kriging ERI",
                        ifelse(sds1$Variable=="UK_ERI","Universal Kriging ERI",sds1$Variable))                        

sds1$Field2=factor(sds1$Field2,levels=c("Ordinary Kriging ERI","Universal Kriging ERI",
                                        "Cases","Population","Incidence"))
Var_sim$Field2=factor(Var_sim$Field2,levels=c("Ordinary Kriging ERI","Universal Kriging ERI",
                                              "Cases","Population","Incidence"))
Var_sim$log_var = ifelse(Var_sim$Field2=="Population"|
                           Var_sim$Field2=="Cases"|
                           Var_sim$Field2=="Incidence",log(Var_sim$variance),Var_sim$variance)
sds1$mean = ifelse(sds1$Field2=="Population"|
                     sds1$Field2=="Cases"|
                     sds1$Field2=="Incidence",log(sds1$mean),sds1$mean)
sds1$ymax = ifelse(sds1$Field2=="Population"|
                     sds1$Field2=="Cases"|
                     sds1$Field2=="Incidence",log(sds1$ymax),sds1$ymax)
sds1$ymin = ifelse(sds1$Field2=="Population"|
                     sds1$Field2=="Cases"|
                   sds1$Field2=="Incidence",log(sds1$ymin),sds1$ymin)

options(scipen = 2)
Var_plot=ggplot(data=Var_sim,aes(x=no_units,y=log_var))+#change y=variance to go back to non-log
  geom_point()+
  geom_smooth(data=sds1,aes(y=mean),method='loess',formula=y~x,se=F)+
  geom_smooth(data=sds1,aes(y=ymax),color='red',method='loess',formula=y~x,se=F)+
  geom_smooth(data=sds1,aes(y=ymin),color='red',method='loess',formula=y~x,se=F)+
  facet_grid(Field2~Voronoi_type,scales='free_y')+
  xlab("Number of polygons")+
  ylab("Variance")+
  theme_classic();Var_plot

# ggsave(Var_plot,filename = paste(getwd(),"/Figures/Variance_plot.pdf",sep=""),
#          height=8.5,width=5.5,units='in',dpi=300)
