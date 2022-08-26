# Sources scripts 1-4
source(paste(getwd(),"/Analysis/Script6_Geostatistics.R",sep=""))
source(paste(getwd(),"/Data_prep/Script5_Worldpop.R",sep=""))
# Warning: Will source script 1 again
source(paste(getwd(),"/Analysis/Script7_Voronoi_gen.R",sep=""))

library(spdep)
library(exactextractr)

Poly_type=c('Voronoi_polygons','Weighted_Voronoi_polygons')

pb = txtProgressBar(min = 1, max = 59420, initial = 1,style = 3)
ind=0
for(i in 30:3000){
  for(j in 1:10){
    Gen_V_polys(as.numeric(i),NYS_ex_NYC)
    Gen_Weighted_V_polys(rast=NYS_pop,n=as.numeric(i),polygon = NYS_ex_NYC)
    for(k in 1:2){
    setTxtProgressBar(pb,ind)
    ind=ind+1
    if(k==1){V_polys=Voronoi_polygons
    V_type="Random"}
    if(k==2){V_polys=Weighted_Voronoi_polygons
    V_type="Weighted"}
    V_Centroids = st_centroid(V_polys)
    v2 <- exact_extract(NYS_pop,
                        V_polys,
                          fun='sum',
                          progress=FALSE)
    v3 <-exact_extract(OK_ERI,
                       V_polys,
                          fun='mean',
                          progress=FALSE)
    v3.1 <- exact_extract(UK_ERI,
                          V_polys,
                            fun='mean',
                            progress=FALSE)
    v4 =  cbind(v2,v3)
    v4.1 = cbind(v4,v3.1)
    v5 = data.frame(v4.1,lengths(st_intersects(V_polys, sample_cases)))
    names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
    v5$inc_per_hund = (v5$Cases/v5$Population)*100000
    rmv_rows=c(which(v5$Population==0))
    if((length(which(v5$Population==0))>0)==TRUE){
    v5 = v5 %>% filter(Population!=0)
    V_Centroids=V_Centroids[-c(rmv_rows),]}
    ct=cor.test(x = v5$Cases/v5$Population,
                v5$OK_ERI,method = 'spearman')
    ct2=cor.test(x = v5$Cases/v5$Population,
                 v5$UK_ERI,method = 'spearman')
    v6=cbind(V_Centroids,v5)
    rmv_OK_ERI=c(which(v6$OK_ERI=="NaN"))
    if((length(which(v6$OK_ERI=="NaN"))>0)==T){
      v6 = v6[-c(rmv_OK_ERI),]
    }
    rmv_UK_ERI = c(which(v6$UK_ERI=="NaN"))
    if((length(which(v6$UK_ERI=="NaN"))>0)==T){
      v6 = v6[-c(rmv_UK_ERI),]
    }
    longitude = st_coordinates(st_centroid(v6))[,1]
    latitude = st_coordinates(st_centroid(v6))[,2]
    latlong = data.frame(latitude=latitude,longitude=longitude)
    xy=latlong[,c(1,2)]
    latlong = SpatialPointsDataFrame(coords=xy,data=latlong)
    knn5list = knearneigh(x=latlong, k=5, longlat=T)
    knn5 = knn2nb(knn5list, sym = TRUE)
    Inc_moran=moran.test(x = v6$inc_per_hund,
               listw = nb2listw(knn5))
    OK_moran=moran.test(x=v6$OK_ERI,
                        listw = nb2listw(knn5))
    UK_moran=moran.test(x=v6$UK_ERI,
                        listw=nb2listw(knn5))
    if(ind==1){
      tot_var_df = data.frame("no_units"=c(i,i,i,i,i),
                              "Sim_num"=c(j,j,j,j,j),
                               "variance"=c(var(v5$inc_per_hund),
                                            var(v5$Cases),
                                            var(v5$Population),
                                            var(v5$OK_ERI),
                                            var(v5$UK_ERI)),
                               "Variable"=c("Incidence",
                                            "Cases",
                                            "Population",
                                            "OK_ERI",
                                            "UK_ERI"),
                              "Voronoi_type"=c(V_type,V_type,V_type,V_type,V_type))
      tot_df=data.frame("no_units"=c(i,i),
                                 "correlation"=c(ct$estimate,ct2$estimate),
                                 "P-val"=c(ct$p.value,ct2$p.value),
                                 "Sim_num"=c(j,j),
                                 "Geostat"=c("Ordinary","Universal"),
                        "Voronoi_type"=c(V_type,V_type))
              Moran_tot_df=data.frame("no_units"=c(i,i,i),
                                      "Moran" = c(OK_moran$estimate[1],UK_moran$estimate[1],Inc_moran$estimate[1]),
                                      "P-val"=c(OK_moran$p.value,UK_moran$p.value,Inc_moran$p.value),
                                      "Sim_num"=c(j,j,j),
                                      "Field"=c("OK_ERI","UK_ERI","Incidence"),
                                      "Voronoi_type"=c(V_type,V_type,V_type))}
    if(ind>1){
      temp_var_df = data.frame("no_units"=c(i,i,i,i,i),
                               "Sim_num"=c(j,j,j,j,j),
                               "variance"=c(var(v5$inc_per_hund),
                                            var(v5$Cases),
                                            var(v5$Population),
                                            var(v5$OK_ERI),
                                            var(v5$UK_ERI)),
                               "Variable"=c("Incidence",
                                            "Cases",
                                            "Population",
                                            "OK_ERI",
                                            "UK_ERI"),
                               "Voronoi_type"=c(V_type,V_type,V_type,V_type,V_type))
      tot_var_df = rbind(tot_var_df,temp_var_df)
      temp_df=data.frame("no_units"=c(i,i),
                         "correlation"=c(ct$estimate,ct2$estimate),
                         "P-val"=c(ct$p.value,ct2$p.value),
                         "Sim_num"=c(j,j),
                         "Geostat"=c("Ordinary","Universal"),
                         "Voronoi_type"=c(V_type,V_type))
    tot_df=rbind(tot_df,temp_df)
    Moran_temp_df=data.frame("no_units"=c(i,i,i),
                            "Moran" = c(OK_moran$estimate[1], UK_moran$estimate[1], Inc_moran$estimate[1]),
                            "P-val"=c(OK_moran$p.value ,UK_moran$p.value, Inc_moran$p.value),
                            "Sim_num"=c(j,j,j),
                            "Field"=c("OK_ERI","UK_ERI","Incidence"),
                            "Voronoi_type"=c(V_type,V_type,V_type))
    Moran_tot_df=rbind(Moran_tot_df,Moran_temp_df)}}}}

tot_df$adjusted=p.adjust(tot_df$P.val,method='BH')
Moran_tot_df$adjusted=p.adjust(Moran_tot_df$P.val,method='BH')

#  write.csv(Moran_tot_df,file=paste(getwd(),"/Results/Moran_simulation.csv",sep=""))
#  write.csv(tot_df,file=paste(getwd(),"/Results/Corr_simulation.csv",sep=""))
#  write.csv(tot_var_df,file=paste(getwd(),"/Results/Var_simulation.csv",sep=""))
