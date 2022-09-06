library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(exactextractr)

Full_sim <- read_csv(paste(getwd(),"/Results/Corr_simulation.csv",sep=""))

# Add true County estimates
v2 <- exact_extract(NYS_pop,
                    NYS_Counties_ex_NYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                      NYS_Counties_ex_NYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      NYS_Counties_ex_NYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(NYS_Counties_ex_NYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_County=cor.test(x = v5$Cases/v5$Population,
                     v5$OK_ERI,method = 'spearman')
True_County_UK=cor.test(x=v5$Cases/v5$Population,v5$UK_ERI,
                        method="spearman")

True_County1 = data.frame(correlation=c(True_County$estimate,True_County_UK$estimate),
                          Geostat = c("Ordinary","Universal"),
                          P.val = c(True_County$p.value,True_County_UK$p.value))
True_County1$no_units=57
True_County1$`Polygon type` = "Counties"
True_County1$`P-value` = "< 0.05"

# Add true ZIP estimates
v2 <- exact_extract(NYS_pop,
                      NYS_ZCTAs_ex_NYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                    NYS_ZCTAs_ex_NYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      NYS_ZCTAs_ex_NYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(NYS_ZCTAs_ex_NYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_ZCTAs=cor.test(x = v5$Cases/v5$Population,
                    v5$OK_ERI,method = 'spearman')
True_ZCTAs_UK=cor.test(x = v5$Cases/v5$Population,
                       v5$UK_ERI,method = 'spearman')
True_ZIP1 = data.frame(correlation=c(True_ZCTAs$estimate,True_ZCTAs_UK$estimate),
                       Geostat = c("Ordinary","Universal"),
                       P.val = c(True_ZCTAs$p.value,True_ZCTAs_UK$p.value))
names(True_ZIP1)[1]="correlation"
True_ZIP1$no_units=1583
True_ZIP1$`Polygon type` = "ZCTAs"
True_ZIP1$`P-value` = "< 0.05"
# , State_Legislative_districts_lower, upper, NYS_Census_tracts, United Nations Statistical Definitions
#####################################
# Add true County_subdivision estimates
County_subunits = County_subunits %>%
  st_transform(.,crs=st_crs(sample_cases))
County_sub_exNYC = County_subunits %>%
  filter(!c(COUSUBFP%in%c("70915","10022","60323","44919","08510")))
v2 <- exact_extract(NYS_pop,
                    County_sub_exNYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                    County_sub_exNYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      County_sub_exNYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(County_sub_exNYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_County_subunits=cor.test(x = v5$Cases/v5$Population,
                    v5$OK_ERI,method = 'spearman')
True_County_subunits_UK=cor.test(x = v5$Cases/v5$Population,
                       v5$UK_ERI,method = 'spearman')
True_County_subunits1 = data.frame(correlation=c(True_County_subunits$estimate,True_County_subunits_UK$estimate),
                       Geostat = c("Ordinary","Universal"),
                       P.val = c(True_County_subunits$p.value,True_County_subunits_UK$p.value))
names(True_County_subunits1)[1]="correlation"
True_County_subunits1$no_units=nrow(County_sub_exNYC)
True_County_subunits1$`Polygon type` = "County subdivisions"
True_County_subunits1$`P-value` = ifelse(True_County_subunits1$P.val<0.05,"< 0.05","\u2265 0.05")
########################################################
# Add true Lower_State files
State_Legislative_districts_lower = State_Legislative_districts_lower %>%
  st_transform(.,crs=st_crs(sample_cases))
SLDL_exNYC = st_difference(State_Legislative_districts_lower,st_union(NYC_Counties))
v2 <- exact_extract(NYS_pop,
                    SLDL_exNYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                    SLDL_exNYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      SLDL_exNYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(SLDL_exNYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_State_Legislative_districts_lower=cor.test(x = v5$Cases/v5$Population,
                              v5$OK_ERI,method = 'spearman')
True_State_Legislative_districts_lower_UK=cor.test(x = v5$Cases/v5$Population,
                                 v5$UK_ERI,method = 'spearman')
True_State_Legislative_districts_lower1 = data.frame(correlation=c(True_State_Legislative_districts_lower$estimate,
                                                True_State_Legislative_districts_lower_UK$estimate),
                                   Geostat = c("Ordinary","Universal"),
                                   P.val = c(True_State_Legislative_districts_lower$p.value,
                                             True_State_Legislative_districts_lower_UK$p.value))
names(True_State_Legislative_districts_lower1)[1]="correlation"
True_State_Legislative_districts_lower1$no_units=nrow(SLDL_exNYC)
True_State_Legislative_districts_lower1$`Polygon type` = "State legislative\ndistricts (lower)"
True_State_Legislative_districts_lower1$`P-value` = ifelse(True_State_Legislative_districts_lower1$P.val<0.05,"< 0.05","\u2265 0.05")

#########################################################
# True upper state
State_Legislative_districts_upper = State_Legislative_districts_upper %>%
  st_transform(.,crs=st_crs(sample_cases))
SLDU_exNYC = st_difference(State_Legislative_districts_upper,st_union(NYC_Counties))

v2 <- exact_extract(NYS_pop,
                    SLDU_exNYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                    SLDU_exNYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      SLDU_exNYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(SLDU_exNYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_State_Legislative_districts_upper=cor.test(x = v5$Cases/v5$Population,
                                                 v5$OK_ERI,method = 'spearman')
True_State_Legislative_districts_upper_UK=cor.test(x = v5$Cases/v5$Population,
                                                    v5$UK_ERI,method = 'spearman')
True_State_Legislative_districts_upper1 = data.frame(correlation=c(True_State_Legislative_districts_upper$estimate,
                                                                    True_State_Legislative_districts_upper_UK$estimate),
                                                      Geostat = c("Ordinary","Universal"),
                                                      P.val = c(True_State_Legislative_districts_upper$p.value,
                                                                True_State_Legislative_districts_upper_UK$p.value))
names(True_State_Legislative_districts_upper1)[1]="correlation"
True_State_Legislative_districts_upper1$no_units=nrow(SLDU_exNYC)
True_State_Legislative_districts_upper1$`Polygon type` = "State legislative\ndistricts (upper)"
True_State_Legislative_districts_upper1$`P-value` = ifelse(True_State_Legislative_districts_upper1$P.val<0.05,"< 0.05","\u2265 0.05")



################################################
# Congressional districts
NY_Cong_dists = NY_Cong_dists %>%
  st_transform(.,crs=st_crs(sample_cases))
NY_Cong_dists_exNYC = st_difference(NY_Cong_dists,st_union(NYC_Counties))
v2 <- exact_extract(NYS_pop,
                    NY_Cong_dists_exNYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                    NY_Cong_dists_exNYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      NY_Cong_dists_exNYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(NY_Cong_dists_exNYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_NY_Cong_dists=cor.test(x = v5$Cases/v5$Population,
                                                v5$OK_ERI,method = 'spearman')
True_NY_Cong_dists_UK=cor.test(x = v5$Cases/v5$Population,
                                                   v5$UK_ERI,method = 'spearman')
True_NY_Cong_dists1 = data.frame(correlation=c(True_NY_Cong_dists$estimate,
                                               True_NY_Cong_dists_UK$estimate),
                                                     Geostat = c("Ordinary","Universal"),
                                                     P.val = c(True_NY_Cong_dists$p.value,
                                                               True_NY_Cong_dists_UK$p.value))
names(True_NY_Cong_dists1)[1]="correlation"
True_NY_Cong_dists1$no_units=nrow(NY_Cong_dists_exNYC)
True_NY_Cong_dists1$`Polygon type` = "Congressional\ndistricts"
True_NY_Cong_dists1$`P-value` = ifelse(True_NY_Cong_dists1$P.val<0.05,"< 0.05","\u2265 0.05")

##################################
NYS_Census_tracts = NYS_Census_tracts %>%
  st_transform(.,crs=st_crs(sample_cases))
NYS_Census_tracts_exNYC = st_difference(NYS_Census_tracts,st_union(NYC_Counties))
v2 <- exact_extract(NYS_pop,
                    NYS_Census_tracts_exNYC,
                    fun='sum',
                    progress=FALSE)
v3 <- exact_extract(OK_ERI,
                    NYS_Census_tracts_exNYC,
                    fun='sum',
                    progress=FALSE)
v3.1 <- exact_extract(UK_ERI,
                      NYS_Census_tracts_exNYC,
                      fun='sum',
                      progress=FALSE)
v4 =  cbind(v2,v3)
v4.1 = cbind(v4,v3.1)
v5 = data.frame(v4.1,lengths(st_intersects(NYS_Census_tracts_exNYC, sample_cases)))
names(v5)=c("Population","OK_ERI","UK_ERI","Cases")
v5$inc_per_hund = (v5$Cases/v5$Population)*100000
True_NYS_Census_tracts=cor.test(x = v5$Cases/v5$Population,
                            v5$OK_ERI,method = 'spearman')
True_NYS_Census_tracts_UK=cor.test(x = v5$Cases/v5$Population,
                               v5$UK_ERI,method = 'spearman')
True_NYS_Census_tracts1 = data.frame(correlation=c(True_NYS_Census_tracts$estimate,
                                                   True_NYS_Census_tracts_UK$estimate),
                                 Geostat = c("Ordinary","Universal"),
                                 P.val = c(True_NYS_Census_tracts$p.value,
                                           True_NYS_Census_tracts_UK$p.value))
names(True_NYS_Census_tracts1)[1]="correlation"
True_NYS_Census_tracts1$no_units=nrow(NYS_Census_tracts_exNYC)
True_NYS_Census_tracts1$`Polygon type` = "Census tracts"
True_NYS_Census_tracts1$`P-value` = ifelse(True_NYS_Census_tracts1$P.val<0.05,"< 0.05","\u2265 0.05")

# Bind into one df

Trues = bind_rows(True_ZIP1,True_County1) %>%
  bind_rows(.,True_County_subunits1) %>%
  bind_rows(.,True_State_Legislative_districts_upper1) %>%
  bind_rows(.,True_State_Legislative_districts_lower1) %>%
  bind_rows(.,True_NY_Cong_dists1) %>%
  bind_rows(.,True_NYS_Census_tracts1) 
#write.csv(Trues,file=paste(getwd(),"/Results/True_polygons.csv",sep=""))
