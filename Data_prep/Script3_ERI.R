# Script 3: Load Tick data
library(readr)

Tickurl = "https://health.data.ny.gov/api/views/vzbp-i2d4/rows.csv?accessType=DOWNLOAD"


if(file.exists(paste(tdir,"/Tick_collections.csv",sep=""))==F){
  download.file(Tickurl,destfile = file.path(tdir,"/Tick_collections.csv"))}

Tick_data2017 = read_csv(paste(tdir,'/Tick_collections.csv',sep="")) %>%
  filter(Year==2017) %>%
  dplyr::select(County,`Tick Population Density`,`A. phagocytophilum (%)`) %>%
  mutate(County = ifelse(.$County=="St Lawrence","St. Lawrence",.$County),
         `Tick Population Density`=`Tick Population Density`/1000,
         `A. phagocytophilum (%)`=`A. phagocytophilum (%)`/100) %>%
  left_join(non_NYC_Counties,.,by=c("NAME"="County")) %>%
  filter(is.na(`Tick Population Density`)==F) %>%
  mutate(ERI = `Tick Population Density`*`A. phagocytophilum (%)`) %>%
  rename(.,"Tick_density" = "Tick Population Density",
         "ANA_prevalence" = "A. phagocytophilum (%)")

Tick_data_kriging = st_centroid(Tick_data2017)
