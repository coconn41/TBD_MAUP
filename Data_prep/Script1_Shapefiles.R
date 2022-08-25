library(sf)
library(dplyr)
library(readxl)
tdir=tempdir()


# Data URLs ---------------------------------------------------------------
obscure_unit_urls = c('bg','county_within_ua','cousub','elsd','place','puma10','scsd',
                      'sldl','sldu','tract','unsd')
ZCTAurl="https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_zcta510_500k.zip"
stateurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip"
countyurl = "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_500k.zip"
crosswalk = "https://udsmapper.org/wp-content/uploads/2020/09/Zip_to_zcta_crosswalk_2020.xlsx"
cong_disturl="https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_cd116_500k.zip"


# USA Counties ------------------------------------------------------------

if(file.exists(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))==F){
  download.file(countyurl,destfile = file.path(tdir,"Counties.zip"))
  unzip(file.path(tdir,"Counties.zip"),exdir=tdir)}

USA_Counties=read_sf(paste(tdir,"/cb_2018_us_county_500k.shp",sep=""))

NYS_Counties = USA_Counties %>%
  filter(STATEFP=="36") %>%
  st_transform(.,crs=32618)

NYC_County_names = c("Kings","Queens","Bronx","Richmond","New York")
NYC_Counties = NYS_Counties %>%
  filter(NAME%in%NYC_County_names)

NYS_Counties_ex_NYC = NYS_Counties %>%
  filter(!c(NAME%in%NYC_County_names))

NYS_ex_NYC = NYS_Counties_ex_NYC %>%
  st_union() %>%
  st_sfc() %>%
  st_as_sf()
# USA ZCTAs ---------------------------------------------------------------

if(file.exists(paste(tdir,"/Crosswalk.xlsx",sep=""))==F){
  download.file(url = crosswalk,mode='wb', destfile = file.path(tdir, "Crosswalk.xlsx"))}
Crosswalk=readxl::read_excel(paste(tdir,"/Crosswalk.xlsx",sep=""))

if(file.exists(paste(tdir,"/cb_2018_us_zcta510_500k.shp",sep=""))==F){
  download.file(ZCTAurl,destfile = file.path(tdir,"ZCTAs.zip"))
  unzip(file.path(tdir,"ZCTAs.zip"),exdir=tdir)}

USA_ZCTAs=read_sf(paste(tdir,"/cb_2018_us_zcta510_500k.shp",sep=""))

NYS_Converter = Crosswalk %>%
  filter(STATE=="NY")

NYS_ZCTAs = USA_ZCTAs %>%
  filter(ZCTA5CE10 %in% NYS_Converter$ZCTA) %>%
  st_transform(.,crs=32618)

# USA Congressional Districts ---------------------------------------------

if(file.exists(paste(tdir,"/cb_2018_us_cd116_500k.shp",sep=""))==F){
  download.file(cong_disturl, destfile = file.path(tdir, "CDists.zip"))
  unzip(file.path(tdir,"CDists.zip"),exdir=tdir)}

NY_Cong_dists = read_sf(paste(tdir,"/cb_2018_us_cd116_500k.shp",sep="")) %>%
  filter(STATEFP=="36") %>%
  st_transform(.,crs=32618)


# USA States --------------------------------------------------------------

if(file.exists(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))==F){
  download.file(stateurl, destfile = file.path(tdir, "States.zip"))
  unzip(file.path(tdir,"States.zip"),exdir=tdir)}

NYS = read_sf(paste(tdir,"/cb_2018_us_state_500k.shp",sep="")) %>%
  filter(NAME=="New York") %>%
  st_transform(.,crs=32618)
USA = read_sf(paste(tdir,"/cb_2018_us_state_500k.shp",sep=""))

remove(Crosswalk,NYS_Converter)



# USA obscure spatial units -----------------------------------------------

name_ind = 0
for(i in unique(obscure_unit_urls)){
  loop_url = paste("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_36_",i,"_500k.zip",sep = "")
  name_ind = name_ind+1
  if(file.exists(paste(tdir,"/cb_2018_36_",i,"_bg_500k.shp",sep=""))==F){ 
    download.file(url=loop_url,
                  destfile = file.path(tdir,paste("/file_", i,"_file.zip",sep="")),
                  method='libcurl')
    unzip(file.path(tdir,paste("/file_",i,"_file.zip",sep="")),exdir=tdir)}}
NY_Block_groups = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[1],"_500k.shp",sep=""))
County_within_ua = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[2],"_500k.shp",sep=""))
County_subunits = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[3],"_500k.shp",sep=""))
elsd = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[4],"_500k.shp",sep=""))
place = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[5],"_500k.shp",sep=""))
PU_Microarea10 = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[6],"_500k.shp",sep=""))
scsd = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[7],"_500k.shp",sep=""))
State_Legislative_districts_lower = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[8],"_500k.shp",sep=""))
State_Legislative_districts_upper = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[9],"_500k.shp",sep=""))
NYS_Census_tracts = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[10],"_500k.shp",sep=""))
United_nations_statistical_division = read_sf(paste(tdir,"/cb_2018_36_",obscure_unit_urls[11],"_500k.shp",sep=""))