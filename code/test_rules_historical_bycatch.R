### testing the four rules on historical bycatch events

#Steps
#### Step 1. Extract spatial means from all new rs layers for each indicator
#### Step 2. Hindcast rules to see if closures would have been enforced
#### Step 3. Check ENSO based closures

###### A. Load libraries ####
library(tidyverse)
library(RCurl)
library(raster)
library(ncdf4)
library(chron)
library(sp)
library(rgdal)
library(raster)
library(ncdf4)
library(R.utils)
library(rworldmap)

####### Step 1. Extract spatial means from all new rs layers for each indicator ####
ras_list=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new",full.names = T)%>% grep(".grd",.,value=T) %>% stack()

# define indicators (taken from test_rules.R)
##### JAS - best indicator in 07, 08, 10, 11 ####
bJAS1=matrix(c(-135,23,  ## define SST box
               -135,25,
               -123,25,
               -123,23,
               -135,23),
             ncol=2,byrow = T)

p=Polygon(bJAS1)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(sps,add=T)
jas=sps

##### WB1 - best indicator in 01, 03, 04, 05 ####
wb1_coords=matrix(c(-120,23,  ## define SST box
                    -120,27,
                    -118.5,27,
                    -118.5,23,
                    -120,23),
                  ncol=2,byrow = T)

p=Polygon(wb1_coords)
ps=Polygons(list(p),1)
wb1 = SpatialPolygons(list(ps))
proj4string(wb1)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
wb1_df=as.data.frame(fortify(wb1,region="id"))

###### OB1 - best indicator in 02, 06, 09 ####
ob1_coords=matrix(c(-124.5,23,  ## define SST box
                    -124.5,24.25,
                    -122.5,24.25,
                    -122.5,23,
                    -124.5,23),
                  ncol=2,byrow = T)

p=Polygon(ob1_coords)
ps=Polygons(list(p),1)
ob1 = SpatialPolygons(list(ps))
proj4string(ob1)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
ob1_df=as.data.frame(fortify(ob1,region="id"))


###### prepare master data frame and extract ######
df=data.frame(matrix(NA,ncol=10,nrow=11))
colnames(df)=c("indicator_date","indicator_month","bycatch_event_date","JAS","WB1","OB1","conservative","moderate","lenient","mod_lenient")
df$indicator_date=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new") %>% grep(".grd",.,value=T) %>% gsub("new_mean_","",.)%>% gsub(".grd","",.)
df$indicator_month=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new") %>% grep(".grd",.,value=T) %>% gsub("new_mean_","",.)%>% gsub(".grd","",.) %>% substr(.,6,7)
df$JAS=raster::extract(ras_list,jas,fun=mean,na.rm=T,df=T) %>% gather() %>% .[2:nrow(.),] %>% .[,2]
df$WB1=raster::extract(ras_list,wb1,fun=mean,na.rm=T,df=T) %>% gather() %>% .[2:nrow(.),] %>% .[,2]
df$OB1=raster::extract(ras_list,ob1,fun=mean,na.rm=T,df=T) %>% gather() %>% .[2:nrow(.),] %>% .[,2]
df=df[order(df$indicator_date),]

#grabbing bycatch event data
turts=read_csv("/Volumes/SeaGate/BREP/DGN_turtle_data_from_Steph/Catch_SeaTurtle_1990-2017.csv") %>% filter(SpCd=="CC") 
set=read_csv("/Volumes/SeaGate/BREP/DGN_turtle_data_from_Steph/Set.csv") %>% as.tibble() 
set$Set=as.numeric(set$Set)
set=unite(set,"TripNumber_Set",TripNumber,Set,sep="_")%>% unite("dt",Year,MM,DD,sep="-")
set$dt=as.Date(set$dt)
set=set[,c(1,2,3,8:15,51:54)]
turts_alldata2=left_join(turts,set,by="TripNumber_Set")
bycatch_dates=turts_alldata2$dt %>% sort() ### final dates ##### CRAP, supposed to be the preceeding month!!!
by_dates=bycatch_dates[1,2,3,]
df$bycatch_event_date=bycatch_dates


