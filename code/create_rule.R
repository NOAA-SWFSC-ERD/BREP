### code to create a sample rule
# follows priority_areas.R

## sample rule 1. working off .75 classification

####### load libraries
library(raster)
library(sp)
library(tidyverse)
library(rworldmap)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(ggmap)
library(grid)
library(lattice)
library(gridExtra)

####### load global objects
#####
plot_dir="/Volumes/SeaGate/BREP/BREP/priority_plots/"

jan=raster(paste0(plot_dir,"mean-01-75.grd"))
feb=raster(paste0(plot_dir,"mean-02-75.grd"))
mar=raster(paste0(plot_dir,"mean-03-75.grd"))
apr=raster(paste0(plot_dir,"mean-04-75.grd"))
may=raster(paste0(plot_dir,"mean-05-75.grd"))
jun=raster(paste0(plot_dir,"mean-06-75.grd"))
jul=raster(paste0(plot_dir,"mean-07-75.grd"))
aug=raster(paste0(plot_dir,"mean-08-75.grd"))
sep=raster(paste0(plot_dir,"mean-09-75.grd"))
oct=raster(paste0(plot_dir,"mean-10-75.grd"))
nov=raster(paste0(plot_dir,"mean-11-75.grd"))
dec=raster(paste0(plot_dir,"mean-12-75.grd"))
#####

#### defining SST boxes
## A. Looking at persistence
sum_rasJFMA=sum(jan,feb,mar,apr)
sum_rasJAS=sum(jul,aug,sep)

## B. JFMA box 1
######
bJFMA1=matrix(c(-120,23,  ## define SST box
               -120,30,
               -118,30,
               -118,23,
               -120,23),
             ncol=2,byrow = T)

p=Polygon(bJFMA1)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(sum_rasJFMA)
plot(sps,add=T)

#####
## c. JFMA box 2
######
bJFMA2=matrix(c(-134,35,  ## define SST box
               -134,38,
               -132,38,
               -132,35,
               -134,35),
             ncol=2,byrow = T)

p=Polygon(bJFMA2)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(sum_rasJFMA)
plot(sps,add=T)
#####

# D. JAS box 1
#####
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

plot(sum_rasJAS)
plot(sps,add=T)
#####

# E. New work September 12, 2017 ####
## Goals: 1. warning box based on January-May conditions, 2. observation box based on May-July conditions
sum_rasJFMAM=sum(jan,feb,mar,apr,may)
sum_rasMJJ=sum(may,jun,jul)

#1. warning box
##### wb1 ####
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
#####

##### wb2 ####
wb2_coords=matrix(c(-119.5,24,  ## define SST box
                    -119.5,26.5,
                    -118.25,26.5,
                    -118.25,24,
                    -119.5,24),
                  ncol=2,byrow = T)

p=Polygon(wb2_coords)
ps=Polygons(list(p),1)
wb2 = SpatialPolygons(list(ps))
proj4string(wb2)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
wb2_df=as.data.frame(fortify(wb2,region="id"))

plot(sum_rasJFMAM)
plot(wb1,add=T)
plot(wb2,add=T,border="blue")
#####

##### making nice figures ####
test_spdf <- as(sum_rasJFMAM, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
test_df$layer=as.factor(test_df$layer)
colnames(test_df) <- c("value", "x", "y")

map.US <- map_data(map="state")
map.world = map_data(map="world")

pdf("/Volumes/SeaGate/BREP/BREP/SST_boxes/WB_JFMAM.pdf",7,7)

map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="grey"),color="black")+coord_cartesian()
map=map+geom_raster(data=test_df,aes(x=x,y=y,group=factor(value),fill=factor(value)))+coord_cartesian()
map=map+geom_polygon(data=wb1_df,aes(long,lat,color="wb1",fill="wb1"),size=1.2)+geom_text(data=wb1_df,aes(long[1],lat[1],label="WB1"),nudge_x = .8,nudge_y = .4,size=2.5)+coord_cartesian()
map=map+geom_polygon(data=wb2_df,aes(long,lat,color="wb2",fill="wb2"),size=1.2)+geom_text(data=wb2_df,aes(long[1],lat[1],label="WB2"),nudge_x = 1.82,nudge_y = 1,size=2.5)+coord_cartesian()
map=map+scale_fill_manual(breaks=c("0","1","2","3","4","5"),values=c("wb2"=NA,"wb1"=NA,"world"="black","grey"="grey","0"="gray87","1"="antiquewhite2","2"="darkgoldenrod","3"="darkolivegreen3","4"="darkorange3","5"="cornflowerblue"))+scale_color_manual(breaks="",values=c("wb2"="dodgerblue4","wb1"="black"))
map2=map+xlim(-140,-107)
map2=map2+ylim(17,43)+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+ggtitle("Early warning boxes based on Jan-May SST conditions")+theme(plot.title = element_text(size = 10, face = "bold"))
map2=map2+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map2
dev.off()
#####

###### 2. observation box ####
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

plot(sum_rasMJJ)
plot(ob1,add=T)

##### making nice figures ####
test_spdf <- as(sum_rasMJJ, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
test_df$layer=as.factor(test_df$layer)
colnames(test_df) <- c("value", "x", "y")

map.US <- map_data(map="state")
map.world = map_data(map="world")

pdf("/Volumes/SeaGate/BREP/BREP/SST_boxes/OB_MJJ.pdf",7,7)

map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="grey"),color="black")+coord_cartesian()
map=map+geom_raster(data=test_df,aes(x=x,y=y,group=factor(value),fill=factor(value)))+coord_cartesian()
map=map+geom_polygon(data=ob1_df,aes(long,lat,color="ob1",fill="ob1"),size=1.2)+geom_text(data=ob1_df,aes(long[1],lat[1],label="OB1"),nudge_x = .8,nudge_y = .4,size=2.5)+coord_cartesian()
map=map+scale_fill_manual(breaks=c("0","1","2"),values=c("ob1"=NA,"world"="black","grey"="grey","0"="gray87","1"="antiquewhite2","2"="darkgoldenrod","3"="darkolivegreen3"))+scale_color_manual(breaks="",values=c("ob1"="black"))
map2=map+xlim(-140,-107)
map2=map2+ylim(17,43)+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+ggtitle("Observation box based on May-July SST conditions")+theme(plot.title = element_text(size = 10, face = "bold"))
map2=map2+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map2
dev.off()
#####

#####

# month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))
#  
# calculate_cor=function(month_list,latMin,latMax,lonMin,lonMax,boxID){  
#   DF=data.frame(month=character(),
#                 cor_blanks=double(),
#                 cor_zeros=double(),
#                 stringsAsFactors = F)
# for(i in 1:12){
# month=month_list[i]
# print(month)
# a=readRDS(paste0(getwd(),"/monthly/sst",month,"mean_sightings.rds"))
# 
# if(grepl("2017-01-16",rownames(a)[length(rownames(a))])){
#   a=a[1:(nrow(a)-1),]
# }
# 
# a3=as.data.frame(t(a))
# a4=a3[a3$lat>latMin & a3$lat<latMax,]
# a5=a4[a4$lon> lonMin & a4$lon< lonMax,]
# 
# month2=gsub("-","",month)
# 
# a6=as.data.frame(t(a5))
# a6$spatial_average=rowMeans(a6,na.rm = T)
# a7=as.data.frame(a6[,ncol(a6)])
# rownames(a7)=rownames(a6)
# colnames(a7)=paste0(month2,"_mean")
# a7$sightings_blank=a[,ncol(a)-1]
# a7$sightings_zero=a[,ncol(a)]
# a7=a7[3:nrow(a7),]
# 
# nas=a7[!is.na(a7$sightings_blank),]
# cor(nas[,1],nas$sightings_blank)
# 
# zeros=a7[!is.na(a7$sightings_zero),]
# cor(zeros[,1],zeros$sightings_zero)
# 
# DF[i,1]=month2
# DF[i,2]=cor(nas[,1],nas$sightings_blank)
# DF[i,3]=cor(zeros[,1],zeros$sightings_zero)
# }
#   colnames(DF)[2]=paste0("cor_blanks",boxID)
#   colnames(DF)[3]=paste0("cor_zeros",boxID)
#   return(DF)
# }
# JFMA1=calculate_cor(month_list = month_list,latMin = 23, latMax = 30,lonMin = -120,lonMax = -118,boxID = "JFMA1")
# JFMA2=calculate_cor(month_list = month_list,latMin = 35, latMax = 38,lonMin = -134,lonMax = -132,boxID = "JFMA2")
# JAS1=calculate_cor(month_list = month_list,latMin = 23, latMax = 25,lonMin = -135,lonMax = -123,boxID = "JAS1")
# 
# df_full=cbind(JFMA1,JFMA2,JAS1)
# write.csv(df_full,"/Volumes/SeaGate/BREP/BREP/schematics/3box_corr.csv")
# 
