#### code to make figures for the BREP paper
### Written 01.24.18 by HW

####### load libraries
#######
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
library(maptools)
library(dplyr)
library(reshape)
library(tidyverse)
library(cowplot)
#######


## ------------------------------------>figure 1: Study Area
#### nah fuck this doing it in ArcGIS "~/Desktop/Win7Desktop/BREP/fig1.mxd" ####
# components: large map: EEZ, species data, US; smaller inset: PLCA closure area, boundary of analysis, US

load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData")
species=scb.cc.xpts
coordinates(species)=~lon+lat
plot(species,pch=1,cex=.5)
writeSpatialShape(species[,c(1,3,4)],"~/Desktop/Win7Desktop/ICCB/loggerhead_points")

map.US <- map_data(map="state")
map.world = map_data(map="world")

EEZ=readShapeSpatial("/Volumes/SeaGate/BREP/BREP/spatial_files_for_figures/US_EEZ2.shp")
test_EEZ <- fortify(EEZ)
pla=readShapeSpatial("/Volumes/SeaGate/BREP/BREP/spatial_files_for_figures/loggerhead.shp")
test_pla <- fortify(pla)

#larger map
map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))#+coord_cartesian()
map=map+geom_polygon(data=test_EEZ,aes(long,lat,color="EEZ",fill="EEZ"),size=.7,alpha=.4)
map=map+geom_polygon(data=test_pla,aes(long,lat,color="pla",fill="pla"),size=.7)
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="usa"),color="black")#+coord_cartesian()
map=map+coord_cartesian(xlim=c(-135,-115),ylim=c(25,60),expand=F)
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))
map=map+scale_fill_manual(breaks="",values=c("EEZ"="cornflowerblue","pla"=NA,"world"="darkgray","usa"="darkgray"))+scale_color_manual(breaks="",values=c("EEZ"=NA,"pla"="black"))
map=map+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map

#smaller map
map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))#+coord_cartesian()
map=map+geom_polygon(data=test_EEZ,aes(long,lat,color="EEZ",fill="EEZ"),size=.7,alpha=.4)
map=map+geom_polygon(data=test_pla,aes(long,lat,color="pla",fill="pla"),size=.7)
map=map+geom_point(data=scb.cc.xpts,aes(x=lon,y=lat,fill="points"),alpha=.5,size=.7)
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="usa"),color="black")#+coord_cartesian()
map=map+coord_cartesian(xlim=c(-135,-115),ylim=c(25,40),expand=F)
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))
map=map+scale_fill_manual(breaks="",values=c("EEZ"="cornflowerblue","pla"=NA,"world"="darkgray","points"="grey","usa"="darkgray"))+scale_color_manual(breaks="",values=c("EEZ"=NA,"pla"="black"))
map=map+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map
#####

## ------------------------------------>figure 2: Failed ENSO quantification ####
enso_anom=read.table("/Volumes/SeaGate/BREP/BREP/ENSO/detrend.nino34.ascii.txt",header = T) 
enso_anom[,2]=str_pad(enso_anom[,2],width=2,side="left",pad=0)
enso_anom=mutate(enso_anom,indicator_date=paste(YR,MON,"16",sep="-")) %>% .[,c(1:2,5:6)]
# a. find anomalies for months proceeding historical closures

# b. calc means
closures=data.frame(matrix(NA,ncol=1,nrow=3))
colnames(closures)="indicator_date"
closures$indicator_date=c("2014-07-16","2015-05-16","2016-05-16") ## months directly preceeding each closure period
a=left_join(closures,enso_anom)
mean_anom_1month=mean(a$ANOM) # 0.4533333

closures=data.frame(matrix(NA,ncol=1,nrow=6))
colnames(closures)="indicator_date"
closures$indicator_date=c("2014-05-16","2014-06-16","2015-03-16","2015-04-16","2016-03-16","2016-04-16") ## second and third preceeding each closure period
a=left_join(closures,enso_anom)
mean_anom_23=mean(a$ANOM) # 0.635

closures=data.frame(matrix(NA,ncol=1,nrow=18))
colnames(closures)="indicator_date"
closures$indicator_date=c("2014-02-16","2014-03-16","2014-04-16","2014-05-16","2014-06-16","2014-07-16","2014-12-16","2015-01-16","2015-02-16","2015-03-16","2015-04-16","2015-05-16","2015-12-16","2016-01-16","2016-02-16","2016-03-16","2016-04-16","2016-05-16") ## 6 months preceeding each closure period
a=left_join(closures,enso_anom)
mean_anom_6month=mean(a$ANOM) # 0.6855556

contemp=filter(enso_anom,YR>2001) #%>% spread(Year,SST_Anomaly) %>% select(-Month)
contemp$one_month= 0.4533333
contemp$two_three= 0.635
contemp$six_month= 0.6855556
contemp$one_month_value=NA
contemp$two_three_value=NA
contemp$six_month_value=NA 
contemp$one_month_status=NA
contemp$two_three_status=NA
contemp$six_month_status=NA 

# c. calc closure status
for(i in 7:nrow(contemp)){
  one_month_status=contemp$ANOM[i-1]
  two_three_status=mean(c(contemp$ANOM[i-2],contemp$ANOM[i-3]))
  six_month_status=mean(c(contemp$ANOM[i-1],contemp$ANOM[i-2],contemp$ANOM[i-3],contemp$ANOM[i-4],contemp$ANOM[i-5],contemp$ANOM[i-6]))
  
  contemp$one_month_value[i]=one_month_status
  contemp$two_three_value[i]=two_three_status
  contemp$six_month_value[i]=six_month_status
  
  if(one_month_status>=contemp$one_month[1]){contemp$one_month_status[i]="Closed"}
  if(one_month_status<contemp$one_month[1]){contemp$one_month_status[i]="Open"}
  
  if(two_three_status>=contemp$one_month[1]){contemp$two_three_status[i]="Closed"}
  if(two_three_status<contemp$one_month[1]){contemp$two_three_status[i]="Open"}
  
  if(six_month_status>=contemp$one_month[1]){contemp$six_month_status[i]="Closed"}
  if(six_month_status<contemp$one_month[1]){contemp$six_month_status[i]="Open"}
}
contemp=contemp %>% filter(YR>2002) %>% dplyr::mutate(indicator_date=as.Date(indicator_date))
closures=contemp %>% filter(indicator_date=="2014-08-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16")

 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=ANOM,color="Anomalies"))+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="1 month indicator"))+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="2-3 months indicator"))+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="6 month indicator"))
 plot=plot+geom_point(data=closures,aes(x=indicator_date,y=ANOM,color="Closures"),size=1)
 plot=plot+ggtitle("A.")+labs(x="Date")+labs(y="El Niño 34 anomaly time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("Anomalies"="black","1 month indicator"="red","2-3 months indicator"="blue","6 month indicator"="green","Closures"="black"),guide=guide_legend(override.aes = list(linetype=c(rep("solid",4),"blank"),shape=c(rep(NA,4),16))))+theme(legend.key.size = unit(.5,'lines'))#guides(fill=guide_legend(keyheight = .01,default.unit = "inch"))
 plot=plot+theme(legend.position=c(.3,1),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot1=plot
 plot1
  
 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=one_month_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="indicator"))
 plot=plot+geom_point(data=closures,aes(x=indicator_date,y=one_month_value,color="Closures"),size=1)
 plot=plot+ggtitle("B.")+labs(x="Date")+labs(y="One month prior to closures time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("timeseries"="black","indicator"="red","Closures"="black"))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot2=plot
 plot2
 
 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=two_three_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="indicator"))
 plot=plot+geom_point(data=closures,aes(x=indicator_date,y=two_three_value,color="Closures"),size=1)
 plot=plot+ggtitle("C.")+labs(x="Date")+labs(y="Two - three months prior to closures time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("timeseries"="black","indicator"="blue","Closures"="black"))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot3=plot
 plot3

 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=six_month_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="indicator"))
 plot=plot+geom_point(data=closures,aes(x=indicator_date,y=six_month_value,color="Closures"),size=1)
 plot=plot+ggtitle("D.")+labs(x="Date")+labs(y="Six months prior to closures time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("timeseries"="black","indicator"="green","Closures"="black"))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot4=plot
 plot4
 
 png("/Volumes/SeaGate/BREP/manuscript/figures.01.19.2018/fig2.png",width=7, height=5, units="in", res=400)
 par(ps=10)
 par(mar=c(4,4,1,1))
 par(cex=1)
 plot_grid(plot1,plot2,plot3,plot4,ncol = 2,nrow = 2)
 dev.off()


time=filter(enso_anom,date>=as.Date("2010-05-16")&date<as.Date("2017-01-16")) %>% separate(indicator_date,c("year","month","day"),sep="-") %>% filter(month=="05" | month=="06" | month=="07") %>% group_by(year)
time$threshold=0.4414286
time$upper=0.4414286+0.5833361
time$lower=0.4414286-0.5833361 #-0.1419075
#b=ggplot()+geom_line(data=time,aes(x=date,y=ANOM))+geom_line(data=time,aes(x=date,y=threshold),color="blue")+geom_ribbon(data=time,aes(x=date,ymin=lower, ymax=upper),fill="blue",alpha=0.2)+
# geom_text(data=time,aes(x=date,y=ANOM,label=date))
a$indicator_date=as.Date(a$indicator_date)

b=ggplot()+geom_line(data=time,aes(x=date,y=ANOM))+geom_line(data=time,aes(x=date,y=threshold),color="blue")+geom_line(data=time,aes(x=date,y=lower),color="red")+
  geom_text(data=time,aes(x=date,y=ANOM,label=date)) + geom_text(data=a,aes(x=indicator_date,y=ANOM,label=indicator_date),color="green")

#####


## ------------------------------------> figure 3: Map of SST indicator boxes
##### load rasters ####
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

sum_rasJAS=sum(jul,aug,sep)
## Goals: 1. warning box based on January-May conditions, 2. observation box based on May-July conditions
sum_rasJFMAM=sum(jan,feb,mar,apr,may)
sum_rasMJJ=sum(may,jun,jul)
####### 

##### warning box (WB) ####
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

###### 2. observation box (OB)####
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

##### D. JAS box 1#####
JAS_coords=matrix(c(-135,23,  ## define SST box
               -135,25,
               -123,25,
               -123,23,
               -135,23),
             ncol=2,byrow = T)

p=Polygon(JAS_coords)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
JAS=as.data.frame(fortify(sps,region="id"))

plot(sum_rasJAS)
plot(sps,add=T)

##### making figures ####
## ------------------------------------> WB
test_spdf <- as(sum_rasJFMAM, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
test_df$layer=as.factor(test_df$layer)
colnames(test_df) <- c("value", "x", "y")

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_raster(data=test_df,aes(x=x,y=y,group=factor(value),fill=factor(value)))+coord_cartesian()
map=map+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="grey"),color="black")+coord_cartesian()
map=map+geom_polygon(data=wb1_df,aes(long,lat,color="wb1",fill="wb1"),size=1.2)+geom_text(data=wb1_df,aes(long[1],lat[1],label="WB"),nudge_x = .8,nudge_y = .4,size=2.5)+coord_cartesian()
map=map+scale_fill_manual(breaks=c("0","1","2","3","4","5"),values=c("wb2"=NA,"wb1"=NA,"world"="grey","grey"="grey","0"="gray87","1"="antiquewhite2","2"="darkgoldenrod","3"="darkolivegreen3","4"="darkorange3","5"="cornflowerblue"))+scale_color_manual(breaks="",values=c("wb2"="dodgerblue4","wb1"="black"))
map2=map+coord_cartesian(xlim=c(-140,-115),ylim=c(17,43),expand=F)
map2=map2+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+ggtitle("Early warning boxes based on Jan-May SST conditions")+theme(plot.title = element_text(size = 10, face = "bold"))
map2=map2+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map2

png("/Volumes/SeaGate/BREP/manuscript/figures/WB_JFMAM.png",width=7, height=7, units="in", res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
map2
dev.off()

## ------------------------------------> OB
test_spdf <- as(sum_rasMJJ, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
test_df$layer=as.factor(test_df$layer)
colnames(test_df) <- c("value", "x", "y")

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_raster(data=test_df,aes(x=x,y=y,group=factor(value),fill=factor(value)))+coord_cartesian()
map=map+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="grey"),color="black")+coord_cartesian()
map=map+geom_polygon(data=ob1_df,aes(long,lat,color="ob1",fill="ob1"),size=1.2)+geom_text(data=ob1_df,aes(long[1],lat[1],label="OB"),nudge_x = .8,nudge_y = .4,size=2.5)+coord_cartesian()
map=map+scale_fill_manual(breaks=c("0","1","2"),values=c("ob1"=NA,"world"="grey","grey"="grey","0"="gray87","1"="antiquewhite2","2"="darkgoldenrod","3"="darkolivegreen3"))+scale_color_manual(breaks="",values=c("ob1"="black"))
map2=map+coord_cartesian(xlim=c(-140,-115),ylim=c(17,43),expand=F)+theme(legend.position="none",legend.key = element_blank())
map2=map2+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+ggtitle("Observation box based on May-July SST conditions")+theme(plot.title = element_text(size = 10, face = "bold"))
#map2=map2+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map2

png("/Volumes/SeaGate/BREP/manuscript/figures/OB_MJJ.png",width=7, height=7, units="in", res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
map2
dev.off()

## ------------------------------------> JAS
test_spdf <- as(sum_rasJAS, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
test_df$layer=as.factor(test_df$layer)
colnames(test_df) <- c("value", "x", "y")

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_raster(data=test_df,aes(x=x,y=y,group=factor(value),fill=factor(value)))+coord_cartesian()
map=map+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat,fill="world"))+coord_cartesian()
map=map+geom_map(data=map.US,map=map.US,aes(map_id=region,x=long,y=lat,fill="grey"),color="black")+coord_cartesian()
map=map+geom_polygon(data=JAS,aes(long,lat,color="ob1",fill="ob1"),size=1.2)+geom_text(data=ob1_df,aes(long[1],lat[1],label="JAS"),nudge_x = .8,nudge_y = .4,size=2.5)+coord_cartesian()
map=map+scale_fill_manual(breaks=c("0","1","2","3"),values=c("ob1"=NA,"world"="grey","grey"="grey","0"="gray87","1"="antiquewhite2","2"="darkgoldenrod","3"="darkolivegreen3","4"="darkorange3"))+scale_color_manual(breaks="",values=c("ob1"="black"))
map2=map+coord_cartesian(xlim=c(-140,-115),ylim=c(17,43),expand=F)+theme(legend.position="none",legend.key = element_blank())
map2=map2+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+ggtitle("Observation box based on July-August SST conditions")+theme(plot.title = element_text(size = 10, face = "bold"))
#map2=map2+guides(fill=guide_legend(title="Persistence (months)"))+theme(legend.title = element_text(size=8),legend.position=c(.9,.9),legend.justification = c(.9,.9))+theme(legend.text=element_text(size=8))
map2

png("/Volumes/SeaGate/BREP/manuscript/figures/JAS.png",width=7, height=7, units="in", res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
map2
dev.off()


## ------------------------------------> Table 1
#rough csvs come from create_rule.R

## ------------------------------------> Table 2
#rough csvs come from test_rules_hindcast.01.16.2018.R
#write_csv(mod_lenient_wENSO,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/mod_lenient_wENSO.csv")

## ------------------------------------> Table 3
#rough csvs come from test_rules_historical_bycatch.01.16.2018.R
#write.csv(df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/historical_bycatch.csv")