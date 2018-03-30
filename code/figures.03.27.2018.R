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
library(stringr)
library(DataCombine)
library(lubridate)
library(sf)
#######


## ------------------------------------>figure 1: Study Area ####
# writing shapefile boxes for arcgis map

# local SST box
df=data.frame(layer_name="Local_SST",layer_ID="1")
writeDir="~/Desktop/Win7Desktop/BREP/boxes"
scb_coords=matrix(c(-120.3, 30.8,  ## define SST box
                    -120.3,34.5,
                    -116,34.5,
                    -116, 30.8,
                    -120.3, 30.8),
                  ncol=2,byrow = T)

p=Polygon(scb_coords)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps)) 
test = SpatialPolygonsDataFrame(sps,df)
proj4string(test)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
writeSpatialShape(test,"~/Desktop/Win7Desktop/BREP/boxes/local_sst")

##### warning box (WB) 
wb1_coords=matrix(c(-120,23,  ## define SST box
                    -120,27,
                    -118.5,27,
                    -118.5,23,
                    -120,23),
                  ncol=2,byrow = T)

p=Polygon(wb1_coords)
ps=Polygons(list(p),1)
wb1 = SpatialPolygons(list(ps))
test = SpatialPolygonsDataFrame(wb1,df)
proj4string(test)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
writeSpatialShape(test,"~/Desktop/Win7Desktop/BREP/boxes/pelagic_box1")

###### 2. observation box (OB)
ob1_coords=matrix(c(-124.5,23,  ## define SST box
                    -124.5,24.25,
                    -122.5,24.25,
                    -122.5,23,
                    -124.5,23),
                  ncol=2,byrow = T)

p=Polygon(ob1_coords)
ps=Polygons(list(p),1)
ob1 = SpatialPolygons(list(ps))
test = SpatialPolygonsDataFrame(ob1,df)
proj4string(test)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
writeSpatialShape(test,"~/Desktop/Win7Desktop/BREP/boxes/pelagic_box2")

##### D. JAS box 1
JAS_coords=matrix(c(-135,23,  ## define SST box
                    -135,25,
                    -123,25,
                    -123,23,
                    -135,23),
                  ncol=2,byrow = T)

p=Polygon(JAS_coords)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
test = SpatialPolygonsDataFrame(sps,df)
proj4string(test)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
writeSpatialShape(test,"~/Desktop/Win7Desktop/BREP/boxes/pelagic_box3")


#### nah fuck this doing it in ArcGIS "~/Desktop/Win7Desktop/BREP/fig1.mxd"
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

## ------------------------------------>figure 2: Failed ENSO quantification (current decision making process) ####
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

#contemp=filter(enso_anom,YR>2001) %>% spread(Year,SST_Anomaly) %>% select(-Month)
contemp=enso_anom 
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
#closures=contemp %>% filter(indicator_date=="2014-08-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16")
closures=contemp %>% filter(indicator_date=="2014-08-16"|indicator_date=="2014-09-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2015-09-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16"|indicator_date=="2016-09-16") %>% group_by(YR)

plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=ANOM,color="El Niño anomalies"),size=.5)
plot=plot+geom_line(data=closures,aes(x=indicator_date,y=ANOM,group=YR,color="Closures periods"),size=1)
plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="1 month threshold"),size=.5)
plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="2-3 months threshold"),size=.5)
plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="6 month threshold"),size=.5)
plot=plot+ggtitle("A.")+labs(x="Date")+labs(y="El Niño 3.4 anomalies")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
plot=plot+scale_color_manual("",values=c("El Niño anomalies"="black","1 month threshold"="red","2-3 months threshold"="blue","6 month threshold"="green","Closures periods"="azure4"),guide=guide_legend(override.aes = list(linetype=c(rep("solid",5))))) +  guides(colour = guide_legend(override.aes = list(size=c(.5,.5,.5,1,.5)))) +theme(legend.key.size = unit(.5,'lines'))
plot=plot+theme(legend.position=c(.3,1.1),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
plot1=plot
plot1
  
plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=one_month_value,color="timeseries"))
plot=plot+geom_line(data=closures,aes(x=indicator_date,y=one_month_value,group=YR,color="Closures periods"),size=1)
plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="threshold"))
plot=plot+ggtitle("B.")+labs(x="Date")+labs(y="Average of one month prior to closures indicator")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
plot=plot+scale_color_manual("",values=c("timeseries"="black","threshold"="red","Closures periods"="azure4"))
plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
plot2=plot
plot2

plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=two_three_value,color="timeseries"))
plot=plot+geom_line(data=closures,aes(x=indicator_date,y=two_three_value,group=YR,color="Closures periods"),size=1)
plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="threshold"))
plot=plot+ggtitle("C.")+labs(x="Date")+labs(y="Average of two and three months prior to closures indicator")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
plot=plot+scale_color_manual("",values=c("timeseries"="black","threshold"="blue","Closures periods"="azure4"))
plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
plot3=plot
plot3

plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=six_month_value,color="timeseries"))
plot=plot+geom_line(data=closures,aes(x=indicator_date,y=six_month_value,group=YR,color="Closures periods"),size=1)
plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="threshold"))
plot=plot+ggtitle("D.")+labs(x="Date")+labs(y="Average of six months prior to closures indicator")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
plot=plot+scale_color_manual("",values=c("timeseries"="black","threshold"="green","Closures periods"="azure4"))
plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
plot4=plot
plot4

 png("/Volumes/SeaGate/BREP/manuscript/figures.01.19.2018/fig2.png",width=7, height=5, units="in", res=400)
 par(ps=10)
 par(mar=c(4,4,1,1))
 par(cex=1)
 plot_grid(plot1,plot2,plot3,plot4,ncol = 2,nrow = 2)
 dev.off()

 ##### ------------> old original code
 # plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=one_month_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="indicator"))
 # plot=plot+geom_point(data=closures,aes(x=indicator_date,y=one_month_value,color="Closures"),size=1)
 # plot=plot+ggtitle("B.")+labs(x="Date")+labs(y="One month prior to closures time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 # plot=plot+scale_color_manual("",values=c("timeseries"="black","indicator"="red","Closures"="black"))
 # plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 # plot2=plot
 # plot2
 # 
 # plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=two_three_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="indicator"))
 # plot=plot+geom_point(data=closures,aes(x=indicator_date,y=two_three_value,color="Closures"),size=1)
 # plot=plot+ggtitle("C.")+labs(x="Date")+labs(y="Two - three months prior to closures time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 # plot=plot+scale_color_manual("",values=c("timeseries"="black","indicator"="blue","Closures"="black"))
 # plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 # plot3=plot
 # plot3
 # 
 # plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=six_month_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="indicator"))
 # plot=plot+geom_point(data=closures,aes(x=indicator_date,y=six_month_value,color="Closures"),size=1)
 # plot=plot+ggtitle("D.")+labs(x="Date")+labs(y="Six months prior to closures time-series")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 # plot=plot+scale_color_manual("",values=c("timeseries"="black","indicator"="green","Closures"="black"))
 # plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 # plot4=plot
 # plot4
 
 

## ------------------------------------>figure 3: Local SST anomalies (current decision making process) ####
 anoms=read.csv("/Volumes/SeaGate/BREP/BREP/roms_anomalies/BREP_historical_SST_anomaly.txt")
 
 # a. find anomalies for months proceeding historical closures
 #contemp=filter(anoms,Year>2001) %>% mutate(data="ROMS")
 contemp=anoms
 contemp$Month=str_pad(contemp$Month,2,pad="0")
 mean_anom_1month=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-07"|date=="2015-05"|date=="2016-05") %>% summarise(mean=min(SST_Anomaly)) %>% .[1,1] ## 1 month preceding closures
 mean_anom_23=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-05"|date=="2014-06"|date=="2015-03"|date=="2015-04"|date=="2016-03"|date=="2016-04") %>% summarise(mean=min(SST_Anomaly))%>% .[1,1] ## months preceeding closures, 2nd and 3rd month as in registrar
 mean_anom_6month=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-02"|date=="2014-03"|date=="2014-04"|date=="2014-05"|date=="2014-06"|date=="2014-07"|date=="2014-12"|date=="2015-01"|date=="2015-02"|date=="2015-03"|date=="2015-04"|date=="2015-05"|date=="2015-12"|date=="2016-01"|date=="2016-02"|date=="2016-03"|date=="2016-04"|date=="2016-05")%>% summarise(mean=min(SST_Anomaly))%>% .[1,1] ## 6 months preceding closures
 
 contemp=contemp %>% dplyr::select(YR=Year,MON=Month,ANOM=SST_Anomaly)  #
 contemp=contemp %>% dplyr::mutate(indicator_date=paste0(YR,"-",MON,"-16"))
 
 contemp$one_month= mean_anom_1month
 contemp$two_three= mean_anom_23
 contemp$six_month= mean_anom_6month
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
   
   if(two_three_status>=contemp$two_three[1]){contemp$two_three_status[i]="Closed"}
   if(two_three_status<contemp$two_three[1]){contemp$two_three_status[i]="Open"}
   
   if(six_month_status>=contemp$six_month[1]){contemp$six_month_status[i]="Closed"}
   if(six_month_status<contemp$six_month[1]){contemp$six_month_status[i]="Open"}
 }
 contemp=contemp %>% filter(YR>2002) %>% dplyr::mutate(indicator_date=as.Date(indicator_date))
 closures=contemp %>% filter(indicator_date=="2014-08-16"|indicator_date=="2014-09-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2015-09-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16"|indicator_date=="2016-09-16") %>% group_by(YR)
 #closures=contemp %>% filter(indicator_date=="2014-08-16"|indicator_date=="2015-06-16"|indicator_date=="2016-06-16")
 
 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=ANOM,color="SST anomalies"),size=.5)
 plot=plot+geom_line(data=closures,aes(x=indicator_date,y=ANOM,group=YR,color="Closures periods"),size=1)
 plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="1 month threshold"),size=.5)
 plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="2-3 months threshold"),size=.5)
 plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="6 month threshold"),size=.5)
 plot=plot+ggtitle("A.")+labs(x="Date")+labs(y="ROMS SST anomalies")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("SST anomalies"="black","1 month threshold"="red","2-3 months threshold"="blue","6 month threshold"="green","Closures periods"="azure4"),guide=guide_legend(override.aes = list(linetype=c(rep("solid",5))))) +  guides(colour = guide_legend(override.aes = list(size=c(.5,.5,.5,1,.5)))) +theme(legend.key.size = unit(.5,'lines'))
 plot=plot+theme(legend.position=c(.3,1.1),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot1=plot
 plot1
 
 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=one_month_value,color="timeseries"))
 plot=plot+geom_line(data=closures,aes(x=indicator_date,y=one_month_value,group=YR,color="Closures periods"),size=1)
 plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=one_month,color="threshold"))
 plot=plot+ggtitle("B.")+labs(x="Date")+labs(y="Average of one month prior to closures indicator")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("timeseries"="black","threshold"="red","Closures periods"="azure4"))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot2=plot
 plot2
 
 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=two_three_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="threshold"))
 plot=plot+geom_line(data=closures,aes(x=indicator_date,y=two_three_value,group=YR,color="Closures periods"),size=1)
 plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=two_three,color="threshold"))
 plot=plot+ggtitle("C.")+labs(x="Date")+labs(y="Average of two and three months prior to closures indicator")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("timeseries"="black","threshold"="blue","Closures periods"="azure4"))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot3=plot
 plot3
 
 plot=ggplot()+geom_line(data=contemp,aes(x=indicator_date,y=six_month_value,color="timeseries"))+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="threshold"))
 plot=plot+geom_line(data=closures,aes(x=indicator_date,y=six_month_value,group=YR,color="Closures periods"),size=1)
 plot=plot+geom_line(data=contemp,aes(x=indicator_date,y=six_month,color="threshold"))
 plot=plot+ggtitle("D.")+labs(x="Date")+labs(y="Average of six months prior to closures indicator")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("timeseries"="black","threshold"="green","Closures periods"="azure4"))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot4=plot
 plot4
 
 png("/Volumes/SeaGate/BREP/manuscript/figures.01.19.2018/fig3.png",width=7, height=5, units="in", res=400)
 par(ps=10)
 par(mar=c(4,4,1,1))
 par(cex=1)
 plot_grid(plot1,plot2,plot3,plot4,ncol = 2,nrow = 2)
 dev.off()
 

## ------------------------------------>figure 4: Comparison of coastwatch and ROMS sst anomalies (current decision making process) ####
 # working with coast watch data
 scb_coords=matrix(c(-120.3, 30.8,  ## define SST box
                     -120.3,34.5,
                     -116,34.5,
                     -116, 30.8,
                     -120.3, 30.8),
                   ncol=2,byrow = T)
 
 p=Polygon(scb_coords)
 ps=Polygons(list(p),1)
 sps = SpatialPolygons(list(ps))
 proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
 plot(sps)
 scb=sps
 
 ras_list=list.files("/Volumes/SeaGate/BREP/jplmur_raster",pattern = "anom",full.names = T) %>% grep(".grd",.,value=T) %>% stack()
 names=list.files("/Volumes/SeaGate/BREP/jplmur_raster",pattern = "anom") %>% grep(".grd",.,value=T) %>% gsub("anom_","",.) %>% gsub(".grd","",.)
 ex_mean=raster::extract(ras_list,scb,fun=mean,na.rm=T,df=T)
 
 a=t(ex_mean) %>% as.data.frame() %>% .[2:nrow(.),] %>% as.data.frame()%>% mutate(date=as.Date(names)) %>% .[8:nrow(.),] 
 colnames(a)=c("ANOM","date")
 #write.csv(a,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/scb_anom_mean.csv")

 # working with ROMS data
 anoms=read.csv("/Volumes/SeaGate/BREP/BREP/roms_anomalies/BREP_historical_SST_anomaly.txt")
 contemp=filter(anoms,Year>2002) %>% mutate(data="ROMS") %>% dplyr::mutate(indicator_date=paste0(Year,"-",Month,"-16"))
 contemp=contemp %>% mutate(date=as.Date(indicator_date))

 # make some plots
 plot=ggplot()+geom_line(data=contemp,aes(x=date,y=SST_Anomaly,color="ROMS"))
 plot=plot+geom_line(data=a,aes(x=date,y=ANOM,color="CoastWatch"))
 plot=plot+ggtitle("Comparison of ROMS and CoastWatch SST anomaly indicators in the Southern California Bight")+labs(x="Date")+labs(y="SST anomaly")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
 plot=plot+scale_color_manual("",values=c("CoastWatch"="red","ROMS"="blue"))+theme(legend.key.size = unit(.5,'lines'))
 plot=plot+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +theme(legend.position="none")+scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
 plot=plot+theme(legend.position=c(.1,1.1),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) 
 plot
 
 png("/Volumes/SeaGate/BREP/manuscript/figures.01.19.2018/fig4.png",width=7, height=5, units="in", res=400)
 par(ps=10)
 par(mar=c(4,4,1,1))
 par(cex=1)
 plot
 dev.off()
 

## ------------------------------------>figure 5: Map of SST indicator boxes ####
##### load rasters
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

##### warning box (WB) 
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

###### 2. observation box (OB)
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

##### D. JAS box 1
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

##### making figures
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



## ------------------------------------>figure 6: SST box indicator ####
#1. extract best indicator for each month
#1a warning box (WB) ---> best indicator in jan, mar, apr, may
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

#1b observation box (OB) ---> best indicator in feb, jun, sep

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

#1c JAS box 1---> best indicator in jul, aug, oct, nov, dec
JAS_coords=matrix(c(-135,23,  ## define SST box
                    -135,25,
                    -123,25,
                    -123,23,
                    -135,23),
                  ncol=2,byrow = T)

p=Polygon(JAS_coords)
ps=Polygons(list(p),1)
jas = SpatialPolygons(list(ps))
proj4string(jas)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#2. redetermine thresholds
month_list=list.files("/Volumes/SeaGate/BREP/jplmur_raster",pattern = "mean",full.names = T) %>% grep(".grd",.,value=T)
id02=grep("2002",month_list,value = F)
id17=grep("2017",month_list,value = F)
month_list=month_list[-c(id02,id17)]
ras="/Volumes/SeaGate/BREP/jplmur_raster/mean_2002-12-16.grd" ## need this to determine closure status 2003-01-01

jan=month_list %>% grep("-01-",.,value=T) %>% stack()
feb=month_list %>% grep("-02-",.,value=T) %>% stack()
mar=month_list %>% grep("-03-",.,value=T) %>% stack()
apr=month_list %>% grep("-04-",.,value=T) %>% stack()
may=month_list %>% grep("-05-",.,value=T) %>% stack()
jun=month_list %>% grep("-06-",.,value=T) %>% stack()
jul=month_list %>% grep("-07-",.,value=T) %>% stack()
aug=month_list %>% grep("-08-",.,value=T) %>% stack()
sep=month_list %>% grep("-09-",.,value=T) %>% stack()
oct=month_list %>% grep("-10-",.,value=T) %>% stack()
nov=month_list %>% grep("-11-",.,value=T) %>% stack()
dec=month_list %>% grep("-12-",.,value=T) %>% list(ras,.) %>% unlist() %>% stack()

names=list.files("/Volumes/SeaGate/BREP/jplmur_raster",pattern = "anom") %>% grep(".grd",.,value=T)%>% grep("-01-",.,value=T) %>% gsub("anom_","",.) %>% gsub(".grd","",.) %>% .[1:14] %>% list("2002",.) %>% unlist()

empty=data.frame(matrix(nrow=14,ncol=14))
colnames(empty)=c("date","box","jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")

#2a warning box (WB) ---> best indicator in jan, mar, apr, may
jan_mean=raster::extract(jan,wb1,fun=mean,na.rm=T,df=T)
mar_mean=raster::extract(mar,wb1,fun=mean,na.rm=T,df=T)
apr_mean=raster::extract(apr,wb1,fun=mean,na.rm=T,df=T)
may_mean=raster::extract(may,wb1,fun=mean,na.rm=T,df=T)

empty$jan=t(jan_mean)[2:15]
empty$mar=t(mar_mean)[2:15]
empty$apr=t(apr_mean)[2:15]
empty$may=t(may_mean)[2:15]

#2b observation box (OB) ---> best indicator in feb, jun, sep
feb_mean=raster::extract(feb,ob1,fun=mean,na.rm=T,df=T)
jun_mean=raster::extract(jun,ob1,fun=mean,na.rm=T,df=T)
sep_mean=raster::extract(sep,ob1,fun=mean,na.rm=T,df=T)

empty$feb=t(feb_mean)[2:15]
empty$jun=t(jun_mean)[2:15]
empty$sep=t(sep_mean)[2:15]

#2c JAS box 1---> best indicator in jul, aug, oct, nov, dec
jul_mean=raster::extract(jul,jas,fun=mean,na.rm=T,df=T)
aug_mean=raster::extract(aug,jas,fun=mean,na.rm=T,df=T)
oct_mean=raster::extract(oct,jas,fun=mean,na.rm=T,df=T)
nov_mean=raster::extract(nov,jas,fun=mean,na.rm=T,df=T)
dec_mean=raster::extract(dec,jas,fun=mean,na.rm=T,df=T)

empty$jul=t(jul_mean)[2:15]
empty$aug=t(aug_mean)[2:15]
empty$oct=t(oct_mean)[2:15]
empty$nov=t(nov_mean)[2:15]
empty=InsertRow(empty,rep(NA,14),RowNum=1)
empty$dec=t(dec_mean)[2:16]

empty$date=gsub("-01-16","",names)
colnames(empty)=c("date","box","01","02","03","04","05","06","07","08","09","10","11","12")
a=empty %>% gather(Month,anomaly,-date,-box)
a=within(a,box[a$Month=="01"|a$Month=="03"|a$Month=="04"|a$Month=="05"]<-"WB")
a=within(a,box[a$Month=="02"|a$Month=="06"|a$Month=="09"]<-"OB")
a=within(a,box[a$Month=="07"|a$Month=="08"|a$Month=="10"|a$Month=="11"|a$Month=="12"]<-"JAS")
a=a %>% dplyr::rename(Year=date)
a=a %>% mutate(Date=as.Date(paste(Year,Month,"16",sep="-")))
a=a %>% mutate(id=paste(box,Month,sep="-"))

# get only turtle years: 2003,2005,2006,2013,2014,2015,2016, find moderate threshold (i.e. mean)
turtle_a=a %>% filter(Year==2003|Year==2005|Year==2006|Year==2013|Year==2014|Year==2015|Year==2016)
thresholds=turtle_a %>% group_by(box,Month) %>% summarise(mean=mean(anomaly))
thresholds=thresholds %>% mutate(id=paste(box,Month,sep="-"))
write.csv(thresholds,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/sst_box_thresholds.csv")

master=left_join(a,thresholds) %>% dplyr::rename(Threshold=mean) %>% select(-id) %>% dplyr::rename(Temperature=anomaly) %>% arrange(Date) %>% .[complete.cases(.),]
write.csv(master,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/sst_box_indicator.csv") 

#3. negative/positive indicators relative to thresholds, setting values for proceding month
# need 2002-12-16
master$Ruling=NA
for(i in 1:(nrow(master)-1)){
  master$Ruling[i+1]=(master$Temperature[i]-master$Threshold[i])
}

master=master[complete.cases(master),]%>% mutate(Zero=0)

#master=master %>% mutate(Ruling=Temperature-Threshold) %>% mutate(Zero=0)
closures=master %>% filter(Date=="2014-08-16"|Date=="2014-09-16"|Date=="2015-06-16"|Date=="2015-07-16"|Date=="2015-08-16"|Date=="2015-09-16"|Date=="2016-06-16"|Date=="2016-07-16"|Date=="2016-08-16"|Date=="2016-09-16") %>% group_by(Year)

#4. adding in sightings data
turtdata=load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData")
pla=readShapeSpatial("/Volumes/SeaGate/BREP/BREP/spatial_files_for_figures/loggerhead.shp")
sightings=scb.cc.xpts
coordinates(sightings)=~lon+lat
pla_sightings=raster::intersect(sightings,pla)
pla_sightings@data$lat=pla_sightings@coords[,2]
pla_sightings@data$lon=pla_sightings@coords[,1]
pla_sightings=pla_sightings@data
pla_sightings=pla_sightings %>% dplyr::rename(Date=dtime) 
pla_sightings=pla_sightings%>% dplyr::select(Date,ptt)%>%mutate(ptt=1)
pla_sightings$dt=strtrim(as.character(pla_sightings$Date),8)
pla_sightings$dt=as.Date(paste0(pla_sightings$dt,"16"))
pla_sightings$Date=pla_sightings$dt

turtles=left_join(pla_sightings,master) %>% filter(Date>"2002-12-31") %>% group_by(Date) %>% summarise(sum=sum(ptt)) %>% .[2:6,] %>% left_join(.,master)

# make some plots
plot=ggplot()+geom_line(data=master,aes(x=Date,y=Ruling,color="Indicator minus threshold"),size=.5)
plot=plot+geom_line(data=closures,aes(x=Date,y=Ruling,color="Closure periods",group=Year),size=1)
plot=plot+geom_point(data=turtles,aes(x=Date,y=Ruling,color="Turtle sightings"),size=1)
plot=plot+geom_line(data=master,aes(x=Date,y=Zero),color="red")
plot=plot+ggtitle("Pelagic SST box based indicator")+labs(x="Date")+labs(y="Indicator minus threshold (C)")+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
plot=plot+scale_color_manual("",values=c("Indicator minus threshold"="black","Closure periods"="azure4","Turtle sightings"="blue"),guide=guide_legend(override.aes = list(linetype=c(rep("solid",2),"blank"),shape=c(rep(NA,2),16),size=c(1,.5,1))))+theme(legend.key.size = unit(.5,'lines'))
plot=plot+theme(legend.position=c(.2,1),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5))+ theme(legend.key=element_blank()) +scale_y_continuous(expand = c(0, 0))+scale_x_date(date_breaks="year",date_labels = "%Y",date_minor_breaks = "months",expand = c(0,0))
plot

png("/Volumes/SeaGate/BREP/manuscript/figures.01.19.2018/fig5.png",width=7, height=5, units="in", res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot
dev.off()

## ------------------------------------> Table 1, most correlated SST boxes with turtle sightings  ####
#rough csvs come from create_rule.R
#thresholds come from thresholds object in making figure 5

## ------------------------------------> Table 2, hindcast of historical bycatch  ####
turtle_dat=read.csv("/Volumes/SeaGate/BREP/official_turtleData_Dana/observer.csv")
turtle_dat=turtle_dat %>% mutate(Date=ymd(paste(turtle_dat$Year,turtle_dat$MM,turtle_dat$DD))) %>% mutate(ym=paste(Year,str_pad(turtle_dat$MM,width=2,side="left",pad=0),"16",sep="-")) %>% mutate(count=1) #%>% group_by(ym) %>% mutate(count=1) %>% summarise(countsum=sum(count))
turtle_dat=turtle_dat %>% select(Date,ym,count)

#1. read in indicators
#1a. ENSO indicators, lines 70-124 of figure 2
enso=contemp
a=left_join(turtle_dat,enso,by=c("ym"="indicator_date")) %>% select(Date,ym,count,YR,MON,ANOM,one_month_enso=one_month_status,two_three_enso=two_three_status,six_month_enso=six_month_status) 

#1b. local anomaly indicators, lines 202-243 of figure 3
local=contemp
local=local%>% select(indicator_date,one_month_local=one_month_status,two_three_local=two_three_status,six_month_local=six_month_status) 
b=left_join(a,local,by=c("ym"="indicator_date")) %>% arrange(Date)

#1c. pelagic SST box indicator, boxes defined in lines 493- 531 of figure 6

ras_list=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new",full.names = T)%>% grep(".grd",.,value=T) %>% stack()
names=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new")%>% grep(".grd",.,value=T) %>% gsub("new_mean_","",.) %>% gsub(".grd","",.)
sst_ob1=raster::extract(ras_list,ob1,fun=mean,na.rm=T,df=T) %>% t()%>% as.data.frame() %>% dplyr::rename(Temperature=V1) %>% slice(-1) %>% as.data.frame() %>%  dplyr::mutate(Date=names) %>% mutate(box="OB")
sst_wb1=raster::extract(ras_list,wb1,fun=mean,na.rm=T,df=T) %>% t()%>% as.data.frame() %>% dplyr::rename(Temperature=V1) %>% slice(-1) %>% as.data.frame() %>%  dplyr::mutate(Date=names) %>% mutate(box="WB")
sst_jas=raster::extract(ras_list,jas,fun=mean,na.rm=T,df=T) %>% t()%>% as.data.frame() %>% dplyr::rename(Temperature=V1) %>% slice(-1) %>% as.data.frame() %>%  dplyr::mutate(Date=names) %>% mutate(box="JAS")

pelagic=do.call("rbind",list(sst_ob1,sst_wb1,sst_jas))
pelagic=pelagic %>% mutate(Month=substr(Date,6,7))
pelagic_wb=pelagic %>% filter((Month=="01" & box=="WB")|(Month=="03" & box=="WB")|(Month=="04" & box=="WB")|(Month=="05" & box=="WB"))
pelagic_ob=pelagic %>% filter((Month=="02" & box=="OB")|(Month=="06" & box=="OB")|(Month=="09" & box=="OB"))
pelagic_jas=pelagic %>% filter((Month=="07" & box=="JAS")|(Month=="08" & box=="JAS")|(Month=="10" & box=="JAS")|(Month=="11" & box=="JAS")|(Month=="12" & box=="JAS"))

pelagic=do.call("rbind",list(pelagic_jas,pelagic_ob,pelagic_wb)) %>% mutate(id=paste(box,Month,sep="-"))
thresholds=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/sst_box_thresholds.csv") %>% select(mean,id)
pelagic=left_join(pelagic,thresholds) %>% arrange(Date) %>% mutate(Ruling=Temperature-mean)
pelagic$status=NA

for(i in 1:nrow(pelagic)){
  if(pelagic$Ruling[i]<0){pelagic$status[i]="Open"}
  if(pelagic$Ruling[i]>0){pelagic$status[i]="Closed"}
}

bycatch_dates=unique(b$ym)
pelagic=pelagic[2:nrow(pelagic),]
pelagic$bycatch=bycatch_dates
pelagic=pelagic %>% select(status,bycatch)

master=left_join(b,pelagic,by=c("ym"="bycatch")) 
colnames(master)[13]="pelagic_box"
write.csv(master,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/bycatch_hindcast.csv")

## ------------------------------------> Table 3, hindcast comparison of indicator utility ####
master=data.frame("Indicator"=NA, "Alignment"=NA,"Opertunity Cost"=NA, "% sightings aoided"=NA,"% bycatch avoided"=NA)

# ---> adding in new rule, enso and local 6 month average combination
loc=local %>% select(indicator_date,six_month_status) %>% dplyr::rename(six_month_status_local=six_month_status)
ens=enso %>% select(indicator_date,six_month_status) %>% dplyr::rename(six_month_status_enso=six_month_status)
combo=left_join(loc,ens,by="indicator_date")
combo$combn=NA
for(i in 7:nrow(combo)){
  if(combo$six_month_status_local[i]=="Closed" && combo$six_month_status_enso[i]=="Closed"){
    combo$combn[i]="Closed"
  } else{
    combo$combn[i]="Open"
  }
}

#0. alignment with current closures
#0a. ENSO indicators, lines 70-124 of figure 2
#enso=contemp
closures=enso %>% filter(indicator_date=="2014-08-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16") 
master[1:3,1]=c("one_month_enso","two_three_enso","six_month_enso")
master[1,2]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[2,2]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[3,2]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0b. local anomaly indicators, lines 202-243 of figure 3
#local=contemp
closures=local %>% filter(indicator_date=="2014-08-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16") 
master[4:6,1]=c("one_month_local","two_three_local","six_month_local")
master[4,2]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[5,2]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[6,2]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0c. pelagic sst box indicator, written out in figure 4
box=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/sst_box_indicator.csv") 
box$Ruling=NA
box$Status=NA
for(i in 1:(nrow(box)-1)){
  box$Ruling[i+1]=(box$Temperature[i]-box$Threshold[i])
  if (box$Ruling[i+1]<0){box$Status[i+1]="Open"}
  if (box$Ruling[i+1]>0){box$Status[i+1]="Closed"}
}

box=box[complete.cases(box),]%>% mutate(Zero=0)
closures=box %>% filter(Date=="2014-08-16"|Date=="2015-06-16"|Date=="2015-07-16"|Date=="2015-08-16"|Date=="2016-06-16"|Date=="2016-07-16"|Date=="2016-08-16") %>% group_by(Year)
master[7,1]=c("pelagic_SST")
master[7,2]=as.data.frame(table(closures$Status))%>%.[.$Var1=="Closed",]%>%.[1,2]

# ---> adding in new rule, enso and local 6 month average combination
closures=combo %>% filter(indicator_date=="2014-08-16"|indicator_date=="2015-06-16"|indicator_date=="2015-07-16"|indicator_date=="2015-08-16"|indicator_date=="2016-06-16"|indicator_date=="2016-07-16"|indicator_date=="2016-08-16") 
master[8,1]=c("Six_month_combo")
master[8,2]=as.data.frame(table(closures$combn))%>%.[.$Var1=="Closed",]%>%.[1,2]

#1. operturnity cost 2003-2007
#0a. ENSO indicators, lines 70-124 of figure 2
#enso=contemp
closures=enso %>% filter(indicator_date>2003)
master[1,3]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[2,3]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[3,3]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0b. local anomaly indicators, lines 202-243 of figure 3
#local=contemp
closures=local %>% filter(indicator_date>2003)
master[4,3]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[5,3]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[6,3]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0c. pelagic sst box indicator, written out in figure 4
closures=box %>% filter(Year>2002)
master[7,3]=as.data.frame(table(closures$Status))%>%.[.$Var1=="Closed",]%>%.[1,2]

# ---> adding in new rule, enso and local 6 month average combination
closures=combo %>% filter(indicator_date>2003)
master[8,3]=as.data.frame(table(closures$combn))%>%.[.$Var1=="Closed",]%>%.[1,2]


#2. % sightings avoided (Figure 6, SST box indicator)
pla_sightings=pla_sightings %>% filter(Date>"2015-01-01") ## from lines 623-636 figure 6
pla_sightings=pla_sightings %>% mutate(indicator_date=as.character(Date))

#0a. ENSO indicators, lines 70-124 of figure 2
closures=left_join(pla_sightings,enso)
master[1,4]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[2,4]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[3,4]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0b. local anomaly indicators, lines 202-243 of figure 3
closures=left_join(pla_sightings,local)
master[4,4]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[5,4]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[6,4]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0c. pelagic sst box indicator, written out in figure 4
box=box %>% mutate(indicator_date=as.character(Date))
closures=left_join(pla_sightings,box,by="indicator_date")
master[7,4]=as.data.frame(table(closures$Status))%>%.[.$Var1=="Closed",]%>%.[1,2]

# ---> adding in new rule, enso and local 6 month average combination
closures=left_join(pla_sightings,combo)
master[8,4]=as.data.frame(table(closures$combn))%>%.[.$Var1=="Closed",]%>%.[1,2]

#3. % of historical bycatch avoided ##--> 662-664 of table 2
#1a. ENSO indicators, lines 70-124 of figure 2
closures=left_join(turtle_dat,enso,by=c("ym"="indicator_date")) #%>% select(Date,ym,count,YR,MON,ANOM,one_month_enso=one_month_status,two_three_enso=two_three_status,six_month_enso=six_month_status) 
master[1,5]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[2,5]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[3,5]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#1b. local anomaly indicators, lines 202-243 of figure 3
closures=left_join(turtle_dat,local,by=c("ym"="indicator_date")) %>% arrange(Date)
master[4,5]=as.data.frame(table(closures$one_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[5,5]=as.data.frame(table(closures$two_three_status))%>%.[.$Var1=="Closed",]%>%.[1,2]
master[6,5]=as.data.frame(table(closures$six_month_status))%>%.[.$Var1=="Closed",]%>%.[1,2]

#0c. pelagic sst box indicator, written out in figure 4 ##---> from table 2
closures=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/bycatch_hindcast.csv")
master[7,5]=as.data.frame(table(closures$pelagic_box))%>%.[.$Var1=="Closed",]%>%.[1,2]

# ---> adding in new rule, enso and local 6 month average combination
closures=left_join(turtle_dat,combo,by=c("ym"="indicator_date")) %>% arrange(Date)
master[8,5]=as.data.frame(table(closures$combn))%>%.[.$Var1=="Closed",]%>%.[1,2]

## column totals
master[9,1]="Totals"
# alignment
master[9,2]=7
# opertunity cost
closures=box %>% filter(Year>2002) %>% nrow()
master[9,3]=closures
#pla sightings
master[9,4]=nrow(pla_sightings)
#bycatch avoided
master[9,5]=nrow(turtle_dat)

write.csv(master,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/hindcast_eval_all_indicators_combo.csv")

## ------------------------------------> Table 2  ####
#rough csvs come from test_rules_hindcast.01.16.2018.R
#write_csv(mod_lenient_wENSO,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/mod_lenient_wENSO.csv")

## ------------------------------------> Table 3  ####
#rough csvs come from test_rules_historical_bycatch.01.16.2018.R
#write.csv(df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/historical_bycatch.csv")