### testing rules for the most correlated indicators in each month (actually more like developing rules)
## follows create_rule.R, which honestly I thought was supposed to do what this script does but might have gotten deleted
# https://rpubs.com/rural_gis/254726

## steps
#1. define all indicators
#2. find spatial average SST in each indicator in all months

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
library(maptools)
library(dplyr)
library(reshape)

####### global objects ####
ras_DIR="/Volumes/SeaGate/BREP/jplmur_raster"
mean_ras=list.files(ras_DIR,pattern = "mean_*",full.names = T)%>%grep(".grd$",.,value=T)
month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))
year_list=seq(2003,2016)
lapply(month_list,function(x)grep(x,mean_ras,value = T)%>%stack(.)%>%assign(x,.))
for(month in month_list){
  a=grep(month,mean_ras,value=T)%>%stack()
}

for(i in 1:15){
  ex=raster::extract(a,sps,fun=mean,na.rm=T,df=T)
}

#1. define all indicators

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

#plot(sum_rasMJJ)
plot(ob1,add=T)
##### ENSO - best indicator in 12 ####
## dataframe created at line 205

############ ----------> 2. find spatial average SST in each indicator in all months #########

##### a. create extraction function####

df=data.frame(matrix(NA,ncol=15))
colnames(df)=c("month",year_list)

focal_stats=function(indicator,month,ras_list){
  print(month)
  a=grep(month,mean_ras,value=T)
  pos_2002=grep("2002",a)
  pos_2017=grep("2017",a)
  if(length(pos_2002)!=0) {a=a[-pos_2002]}
  if(length(pos_2017)!=0) {a=a[-pos_2017]}
  a_stk=stack(a)
  ex=raster::extract(a_stk,indicator,fun=mean,na.rm=T,df=T)
  #ex=raster::extract(a_stk,JAS,fun=mean,na.rm=T,df=T)
  years=unlist(lapply(a,function(x)gsub("/Volumes/SeaGate/BREP/jplmur_raster/mean_","",x)))%>%gsub('.{10}$',"",.)
  if("2017"%in%years){
    years=years[years!="2017"]
  }
  if("2002"%in%years){
    years=years[years!="2002"]
  }
  ex[1,1]=month
  colnames(ex)=c("month",year_list)
  df=rbind(df,ex)
  return(df)
}

###### jas ####
df=data.frame(matrix(NA,ncol=15))
colnames(df)=c("month",year_list)
dff=lapply(month_list,indicator=jas,ras_list=ras_list,FUN=focal_stats)

for(i in 1:12){
  df[i,]=dff[[i]][2,]
}

jas_df=df

jas_df[13,1]="Num Turts"
jas_df[13,2]=10 #2003
jas_df[13,3]=0 #2004
jas_df[13,4]=1 #2005
jas_df[13,5]=34 #2006
jas_df[13,6]=0 #2007
jas_df[13,7]=0 #2008
jas_df[13,8]=0 #2009
jas_df[13,9]=0 #2010
jas_df[13,10]=0 #2011
jas_df[13,11]=0 #2012
jas_df[13,12]=1 #2013
jas_df[13,13]=94 #2014
jas_df[13,14]=469 #2015
jas_df[13,15]=56 #2016

write.csv(jas_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/jas_df.csv")

###### wb1 ####
df=data.frame(matrix(NA,ncol=15))
colnames(df)=c("month",year_list)
dff=lapply(month_list,indicator=wb1,ras_list=ras_list,FUN=focal_stats)

for(i in 1:12){
  df[i,]=dff[[i]][2,]
}
df[13,1]="Num Turts"
df[13,2]=10 #2003
df[13,3]=0 #2004
df[13,4]=1 #2005
df[13,5]=34 #2006
df[13,6]=0 #2007
df[13,7]=0 #2008
df[13,8]=0 #2009
df[13,9]=0 #2010
df[13,10]=0 #2011
df[13,11]=0 #2012
df[13,12]=1 #2013
df[13,13]=94 #2014
df[13,14]=469 #2015
df[13,15]=56 #2016

wb1_df=df
write.csv(wb1_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/wb1_df.csv")

###### ob1 ####
df=data.frame(matrix(NA,ncol=15))
colnames(df)=c("month",year_list)
dff=lapply(month_list,indicator=ob1,ras_list=ras_list,FUN=focal_stats)

for(i in 1:12){
  df[i,]=dff[[i]][2,]
}

df[13,1]="Num Turts"
df[13,2]=10 #2003
df[13,3]=0 #2004
df[13,4]=1 #2005
df[13,5]=34 #2006
df[13,6]=0 #2007
df[13,7]=0 #2008
df[13,8]=0 #2009
df[13,9]=0 #2010
df[13,10]=0 #2011
df[13,11]=0 #2012
df[13,12]=1 #2013
df[13,13]=94 #2014
df[13,14]=469 #2015
df[13,15]=56 #2016

ob1_df=df
write.csv(ob1_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/ob1_df.csv")



###### enso ####
##1. read in ENSO index
ENSO=read.table("/Volumes/SeaGate/BREP/BREP/ENSO/detrend.nino34.ascii.txt",header = T)
enso1=ENSO[ENSO$YR>2002 & ENSO$YR < 2017,]
enso1$year=enso1$YR
enso2=merge(enso1,scb.pts.freq,by="year",all.x=T)
enso2.5=enso2[,c(2,3,6,7)]
enso_index=cast(enso2.5,YR~MON,value = "ANOM")
rownames(enso_index)=enso_index$YR
enso_index=enso_index[,2:ncol(enso_index)]

enso_index[1,13]=10 #2003
enso_index[2,13]=0 #2004
enso_index[3,13]=1 #2005
enso_index[4,13]=34 #2006
enso_index[5,13]=0 #2007
enso_index[6,13]=0 #2008
enso_index[7,13]=0 #2009
enso_index[8,13]=0 #2010
enso_index[9,13]=0 #2011
enso_index[10,13]=0 #2012
enso_index[11,13]=1 #2013
enso_index[12,13]=94 #2014
enso_index[13,13]=469 #2015
enso_index[14,13]=56 #2016

colnames(enso_index)=c("January","February","March","April","May","June","July","August","September","October","November","December","Turtles")
write.csv(enso_index,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/enso_df.csv")


############ ----------> 3. plot relationships#####
JAS_box=as.data.frame(t(jas_df))
colnames(JAS_box)=c("January","February","March","April","May","June","July","August","September","October","November","December","Turtles")
JAS_box=JAS_box[2:nrow(JAS_box),]

WB1_box=as.data.frame(t(wb1_df))
colnames(WB1_box)=c("January","February","March","April","May","June","July","August","September","October","November","December","Turtles")
WB1_box=WB1_box[2:nrow(WB1_box),]

OB1_box=as.data.frame(t(ob1_df))
colnames(OB1_box)=c("January","February","March","April","May","June","July","August","September","October","November","December","Turtles")
OB1_box=OB1_box[2:nrow(OB1_box),]


#### making plots. change indicator name, month name, and chart object name ####

##### jan ####
name="WB1"
month="January"
indicator_df=WB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(January))[January],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(January))[January],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(January))[January],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
jan=chart

##### feb ####
name="OB1"
month="February"
indicator_df=OB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(February))[February],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(February))[February],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(February))[February],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
feb=chart

##### mar ####
name="WB1"
month="March"
indicator_df=WB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(March))[March],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(March))[March],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(March))[March],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
mar=chart

##### apr ####
name="WB1"
month="April"
indicator_df=WB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(April))[April],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(April))[April],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(April))[April],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
apr=chart

##### may ####
name="WB1"
month="May"
indicator_df=WB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(May))[May],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(May))[May],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(May))[May],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
may=chart

##### jun ####
name="OB1"
month="June"
indicator_df=OB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(June))[June],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(June))[June],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(June))[June],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=20.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
jun=chart


##### jul ####
name="JAS"
month="July"
indicator_df=JAS_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(July))[July],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(July))[July],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(July))[July],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=21.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=21.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=21.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=21.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=21.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
jul=chart

##### aug ####
name="JAS"
month="August"
indicator_df=JAS_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(August))[August],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(August))[August],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(August))[August],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
aug=chart


##### sep ####
name="OB1"
month="September"
indicator_df=OB1_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(September))[September],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(September))[September],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(September))[September],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
sep=chart


##### oct ####
name="JAS"
month="October"
indicator_df=JAS_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(October))[October],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(October))[October],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(October))[October],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
oct=chart

##### nov ####
name="JAS"
month="November"
indicator_df=JAS_box

indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[as.numeric(indicator_df$Turtles)==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(as.numeric(levels(min[,month]))[min[,month]]),digits=2)
average=round(mean(as.numeric(levels(min[,month]))[min[,month]]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(as.numeric(levels(November))[November],Turtles))+geom_text(data=indicator_df,aes(as.numeric(levels(November))[November],Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(as.numeric(levels(November))[November],Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=22.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
nov=chart

##### dec ####
name="ENSO"
month="December"
name="ENSO"
month="December"
indicator_df=enso_index

#indicator_df$Turtles=as.numeric(levels(indicator_df$Turtles))[indicator_df$Turtles]
indicator_df_zero=indicator_df[indicator_df$Turtles==0,]
min=indicator_df[as.numeric(indicator_df$Turtles)>0,]
minVal=round(min(min[,month]),digits=2)
average=round(mean(min[,month]),digits=2)

chart=ggplot()+geom_point(data=indicator_df,aes(December,Turtles))+geom_text(data=indicator_df,aes(December,Turtles),label=rownames(indicator_df),vjust=-.5,hjust=-.5,size=2)
chart=chart+geom_point(data=indicator_df_zero,aes(December,Turtles),color="blue")
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Monthly SST within indicator in each year")+labs(y="Number of turtles seen in CA Bight each year")
chart=chart+geom_text(data=indicator_df,aes(x=1.13,y=330),label=paste0("Month is: ",month),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=1.129,y=310),label=paste0("Indicator is: ",name),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=1.13,y=290),label=paste0("Min turtle SST: ",minVal),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=1.13,y=270),label=paste0("Ave. turtle SST: ",average),size=2)
chart=chart+geom_text(data=indicator_df,aes(x=1.14,y=250),label=paste0("Blue = zero turtles"),size=2)
chart
dec=chart

pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/sst_by_indicator.pdf",width=15,height=17,pointsize=50)
grid.arrange(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,ncol=3,top=textGrob("Relationship between turtle sightings and SST in best indicator in each month",gp=gpar(fontsize=13)))
dev.off()

png("/Volumes/SeaGate/BREP/BREP/set_in_indicators/sst_by_indicator.png",width=1500,height=1700,units="px",pointsize=50)
grid.arrange(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec,ncol=3,top=textGrob("Relationship between turtle sightings and SST in best indicator in each month",gp=gpar(fontsize=13)))
dev.off()
