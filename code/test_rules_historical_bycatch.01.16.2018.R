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
library(stringr)
library(rgeos)
library(sp)

####### Step 1. Extract spatial means from all new rs layers for each indicator ####
ras_list=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new",full.names = T)%>% grep(".grd",.,value=T) %>% stack()
enso_anom=read.table("/Volumes/SeaGate/BREP/BREP/ENSO/detrend.nino34.ascii.txt",header = T) 
enso_anom[,2]=str_pad(enso_anom[,2],width=2,side="left",pad=0)
enso_anom=mutate(enso_anom,indicator_date=paste(YR,MON,"16",sep="-")) %>% .[,5:6]

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
colnames(df)=c("indicator_date","indicator_month","bycatch_event_date","n.turts","JAS","WB1","OB1","ENSO","moderate","mod_lenient")
df$indicator_date=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new") %>% grep(".grd",.,value=T) %>% gsub("new_mean_","",.)%>% gsub(".grd","",.)
df$indicator_month=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday_raster",pattern="new") %>% grep(".grd",.,value=T) %>% gsub("new_mean_","",.)%>% gsub(".grd","",.) %>% substr(.,6,7)
df$JAS=raster::extract(ras_list,jas,fun=mean,na.rm=T,df=T) %>% gather() %>% .[2:nrow(.),] %>% .[,2]
df$WB1=raster::extract(ras_list,wb1,fun=mean,na.rm=T,df=T) %>% gather() %>% .[2:nrow(.),] %>% .[,2]
df$OB1=raster::extract(ras_list,ob1,fun=mean,na.rm=T,df=T) %>% gather() %>% .[2:nrow(.),] %>% .[,2]
df$ENSO=left_join(df,enso_anom) %>% .[,11]
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
n_turts=c(1,1,1,3,2,2,1,1,3,1,1)
df$n.turts=n_turts
bycatch_event_date=c("1992-04-16","1992-06-16","1992-07-16","1993-01-16","1993-08-16","1997-08-16","1997-10-16","1998-01-16","1998-08-16","2001-08-16","2006-10-16")
df$bycatch_event_date=bycatch_event_date

### compile indicators
df$indicator=NA
ind=c("wb1","wb1","ob1","enso","jas","jas","ob1","enso","jas","jas","ob1")
df$indicator=ind
df$indicator_values=NA
ind_vals=c(18.58767,20.87946,21.96880,-0.04,22.79958,21.70210,23.44478,2.23,19.75943,20.68157,22.39351) ## taken from DF, these are the thresholds based on the best indicator
df$indicator_values=ind_vals

######## Step 2. Hindcast rules to see if closures would have been enforced #####
rules=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/mod_lenient_wENSO.csv")
rownames(rules)=c("January","February","March","April","May","June","July","August","September","October","November","December","closure_feq","n_turts")

for(i in 1:11){ ## for every row
  month=df[i,2] %>% as.numeric()
  print(paste0("Bycatch occurred in ",month+1))
  print(paste0("Proceeding month for indicator value is ",month))
  # add in NA
  for(indicator in 16:17){ ## for every rule (n=2)
    print(paste0("Rule we're evaluating is ",colnames(rules[indicator])))
    indicator_val=rules[month,indicator]
    print(paste0("rule threshold for ",colnames(rules[indicator]), " in ",month," is ", indicator_val))
    df_col=indicator-7
      print(paste0("best indicator for bycatch month is ",df[i,11]))
      print(paste0("Observed value for indicator in bycatch month is ",df[i,12]))
      open_close=df[i,12]-indicator_val
      print(open_close)
      df_col=indicator-7
      if(open_close>=0){df[i,df_col]="Closed"
      print("Closed")}
      if(open_close<0){df[i,df_col]="Open"
      print("Open")}
    }
  }


df=df[,c(1:4,11,12,9:10)]
write.csv(df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/historical_bycatch.csv")

######## Step 3. Check ENSO based closures #####
# a. find anomalies for months proceeding historical closures
# b. calc value +/- 1SD
# c. test threshold found in b against historical closures

# a. find anomalies for months proceeding historical closures
closures=data.frame(matrix(NA,ncol=1,nrow=7))
colnames(closures)="indicator_date"
closures$indicator_date=c("2014-07-16","2015-05-16","2015-06-16","2015-07-16","2016-05-16","2016-06-16","2016-07-16") ## months preceeding closures
a=left_join(closures,enso_anom)

# b. calc value +/- 1SD
mean_anom=mean(a$ANOM) #0.4414286
sd_anom=sd(a$ANOM) #0.5833361
enso_anom$date=as.Date(enso_anom$indicator_date)
time=filter(enso_anom,date>=as.Date("2010-05-16")&date<as.Date("2017-01-16")) %>% separate(indicator_date,c("year","month","day"),sep="-") %>% filter(month=="05" | month=="06" | month=="07") %>% group_by(year)
time$threshold=0.4414286
time$upper=0.4414286+0.5833361
time$lower=0.4414286-0.5833361 #-0.1419075
#b=ggplot()+geom_line(data=time,aes(x=date,y=ANOM))+geom_line(data=time,aes(x=date,y=threshold),color="blue")+geom_ribbon(data=time,aes(x=date,ymin=lower, ymax=upper),fill="blue",alpha=0.2)+
  # geom_text(data=time,aes(x=date,y=ANOM,label=date))
a$indicator_date=as.Date(a$indicator_date)

b=ggplot()+geom_line(data=time,aes(x=date,y=ANOM))+geom_line(data=time,aes(x=date,y=threshold),color="blue")+geom_line(data=time,aes(x=date,y=lower),color="red")+
  geom_text(data=time,aes(x=date,y=ANOM,label=date)) + geom_text(data=a,aes(x=indicator_date,y=ANOM,label=indicator_date),color="green")

# c. test threshold found in b against historical closures
c=left_join(df,enso_anom)
c$Enso_conservative=0.4414286
c$Enso_moderate=-0.1419075

for(i in 1:11){ ## for every row
  if(c[i,9] - 0.4414286 >=0){c[i,11]="Closed"} ## conservative
  if(c[i,9] - 0.4414286 <0){c[i,11]="Open"} ## conservative
  
  if(c[i,9] - -0.1419075 >=0){c[i,12]="Closed"} ## moderate
  if(c[i,9] - -0.1419075 <0){c[i,12]="Open"} ## moderate
}
  
d=c[,c(1:8,11:12)]
  
######## Step 3. Check ENSO based closures #####  this is new from HW 02.26.18. checking anomalies following fed registrar data ####
# a. define SCB watch rectangle
# b. extract average anomaly value w.in rectangle, and SD
# c. test threshold found in b against historical closures (2003 - 2016)
# d. test threshold found in b again historical catch events

# a. define SCB watch rectangle
scb_coords=matrix(c(-125,27.5,  ## define SST box
                   -125,35,
                   -116,35,
                   -116,27.5,
                   -125,27.5),
                 ncol=2,byrow = T)

p=Polygon(scb_coords)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(sps,add=T)
scb=sps

# b. extract average anomaly value w.in rectangle, and SD
closures=c("2014-05-16","2014-06-16","2015-03-16","2015-04-16","2016-03-16","2016-04-16") ## months preceeding closures, 2nd and 3rd month as in registrar
closures=lapply(closures,function(x)paste0("/Volumes/SeaGate/BREP/jplmur_raster/anom_",x,".grd")) %>% unlist()

df=data.frame("date"=NA,"mean"=NA,"sd"=NA)
for  (i in 1:length(closures)){
  name=gsub("/Volumes/SeaGate/BREP/jplmur_raster/anom_","",closures[i]) %>% gsub(".grd","",.)
  print(name)
  ras=raster(closures[i])
  ex_mean=raster::extract(ras,scb,fun=mean,na.rm=T,df=T)
  ex_sd=raster::extract(ras,scb,fun=sd,na.rm=T,df=T)
  df[i,1]=name
  df[i,2]=ex_mean$layer
  df[i,3]=ex_sd$layer
}

### find mean and mean sd
anom_mean=mean(df$mean)
anom_sd=mean(df$sd)
anom_mean_sd=anom_mean+anom_sd
scb_anoms=df

# c. test threshold found in b against historical closures (2003 - 2016) 
#c1: extract w.in box
#c2: calc open/closed

#c1 (this function is from test_rules.R)
############ ----------> 2. find spatial average SST in each indicator in all months #########

##### a. create extraction function####

ras_DIR="/Volumes/SeaGate/BREP/jplmur_raster"
anom_ras=list.files(ras_DIR,pattern = "anom_*",full.names = T)%>%grep(".grd$",.,value=T)
month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))
year_list=seq(2003,2016)

df=data.frame(matrix(NA,ncol=15))
colnames(df)=c("month",year_list)

focal_stats=function(indicator,month,ras_list){
  print(month)
  a=grep(month,anom_ras,value=T)
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
dff=lapply(month_list,indicator=scb,ras_list=anom_ras,FUN=focal_stats) 
dff=do.call("rbind",dff) %>% .[complete.cases(.),]
write.csv(dff,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/scb_anom_df.csv")

#c2: calc open/closed (from test_rules_hindcast.01.16.2018.R)
df_empty=dff
rownames(df_empty)=dff$month
df_empty=df_empty[,2:ncol(df_empty)]
df_empty$anom_conservative=anom_mean
df_empty$Enso_moderate=anom_mean_sd
df_empty$min=min(scb_anoms$mean)

## Enso_conservative -------------------------------------> 
Enso_conservative=df_empty
Enso_conservative[1:14]=Enso_conservative[1:14]-Enso_conservative[,15]
empty=Enso_conservative

for(i in 1:12){
  for(ii in 1:14){
    print(Enso_conservative[i,ii])
    if(i==12 && Enso_conservative[i,ii]>=0){empty[1,ii]=("Closed")} ## december rule impacting jan closure
    if(i==12 && Enso_conservative[i,ii]<0){empty[1,ii]=("Open")} ## december rule impacting jan closure
    if(Enso_conservative[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(Enso_conservative[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}

empty=empty[1:12,]
empty["Closure freq",]=NA
for(i in 1:14){
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[13,i]=a
}
empty[13,c(1:14)][is.na(empty[13,c(1:14)])]<-0


empty["Turtle freq",]=NA
empty[14,1]=10 #2003
empty[14,2]=0 #2004
empty[14,3]=1 #2005
empty[14,4]=34 #2006
empty[14,5]=0 #2007
empty[14,6]=0 #2008
empty[14,7]=0 #2009
empty[14,8]=0 #2010
empty[14,9]=0 #2011
empty[14,10]=0 #2012
empty[14,11]=1 #2013
empty[14,12]=94 #2014
empty[14,13]=469 #2015
empty[14,14]=56 #2016

Enso_conservative=empty
write_csv(Enso_conservative,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/scb_anom_conservative.csv")

## Enso_moderate -------------------------------------> 
Enso_moderate=df_empty
Enso_moderate[1:14]=Enso_moderate[1:14]-Enso_moderate[,16]
empty=Enso_moderate

for(i in 1:12){
  for(ii in 1:14){
    print(Enso_moderate[i,ii])
    if(i==12 && Enso_moderate[i,ii]>=0){empty[1,ii]=("Closed")} ## december rule impacting jan closure
    if(i==12 && Enso_moderate[i,ii]<0){empty[1,ii]=("Open")} ## december rule impacting jan closure
    if(Enso_moderate[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(Enso_moderate[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}

empty=empty[1:12,]
empty["Closure freq",]=NA
for(i in 1:14){
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[13,i]=a
}
empty[13,c(1:14)][is.na(empty[13,c(1:14)])]<-0

empty["Turtle freq",]=NA
empty[14,1]=10 #2003
empty[14,2]=0 #2004
empty[14,3]=1 #2005
empty[14,4]=34 #2006
empty[14,5]=0 #2007
empty[14,6]=0 #2008
empty[14,7]=0 #2009
empty[14,8]=0 #2010
empty[14,9]=0 #2011
empty[14,10]=0 #2012
empty[14,11]=1 #2013
empty[14,12]=94 #2014
empty[14,13]=469 #2015
empty[14,14]=56 #2016

Enso_moderate=empty
write_csv(Enso_moderate,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/scb_anom_moderate.csv")

## Enso_min -------------------------------------> 
Enso_min=df_empty
Enso_min[1:14]=Enso_min[1:14]-Enso_min[,17]
empty=Enso_min

for(i in 1:12){
  for(ii in 1:14){
    print(Enso_min[i,ii])
    if(i==12 && Enso_min[i,ii]>=0){empty[1,ii]=("Closed")} ## december rule impacting jan closure
    if(i==12 && Enso_min[i,ii]<0){empty[1,ii]=("Open")} ## december rule impacting jan closure
    if(Enso_min[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(Enso_min[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}

empty=empty[1:12,]
empty["Closure freq",]=NA
for(i in 1:14){
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[13,i]=a
}
empty[13,c(1:14)][is.na(empty[13,c(1:14)])]<-0

empty["Turtle freq",]=NA
empty[14,1]=10 #2003
empty[14,2]=0 #2004
empty[14,3]=1 #2005
empty[14,4]=34 #2006
empty[14,5]=0 #2007
empty[14,6]=0 #2008
empty[14,7]=0 #2009
empty[14,8]=0 #2010
empty[14,9]=0 #2011
empty[14,10]=0 #2012
empty[14,11]=1 #2013
empty[14,12]=94 #2014
empty[14,13]=469 #2015
empty[14,14]=56 #2016

Enso_min=empty
write_csv(Enso_min,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/scb_anom_min.csv")


######## Step 3. Check ENSO based closures #####  this is new from HW 02.28.18. checking anomalies following fed registrar data, using Mike's extracted ROMS data ####
library(stringr)
anoms=read.csv("/Volumes/SeaGate/BREP/BREP/roms_anomalies/BREP_historical_SST_anomaly.txt")
contemp=filter(anoms,Year>2002) %>% mutate(data="ROMS")#%>% spread(Year,SST_Anomaly)
contemp$Month=str_pad(contemp$Month,2,pad="0")

## compare to CoastWatch data
coastwatch=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/scb_anom_df.csv")
colnames(coastwatch)=gsub("X","",colnames(coastwatch))
coastwatch=coastwatch[,2:ncol(coastwatch)]
coastwatch$month=gsub("-","",coastwatch$month)
contemp_cw=coastwatch %>% gather(Year,SST_Anomaly,-month) %>% mutate(data="CoastWatch")
colnames(contemp_cw)[1]="Month"

combined=rbind(contemp,contemp_cw) %>% mutate(date=paste0(Year,"-",Month))

test=ggplot()+geom_line(data=combined[combined$data=="CoastWatch",],aes(x=date,y=SST_Anomaly,group=1),color="red")
test=test+geom_line(data=combined[combined$data=="ROMS",],aes(x=date,y=SST_Anomaly,group=1),color="blue")
test

### hindcast closure threshod
contemp=filter(anoms,Year>2002) %>% mutate(data="ROMS")
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-05"|date=="2014-06"|date=="2015-03"|date=="2015-04"|date=="2016-03"|date=="2016-04") ## months preceeding closures, 2nd and 3rd month as in registrar
threshold=mean(contemp$SST_Anomaly) ## 1.73
threshold=max(contemp$SST_Anomaly) ## 2.54
threshold=min(contemp$SST_Anomaly) ## 1.4

### test hindcast closure threshod
contemp=filter(anoms,Year>2002) %>% spread(Year,SST_Anomaly) %>% select(-Month)

contemp_test=contemp
contemp_test[1:14]=contemp_test[1:14]-1.4
empty=contemp

for(i in 1:12){
  for(ii in 1:14){
    print(contemp[i,ii])
    if(i==10 && mean(c(contemp_test[i,ii],contemp_test[i+1,ii]))>=0){empty[1,ii]=("Closed")} ## oct/nov rule impacting jan closure
    if(i==10 && mean(c(contemp_test[i,ii],contemp_test[i+1,ii]))<0){empty[1,ii]=("Open")} ## oct/nov rule impacting jan closure
    
    if(i==11 && mean(c(contemp_test[i,ii],contemp_test[i+1,ii]))>=0){empty[2,ii]=("Closed")} ## nov/dec rule impacting feb closure
    if(i==11 && mean(c(contemp_test[i,ii],contemp_test[i+1,ii]))<0){empty[2,ii]=("Open")} ## nov/dec rule impacting feb closure
    
    if(i==12 && mean(c(contemp_test[i,ii],contemp_test[1,ii]))>=0){empty[3,ii]=("Closed")} ## dec/jan rule impacting mar closure
    if(i==12 && mean(c(contemp_test[i,ii],contemp_test[1,ii]))<0){empty[3,ii]=("Open")} ## dec/jan rule impacting mar closure
    
    print(c(i,i+1,i+3))
    if(i<=9 && mean(c(contemp_test[i,ii],contemp_test[i+1,ii]))>=0){empty[i+3,ii]=("Closed")}
    if(i<=9 && mean(c(contemp_test[i,ii],contemp_test[i+1,ii]))<0){empty[i+3,ii]=("Open")}
  }
  
}
## lets try plotting it
contemp=filter(anoms,Year>2002) %>% mutate(data="ROMS")
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
contemp$mean=1.73
contemp$max=2.54
contemp$min=1.4

df=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-05"|date=="2014-06"|date=="2015-03"|date=="2015-04"|date=="2016-03"|date=="2016-04") ## months preceeding closures, 2nd and 3rd month as in registrar
df=df %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))


b=ggplot()+geom_line(data=contemp,aes(x=date,y=SST_Anomaly))+geom_line(data=contemp,aes(x=date,y=mean),color="black")+geom_line(data=contemp,aes(x=date,y=min),color="red")+geom_line(data=contemp,aes(x=date,y=max),color="blue") +
  geom_point(data=df,aes(x=date,y=SST_Anomaly),color="pink")
  #geom_text(data=contemp,aes(x=date,y=SST_Anomaly,label=date)) #+ geom_text(data=a,aes(x=indicator_date,y=SST_Anomaly,label=indicator_date),color="green")

### test if historical bycatch would have been avoided
contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
bycatch_event_date=c("1992-04-16","1992-06-16","1992-07-16","1993-01-16","1993-08-16","1997-08-16","1997-10-16","1998-01-16","1998-08-16","2001-08-16","2006-10-16")
# df=data.frame(Bycatch=character(),Anom=numeric(),Status=character())
dff=data.frame(matrix(NA,nrow=11,ncol = 3))
dff[1]=bycatch_event_date
colnames(dff)[1]="Bycatch"

for(i in 1:nrow(dff)){
  time=dff[i,1]
  print(time)
  pos=grep(time,as.character(contemp$date))
  value=mean(c(contemp$SST_Anomaly[pos-3],contemp$SST_Anomaly[pos-2]))
  dff[i,2]=value
  if(value>=1.4){dff[i,3]="Closed"}
  if(value<1.4){dff[i,3]="Open"}
}

colnames(dff)[3]="Current_anomaly_rule"

######## Step 4. adding sightings data into the evaluation ####
turtdata=load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData")
scb.cc.xpts$anom=NA
scb.cc.xpts$ruling=NA

for(i in 1:nrow(scb.cc.xpts)){
  time=as.character(scb.cc.xpts[i,4]) %>% strsplit(.,"-") %>%unlist() %>% .[1:2] 
  time= paste0(time[[1]],"-",time[[2]])
  print(time)
  pos=grep(time,as.character(contemp$date))
  value=mean(c(contemp$SST_Anomaly[pos-3],contemp$SST_Anomaly[pos-2]))
  scb.cc.xpts[i,7]=value
  if(value>=1.4){scb.cc.xpts[i,8]="Closed"}
  if(value<1.4){scb.cc.xpts[i,8]="Open"}
}

map.world = map_data(map="world")
map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat),fill="grey")#+coord_cartesian()
map=map+geom_point(data=scb.cc.xpts,aes(x=lon,y=lat,color=ruling),size=.2)
map=map+coord_cartesian(xlim=c(-135,-115),ylim=c(25,60),expand=F)
map

pla=readShapeSpatial("/Volumes/SeaGate/BREP/BREP/spatial_files_for_figures/loggerhead.shp")
sightings=scb.cc.xpts
coordinates(sightings)=~lon+lat
pla_sightings=intersect(sightings,pla)
pla_sightings@data$lat=pla_sightings@coords[,2]
pla_sightings@data$lon=pla_sightings@coords[,1]
pla_sightings=pla_sightings@data

map.world = map_data(map="world")
map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat),fill="grey")#+coord_cartesian()
map=map+geom_point(data=pla_sightings,aes(x=lon,y=lat,color=ruling),size=.2)
map=map+coord_cartesian(xlim=c(-122,-115),ylim=c(30,40),expand=F)
map


### hindcast closure threshod --------------------------> trying again based on month directly proceeding #####
contemp=filter(anoms,Year>2002) %>% mutate(data="ROMS")
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-07"|date=="2015-05"|date=="2016-05") ## months preceeding closures, 2nd and 3rd month as in registrar
threshold=mean(contemp$SST_Anomaly) ## 1.183333
threshold=max(contemp$SST_Anomaly) ## 1.62
threshold=min(contemp$SST_Anomaly) ## 0.92

### test hindcast closure threshod
contemp=filter(anoms,Year>2002) %>% spread(Year,SST_Anomaly) %>% select(-Month)

contemp_test=contemp
contemp_test[1:14]=contemp_test[1:14]-0.92
empty=contemp

for(i in 1:12){
  for(ii in 1:14){
    print(contemp[i,ii])
    
    if(i==12 && contemp_test[i,ii]>=0){empty[1,ii]=("Closed")} ## dec/jan rule impacting mar closure
    if(i==12 && contemp_test[i,ii]<0){empty[1,ii]=("Open")} ## dec/jan rule impacting mar closure
    
    print(c(i,i+1,i+3))
    if(i<=11 && contemp_test[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(i<=11 && contemp_test[i,ii]<0){empty[i+1,ii]=("Open")}
  }
}

turtdata=load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData")
scb.cc.xpts$anom=NA
scb.cc.xpts$ruling=NA

contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))

for(i in 1:nrow(scb.cc.xpts)){
  time=as.character(scb.cc.xpts[i,4]) %>% strsplit(.,"-") %>%unlist() %>% .[1:2] 
  time= paste0(time[[1]],"-",time[[2]])
  print(time)
  pos=grep(time,as.character(contemp$date))
  value=contemp$SST[pos-1]
  scb.cc.xpts[i,7]=value
  if(value>=0.92){scb.cc.xpts[i,8]="Closed"}
  if(value<0.92){scb.cc.xpts[i,8]="Open"}
}

sightings=scb.cc.xpts
coordinates(sightings)=~lon+lat
pla_sightings=intersect(sightings,pla)
pla_sightings@data$lat=pla_sightings@coords[,2]
pla_sightings@data$lon=pla_sightings@coords[,1]
pla_sightings=pla_sightings@data

map.world = map_data(map="world")
map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat),fill="grey")#+coord_cartesian()
map=map+geom_point(data=pla_sightings,aes(x=lon,y=lat,color=ruling),size=.2)
map=map+coord_cartesian(xlim=c(-122,-115),ylim=c(30,40),expand=F)
map

### test if historical bycatch would have been avoided
contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
bycatch_event_date=c("1992-04-16","1992-06-16","1992-07-16","1993-01-16","1993-08-16","1997-08-16","1997-10-16","1998-01-16","1998-08-16","2001-08-16","2006-10-16")
# df=data.frame(Bycatch=character(),Anom=numeric(),Status=character())
dff=data.frame(matrix(NA,nrow=11,ncol = 3))
dff[1]=bycatch_event_date
colnames(dff)[1]="Bycatch"

for(i in 1:nrow(dff)){
  time=dff[i,1]
  print(time)
  pos=grep(time,as.character(contemp$date))
  value=contemp$SST_Anomaly[pos-1]
  dff[i,2]=value
  if(value>=0.92){dff[i,3]="Closed"}
  if(value<0.92){dff[i,3]="Open"}
}

colnames(dff)[3]="Current_anomaly_rule_.92"

## lets try plotting raw anomalies ROMS
## lets try plotting it
contemp=anoms %>% mutate(data="ROMS")
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
contemp$max=.92
contemp$min=1.4

df=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-05"|date=="2014-06"|date=="2015-03"|date=="2015-04"|date=="2016-03"|date=="2016-04") ## months preceeding closures, 2nd and 3rd month as in registrar
df=df %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))


b=ggplot()+geom_line(data=contemp,aes(x=date,y=SST_Anomaly))+geom_line(data=contemp,aes(x=date,y=min),color="red")+geom_line(data=contemp,aes(x=date,y=max),color="blue") +
  geom_point(data=df,aes(x=date,y=SST_Anomaly),color="pink")
#geom_text(data=contemp,aes(x=date,y=SST_Anomaly,label=date)) #+ geom_text(data=a,aes(x=indicator_date,y=SST_Anomaly,label=indicator_date),color="green")



### hindcast closure threshod --------------------------> trying again based on the 6 months directly proceeding #####
contemp=filter(anoms,Year>2002) %>% mutate(data="ROMS")
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=paste0(Year,"-",Month))
one=contemp[grep("2014-07",contemp$date):(grep("2014-07",contemp$date)-5),]
two=contemp[grep("2015-05",contemp$date):(grep("2015-05",contemp$date)-5),]
three=contemp[grep("2016-05",contemp$date):(grep("2016-05",contemp$date)-5),]
contemp=do.call("rbind",list(one,two,three))

threshold=mean(contemp$SST_Anomaly) ## 1.553889
threshold=max(contemp$SST_Anomaly) ## 2.54
threshold=min(contemp$SST_Anomaly) ## 0.77

### test hindcast closure threshod
contemp=filter(anoms,Year>2001) #%>% spread(Year,SST_Anomaly) %>% select(-Month)
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=paste0(Year,"-",Month))
contemp$eval=NA

for(i in 7:nrow(contemp)){
contemp[i,5]=mean(c(contemp[i-1,3],contemp[i-2,3],contemp[i-3,3],contemp[i-4,3],contemp[i-5,3],contemp[i-6,3]))
}

contemp$ruling=NA
for(i in 7:nrow(contemp)){
  if(contemp[i,5]>=0.77){contemp[i,6]="Closed"}
  if(contemp[i,5]<0.77){contemp[i,6]="Open"}
}

contempp= contemp %>% select(-c(eval,date,SST_Anomaly)) %>% filter(Year>2003) %>% spread(Year,ruling) %>% select(-Month)

### test if historical sightings would have been avoided
turtdata=load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData")
scb.cc.xpts$anom=NA
scb.cc.xpts$ruling=NA

contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))

for(i in 1:nrow(scb.cc.xpts)){
  time=as.character(scb.cc.xpts[i,4]) %>% strsplit(.,"-") %>%unlist() %>% .[1:2] 
  time= paste0(time[[1]],"-",time[[2]])
  print(time)
  pos=grep(time,as.character(contemp$date))
  value=contemp$SST[pos-1]
  scb.cc.xpts[i,7]=value
  if(value>=0.77){scb.cc.xpts[i,8]="Closed"}
  if(value<0.77){scb.cc.xpts[i,8]="Open"}
}

sightings=scb.cc.xpts
coordinates(sightings)=~lon+lat
pla_sightings=intersect(sightings,pla)
pla_sightings@data$lat=pla_sightings@coords[,2]
pla_sightings@data$lon=pla_sightings@coords[,1]
pla_sightings=pla_sightings@data

map.world = map_data(map="world")
map=ggplot()+geom_map(data=map.world,map=map.world,aes(map_id=region,x=long,y=lat),fill="grey")#+coord_cartesian()
map=map+geom_point(data=pla_sightings,aes(x=lon,y=lat,color=ruling),size=.2)
map=map+coord_cartesian(xlim=c(-122,-115),ylim=c(30,40),expand=F)
map

### test if historical bycatch would have been avoided
contemp=anoms
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
bycatch_event_date=c("1992-04-16","1992-06-16","1992-07-16","1993-01-16","1993-08-16","1997-08-16","1997-10-16","1998-01-16","1998-08-16","2001-08-16","2006-10-16")
# df=data.frame(Bycatch=character(),Anom=numeric(),Status=character())
dff=data.frame(matrix(NA,nrow=11,ncol = 3))
dff[1]=bycatch_event_date
colnames(dff)[1]="Bycatch"

for(i in 1:nrow(dff)){
  time=dff[i,1]
  print(time)
  pos=grep(time,as.character(contemp$date))
  value=mean(c(contemp$SST_Anomaly[pos-1],contemp$SST_Anomaly[pos-2],contemp$SST_Anomaly[pos-3],contemp$SST_Anomaly[pos-4],contemp$SST_Anomaly[pos-5],contemp$SST_Anomaly[pos-6]))
  dff[i,2]=value
  if(value>=0.77){dff[i,3]="Closed"}
  if(value<0.77){dff[i,3]="Open"}
}

colnames(dff)[3]="Current_anomaly_rule_.77"

## lets try plotting raw anomalies ROMS
## lets try plotting it
contemp=anoms %>% mutate(data="ROMS")
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
contemp$max=.92
contemp$min=1.4
contemp$mean=.77

df=contemp %>% mutate(date=paste0(Year,"-",Month)) %>% filter(date=="2014-05"|date=="2014-06"|date=="2015-03"|date=="2015-04"|date=="2016-03"|date=="2016-04") ## months preceeding closures, 2nd and 3rd month as in registrar
df=df %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))


b=ggplot()+geom_line(data=contemp,aes(x=date,y=SST_Anomaly))+geom_line(data=contemp,aes(x=date,y=min),color="red")+geom_line(data=contemp,aes(x=date,y=max),color="blue")+ geom_line(data=contemp,aes(x=date,y=mean),color="green") +
  geom_point(data=df,aes(x=date,y=SST_Anomaly),color="pink")
#geom_text(data=contemp,aes(x=date,y=SST_Anomaly,label=date)) #+ geom_text(data=a,aes(x=indicator_date,y=SST_Anomaly,label=indicator_date),color="green")

### test hindcast closure threshod
## then plot over anomaly

contemp=anoms #%>% spread(Year,SST_Anomaly) %>% select(-Month)
contemp$Month=str_pad(contemp$Month,2,pad="0")
contemp=contemp %>% mutate(date=as.Date(paste0(Year,"-",Month,"-16")))
contemp$eval=NA

for(i in 7:nrow(contemp)){
  contemp[i,5]=mean(c(contemp[i-1,3],contemp[i-2,3],contemp[i-3,3],contemp[i-4,3],contemp[i-5,3],contemp[i-6,3]))
}

contemp$ruling=NA
for(i in 7:nrow(contemp)){
  if(contemp[i,5]>=0.77){contemp[i,6]="Closed"}
  if(contemp[i,5]<0.77){contemp[i,6]="Open"}
}

contemp$max=.92
contemp$min=1.4
contemp$mean=.77

b=ggplot()+geom_line(data=contemp,aes(x=date,y=SST_Anomaly))+geom_line(data=contemp,aes(x=date,y=min),color="red")+geom_line(data=contemp,aes(x=date,y=max),color="blue")+ geom_line(data=contemp,aes(x=date,y=mean),color="green")+
  geom_point(data=contemp,aes(x=date,y=SST_Anomaly,color=ruling))+scale_color_manual(values=c("Closed"="red","Open"=NA))
