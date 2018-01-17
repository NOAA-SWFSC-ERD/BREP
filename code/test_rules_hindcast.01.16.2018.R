###### test rules hindcast
### script to test moderate and mod_lenient rules to see how often they would have been applied historically
### written by HW 01.16.2018

#1. create a table, columns months, rows years, values=SST in indicator in year month/year

####### load libraries ####
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
library(plyr)

##### read in indicator dfs ----> code for procesing these is in test_rules_hindcast.R ###### 
ob1_df=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/ob1_df.csv") %>% .[,2:15]
jas_df=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/jas_df.csv")%>% .[,2:15]
wb1_df=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/wb1_df.csv")%>% .[,2:15]
enso_df=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/enso_df.csv")%>% .[,2:15]

###### populating data frame with indicators and thresholds ###### 
df_empty=data.frame(matrix(NA,nrow=12,ncol=14))
colnames(df_empty)=c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
rownames(df_empty)=c("January","February","March","April","May","June","July","August","September","October","November","December")

## populate with best indicators
df_empty[1,]=wb1_df[1,]
df_empty[2,]=ob1_df[2,]
df_empty[3,]=wb1_df[3,]
df_empty[4,]=wb1_df[4,]
df_empty[5,]=wb1_df[5,]
df_empty[6,]=ob1_df[6,]
df_empty[7,]=jas_df[7,]
df_empty[8,]=jas_df[8,]
df_empty[9,]=ob1_df[9,]
df_empty[10,]=jas_df[10,]
df_empty[11,]=jas_df[11,]
df_empty[12,]=enso_df[12,]

## populate with best indicator name
df_empty[15]=NA
colnames(df_empty)[15]="Indicator"
df_empty[1,15]="wb1"
df_empty[2,15]="ob1"
df_empty[3,15]="wb1"
df_empty[4,15]="wb1"
df_empty[5,15]="wb1"
df_empty[6,15]="ob1"
df_empty[7,15]="jas"
df_empty[8,15]="jas"
df_empty[9,15]="ob1"
df_empty[10,15]="jas"
df_empty[11,15]="jas"
df_empty[12,15]="enso"

###### populate with moderate + mod_lenient (conservative and lenient are not longer relevant as of 01.16.18) ###### 

## get only turtle years: 2003,2005,2006,2013,2014,2015,2016
df_turtle=df_empty[,c(1,3,4,11:15)]
df_turtle[9]=apply(df_turtle[,1:7],1,mean)
df_turtle[10]=apply(df_turtle[,1:7],1,sd)
df_turtle[11]=1/2*df_turtle[10] ## half an sd
colnames(df_turtle)[9]="mean"
colnames(df_turtle)[10]="sd"
colnames(df_turtle)[11]="half_sd"
df_turtle[12]=df_turtle$mean+df_turtle$half_sd
colnames(df_turtle)[12]="mean_plus_half_sd"

df_empty[16]=NA
colnames(df_empty)[16]="moderate"
df_empty[16]=df_turtle[9] ###indicator values should be based on turtle years only

df_empty[17]=NA
colnames(df_empty)[17]="mod_lenient"
df_empty[17]=df_turtle[12] ###indicator values should be based on turtle years only

#### making open/closed tables to put in schematics #####

## moderate -------------------------------------> 
moderative=df_empty
moderative[1:14]=moderative[1:14]-moderative[,16]
empty=moderative

for(i in 1:12){
  for(ii in 1:14){
    print(moderative[i,ii])
    if(i==12 && moderative[i,ii]>=0){empty[1,ii]=("Closed")} ## december rule impacting jan closure
    if(i==12 && moderative[i,ii]<0){empty[1,ii]=("Open")} ## december rule impacting jan closure
    if(moderative[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(moderative[i,ii]<0){empty[i+1,ii]=("Open")}
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

moderate_wENSO=empty
write_csv(moderate_wENSO,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/moderate_wENSO.csv")

## mod_lenient -------------------------------------> 
mod_lenient=df_empty
mod_lenient[1:14]=mod_lenient[1:14]-mod_lenient[,17]
empty=mod_lenient

for(i in 1:12){
  for(ii in 1:14){
    print(mod_lenient[i,ii])
    if(i==12 && mod_lenient[i,ii]>=0){empty[1,ii]=("Closed")} ## december rule impacting jan closure
    if(i==12 && mod_lenient[i,ii]<0){empty[1,ii]=("Open")} ## december rule impacting jan closure
    if(mod_lenient[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(mod_lenient[i,ii]<0){empty[i+1,ii]=("Open")}
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

mod_lenient_wENSO=empty
write_csv(mod_lenient_wENSO,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/mod_lenient_wENSO.csv")

#### plotting relationships #####

## moderate -------------------------------------> 
cons=as.data.frame(t(moderate_wENSO))
colnames(cons)[13]="Closure_freq"
colnames(cons)[14]="Turtle_freq"
cons=cons[1:14,]
cons$Closure_freq=as.numeric(levels(cons$Closure_freq))[cons$Closure_freq]
cons$Turtle_freq=as.numeric(levels(cons$Turtle_freq))[cons$Turtle_freq]
chart=ggplot()+geom_smooth(data=cons,aes(x=Turtle_freq,y=Closure_freq),method='lm',se=F)
chart=chart+geom_point(data=cons,aes(x=Turtle_freq,y=Closure_freq))
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Annual turtle frequency")+labs(y="Number of months closed each year")
chart=chart+ggtitle("Relationship between turtle and closure frequency: Moderate Threshold")
chart=chart+annotate(x=400,y=14,label=paste("R=",round(cor(cons$Turtle_freq,cons$Closure_freq),2)),geom = "text",size=2)


pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/turt_closure_cor_mod_wENSO.pdf",width=6,height=6,pointsize=50)
chart
dev.off()

## mod_lenient -------------------------------------> 
cons=as.data.frame(t(mod_lenient_wENSO))
colnames(cons)[13]="Closure_freq"
colnames(cons)[14]="Turtle_freq"
cons=cons[1:14,]
cons$Closure_freq=as.numeric(levels(cons$Closure_freq))[cons$Closure_freq]
cons$Turtle_freq=as.numeric(levels(cons$Turtle_freq))[cons$Turtle_freq]
chart=ggplot()+geom_smooth(data=cons,aes(x=Turtle_freq,y=Closure_freq),method='lm',se=F)
chart=chart+geom_point(data=cons,aes(x=Turtle_freq,y=Closure_freq))
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Annual turtle frequency")+labs(y="Number of months closed each year")
chart=chart+ggtitle("Relationship between turtle and closure frequency: Mod_lenient Threshold")
chart=chart+annotate(x=400,y=14,label=paste("R=",round(cor(cons$Turtle_freq,cons$Closure_freq),2)),geom = "text",size=2)


pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/turt_closure_cor_mod_lenient_wENSO.pdf",width=6,height=6,pointsize=50)
chart
dev.off()

#### Evaluating conservative and moderate ENSO rules found in test_rules_historical_bycatch.01.16.18 #####
enso_anom=read.table("/Volumes/SeaGate/BREP/BREP/ENSO/detrend.nino34.ascii.txt",header = T) %>% filter(YR>=2003&YR<2017) %>% select(YR,MON,ANOM) %>% group_by(MON) %>% spread(YR,ANOM) %>% as.data.frame() %>% .[,2:ncol(.)]
colnames(enso_anom)=c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
rownames(enso_anom)=c("January","February","March","April","May","June","July","August","September","October","November","December")

df_empty=enso_anom
df_empty$Enso_conservative=0.4414286
df_empty$Enso_moderate=-0.1419075

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
write_csv(Enso_conservative,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/Enso_conservative.csv")

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
write_csv(Enso_conservative,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/Enso_conservative.csv")

