###### test rules hindcast
### script to test moderate and conservative rules to see how often they would have been applied historically

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

##### read in indicator dfs ####
df_list=list.files("/Volumes/SeaGate/BREP/BREP/set_in_indicators",pattern=".csv$",full.names = T)

for(df in df_list){
  a=read.csv(df)
  if(nrow(a)==14){
    a=as.data.frame(t(a))
    a=a[2:nrow(a)-1,]
    colnames(a)=c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
    a=a[2:nrow(a),]
    }
  if(nrow(a)==13){
    a=a[1:12,]
    a=a[,3:ncol(a)]
    colnames(a)=c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
    rownames(a)=c("January","February","March","April","May","June","July","August","September","October","November","December")
    }
  
  b=gsub("/Volumes/SeaGate/BREP/BREP/set_in_indicators/","",df)%>%gsub(".csv","",.)
  assign(b,a)
}
rm(df,a,b)

#### writing out in standard formate
write.csv(ob1_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/ob1_df.csv")
write.csv(jas_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/jas_df.csv")
write.csv(wb1_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/wb1_df.csv")
write.csv(enso_df,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/enso_df.csv")

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

## populate with moderate +conservative -------------------------------------> CHECK WHY THESE DON"T ALLIGN W PLOTS (because all years are in)
df_empty[16]=NA
colnames(df_empty)[16]="conservative"
df_empty[16]=apply(df_empty[,1:14],1,min)

df_empty[17]=NA
colnames(df_empty)[17]="moderate"
df_empty[17]=apply(df_empty[,1:14],1,mean)

## get only turtle years: 2003,2005,2006,2013,2014,2015,2016
df_turtle=df_empty[,c(1,3,4,11:17)]
df_turtle[9]=apply(df_turtle[,1:7],1,min)
df_turtle[10]=apply(df_turtle[,1:7],1,mean)

df_empty[16]=df_turtle[9] ###indicator values should be based on turtle years only
df_empty[17]=df_turtle[10] ###indicator values should be based on turtle years only

write.csv(df_empty,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/best_indicator_all_years.csv")

##### testing out rules #####
## conservative -------------------------------------> gonna have to handle ENSO separately
conservative=df_empty
conservative[1:14]=conservative[1:14]-conservative[,16]

for(i in 1:12){
  for(ii in 1:14){
   print(conservative[i,ii])
    
    if(conservative[i,ii]>=0){conservative[i,ii]=("Closed")}
    if(conservative[i,ii]<0){conservative[i,ii]=("Open")}
  }
  
}

## moderative -------------------------------------> gonna have to handle ENSO separately
moderative=df_empty
moderative[1:14]=moderative[1:14]-moderative[,17]

for(i in 1:12){
  for(ii in 1:14){
    print(moderative[i,ii])
    
    if(moderative[i,ii]>=0){moderative[i,ii]=("Closed")}
    if(moderative[i,ii]<0){moderative[i,ii]=("Open")}
  }
  
}

###### both of these rules are insane. removing 2015 which is an anomaly
df_turtle2=df_turtle[,c(1:5,7:ncol(df_turtle))]
df_turtle2[8]=apply(df_turtle2[,1:6],1,min)
df_turtle2[9]=apply(df_turtle2[,1:6],1,mean)

df_empty[16]=df_turtle2[8] ###indicator values should be based on turtle years only
df_empty[17]=df_turtle2[9] ###indicator values should be based on turtle years only

## conservative -------------------------------------> gonna have to handle ENSO separately
conservative=df_empty
conservative[1:14]=conservative[1:14]-conservative[,16]

for(i in 1:12){
  for(ii in 1:14){
    print(conservative[i,ii])
    
    if(conservative[i,ii]>=0){conservative[i,ii]=("Closed")}
    if(conservative[i,ii]<0){conservative[i,ii]=("Open")}
  }
  
}

## moderative -------------------------------------> gonna have to handle ENSO separately
moderative=df_empty
moderative[1:14]=moderative[1:14]-moderative[,17]

for(i in 1:12){
  for(ii in 1:14){
    print(moderative[i,ii])
    
    if(moderative[i,ii]>=0){moderative[i,ii]=("Closed")}
    if(moderative[i,ii]<0){moderative[i,ii]=("Open")}
  }
  
}





#### making tables to put in schematics ######
###### ---------------> orignal rules, min and mean, all turtle years #####
df_turtle=df_empty[,c(1,3,4,11:17)]
df_turtle[9]=apply(df_turtle[,1:7],1,min)
df_turtle[10]=apply(df_turtle[,1:7],1,mean)

df_empty[16]=df_turtle[9] ###indicator values should be based on turtle years only
df_empty[17]=df_turtle[10] ###indicator values should be based on turtle years only

## conservative -------------------------------------> gonna have to handle ENSO separately
conservative=df_empty
conservative[1:14]=conservative[1:14]-conservative[,16]
empty=conservative

for(i in 1:11){
  for(ii in 1:14){
    print(conservative[i,ii])
    
    if(conservative[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(conservative[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}

empty=empty[2:nrow(empty),]
empty["Closure freq",]=NA
for(i in 1:14){
  a=sum(empty[,i]=="Closed")
  print(a)
  empty[12,i]=a
}


conservative_w2015=empty[2:nrow(empty),]
## moderative -------------------------------------> gonna have to handle ENSO separately
moderative=df_empty
moderative[1:14]=moderative[1:14]-moderative[,17]
empty=moderative

for(i in 1:11){
  for(ii in 1:14){
    print(moderative[i,ii])
    
    if(moderative[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(moderative[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}
moderate_w2015=empty[2:nrow(empty),]

###### ---------------> new rules, min and mean, all turtle years w.o. 2015 ##### NEVERMIND, THESE GET WORSE ####
df_turtle2=df_turtle[,c(1:5,7:ncol(df_turtle))]
df_turtle2[8]=apply(df_turtle2[,1:6],1,min)
df_turtle2[9]=apply(df_turtle2[,1:6],1,mean)

df_empty[16]=df_turtle2[8] ###indicator values should be based on turtle years only
df_empty[17]=df_turtle2[9] ###indicator values should be based on turtle years only

## conservative -------------------------------------> gonna have to handle ENSO separately
conservative=df_empty
conservative[1:14]=conservative[1:14]-conservative[,16]
empty=conservative

for(i in 1:11){
  for(ii in 1:14){
    print(conservative[i,ii])
    
    if(conservative[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(conservative[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}
conservative_no2015=empty[2:nrow(empty),]

## moderative -------------------------------------> gonna have to handle ENSO separately
moderative=df_empty
moderative[1:14]=moderative[1:14]-moderative[,17]
empty=moderative

for(i in 1:11){
  for(ii in 1:14){
    print(moderative[i,ii])
    
    if(moderative[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(moderative[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}
moderate_no2015=empty[2:nrow(empty),]
