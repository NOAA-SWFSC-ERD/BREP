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

##### read in indicator dfs 
###### 
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

###### 

###### populating data frame with indicators and thresholds
###### 
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
###### 

##### testing out rules --------> just testing, ignore this section
###### 
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
###### 

#### making tables to put in schematics
###### 
###### ---------------> orignal rules, min and mean, all turtle years 
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
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[12,i]=a
}
empty[12,c(1:14)][is.na(empty[12,c(1:14)])]<-0

empty["Turtle freq",]=NA
empty[13,1]=10 #2003
empty[13,2]=0 #2004
empty[13,3]=1 #2005
empty[13,4]=34 #2006
empty[13,5]=0 #2007
empty[13,6]=0 #2008
empty[13,7]=0 #2009
empty[13,8]=0 #2010
empty[13,9]=0 #2011
empty[13,10]=0 #2012
empty[13,11]=1 #2013
empty[13,12]=94 #2014
empty[13,13]=469 #2015
empty[13,14]=56 #2016

conservative_w2015=empty
write_csv(conservative_w2015,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/conservative_w2015.csv")
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

empty=empty[2:nrow(empty),]
empty["Closure freq",]=NA
for(i in 1:14){
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[12,i]=a
}
empty[12,c(1:14)][is.na(empty[12,c(1:14)])]<-0

empty["Turtle freq",]=NA
empty[13,1]=10 #2003
empty[13,2]=0 #2004
empty[13,3]=1 #2005
empty[13,4]=34 #2006
empty[13,5]=0 #2007
empty[13,6]=0 #2008
empty[13,7]=0 #2009
empty[13,8]=0 #2010
empty[13,9]=0 #2011
empty[13,10]=0 #2012
empty[13,11]=1 #2013
empty[13,12]=94 #2014
empty[13,13]=469 #2015
empty[13,14]=56 #2016

moderate_w2015=empty
write_csv(moderate_w2015,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/moderate_w2015.csv")
###### 

#### plotting relationships
######
cons=as.data.frame(t(conservative_w2015))
colnames(cons)[12]="Closure_freq"
colnames(cons)[13]="Turtle_freq"
cons=cons[1:14,]
cons$Closure_freq=as.numeric(levels(cons$Closure_freq))[cons$Closure_freq]
cons$Turtle_freq=as.numeric(levels(cons$Turtle_freq))[cons$Turtle_freq]
chart=ggplot()+geom_smooth(data=cons,aes(x=Turtle_freq,y=Closure_freq),method='lm',se=F)
chart=chart+geom_point(data=cons,aes(x=Turtle_freq,y=Closure_freq))
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Annual turtle frequency")+labs(y="Number of months closed each year")
chart=chart+ggtitle("Relationship between turtle and closure frequency: Conservative Threshold")
chart=chart+annotate(x=400,y=14,label=paste("R=",round(cor(cons$Turtle_freq,cons$Closure_freq),2)),geom = "text",size=2)

pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/turt_closure_cor_cons.pdf",width=6,height=6,pointsize=50)
chart
dev.off()


cons=as.data.frame(t(moderate_w2015))
colnames(cons)[12]="Closure_freq"
colnames(cons)[13]="Turtle_freq"
cons=cons[1:14,]
cons$Closure_freq=as.numeric(levels(cons$Closure_freq))[cons$Closure_freq]
cons$Turtle_freq=as.numeric(levels(cons$Turtle_freq))[cons$Turtle_freq]
chart=ggplot()+geom_smooth(data=cons,aes(x=Turtle_freq,y=Closure_freq),method='lm',se=F)
chart=chart+geom_point(data=cons,aes(x=Turtle_freq,y=Closure_freq))
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Annual turtle frequency")+labs(y="Number of months closed each year")
chart=chart+ggtitle("Relationship between turtle and closure frequency: Moderate Threshold")
chart=chart+annotate(x=400,y=14,label=paste("R=",round(cor(cons$Turtle_freq,cons$Closure_freq),2)),geom = "text",size=2)


pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/turt_closure_cor_mod.pdf",width=6,height=6,pointsize=50)
chart
dev.off()


############## ----------------- >>>>>>>>>>>>>>>>>>>>> new work 11/21/18, adding in a lenient threshold (+SD) to test against moderate threshold
df_empty=read.csv("/Volumes/SeaGate/BREP/BREP/set_in_indicators/best_indicator_all_years.csv")

df_turtle=df_empty[,c(2,4,5,12:18)] ## getting only years that have turtles
df_turtle[11]=apply(df_turtle[,1:7],1,sd)
colnames(df_turtle)[11]="sd"
df_turtle[12]=df_turtle$moderate+df_turtle$sd
df_turtle[13]=1/2*df_turtle[11] ## half an sd
df_turtle[14]=df_turtle$moderate+df_turtle$sd.1

df_empty[19]=NA
colnames(df_empty)[19]="lenient"
df_empty[19]=df_turtle[12] ### lenient = mean (moderature) + 1 SD

df_empty[20]=NA
colnames(df_empty)[20]="mod_lenient"
df_empty[20]=df_turtle[14] ### lenient = mean (moderature) + 1/2 SD


## lenient -------------------------------------> gonna have to handle ENSO separately
lenient=df_empty[2:ncol(df_empty)]
lenient[1:14]=lenient[1:14]-lenient[,18]
empty=lenient

for(i in 1:11){
  for(ii in 1:14){
    print(lenient[i,ii])
    
    if(lenient[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(lenient[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}

empty=empty[2:nrow(empty),]
empty["Closure freq",]=NA
for(i in 1:14){
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[12,i]=a
}
empty[12,c(1:14)][is.na(empty[12,c(1:14)])]<-0

empty["Turtle freq",]=NA
empty[13,1]=10 #2003
empty[13,2]=0 #2004
empty[13,3]=1 #2005
empty[13,4]=34 #2006
empty[13,5]=0 #2007
empty[13,6]=0 #2008
empty[13,7]=0 #2009
empty[13,8]=0 #2010
empty[13,9]=0 #2011
empty[13,10]=0 #2012
empty[13,11]=1 #2013
empty[13,12]=94 #2014
empty[13,13]=469 #2015
empty[13,14]=56 #2016

lenient_w2015=empty
write_csv(lenient_w2015,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/lenient_w2015.csv")


## mod_lenient -------------------------------------> gonna have to handle ENSO separately 
mod_lenient=df_empty[2:ncol(df_empty)]
mod_lenient[1:14]=mod_lenient[1:14]-mod_lenient[,19]
empty=mod_lenient

for(i in 1:11){
  for(ii in 1:14){
    print(mod_lenient[i,ii])
    
    if(mod_lenient[i,ii]>=0){empty[i+1,ii]=("Closed")}
    if(mod_lenient[i,ii]<0){empty[i+1,ii]=("Open")}
  }
  
}

empty=empty[2:nrow(empty),]
empty["Closure freq",]=NA
for(i in 1:14){
  a=as.data.frame(table(empty[,i]))%>%.[.$Var1=="Closed",]%>%.[1,2]
  print(a)
  empty[12,i]=a
}
empty[12,c(1:14)][is.na(empty[12,c(1:14)])]<-0

empty["Turtle freq",]=NA
empty[13,1]=10 #2003
empty[13,2]=0 #2004
empty[13,3]=1 #2005
empty[13,4]=34 #2006
empty[13,5]=0 #2007
empty[13,6]=0 #2008
empty[13,7]=0 #2009
empty[13,8]=0 #2010
empty[13,9]=0 #2011
empty[13,10]=0 #2012
empty[13,11]=1 #2013
empty[13,12]=94 #2014
empty[13,13]=469 #2015
empty[13,14]=56 #2016

mod_lenient_w2015=empty
write_csv(mod_lenient_w2015,"/Volumes/SeaGate/BREP/BREP/set_in_indicators/mod_lenient_w2015.csv")


#### plotting relationships
######
cons=as.data.frame(t(mod_lenient_w2015))
colnames(cons)[12]="Closure_freq"
colnames(cons)[13]="Turtle_freq"
cons=cons[1:14,]
cons$Closure_freq=as.numeric(levels(cons$Closure_freq))[cons$Closure_freq]
cons$Turtle_freq=as.numeric(levels(cons$Turtle_freq))[cons$Turtle_freq]
chart=ggplot()+geom_smooth(data=cons,aes(x=Turtle_freq,y=Closure_freq),method='lm',se=F)
chart=chart+geom_point(data=cons,aes(x=Turtle_freq,y=Closure_freq))
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Annual turtle frequency")+labs(y="Number of months closed each year")
chart=chart+ggtitle("Relationship between turtle and closure frequency: Mod_lenient Threshold")
chart=chart+annotate(x=400,y=14,label=paste("R=",round(cor(cons$Turtle_freq,cons$Closure_freq),2)),geom = "text",size=2)

pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/turt_closure_cor_mod_len.pdf",width=6,height=6,pointsize=50)
chart
dev.off()


cons=as.data.frame(t(lenient_w2015))
colnames(cons)[12]="Closure_freq"
colnames(cons)[13]="Turtle_freq"
cons=cons[1:14,]
cons$Closure_freq=as.numeric(levels(cons$Closure_freq))[cons$Closure_freq]
cons$Turtle_freq=as.numeric(levels(cons$Turtle_freq))[cons$Turtle_freq]
chart=ggplot()+geom_smooth(data=cons,aes(x=Turtle_freq,y=Closure_freq),method='lm',se=F)
chart=chart+geom_point(data=cons,aes(x=Turtle_freq,y=Closure_freq))
chart=chart+theme(panel.background = element_blank())+ theme(axis.line = element_line(colour = "black"))+ theme(text = element_text(size=7))
chart=chart+labs(x="Annual turtle frequency")+labs(y="Number of months closed each year")
chart=chart+ggtitle("Relationship between turtle and closure frequency: Lenient Threshold")
chart=chart+annotate(x=400,y=14,label=paste("R=",round(cor(cons$Turtle_freq,cons$Closure_freq),2)),geom = "text",size=2)


pdf("/Volumes/SeaGate/BREP/BREP/set_in_indicators/turt_closure_cor_len.pdf",width=6,height=6,pointsize=50)
chart
dev.off()



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
