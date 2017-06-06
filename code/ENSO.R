## checking SST box again ENSO index
# index downloaded from: http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ONI_change.shtml

library(dplyr)
library(reshape)

DF=data.frame(month=character(),
              cor_blanks_box=double(),
              cor_zeros_box=double(),
              cor_blanks_enso=double(),
              cor_zeros_enso=double(),
              stringsAsFactors = F)

month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))

##1. read in ENSO index

ENSO=read.table("/Volumes/SeaGate/BREP/BREP/ENSO/detrend.nino34.ascii.txt",header = T)
enso1=ENSO[ENSO$YR>2002 & ENSO$YR < 2017,]

enso1$year=enso1$YR

load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData")

enso2=merge(enso1,scb.pts.freq,by="year",all.x=T)
enso2.5=enso2[,c(2,3,6,7)]

### zeros
enso_obs=cast(enso2.5,YR~MON,value = "nObs")
enso_obs[is.na(enso_obs)]<-0
rownames(enso_obs)=enso_obs$YR
enso_obs=enso_obs[,2:ncol(enso_obs)]

enso_index=cast(enso2.5,YR~MON,value = "ANOM")
rownames(enso_index)=enso_index$YR
enso_index=enso_index[,2:ncol(enso_index)]

for(i in 1:12){
  x=enso_index[,i]
  y=enso_obs[,i]
  DF[i,1]=month_list[i]
  DF[i,5]=cor(x,y)
}



### NAs
enso_obs=cast(enso2.5,YR~MON,value = "nObs")
enso_index=cast(enso2.5,YR~MON,value = "ANOM")
enso3=cbind(enso_index,enso_obs)
enso4=enso3[complete.cases(enso3),]


enso_obs=enso4[,1:13]
rownames(enso_obs)=enso_obs$YR
enso_obs=enso_obs[,2:ncol(enso_obs)]

enso_index=enso4[,14:ncol(enso4)]
rownames(enso_index)=enso_index$YR
enso_index=enso_index[,2:ncol(enso_index)]

for(i in 1:12){
  x=enso_index[,i]
  y=enso_obs[,i]
  DF[i,1]=month_list[i]
  DF[i,4]=cor(x,y)
}

write.csv(DF,"enso_cor.csv")
