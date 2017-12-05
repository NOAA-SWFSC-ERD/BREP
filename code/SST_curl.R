#### script to download historical SST data evaluate rules against historical bycatch events and historical el nino based closures

#### curl from ERDDAP

# step 1. figure out historical loggerhead bycatch dates
# step 2. set up download for monthly ERDDAP data to cover dates

#####--- load libraries ####
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

#####--- step 1. figure out historical loggerhead bycatch dates ####
turts=read_csv("/Volumes/SeaGate/BREP/DGN_turtle_data_from_Steph/Catch_SeaTurtle_1990-2017.csv") %>% filter(SpCd=="CC") 
alldata=readRDS("/Volumes/SeaGate/BREP/DGN_turtle_data_from_Steph/DGN_allSets_1990-2017_ROMSExtracted.rds") %>% as.tibble() #%>% 
  separate(TripNumber_Set,c("TripNumber","Set"),sep="_")

turts_alldata=left_join(turts,alldata,by="TripNumber_Set")

trip=read_csv("/Volumes/SeaGate/BREP/DGN_turtle_data_from_Steph/Trip.csv") %>% as.tibble()
set=read_csv("/Volumes/SeaGate/BREP/DGN_turtle_data_from_Steph/Set.csv") %>% as.tibble() 
set$Set=as.numeric(set$Set)

set=unite(set,"TripNumber_Set",TripNumber,Set,sep="_")%>% unite("dt",Year,MM,DD,sep="-")
set$dt=as.Date(set$dt)
set=set[,c(1,2,3,8:15,51:54)]

turts_alldata2=left_join(turts,set,by="TripNumber_Set")

bycatch_dates=turts_alldata2$dt ### final dates ##### CRAP, supposed to be the preceeding month!!!
bycatch_dates=c("1992-12-16", "1998-07-16", "1998-07-16", "1998-07-16", "1992-03-16" ,"1992-12-16" ,"2006-09-16" ,"1997-07-16" ,"1997-09-16", "2001-07-16" ,"1993-07-16" ,"1993-07-16" ,"1992-12-16" ,"1992-06-16","1997-12-16" ,"1997-07-16", "1992-05-16") ## DOING BY HAND

bycatch_dates="1997-09-16"
#####--- step 2. set up download for monthly ERDDAP data to cover dates ####

### A. Pauses system for a period of time to allow url requests to go through
waitfor <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  print(proc.time() - p1) # The cpu usage should be negligible
}

########## erdPH2sstamday  (monthly mean)
setwd("/Volumes/SeaGate/BREP/erdPH2sstamday")  ######## change for each magine
i<-1
counter=0
time=3
while (i < length(bycatch_dates)){
  print(bycatch_dates[i])
  filenm<-paste("/Volumes/SeaGate/BREP/erdPH2sstamday/erdPH2sstamday_new_",bycatch_dates[i],".nc",sep="")
  url<-paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdPH2sstamday.nc?sea_surface_temperature[(",bycatch_dates[i],"):1:(",bycatch_dates[i],")][(60):1:(10)][(-180):1:(-100)]",sep="") ##check product name!!
  f = CFILE(filenm,mode="wb")
  curlPerform(url=url,writedata=f@ref) 
  close(f)
  i<-i+1
  counter <- sum(counter, 1)
  print(counter)
  if (is.na(file.info(filenm)$size)) {
    i<-i-1
  }
  if (file.info(filenm)$size < 2000){
    i<-i-1
  }
}


#####--- step 3. convert netcdfs to rasters ####
output_dir="/Volumes/SeaGate/BREP/erdPH2sstamday_raster"
netcdf=list.files("/Volumes/SeaGate/BREP/erdPH2sstamday",pattern="new",full.names = T)
template_native=raster(netcdf[1])

### extract SST values from each pixel in each timeslice
for(nc in netcdf){
  print(nc)
  #ncc=paste(nc,".nc",sep="")
  ncc=nc
  ncin <- nc_open(ncc)
  print(ncin)
  dname="sea_surface_temperature" 
  print("defining variables")
  lon <- ncvar_get(ncin, "longitude") # define longitude
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "latitude", verbose = F) # define latitude
  nlat <- dim(lat)
  t <- ncvar_get(ncin, "time") # define time field
  tunits <- ncatt_get(ncin, "time", "units") # get time units
  nt <- dim(t)
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
  dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
  fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
  print("changing date format")
  tustr <- strsplit(tunits$value, " ") #changing date format
  tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
  tmonth = as.integer(unlist(tdstr)[2]) #changing date format
  tday = as.integer(unlist(tdstr)[3]) #changing date format ## block out for chla
  tday=as.integer(gsub("T00:00:00Z","",unlist(tdstr)[3]))
  tyear = as.integer(unlist(tdstr)[1]) #changing date format
  tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
  tmp.vec.long <- as.vector(tmp.array)
  length(tmp.vec.long)
  tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
  print("Formatting column names")
  date1=as.character(as.POSIXlt(t,origin='1970-01-01',tz= "UTC"))
  print("Creating spatial dataframe")
  lonlat <- expand.grid(lon, lat)
  names(lonlat) <- c("lon","lat")
  tmp.df02 <- data.frame(tmp.mat)
  names(tmp.df02) <- date1
  tmp.df02 <- cbind(lonlat, tmp.df02)
  coordinates(tmp.df02)=~lon+lat
  print("converting to raster")
  for(n in names(tmp.df02)){
    print(n)
    a=strsplit(n,"-") 
    b=paste0(a[[1]][1],"-",a[[1]][2],"-16")
    path=paste(output_dir,"/new_mean_",b,".grd",sep="")
    print(path)
    if(file.exists(path)==FALSE){
      r=rasterize(tmp.df02,template_native,field=n,fun=mean) # points to raster
      writeRaster(r,paste(output_dir,"/new_mean_",b,sep=""),overwrite=TRUE)
    }
  }
  }


