#### prepare sat data for correlation script
#0. unzip all files
#1. read in all netcdfs and turn into dataframe (cols = time, rows = pixels)
#2. append turtel ts
#3. rewrite ts function


library(chron)
library(sp)
library(rgdal)
library(raster)
library(ncdf4)
library(R.utils)
library(rworldmap)

#0. unzip all files
for(netcdf in list.files("/Volumes/SeaGate/BREP/jplmur")){
  print(netcdf)
  gunzip(paste("/Volumes/SeaGate/BREP/jplmur/",netcdf,sep=""),ext="gz", FUN=gzfile) # unzip the file
}


#1. read in all netcdfs and turn into dataframe (cols = time, rows = pixels)
##sst
saveRDS(tmp.df02,"Master_Empty.csv")

netcdf=list.files("/Volumes/SeaGate/BREP/jplmur",pattern="*jplMURSST41mday_*",full.names = T)#names of netcdffiles
template_native=raster(netcdf[1])
masterDF=readRDS("/Volumes/SeaGate/BREP/BREP/Master_Empty.csv")

### extract SST values from each pixel in each timeslice
for(nc in netcdf){
  print(nc)
  #ncc=paste(nc,".nc",sep="")
  ncc=nc
  ncin <- nc_open(ncc)
  print(ncin)
  dname="sst" # mean
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
  for(i in 3:ncol(tmp.df02)){
    if(!names(tmp.df02)[i] %in% colnames(masterDF)){
      print(names(tmp.df02)[i])
      masterDF[,names(tmp.df02)[i]]=tmp.df02[i]
    }
  }
}

## get list of data.frames in global environment
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls()))

