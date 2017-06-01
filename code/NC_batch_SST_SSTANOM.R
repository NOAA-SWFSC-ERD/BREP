#### prepare sat data for correlation script
#0. unzip all files
#1. read in all netcdfs and turn into dataframe (cols = time, rows = pixels)

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
#saveRDS(tmp.df02,"Master_Empty.csv")

output_dir="/Volumes/SeaGate/BREP/jplmur_raster"
netcdf=list.files("/Volumes/SeaGate/BREP/jplmur",pattern="*jplMURSST41mday_*",full.names = T)#names of netcdffiles
template_native=raster(netcdf[1])
#masterDF1=readRDS("/Volumes/SeaGate/BREP/BREP/Master_Empty.csv")
#masterDF2=readRDS("/Volumes/SeaGate/BREP/BREP/Master_Empty.csv")
masterDF3=readRDS("/Volumes/SeaGate/BREP/BREP/Master_Empty.csv")


#netcdf1=netcdf[1:58]
#netcdf2=netcdf[58:116]
netcdf3=netcdf[116:length(netcdf)]

### extract SST values from each pixel in each timeslice
for(nc in netcdf3){
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
  #coordinates(tmp.df02)=~lon+lat
  print("converting to raster")
  # for(n in names(tmp.df02)){
  #   print(n)
  #   path=paste(output_dir,"/mean_",n,".grd",sep="")
  #   if(file.exists(path)==FALSE){
  #     r=rasterize(tmp.df02,template_native,field=n,fun=mean) # points to raster 
  #     print(paste(output_dir,"/mean_",n,".grd",sep=""))
  #     writeRaster(r,paste(output_dir,"/mean_",n,sep=""),overwrite=TRUE)
  #   }
  # }
  for(i in 3:ncol(tmp.df02)){
  if(!names(tmp.df02)[i] %in% colnames(masterDF3)){
    print(names(tmp.df02)[i])
    masterDF3[,names(tmp.df02)[i]]=tmp.df02[i]
  }
    
  }
}

#saveRDS(masterDF1,"MasterDF1.csv")
#saveRDS(masterDF2,"MasterDF2.csv")
saveRDS(masterDF3,"MasterDF3.csv")


### merge RDS files into master list, seperate by months?
masterDF1=readRDS("MasterDF1.csv")
  masterDF1=masterDF1[masterDF1$lat>18 & masterDF1$lat<42,] ##subset to BREP area
  masterDF1=masterDF1[masterDF1$lon>-140 & masterDF1$lon < -108,]##subset to BREP area
masterDF2=readRDS("MasterDF2.csv")
  masterDF2=masterDF2[masterDF2$lat>18 & masterDF2$lat<42,]##subset to BREP area
  masterDF2=masterDF2[masterDF2$lon>-140 & masterDF2$lon < -108,]##subset to BREP area
masterDF3=readRDS("MasterDF3.csv")
  masterDF3=masterDF3[masterDF3$lat>18 & masterDF3$lat<42,]##subset to BREP area
  masterDF3=masterDF3[masterDF3$lon>-140 & masterDF3$lon < -108,]##subset to BREP area


  ### read in blank file to initiate monthly files
masterDF=readRDS("/Volumes/SeaGate/BREP/BREP/Master_Empty.csv")
masterDF_blank=masterDF[1:2]
  masterDF_blank=masterDF_blank[masterDF_blank$lat>18 & masterDF_blank$lat<42,]
  masterDF_blank=masterDF_blank[masterDF_blank$lon>-140 & masterDF_blank$lon < -108,]

month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))


##### start creating monthly data frames for correlation analysis
for(month in month_list){ ### first data frame only
  print(month)
  masterDF_blank2=masterDF_blank
    for(i in 3:ncol(masterDF1)){
      if(grepl(month,colnames(masterDF1)[i])){
        print(colnames(masterDF1)[i])
        masterDF_blank2[,colnames(masterDF1)[i]]=masterDF1[i]
        #paste0("df_",month)[,names(df)[i]]=df[i]
      }
    }
  assign(paste0("sst",month,"mean"),masterDF_blank2)
}


for(month in month_list){ ### second frame
  print(month)
  masterDF_blank2=get(paste0("sst",month,"mean"))
  for(i in 3:ncol(masterDF2)){
    if(grepl(month,colnames(masterDF2)[i])){
      print(colnames(masterDF2)[i])
      masterDF_blank2[,colnames(masterDF2)[i]]=masterDF2[i]
      #paste0("df_",month)[,names(df)[i]]=df[i]
    }
  }
  assign(paste0("sst",month,"mean"),masterDF_blank2)
}

#### write out the data frames as rds files because shit keeps falling apart
dfs=ls(pattern=("sst*"))
for(df in dfs){
  saveRDS(get(df),paste0(getwd(),"/monthly/",df,".rds"))
  print(df)
}

rm(masterDF1,masterDF2,masterDF) ## get rid of extra objects so hopefully r doesn't fall apart

for(month in month_list){ ### third data frame
  print(month)
  masterDF_blank2=get(paste0("sst",month,"mean"))
  for(i in 3:ncol(masterDF3)){
    if(grepl(month,colnames(masterDF3)[i])){
      print(colnames(masterDF3)[i])
      masterDF_blank2[,colnames(masterDF3)[i]]=masterDF3[i]
      #paste0("df_",month)[,names(df)[i]]=df[i]
    }
  }
  assign(paste0("sst",month,"mean"),masterDF_blank2)
}

#### write out the data frames as rds files with full data attached
dfs=ls(pattern=("sst*"))
for(df in dfs){
  saveRDS(get(df),paste0(getwd(),"/monthly/",df,".rds"))
  print(df)
}

