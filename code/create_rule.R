### code to create a sample rule
# follows priority_areas.R

## sample rule 1. working off .75 classification

####### load libraries
library(raster)
library(sp)

####### load global objects
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


#### defining SST boxes
## A. Looking at persistence
sum_rasJFMA=sum(jan,feb,mar,apr)
sum_rasJAS=sum(jul,aug,sep)

## B. JFMA box 1
bJFMA1=matrix(c(-120,23,  ## define SST box
               -120,30,
               -118,30,
               -118,23,
               -120,23),
             ncol=2,byrow = T)

p=Polygon(bJFMA1)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(sum_rasJFMA)
plot(sps,add=T)

## c. JFMA box 2
bJFMA2=matrix(c(-134,35,  ## define SST box
               -134,38,
               -132,38,
               -132,35,
               -134,35),
             ncol=2,byrow = T)

p=Polygon(bJFMA2)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(sum_rasJFMA)
plot(sps,add=T)

# D. JAS box 1
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

plot(sum_rasJAS)
plot(sps,add=T)

month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))
 
calculate_cor=function(month_list,latMin,latMax,lonMin,lonMax,boxID){  
  DF=data.frame(month=character(),
                cor_blanks=double(),
                cor_zeros=double(),
                stringsAsFactors = F)
for(i in 1:12){
month=month_list[i]
print(month)
a=readRDS(paste0(getwd(),"/monthly/sst",month,"mean_sightings.rds"))

if(grepl("2017-01-16",rownames(a)[length(rownames(a))])){
  a=a[1:(nrow(a)-1),]
}

a3=as.data.frame(t(a))
a4=a3[a3$lat>latMin & a3$lat<latMax,]
a5=a4[a4$lon> lonMin & a4$lon< lonMax,]

month2=gsub("-","",month)

a6=as.data.frame(t(a5))
a6$spatial_average=rowMeans(a6,na.rm = T)
a7=as.data.frame(a6[,ncol(a6)])
rownames(a7)=rownames(a6)
colnames(a7)=paste0(month2,"_mean")
a7$sightings_blank=a[,ncol(a)-1]
a7$sightings_zero=a[,ncol(a)]
a7=a7[3:nrow(a7),]

nas=a7[!is.na(a7$sightings_blank),]
cor(nas[,1],nas$sightings_blank)

zeros=a7[!is.na(a7$sightings_zero),]
cor(zeros[,1],zeros$sightings_zero)

DF[i,1]=month2
DF[i,2]=cor(nas[,1],nas$sightings_blank)
DF[i,3]=cor(zeros[,1],zeros$sightings_zero)
}
  colnames(DF)[2]=paste0("cor_blanks",boxID)
  colnames(DF)[3]=paste0("cor_zeros",boxID)
  return(DF)
}
JFMA1=calculate_cor(month_list = month_list,latMin = 23, latMax = 30,lonMin = -120,lonMax = -118,boxID = "JFMA1")
JFMA2=calculate_cor(month_list = month_list,latMin = 35, latMax = 38,lonMin = -134,lonMax = -132,boxID = "JFMA2")
JAS1=calculate_cor(month_list = month_list,latMin = 23, latMax = 25,lonMin = -135,lonMax = -123,boxID = "JAS1")

df_full=cbind(JFMA1,JFMA2,JAS1)
write.csv(df_full,"/Volumes/SeaGate/BREP/BREP/schematics/3box_corr.csv")

