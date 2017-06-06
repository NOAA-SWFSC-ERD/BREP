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

sum_ras=sum(jan,feb,mar,apr)

coords=matrix(c(-120,23,  ## define SST box
               -120,30,
               -118,30,
               -118,23,
               -120,23),
             ncol=2,byrow = T)

p=Polygon(coords)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

plot(sum_ras)
plot(sps,add=T)

month_list=unlist(list("-01-","-02-","-03-","-04-"))

DF=data.frame(month=character(),
              cor_blanks=double(),
              cor_zeros=double(),
              stringsAsFactors = F)

df_full=a8[,2:3]
                  
for(i in 1:4){
month=month_list[i]
print(month)
a=readRDS(paste0(getwd(),"/monthly/sst",month,"mean_sightings.rds"))

if(grepl("2017-01-16",rownames(a)[length(rownames(a))])){
  a=a[1:(nrow(a)-1),]
}

a3=as.data.frame(t(a))
#a2=a3[c(nrow(a3)-1:nrow(a3)),]
a4=a3[a3$lat>23 & a3$lat<30,]
a5=a4[a4$lon> -120 & a4$lon< -118,]
#a5[sightings_zeros,]=a3[nrow(a3),]


a6=as.data.frame(t(a5))
a6$spatial_average=rowMeans(a6,na.rm = T)
a7=as.data.frame(a6[,ncol(a6)])
rownames(a7)=rownames(a6)
colnames(a7)=paste0(month2,"_mean")
a7$sightings_blank=a[,ncol(a)-1]
a7$sightings_zero=a[,ncol(a)]
a7=a7[3:nrow(a7),]

# df_full[,(2+i)]=a7[,1]
# colnames(df_full)[2+i]=paste0(month2,"_mean")

nas=a7[!is.na(a7$sightings_blank),]
cor(nas[,1],nas$sightings_blank)

zeros=a7[!is.na(a7$sightings_zero),]
cor(zeros[,1],zeros$sightings_zero)

month2=gsub("-","",month)

DF[i,1]=month2
DF[i,2]=cor(nas[,1],nas$sightings_blank)
DF[i,3]=cor(zeros[,1],zeros$sightings_zero)

}

write.csv(df_full,"spatial.averages.csv")

