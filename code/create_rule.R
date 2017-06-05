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

