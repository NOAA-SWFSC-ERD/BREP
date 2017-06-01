#### code to run ts correlations
library(raster)
library(ncdf4)
library(maps)

for(rds in list.files(pattern="*."))

##rds locations: "/Volumes/SeaGate/BREP/BREP/monthly/sst-MM-mean
load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData") ## attach turtle citings time-series
  jan=readRDS(paste("/Volumes/SeaGate/BREP/BREP/monthly/sst-01-mean.rds",sep=""))
  
  transposed=as.data.frame(t(jan))
if("2002" %in% rownames(transposed)){
  transposed=transposed[,c(1:2,4:nrow(transposed))]
}

transposed$sightings_blanks=NA
transposed[3,7674402]=10 #2003
#transposed[4,7674402]=3 #2004
transposed[5,7674402]=1 #2005
transposed[6,7674402]=34 #2006
#transposed[7,7674402]=3 #2007
#transposed[8,7674402]=3 #2008
#transposed[9,7674402]=3 #2009
#transposed[10,7674402]=3 #2010
#transposed[11,7674402]=3 #2011
#transposed[12,7674402]=3 #2012
transposed[13,7674402]=1 #2013
transposed[14,7674402]=94 #2014
transposed[15,7674402]=469 #2015
transposed[16,7674402]=56 #2016

transposed=transposed[complete.cases(transposed[,7674402]),]
a=cor(transposed[,7674402],transposed[,1:7674401])
b=as.data.frame(t(a))
colnames(b)="Cor"
b$lon=jan$lon
b$lat=jan$lat
coordinates(b)=~lon+lat
netcdf=list.files("/Volumes/SeaGate/BREP/jplmur",pattern="*jplMURSST41mday_*",full.names = T)#names of netcdffiles
template_native=raster(netcdf[1])
extent(template_native)=c(-140,-108,18,42)
r=rasterize(b,template_native,field="Cor",fun=mean)

pal <- colorRampPalette(c("blue", "cyan", "yellow", "red", "yellow", "cyan", "blue"))
ncolors <- 100
breaks <- seq(-1,1,,ncolors+1)
image(r, col=pal(ncolors), breaks=breaks)
map("world", add=TRUE, lwd=2)
contour(r, add=TRUE, col="black",nlevels=3)


##### correlation with zeros
transposed$sightings_zeros=NA
transposed[3,7674403]=10 #2003
transposed[4,7674403]=0 #2004
transposed[5,7674403]=1 #2005
transposed[6,7674403]=34 #2006
transposed[7,7674403]=0 #2007
transposed[8,7674403]=0 #2008
transposed[9,7674403]=0 #2009
transposed[10,7674403]=0 #2010
transposed[11,7674403]=0 #2011
transposed[12,7674403]=0 #2012
transposed[13,7674403]=1 #2013
transposed[14,7674403]=94 #2014
transposed[15,7674403]=469 #2015
transposed[16,7674403]=56 #2016