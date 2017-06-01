#### code to run ts correlations
library(raster)
library(ncdf4)
library(maps)

load("/Volumes/SeaGate/BREP/BREP/brep_scb_CC_pts_enso34.RData") ## attach turtle citings time-series
netcdf=list.files("/Volumes/SeaGate/BREP/jplmur",pattern="*jplMURSST41mday_*",full.names = T)#names of netcdffiles
template_native=raster(netcdf[1])
e=extent(-140,-108,18,42)

for(rdss in list.files("/Volumes/SeaGate/BREP/BREP/monthly",pattern="*.rds")){
  name=gsub(".rds","",rdss)
  rds=readRDS(paste("/Volumes/SeaGate/BREP/BREP/monthly/",rdss,sep=""))
  transposed=as.data.frame(t(rds))
  if(grepl("2002",rownames(transposed)[3])){ ### get rid of 2002 data, only 6 months OTY have data from 2002
    transposed=transposed[c(1:2,4:nrow(transposed)),]
  }
 
  ######## NAs for years missing data
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


######## zeros for years missing data
transposed$sightings_zeros=NA
transposed[3,7674403]=10 #2003
transposed[4,7674403]=0 #2004
transposed[5,7674403]=1 #2005
transposed[6,7674403]=34 #2006
transposed[7, 7674403] = 0 #2007
transposed[8,7674403]=0 #2008
transposed[9,7674403]=0 #2009
transposed[10,7674403]=0 #2010
transposed[11,7674403]=0 #2011
transposed[12,7674403]=0 #2012
transposed[13,7674403]=1 #2013
transposed[14,7674403]=94 #2014
transposed[15,7674403]=469 #2015
transposed[16,7674403]=56 #2016

## blanks
transposed=transposed[complete.cases(transposed[,7674402]),]
a=cor(transposed[,7674402],transposed[,1:7674401])
b=as.data.frame(t(a))
colnames(b)="Cor"
b$lon=rds$lon
b$lat=rds$lat
coordinates(b)=~lon+lat
r=rasterize(b,template_native,field="Cor",fun=mean)
r=crop(r,e)

make_png=function(r,name){

png(paste0("/Volumes/SeaGate/BREP/BREP/monthly_plots/",name,".png"), width=7, height=5, units="in", res=400)

par(ps=10) #settings before layout
layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)
#layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
par(cex=1) # layout has the tendency change par()$cex, so this step is important for control

par(mar=c(4,4,1,1)) # I usually set my margins before each plot
#pal <- colorRampPalette(c("blue", "grey", "red"))
pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
#pal <- colorRampPalette(c("purple4", "white", "blue"))
ncolors <- 100
breaks <- seq(-1,1,,ncolors+1)
image(r, col=pal(ncolors), breaks=breaks)
map("world", add=TRUE, lwd=2)
contour(r, add=TRUE, col="black",nlevels=3)
box()

par(mar=c(4,4,0,1)) # I usually set my margins before each plot
levs <- breaks[-1] - diff(breaks)/2
image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
mtext(paste0("Correlation [R], ",name,", 2003-2016"), side=1, line=2.5)

box()

dev.off() # closes device
}

