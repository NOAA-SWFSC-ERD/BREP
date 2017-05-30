library(raster)
library(maps)

load("/Users/elliotthazen/Dropbox/Documents/R/BREP-Logger/brep_alldata_sst.RData")

### calculate # sightings / year
datesighted<-format(brep.data$dtime, format="%Y")
numsightings<-table(datesighted)

r <- raster("~/Dropbox/shared folders/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/2012-08-01/analysed_sst.grd")
grd<-r

lon<-coordinates(grd)[,1]
lat<-coordinates(grd)[,2]

#files <- list.files("D:\\ECV\\2010", "*.envi", full.names = TRUE)
files <- dir("~/Dropbox/shared folders/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/", recursive=TRUE, full.names=TRUE, pattern="\\.grd$")
sstfiles<-files[grep("sst.grd",files)]
s <- stack(sstfiles[1:17])

corr.TS <- function(x, t=numsightings) { 
    return(cor(x,t) ) 
}
corr.raster<-calc(s, fun=corr.TS)
pal <- colorRampPalette(c("blue", "cyan", "yellow", "red", "yellow", "cyan", "blue"))
#pal <- colorRampPalette(c("purple4", "white", "blue"))
ncolors <- 100
breaks <- seq(-1,1,,ncolors+1)
image(corr.raster, col=pal(ncolors), breaks=breaks)
map("world", add=TRUE, lwd=2)
contour(corr.raster, add=TRUE, col="black",nlevels=3)


### write draft plot
png(paste0("/Users/elliotthazen/Dropbox/Documents/R/BREP-Logger/Correlation_map_TurtleTS.png"), width=7, height=5, units="in", res=400)
#x11(width=7, height=5)

par(ps=10) #settings before layout
layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)
#layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
par(cex=1) # layout has the tendency change par()$cex, so this step is important for control

par(mar=c(4,4,1,1)) # I usually set my margins before each plot
pal <- colorRampPalette(c("blue", "cyan", "yellow", "red", "yellow", "cyan", "blue"))
#pal <- colorRampPalette(c("purple4", "white", "blue"))
ncolors <- 100
breaks <- seq(-1,1,,ncolors+1)
image(corr.raster, col=pal(ncolors), breaks=breaks)
map("world", add=TRUE, lwd=2)
contour(corr.raster, add=TRUE, col="black",nlevels=3)
box()

par(mar=c(4,4,0,1)) # I usually set my margins before each plot
levs <- breaks[-1] - diff(breaks)/2
image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
mtext("Correlation [R]", side=1, line=2.5)
box()

dev.off() # closes device