### script to write out anomalies at pngs
### trying to find a better current quantitative closure rule
## federal register indicates that they use anomalies from the second and third months preceeding a closure
# https://www.federalregister.gov/documents/2014/07/25/2014-17644/fisheries-off-west-coast-states-the-highly-migratory-species-fishery-closure

#### load libraries
library(tidyverse)
library(raster)
library(maps)

#### define functions
make_png=function(r,name){ ### does what it says
  
  png(paste0("/Volumes/SeaGate/BREP/BREP/monthly_plots_raw_png/",name,".png"), width=7, height=5, units="in", res=400)

  
  par(ps=10) #settings before layout
  layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)
  #layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
  par(cex=1) # layout has the tendency change par()$cex, so this step is important for control
  
  par(mar=c(4,4,1,1)) # I usually set my margins before each plot
  pal <- colorRampPalette(c("blue", "white", "red"))
  #pal <- colorRampPalette(c("purple4","blue", "cyan", "yellow", "red"))
  #pal <- colorRampPalette(c("purple4", "white", "blue"))
  ncolors <- 100
  breaks <- seq(-10,10,,ncolors+1)
  image(r, col=pal(ncolors), breaks=breaks)
  maps::map("world", add=TRUE, lwd=2)
  contour(r, add=TRUE, col="black",levels=c(-5,-1,1,5))
  box()
  
  par(mar=c(4,4,0,1)) # I usually set my margins before each plot
  levs <- breaks[-1] - diff(breaks)/2
  image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
  mtext(name, side=1, line=2.5)
  
  box()
  
  dev.off() # closes device
}

a=list.files("/Volumes/SeaGate/BREP/jplmur_raster",pattern="anom",full.names = T) %>% grep(".grd",.,value=T) %>% unlist()

for(aa in a){
  print(aa)
  r=raster(aa)
  name=strsplit(aa,"/") %>% unlist %>% .[6] %>% gsub(".grd","",.)
  make_png(r=r,name=name)
}
 