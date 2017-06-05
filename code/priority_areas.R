## Code to take monthly rasters and search for highly correlated areas
# follows corellation_DF.R

#1. for each monthly raster (NA and zeros, n=24), turn binary for each of >.75, >.8, >.9
#2. for each month, find common areas for each binary iteration
#3. map out

################ load libraries
library(raster)


################ load global objects
rasdir="/Volumes/SeaGate/BREP/BREP/monthly_plots/"
savedir="/Volumes/SeaGate/BREP/BREP/priority_plots/" #;dir.create(savedir)

rec_list_75=c(-1,.75,0,.75,1,1)
reclm_75=matrix(rec_list_75,ncol=3,byrow = T)

rec_list_8=c(-1,.8,0,.8,1,1)
reclm_8=matrix(rec_list_8,ncol=3,byrow = T)

rec_list_9=c(-1,.9,0,.9,1,1)
reclm_9=matrix(rec_list_9,ncol=3,byrow = T)

rec_list_priority=c(0,1.9,0,1.9,2,1)
reclm_P=matrix(rec_list_priority,ncol=3,byrow = T)

month_list=unlist(list("-01-","-02-","-03-","-04-","-05-","-06-","-07-","-08-","-09-","-10-","-11-","-12-"))

################ define functions
make_png=function(r,name,blanks){ ### does what it says
  
  if(blanks=="NA"){
    png(paste0("/Volumes/SeaGate/BREP/BREP/monthly_plots/",name,"_NA.png"), width=7, height=5, units="in", res=400)
  }
  if(blanks=="zeros"){
    png(paste0("/Volumes/SeaGate/BREP/BREP/monthly_plots/",name,"_zeros.png"), width=7, height=5, units="in", res=400)
  }
  
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
  contour(r, add=TRUE, col="black",levels=c(-.75,-.5,.5,.75))
  box()
  
  par(mar=c(4,4,0,1)) # I usually set my margins before each plot
  levs <- breaks[-1] - diff(breaks)/2
  image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
  if(blanks=="NA"){
    mtext(paste0("Correlation [R], ",name,", 2003-2016, years with no sightings removed"), side=1, line=2.5)
  }
  if(blanks=="zeros"){
    mtext(paste0("Correlation [R], ",name,", 2003-2016, years with no sightings zeroed"), side=1, line=2.5)
  }
  
  box()
  
  dev.off() # closes device
}


priority_area=function(month,rasdir,savedir){
  
  na_ras=raster(paste0(rasdir,"sst",month,"mean_NA.grd")) # reclassify NA raster
  na_ras_rec_75=reclassify(na_ras,reclm_75)
  na_ras_rec_8=reclassify(na_ras,reclm_8)
  na_ras_rec_9=reclassify(na_ras,reclm_9)
  
  zero_ras=raster(paste0(rasdir,"sst",month,"mean_zeros.grd")) # reclassify zero raster
  zero_ras_rec_75=reclassify(zero_ras,reclm_75)
  zero_ras_rec_8=reclassify(zero_ras,reclm_8)
  zero_ras_rec_9=reclassify(zero_ras,reclm_9)
  
  sum_75=sum(na_ras_rec_75,zero_ras_rec_75)
  sum_75_rec=reclassify(sum_75,reclm_P)
  sum_8=sum(na_ras_rec_8,zero_ras_rec_8)
  sum_8_rec=reclassify(sum_8,reclm_P)
  sum_9=sum(na_ras_rec_9,zero_ras_rec_9)
  sum_9_rec=reclassify(sum_9,reclm_P)
  
  writeRaster(sum_75,filename = paste0(savedir,"/mean",month,"75.grd"),overwrite=T)
  writeRaster(sum_8,filename = paste0(savedir,"/mean",month,"8.grd"),overwrite=T)
  writeRaster(sum_9,filename = paste0(savedir,"/mean",month,"9.grd"),overwrite=T)
  
  
  
}