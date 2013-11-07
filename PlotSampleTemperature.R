#Name: Jonas van Duijvenbode
#Date: 07-11-2013

#set to directory with rasta data in a data folder
setwd("D:/appliedscripting/Lesson3")
#activate necessary rasters
library(raster)
library(raster)
filepath<-system.file("extdata","anom.2000.03.tiff",package="rasta")
#read kenya outlines as SPDF
kenya<-readOGR(dsn="data",layer="kenya")
#read anomaly raster 
AnomRaster<-raster(filepath)
#crop anomaly raster
AnomRasterC<-crop(AnomRaster,kenya)
#create randomly distributed random points
samppoints<-runifpoint(30,win=as.vector(t(bbox(kenya))))
samppoints$values<-raster::extract(AnomRasterC,data.frame(samppoints))
#calculate mean temperature
meanTemp<-round(mean(samppoints$values,na.rm=T),2)
meanTemp
#calculate standard deviation of temperature
sdTemp<-round(sd(samppoints$values,na.rm=T),2)
#plot the map
plot(AnomRasterC)
plot(samppoints,add=T)
mtext(side=3,line=1,"Sample temperature in Kenya",cex=2)
text(x=38.08,y=5,labels=paste("Mean temperature value: ",meanTemp,"\n","SD Temperature: ",sdTemp), cex=0.7,col="blue")