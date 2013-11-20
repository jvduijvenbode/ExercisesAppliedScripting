library(rasta)
#load data
data(tura)
#extract the years of the layers
sceneinfo<-getSceneinfo(names(tura))
sceneinfo$year<-substr(sceneinfo$date,1,4)
tura<-tura/10000

tura2000<-calc(subset(tura,subset=which(sceneinfo$year==2000)),fun=mean,na.rm=T)
tura2005<-calc(subset(tura,subset=which(sceneinfo$year==2005)),fun=mean,na.rm=T)
tura2010<-calc(subset(tura,subset=which(sceneinfo$year==2010)),fun=mean,na.rm=T)
tura_sel<-brick(tura2000,tura2005,tura2010)
levelplot(tura_sel)
plot(tura2000)
nlayers(tura_sel)

coords1<-rbind(c(819131,829408),c(819131,829772),c(819579,829772),c(819579,829408),c(819131,829408))
coords2<-cbind(c(821152,821152,821582,821582,821152),c(829031,829452,829452,829031,829031))
e2005<-Polygon( coords=coords1)
e2010<-Polygon( coords=coords2)
le2005<-Polygons(list(e2005),ID="s1")
le2010<-Polygons(list(e2010),ID="s2")
e2005sp<-SpatialPolygons(list(le2005,le2010),proj4string=CRS(projection(tura[[1]])))
plotRGB(tura_sel,r=1,g=2,b=3,stretch="hist")
plot(e2005sp,add=T,col=c("red","yellow"))
extract(tura_sel,e2005sp)

