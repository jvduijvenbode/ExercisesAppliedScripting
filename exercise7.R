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
plotRGB(tura_sel,r=1,g=2,b=3,stretch="hist")
matrix(c(819131,819579,829408,829772),ncol=2,nrow=2)
e2005<-SpatialPolygons(coords=as.matrix(c(819131,819579,829408,829772),ncol=2,nrow=2))
e2010<-c(821049,821581,829003,829417)