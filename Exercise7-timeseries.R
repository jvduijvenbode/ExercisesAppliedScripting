library(rasta)
#load data
data(tura)
#extract the years of the layers
sceneinfo<-getSceneinfo(names(tura))
sceneinfo$year<-substr(sceneinfo$date,1,4)
tura<-tura/10000
#create subset of the individual years and calculate mean per year
tura2000<-calc(subset(tura,subset=which(sceneinfo$year==2000)),fun=mean,na.rm=T)
tura2005<-calc(subset(tura,subset=which(sceneinfo$year==2005)),fun=mean,na.rm=T)
tura2010<-calc(subset(tura,subset=which(sceneinfo$year==2010)),fun=mean,na.rm=T)
tura_sel<-brick(tura2000,tura2005,tura2010)
#plot the three maps for individual years
levelplot(tura_sel)



#create spatial polygons of user-defined extent
#the focus is on areas that have seen a lot of change in NDVI between 2000-2005, and 2005-2010 respectively
coords1<-rbind(c(819131,829408),c(819131,829772),c(819579,829772),c(819579,829408),c(819131,829408))
coords2<-cbind(c(821152,821152,821582,821582,821152),c(829031,829452,829452,829031,829031))
e2005<-Polygon( coords=coords1)
e2010<-Polygon( coords=coords2)
le2005<-Polygons(list(e2005),ID="s1")
le2010<-Polygons(list(e2010),ID="s2")
e2005sp<-SpatialPolygons(list(le2005,le2010),proj4string=CRS(projection(tura[[1]])))
#plot the composite RGB plot with selected Area's of interest
plotRGB(tura_sel,r=1,g=2,b=3,stretch="hist")
plot(e2005sp,add=T,col=c("red","yellow"))


#get NDVI values from selected areas
NDVIpArea<-extract(tura,e2005sp)
NDVIpA<-as.data.frame(t(rbind(colMeans(as.data.frame(NDVIpArea[[1]])),colMeans(as.data.frame(NDVIpArea[[2]])))))
NDVIpA<-cbind(NDVIpA,year=sceneinfo$year)
names(NDVIpA)<-c("Area2005","Area2010","year")
#melt the data so the graphs can be plotted and then plot the graphs
MNDVI<-melt(NDVIpA, id=c("year"))
ggpl <- ggplot(data = MNDVI, aes(x=year,y=value))+geom_point(na.rm=T)
ggpl+facet_wrap(~variable)+
  labs(y="NDVI")+
  scale_y_continuous(limits=c(0.5,1))+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45),legend.position=c(0.85,0.85))
