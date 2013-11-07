rm(list = ls())
#ls()
a<-1
class(a)
#dir.create("D:/Lesson1")
#dir.create("data")
setwd(file.path("D:","Lesson1"))
#getwd()
datdir="data"
add <- function(x=5){
  z<-x+1
  return(paste("x=",x," z=",z,sep=""))
  #return(c(x,z))
}
add<- function(x=5){return(c(x,x+1))}
add()
add(6)
newfunc<- function(x,y){
  z <-2*x+y
  return(c(z,x,y))
}
a2b<-newfunc(2,4)
a2b
rm(a,newfunc,a2b)
install.packages("rasta",repos="http://R-Forge.R-project.org")
download.file("http://rasta.r-forge.r-project.org/rasta_0.7.zip",file.path(datdir,"rasta_0.7.zip"))
update.packages(checkBuilt = T,ask=F)
install.packages("raster", dependencies=T)
install.packages(file.path("data","rasta_0.7.zip"),repos=NULL,dependencies=T)
install.packages("reshape",dependencies=T)
install.packages("bitops",dependencies=T)
install.packages("ggplot2",dependencies=T)
install.packages("ggmap",dependencies=T)
install.packages("spdep",dependencies=T)
install.packages("randomForest",dependencies=T)
#install.packages("spatstat",dependencies=T)

library(rasta)

f<- system.file(file.path("extdata","kenpop89to99.csv"),package="rasta")
mydat=read.csv(f)
names(mydat)[1:3]
summary(mydat$Y89Pop)[1:3]
head(mydat$Y89Births)[1:2]
tail(mydat$Y89Pop)[1:3]
class(mydat)
#mydat is a data.frame()
#we can write it as a csv by using the write.csv() function
myreg<-lm(Y99Pop ~ Y89Births+Y89Brate,data=mydat)
myreg[c(1,8)]
names(myreg)[1:3]
myregsum<- summary(myreg)
myregsum[["adj.r.squared"]]
?getData
?raster::getData
getData("ISO3")
#countrycode for Belgium is "BEL"
adm<-raster::getData("GADM",country="PHL",level=2,path=datdir)
plot(adm)
mar<- adm[adm$NAME_1=="Marinduque",]
plot(mar,bg="dodgerblue",axes=T)
#plot.new()
plot(mar,lwd=10,border="skyblue",add=T)
plot(mar,col="green4",add=T)
grid()
box()
invisible(text(getSpPPolygonsLabptSlots(mar),labels=as.character(mar$NAME_2), cex=1.1,col="white",font=2))
mtext(side=3,line=1,"Provincial Map of Marinduque",cex=2)
mtext(side=1,"Longitude",line=2.5,cex=1.1)
mtext(side=2,"Latitude",line=2.5,cex=1.1)
text(122.08,13.22,"Projection: Geographic\n Coordinate System: WGS 1984 \n Data Source: GADM.org", adj= c(0,0), cex=0.7,col="grey20")