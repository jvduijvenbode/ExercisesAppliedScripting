setwd("D:/")
dir.create("Lesson 6")
setwd("D:/Lesson 6")

library(igraph)
library(rasta)
library(ggplot2)
#import the data
data(GewataB2)
data(GewataB3)
data(GewataB4)
#create brick of 3 layers
gewata<-brick(GewataB2,GewataB3,GewataB4)
#use focal on chosen layer
gewata[[4]]<-focal(gewata[[2]])
maxValue(gewata)
#plot histograms
hist(gewata[[2]])
hist(gewata[[4]])
hist(gewata[[3]])
hist(gewata[[1]])
histogram(gewata)
splom(gewata)
gewata<-dropLayer(gewata,4)
ndvi<-overlay(GewataB4,GewataB3,fun=function(x,y){(x-y)/(x+y)})
drawExtent()
#create a smaller plot of the ndvi and store it in ndvicrop
plot(ndvi)

e<-drawExtent()
print(e)
plot(ndvi,ext=e)
ndviCrop<-crop(ndvi,e)
rm(ndviCrop)
ndviTest<-crop(ndvi,e)
w<-matrix(1/9,nc=3,nr=3)
print(w)
sum(w)
ndviMean<-focal(ndviTest,w=w)
opar<-par(mfrow=c(1,2))
plot(ndviTest,main="NDVI")
plot(ndviMean,main="NDVI with 3x3 mean filter")
par(opar)
#load data with the Vegetation continuous field
data(vcfGewata)
vcfGewata
plot(vcfGewata)
histogram(vcfGewata)
vcfGewata[vcfGewata>100]<-NA
summary(vcfGewata)
ndvi<-calc(ndvi,fun=function(x) floor(x*10000))
dataType(ndvi)<-"INT2U"
names(ndvi)<-"NDVI"
covs<-addLayer(gewata,ndvi,vcfGewata)
plot(covs)
data(trainingPoly)
plot(ndvi)
plot(trainingPoly,add=T)
#change classification string to integers
reclass<-function(x){
  which(x==levels(trainingPoly@data$Class))
}
trainingPoly@data$Code<-sapply(trainingPoly@data$Class,FUN=reclass)
classes<-rasterize(trainingPoly,ndvi,field="Code",progress="text")
dataType(classes)<-"INT1U"
#cols<-c("orange","dark green","light blue")
#plot(classes,col=cols,legend=F)
#legend("topright",legend=c("cropland","forest","wetland"),fill=cols,bg="white")
covmasked<-mask(covs,classes)
#plot(covmasked)
names(classes)<-"class"
trainingbrick<-addLayer(covmasked,classes)
plot(trainingbrick)
valuetable<-getValues(trainingbrick)
valuetable<-as.data.frame(valuetable)
head(valuetable)
tail(valuetable)
valuetable<-valuetable[!is.na(valuetable$class),]
valuetable$class<-factor(valuetable$class,levels=c(1:3))
valuetable$label<-with(valuetable,ifelse(class==1,"cropland",ifelse(class==2,"forest","wetland")))
p1<-ggplot(data=valuetable,aes(x=NDVI))+
  geom_histogram(binwidth=300)+
  facet_wrap(~ label)+
  theme_bw()
p1
p2<-ggplot(data=valuetable,aes(x=vcf2000Gewata))+
  geom_histogram(binwidt=5)+
  labs(x="% tree cover")+
  facet_wrap(~label)+
  theme_bw()
p2
p3<-ggplot(data=valuetable,aes(x=gewataB2,y=gewataB3))+
  stat_bin2d()+
  facet_wrap(~ label)+
  theme_bw()
p3
p4<-ggplot(data=valuetable,aes(x=gewataB2,y=gewataB3))+
  stat_bin2d()+
  facet_wrap(~ label)+
  theme_bw()
p4
valuetable<-na.omit(valuetable)
library(randomForest)
system.time(mode1Rf<-randomForest(x=valuetable[,c(1:5)],y=valuetable$class,importance=T))
colnames(mode1Rf$confusion)<-c("cropland","bamboo","bare soil","coffee","forest","wetland","class.error")
rownames(mode1Rf$confusion)<-c("cropland","bamboo","bare soil","coffee","forest","wetland")
mode1Rf$confusion
varImpPlot(mode1Rf)
names(covs)
names(valuetable)
predLC<-predict(covs,model=mode1Rf,na.rm=T)
cols<-c("orange","dark green","light blue")
plot(predLC,col=cols,legend=F)
legend("bottomright",legend=c("cropland","forest","wetland"),fill=cols,bg="white")


