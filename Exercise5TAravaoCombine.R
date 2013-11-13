library(raster)
library(rasta)
data(taravao)
data(taravao2)
TaravaoCombine<-function(taravao,taravao2){
cloud1<-calc(x=taravao[[9]],fun=QA2cloud)
cloud2<-calc(x=taravao2[[9]],fun=QA2cloud)
taravaocto1<-dropLayer(x=taravao,i=9)
taravaocto2<-dropLayer(x=taravao2,i=9)
taravaocto1[cloud1==1]<-NA
taravaocto2[cloud2==1]<-NA
taravaomean<-taravao
combine<-function(layer1,layer2){
  return(mean(layer1,layer2,na.rm=T))    
   
}
for(i in 1:8){
  taravaomean[[i]]<-combine(taravaocto1[[i]],taravaocto2[[i]])
}
return(taravaomean)
}
TaravaoCombined<-TaravaoCombine(taravao,taravao2)
plotRGB(TaravaoCombined,4,5,3,colNA="red",axes=T)