Countryplot<-function(country,level){
  adm<-raster::getData("GADM",country=country,level=level,path=datdir)
  plot(adm,bg=)
}
Countryplot("BEL",2)