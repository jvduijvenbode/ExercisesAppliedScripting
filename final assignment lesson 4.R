#function to draw a map of the yield in a kml file and return the SPDF with the yield in ton/ha per load
DrawYieldPerLoad<-function(directory,outputname,cln_lines_df){
#buffer the lines per load
sp_loadbuffers<-gBuffer(cln_lines_df,byid=T,id=cln_lines_df$ID,width=0.5*cln_lines_df$width,capStyle="FLAT")
#remove gaps
sp_loadbuffers<-gBuffer(sp_loadbuffers,byid=T,id=rownames(sp_loadbuffers),width=2.0)
sp_loadbuffers<-gBuffer(sp_loadbuffers,byid=T,id=rownames(sp_loadbuffers),width=-2.0)
#calculate area per buffer
sp_loadbuffers$area=gArea(sp_loadbuffers,byid=T)
#calculate the yield per hectare per buffer
sp_loadbuffers$yield=sp_loadbuffers$loads/(sp_loadbuffers$area/10000)
#plot the map of the yield in ton/ha per load
spplot(sp_loadbuffers,zcol="loads",colorkey=T,zlim=c(0,100),col.regions=bpy.colors(),pch=19,cex=0.25,main="Yield in ton/ha per load")
#transform to WGS84 projection and write to KML
wgs_sploadbuffers<-spTransform(sp_loadbuffers,prj_string_WGS)
writeOGR(wgs_sploadbuffers,file.path(directory,paste(outputname,".kml")),outputname,driver="KML",overwrite_layer=T)
return(sp_loadbuffers)
}
chreck=DrawYieldPerLoad("../data","check",cln_lines_df)

