library(rgdal)

Site = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/shapes"
list.files(Site)

ConverterKMLtoRDS = function(Path,FileName){
  converter = readOGR(paste(Path,"/",FileName,".kml",sep=""))
  saveRDS(converter,file = paste(Path,"/",FileName,".rds",sep=""))
  plot(readRDS(paste(Path,"/",FileName,".rds",sep="")))
}


  
ConverterKMLtoRDS(Site,"Huila_site1")  
ConverterKMLtoRDS(Site,"Hunan_site1")  
ConverterKMLtoRDS(Site,"Rio_Grande_do_Sul_site1")  

ConverterKMLtoSHP = function(Path,FileName){
  converter = readOGR(paste(Path,"/",FileName,".kml",sep=""))
  writeOGR(converter,"." ,dsn= paste(Path,"/",FileName,".shp",sep=""),driver = 'ESRI Shapefile')
  plot(readOGR(paste(Path,"/",FileName,".shp",sep="")))
  #plot(converter)
}

ConverterKMLtoSHP(Site,"Huila_site1")
ConverterKMLtoSHP(Site,"Hunan_site1")
ConverterKMLtoSHP(Site,"Rio_Grande_do_Sul_site1")
