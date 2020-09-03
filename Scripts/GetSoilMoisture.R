library(RNetCDF)
library(rvest)
library(rgdal)
library(raster)

link = "https://hydro1.gesdisc.eosdis.nasa.gov/opendap/FLDAS/FLDAS_NOAH01_C_GL_M.001/"

MyDataSoilM = function(BeginDate, EndDate){
  SeqDates = seq(as.Date(BeginDate), as.Date(EndDate), by="month")
  SeqDates = gsub("-","",SeqDates)
  SeqDates = substr(SeqDates,1,6)
  seqYears = substr(SeqDates,1,4)
  Datos = matrix(data = NA, ncol = 1, nrow = length(SeqDates))
  for(i in 1:length(SeqDates)){
    Datos[i,1]=(paste(link,"/",seqYears[i],"/","FLDAS_NOAH01_C_GL_M.A",SeqDates[i],".001.nc.html",sep=""))
  }
  Datos = data.frame(Datos, stringsAsFactors = FALSE)
  
  return(Datos)
}

#Example
#DatosHumSuelo = MyDataSoilM("1982-01-01","2020-02-01")

#for(i in 1:nrow(DatosHumSuelo)){
#Destino = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM"
#SeqDates = seq(as.Date("1982-01-01"), as.Date("2020-02-01"), by="month")
#download.file(substr(DatosHumSuelo[i,1],1,121),paste(Destino,"/","GlobalSoilM","-",SeqDates[i],".nc.html",sep=""),method = "auto")
#}

#ArchivosHTML=list.files("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/", pattern = ".html")
#Ruta = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/"
#Datos_link_final = matrix(data = NA, ncol = 1, nrow = length(ArchivosHTML))
#for(i in 1:length(ArchivosHTML)){
  
#  y = read_html(paste(Ruta,ArchivosHTML[i],sep=""))
#  Datos_link_final[i,1]=xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(y, 1), 1), 12), 1), 2), 2), 1))[["value"]]
#}

#Datos_link_final = data.frame(Datos_link_final, stringsAsFactors = FALSE)

#for(i in 1:nrow(Datos_link_final)){
#  Destino = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM"
#  SeqDates = seq(as.Date("1982-01-01"), as.Date("2020-02-01"), by="month")
#  download.file(substr(Datos_link_final[i,1],1,121),paste(Destino,"/","GlobalSoilM","-",SeqDates[i],".nc",sep=""),method = "auto")
#}

#Example2

fid<-open.nc("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/DSnc/FLDAS_NOAH01_C_GL_M.A198201.001.nc.SUB.nc4")
print.nc(fid)
dat<-read.nc(fid)
ylat<-dat$Y
xlon<-dat$X
zSoilM10cm<-dat$SoilMoi00_10cm_tavg
zSoilM40cm<-dat$SoilMoi10_40cm_tavg
zSoilM100cm<-dat$SoilMoi40_100cm_tavg
zSoilM200cm<-dat$SoilMoi100_200cm_tavg
zSoilMT10cm<-dat$SoilTemp00_10cm_tavg
zSoilMT40cm<-dat$SoilTemp10_40cm_tavg
zSoilMT100cm<-dat$SoilTemp40_100cm_tavg
zSoilMT200cm<-dat$SoilTemp100_200cm_tavg
close.nc(fid)



pathNC= "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/DStiff"
pathNC_= "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/DSnc"
ListaNC = list.files("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/DSnc",pattern = ".nc4")
ListaNC2 = gsub(".001.nc.SUB.nc4","",ListaNC)
pathNC2 = paste(pathNC_,"/",ListaNC,sep="")

#CRS("+proj=longlat +datum=WGS84")
#zSoilM10cm2=raster(t(zSoilM10cm), xmn=min(xlon), xmx=max(xlon), ymn=min(ylat), ymx=max(ylat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
#r <- flip(zSoilM10cm2, direction='y')
#plot(zSoilM10cm2)
#plot(r)

### A 10 cm ###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilM10cm<-dat$SoilMoi00_10cm_tavg
  zSoilM10cm=raster(t(zSoilM10cm), xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  zSoilM10cm = flip(zSoilM10cm, direction = "y")
  raster::writeRaster(zSoilM10cm,paste(pathNC,"/",ListaNC2[i],"10cm",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}

#dat = raster("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_SoilM/DStiff/FLDAS_NOAH01_C_GL_M.A19820110cm.tif")
#e <- extent(-160, 10, 30, 60)
#rc <- crop(dat, e)
#crop_extent <- readOGR("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/shapes/Hunan/Hunan.shp")
#rcChina <- crop(dat, crop_extent)

## APLICAR CORRECCION A LAS DE ABAJO ##
### DE 10 A 40 cm ###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilM40cm<-dat$SoilMoi10_40cm_tavg
  zSoilM40cm=raster(zSoilM40cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilM40cm,paste(pathNC,"/",ListaNC2[i],"40cm",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}
### DE 40 A 100 cm ###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilM100cm<-dat$SoilMoi40_100cm_tavg
  zSoilM100cm=raster(zSoilM100cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilM100cm,paste(pathNC,"/",ListaNC2[i],"100cm",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}
### DE 100 A 200 cm ###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilM200cm<-dat$SoilMoi100_200cm_tavg
  zSoilM200cm=raster(zSoilM200cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilM200cm,paste(pathNC,"/",ListaNC2[i],"200cm",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}


### A 10 cm TempSoil###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilMT10cm<-dat$SoilTemp00_10cm_tavg
  zSoilMT10cm=raster(zSoilMT10cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilMT10cm,paste(pathNC,"/",ListaNC2[i],"10cmTS",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}

### De 10 A 40 cm TempSoil###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilMT40cm<-dat$SoilTemp10_40cm_tavg
  zSoilMT40cm=raster(zSoilMT40cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilMT40cm,paste(pathNC,"/",ListaNC2[i],"40cmTS",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}
### De 40 A 100 cm TempSoil###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilMT100cm<-dat$SoilTemp40_100cm_tavg
  zSoilMT100cm=raster(zSoilMT100cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilMT100cm,paste(pathNC,"/",ListaNC2[i],"100cmTS",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}
### De 100 A 200 cm TempSoil###
for(i in 1:459){
  fid = open.nc(pathNC2[i])
  dat<-read.nc(fid)
  ylat<-dat$Y
  xlon<-dat$X
  Xmini = range(xlon)[1]
  Ymini = range(ylat)[1]
  Xmaxi = range(xlon)[2]
  Ymaxi = range(ylat)[2]
  zSoilMT200cm<-dat$SoilTemp100_200cm_tavg
  zSoilMT200cm=raster(zSoilMT200cm, xmn=Xmini, xmx=Xmaxi, ymn=Ymini, ymx=Ymaxi, crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 +proj=longlat +datum=WGS84"))
  raster::writeRaster(zSoilMT200cm,paste(pathNC,"/",ListaNC2[i],"200cmTS",".tiff",sep=""),format = 'GTiff', overwrite = TRUE)
}
