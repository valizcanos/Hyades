library(RNetCDF)
library(rvest)

#NOTA: REVISAR LINKS DE DESCARGA#

link = "https://hydro1.gesdisc.eosdis.nasa.gov/opendap/FLDAS/FLDAS_NOAH01_C_GL_M.001/"

MyTempSupM = function(BeginDate, EndDate){
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
#DatosTempSupM = MyTempSupM("1982-01-01","2020-02-01")

#for(i in 1:nrow(DatosTempSupM)){
#Destino = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_Temp"
#SeqDates = seq(as.Date("1982-01-01"), as.Date("2020-02-01"), by="month")
#download.file(substr(DatosTempSupM[i,1],1,121),paste(Destino,"/","GlobalTempSupM","-",SeqDates[i],".nc.html",sep=""),method = "auto")
#}

#ArchivosHTML=list.files("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_Temp/", pattern = ".html")
#Ruta = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_Temp/"
#Datos_link_final = matrix(data = NA, ncol = 1, nrow = length(ArchivosHTML))
#for(i in 1:length(ArchivosHTML)){

#  y = read_html(paste(Ruta,ArchivosHTML[i],sep=""))
#  Datos_link_final[i,1]=xml_attrs(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(xml_child(y, 1), 1), 12), 1), 2), 2), 1))[["value"]]
#}

#Datos_link_final = data.frame(Datos_link_final, stringsAsFactors = FALSE)

#for(i in 1:nrow(Datos_link_final)){
#  Destino = "H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_Temp"
#  SeqDates = seq(as.Date("1982-01-01"), as.Date("2020-02-01"), by="month")
#  download.file(substr(Datos_link_final[i,1],1,121),paste(Destino,"/","GlobalTempSup","-",SeqDates[i],".nc",sep=""),method = "auto")
#}

#Example2
#fid<-open.nc("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_Temp/GlobalTempSup-1982-01-01.nc.html")
#fid<-open.nc(file.choose())