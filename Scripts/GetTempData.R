#library(tidyverse)
library(googledrive)
#library(RCurl)
#library(raster)

#idTemp = drive_find(pattern  = "Temperaturas")
#Fechas = gsub("Temperaturas","",idTemp$name) 
#Fechas = gsub(".tif","",Fechas) 
#Fechas = as.Date(Fechas,format = "%Y-%m-%d")
#Fechas = data.frame(Fechas)
#DriveIDs = as.character(idTemp$id)
#DriveIDs = data.frame(DriveIDs)
#DriveIDs$DriveIDs = as.character(DriveIDs$DriveIDs)
#DriveFiles = cbind(Fechas,DriveIDs,idTemp$name)

#write.csv2(DriveFiles,"H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/Scripts/LinksTempToDownload.csv")

DriveFiles = read.csv2("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/Scripts/LinksTempToDownload.csv")
DriveFiles$DriveIDs = as.character(DriveFiles$DriveIDs)

Link = "https://drive.google.com/open?id="


MyDataTemp = function(BeginDate, EndDate){
  SeqDates = seq(as.Date(BeginDate), as.Date(EndDate), by="month")
    Datos = matrix(data = NA, ncol = 1, nrow = length(SeqDates))
    
  for(i in 1:length(SeqDates)){
    Datos[i]=(paste(Link,DriveFiles[which(DriveFiles$Fechas==paste(SeqDates[i])),3], sep=""))
  }
  Datos = as.data.frame(Datos)
  return(Datos)
}

