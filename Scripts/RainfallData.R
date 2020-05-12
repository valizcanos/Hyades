library(RCurl)


#setwd("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/Scripts")

link = "ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/"
#WebLinksChirps = read.csv2("LinksChirpsToDownload.csv", header = TRUE)

MyDataRain = function(Location,TResolution,BeginDate, EndDate){
  SeqDates = seq(as.Date(BeginDate), as.Date(EndDate), by="month")
  Datos = matrix(data = NA, ncol = 1, nrow = length(SeqDates))
  for(i in 1:length(SeqDates)){
    Datos[i]=(paste(link,Location,"_",TResolution, "/","tifs/","chirps-v2.0.",format(SeqDates[i], "%Y.%m"),".tif.gz", sep=""))
  }
  Datos = as.data.frame(Datos)
  return(Datos)
}



