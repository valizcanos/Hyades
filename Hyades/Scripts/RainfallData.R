library(RCurl)


link = "https://data.chc.ucsb.edu/products/CHIRPS-2.0/"
#WebLinksChirps = read.csv2("LinksChirpsToDownload.csv", header = TRUE)

MyDataRain = function(Location,TResolution,BeginDate, EndDate){
  
  if(TResolution=="monthly"){
    SeqDates = seq(as.Date(BeginDate), as.Date(EndDate), by="month")
    Datos = matrix(data = NA, ncol = 1, nrow = length(SeqDates))
    for(i in 1:length(SeqDates)){
      Datos[i]=(paste(link,Location,"_",TResolution, "/","tifs/","chirps-v2.0.",format(SeqDates[i], "%Y.%m"),".tif.gz", sep=""))
    }
  Datos = as.data.frame(Datos)
  return(Datos)}
  else if(TResolution=="daily"){
    SeqDates = seq(as.Date(BeginDate), as.Date(EndDate), by="day")
    SeqDatesF = format(SeqDates, "%Y.%m.%d")
    USeqDates = unique(format(SeqDates,"%Y"))
    Datos = matrix(data = NA, ncol = 1, nrow = length(SeqDates))
    for(i in 1:length(USeqDates)){
      for(j in 1:length(SeqDates)){
        Datos[j]=(paste(link,Location,"_",TResolution, "/","tifs/","p25/",USeqDates[i],"/","chirps-v2.0.",SeqDatesF[j],".tif.gz", sep=""))
      }
    }
    Datos = as.data.frame(Datos)
    return(Datos)
  }
  
}


