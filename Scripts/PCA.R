library(devtools)
library(ggbiplot)
library(dplyr)
library(ggplot2)
library(plotly)
library(corrplot)
library(nlme)
library(pracma)

#### NDVI ####
NDVI_BR = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/BRASIL/NDVI_filled.csv",sep=";",dec=".")
NDVI_BR$Fecha = as.Date(NDVI_BR$Fecha)
NDVI_BR_Monthly_Mean = tapply(NDVI_BR$NDVI,NDVI_BR$Mes,mean, na.rm=TRUE)
NDVI_BR_Monthly_Sd = tapply(NDVI_BR$NDVI,NDVI_BR$Mes,sd, na.rm=TRUE)
EVI_BR_Monthly_Mean = tapply(NDVI_BR$EVI,NDVI_BR$Mes,mean, na.rm=TRUE)
EVI_BR_Monthly_Sd = tapply(NDVI_BR$EVI,NDVI_BR$Mes,sd, na.rm=TRUE)
NDWI_BR_Monthly_Mean = tapply(NDVI_BR$NDWI,NDVI_BR$Mes,mean, na.rm=TRUE)
NDWI_BR_Monthly_Sd = tapply(NDVI_BR$NDWI,NDVI_BR$Mes,sd, na.rm=TRUE)
write.table(NDVI_BR_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/NDVI_BR_Monthly_Mean.csv", sep=";",dec=".")
write.table(EVI_BR_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/EVI_BR_Monthly_Mean.csv", sep=";",dec=".")
write.table(NDWI_BR_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/NDWI_BR_Monthly_Mean.csv", sep=";",dec=".")

NDVI_CHN = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/CHINA/NDVI_filled.csv",sep=";",dec=".")
NDVI_CHN$Fecha = as.Date(NDVI_CHN$Fecha)
NDVI_CHN_Monthly_Mean = tapply(NDVI_CHN$NDVI,NDVI_CHN$Mes,mean, na.rm=TRUE)
NDVI_CHN_Monthly_Sd = tapply(NDVI_CHN$NDVI,NDVI_CHN$Mes,sd, na.rm=TRUE)
EVI_CHN_Monthly_Mean = tapply(NDVI_CHN$EVI,NDVI_CHN$Mes,mean, na.rm=TRUE)
EVI_CHN_Monthly_Sd = tapply(NDVI_CHN$EVI,NDVI_CHN$Mes,sd, na.rm=TRUE)
NDWI_CHN_Monthly_Mean = tapply(NDVI_CHN$NDWI,NDVI_CHN$Mes,mean, na.rm=TRUE)
NDWI_CHN_Monthly_Sd = tapply(NDVI_CHN$NDWI,NDVI_CHN$Mes,sd, na.rm=TRUE)
write.table(NDVI_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/NDVI_CHN_Monthly_Mean.csv", sep=";",dec=".")
write.table(EVI_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/EVI_CHN_Monthly_Mean.csv", sep=";",dec=".")
write.table(NDWI_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/NDWI_CHN_Monthly_Mean.csv", sep=";",dec=".")

NDVI_COL = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/COLOMBIA/NDVI_filled.csv",sep=";",dec=".")
NDVI_COL$Fecha = as.Date(NDVI_COL$Fecha)
NDVI_COL_Monthly_Mean = tapply(NDVI_COL$NDVI,NDVI_COL$Mes,mean, na.rm=TRUE)
NDVI_COL_Monthly_Sd = tapply(NDVI_COL$NDVI,NDVI_COL$Mes,sd, na.rm=TRUE)
EVI_COL_Monthly_Mean = tapply(NDVI_COL$EVI,NDVI_COL$Mes,mean, na.rm=TRUE)
EVI_COL_Monthly_Sd = tapply(NDVI_COL$EVI,NDVI_COL$Mes,sd, na.rm=TRUE)
NDWI_COL_Monthly_Mean = tapply(NDVI_COL$NDWI,NDVI_COL$Mes,mean, na.rm=TRUE)
NDWI_COL_Monthly_Sd = tapply(NDVI_COL$NDWI,NDVI_COL$Mes,sd, na.rm=TRUE)
write.table(NDVI_COL_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/NDVI_COL_Monthly_Mean.csv", sep=";",dec=".")
write.table(EVI_COL_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/EVI_COL_Monthly_Mean.csv", sep=";",dec=".")
write.table(NDWI_COL_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/NDWI_COL_Monthly_Mean.csv", sep=";",dec=".")

#### TOTAL NDVI ####

NDVI_T_BRASIL = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/BRASIL/NDVI_T.csv')
NDWI_T_BRASIL = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/BRASIL/NDWI_T.csv')
EVI_T_BRASIL = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/BRASIL/EVI_T.csv')
NDVI_T_CHINA = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/CHINA/NDVI_Total.csv')
NDWI_T_CHINA = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/CHINA/NDWI_Total.csv')
EVI_T_CHINA = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/CHINA/EVI_Total.csv')
NDVI_T_COLOMBIA = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/COLOMBIA/NDVI_T.csv')
NDWI_T_COLOMBIA = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/COLOMBIA/NDWI_T.csv')
EVI_T_COLOMBIA = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/COLOMBIA/EVI_T.csv')

#### SVI WITH MODIS ####

NdviEvi_Modis_Br = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/Brz_NdviEvi.csv',stringsAsFactors = FALSE,sep = ';', dec=',')
Ndwi_Br = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/Brz_Ndwi.csv')
NdviEvi_Modis_Cn = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/Chn_NdviEvi.csv',stringsAsFactors = FALSE,sep = ';', dec=',')
Ndwi_Cn = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/Chn_Ndwi.csv')
NdviEvi_Modis_Co = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/Col_NdviEvi.csv',stringsAsFactors = FALSE,sep = ';', dec=',')
Ndwi_Co = read.csv('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/Col_Ndwi.csv')
SVI_Br_ = dplyr::left_join(NdviEvi_Modis_Br,Ndwi_Br,by='doy') 
SVI_Cn_ = dplyr::left_join(NdviEvi_Modis_Cn,Ndwi_Cn,by='doy')
SVI_Cn_$NDWI_mean[SVI_Cn_$NDWI_mean==0.219] = 0.013
SVI_Co_ = dplyr::left_join(NdviEvi_Modis_Co,Ndwi_Co,by='doy')

#### SOIL ####
SOILM_BR = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosSuelos/BRASIL/MyDataSet_SoilM2.csv",sep=";",dec=".")
SOILM_BR$Fechas = as.Date(SOILM_BR$Fechas)
SOILM_BR$Months = lubridate::month(SOILM_BR$Fechas)
#SOILM_BR$Months = format(as.POSIXct(SOILM_BR$Fechas), "%m")
SOILM_BR_Monthly_Mean = tapply(SOILM_BR$value,SOILM_BR$Months,mean, na.rm=TRUE)
SOILM_BR2 = SOILM_BR[541:1260,]
SOILM_BR_Monthly_Mean2 = tapply(SOILM_BR2$value,SOILM_BR2$Months,mean, na.rm=TRUE)
SOILM_BR_Monthly_Sd = tapply(SOILM_BR$value,SOILM_BR$Months,sd, na.rm=TRUE)
write.table(SOILM_BR_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/SOILM_BR_Monthly_Mean.csv", sep=";",dec=".")

SOILM_CHN = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosSuelos/CHINA/MyDataSet_SoilM2.csv",sep=";",dec=".")
SOILM_CHN$Fechas = as.Date(SOILM_CHN$Fechas)
SOILM_CHN$Months = lubridate::month(SOILM_CHN$Fechas)
#SOILM_CHN$Months = format(as.POSIXct(SOILM_CHN$Fechas), "%m")
SOILM_CHN_Monthly_Mean = tapply(SOILM_CHN$value,SOILM_CHN$Months,mean, na.rm=TRUE)
SOILM_CHN2 = SOILM_CHN[15841:36960,]
SOILM_CHN_Monthly_Mean2 = tapply(SOILM_CHN2$value,SOILM_CHN2$Months,mean, na.rm=TRUE)
SOILM_CHN_Monthly_Sd = tapply(SOILM_CHN$value,SOILM_CHN$Months,sd, na.rm=TRUE)
write.table(SOILM_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/SOILM_CHN_Monthly_Mean.csv", sep=";",dec=".")

SOILM_COL = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosSuelos/COLOMBIA/MyDataSet_SoilM2B.csv",sep=";",dec=".")
SOILM_COL$Fechas = as.Date(SOILM_COL$Fechas)
SOILM_COL$Months = lubridate::month(SOILM_COL$Fechas)
#SOILM_COL$Months = format(as.POSIXct(SOILM_COL$Fechas), "%m")
SOILM_COL_Monthly_Mean = tapply(SOILM_COL$value,SOILM_COL$Months,mean, na.rm=TRUE)
SOILM_COL2 = SOILM_COL[721:1680,]
SOILM_COL_Monthly_Mean2 = tapply(SOILM_COL2$value,SOILM_COL2$Months,mean, na.rm=TRUE)
SOILM_COL_Monthly_Sd = tapply(SOILM_COL$value,SOILM_COL$Months,sd, na.rm=TRUE)
write.table(SOILM_COL_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/SOILM_COL_Monthly_Mean.csv", sep=";",dec=".")

#### TEMP ####
TEMP_BR = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosTemperatura/BRASIL/MyDataSet_Temperature2.csv",sep=";",dec=".")
TEMP_BR$Fechas = as.Date(TEMP_BR$Fechas)
TEMP_BR$Months = lubridate::month(TEMP_BR$Fechas)
#TEMP_BR$Months = format(as.POSIXct(TEMP_BR$Fechas),"%m")
TEMP_BR_Monthly_Mean = tapply(TEMP_BR$value, TEMP_BR$Months, mean)
TEMP_BR2 = TEMP_BR[541:1260,]
TEMP_BR_Monthly_Mean2 = tapply(TEMP_BR2$value, TEMP_BR2$Months, mean)
TEMP_BR_Monthly_Sd = tapply(TEMP_BR$value, TEMP_BR$Months, sd)
write.table(TEMP_BR_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/TEMP_BR_Monthly_Mean.csv", sep=";",dec=".")

TEMP_CHN = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosTemperatura/CHINA/MyDataSet2.csv",sep=";",dec=".")
TEMP_CHN$Fechas = as.Date(TEMP_CHN$Fechas)
TEMP_CHN$Months = lubridate::month(TEMP_CHN$Fechas)
#TEMP_CHN$Months = format(as.POSIXct(TEMP_CHN$Fechas),"%m")
TEMP_CHN_Monthly_Mean = tapply(TEMP_CHN$value, TEMP_CHN$Months, mean)
TEMP_CHN2 = TEMP_CHN[17281:40320,]
TEMP_CHN_Monthly_Mean2 = tapply(TEMP_CHN2$value, TEMP_CHN2$Months, mean)
TEMP_CHN_Monthly_Sd = tapply(TEMP_CHN$value, TEMP_CHN$Months, sd)
write.table(TEMP_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/TEMP_CHN_Monthly_Mean.csv", sep=";",dec=".")

TEMP_COL = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosTemperatura/COLOMBIA/MyDataSet_Temperature_Clustered.csv",sep=";",dec=".")
TEMP_COL$Fechas = as.Date(TEMP_COL$Fechas)
TEMP_COL$Months = lubridate::month(TEMP_COL$Fechas)
#TEMP_COL$Months = format(as.POSIXct(TEMP_COL$Fechas),"%m")
TEMP_COL_Monthly_Mean = tapply(TEMP_COL$value, TEMP_COL$Months, mean)
TEMP_COL2 = TEMP_COL[721:1680,]
TEMP_COL_Monthly_Mean2 = tapply(TEMP_COL2$value, TEMP_COL2$Months, mean)
TEMP_COL_Monthly_Sd = tapply(TEMP_COL$value, TEMP_COL$Months, sd)
write.table(TEMP_COL_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/TEMP_COL_Monthly_Mean.csv", sep=";",dec=".")

#### PREC ####
PREC_BR = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosPrecipitacion/BRASIL/MyDataSet_Precipitation2.csv",sep=";",dec=".")
PREC_BR$Fechas = as.Date(PREC_BR$Fechas)
PREC_BR$Months = lubridate::month(PREC_BR$Fechas)
#PREC_BR$Months = format(as.POSIXct(PREC_BR$Fechas),"%m")
PREC_BR_Monthly_Mean = tapply(PREC_BR$value, PREC_BR$Months, mean)
PREC_BR2 = PREC_BR[2161:5040,]
PREC_BR_Monthly_Mean2 = tapply(PREC_BR2$value, PREC_BR2$Months, mean)
PREC_BR_Monthly_Sd = tapply(PREC_BR$value, PREC_BR$Months, sd)
write.table(PREC_BR_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/PREC_BR_Monthly_Mean.csv", sep=";",dec=".")

PREC_CHN = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosPrecipitacion/CHINA/MyDataSet_Precipitation2.csv",sep=";",dec=".")
PREC_CHN$Fechas = as.Date(PREC_CHN$Fechas)
PREC_CHN$Months = lubridate::month(PREC_CHN$Fechas)
#PREC_CHN$Months = format(as.POSIXct(PREC_CHN$Fechas),"%m")
PREC_CHN_Monthly_Mean = tapply(PREC_CHN$value, PREC_CHN$Months, mean)
PREC_CHN2 = PREC_CHN[62101:144900,]
PREC_CHN_Monthly_Mean2 = tapply(PREC_CHN2$value, PREC_CHN2$Months, mean)
PREC_CHN_Monthly_Sd = tapply(PREC_CHN$value, PREC_CHN$Months, sd)
write.table(PREC_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/PREC_CHN_Monthly_Mean.csv", sep=";",dec=".")

PREC_COL = read.csv("H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DatosPrecipitacion/COLOMBIA/MyDataSet_Precipitation_Clustered.csv",sep=";",dec=".")
PREC_COL$Fechas = as.Date(PREC_COL$Fechas)
PREC_COL$Months = lubridate::month(PREC_COL$Fechas)
#PREC_COL$Months = format(as.POSIXct(PREC_COL$Fechas),"%m")
PREC_COL_Monthly_Mean = tapply(PREC_COL$value, PREC_COL$Months, mean)
PREC_COL2 = PREC_COL[2161:5040,]
PREC_COL_Monthly_Mean2 = tapply(PREC_COL2$value, PREC_COL2$Months, mean)
PREC_COL_Monthly_Sd = tapply(PREC_COL$value, PREC_COL$Months, sd)
write.table(PREC_CHN_Monthly_Mean, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/PREC_CHN_Monthly_Mean.csv", sep=";",dec=".")

#### Medias y Stdv ######

CharNumb = c("01","02","03","04","05","06","07","08","09","10","11","12")
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(SOILM_BR[SOILM_BR$Months==CharNumb[i],5])," ",sd(SOILM_BR[SOILM_BR$Months==CharNumb[i],5]),sep=""))
}
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(TEMP_BR[TEMP_BR$Months==CharNumb[i],5])," ",sd(TEMP_BR[TEMP_BR$Months==CharNumb[i],5]),sep=""))
}
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(PREC_BR[PREC_BR$Months==CharNumb[i],5])," ",sd(PREC_BR[PREC_BR$Months==CharNumb[i],5]),sep=""))
}

for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(SOILM_CHN[SOILM_CHN$Months==CharNumb[i],5])," ",sd(SOILM_CHN[SOILM_CHN$Months==CharNumb[i],5]),sep=""))
}
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(TEMP_CHN[TEMP_CHN$Months==CharNumb[i],6])," ",sd(TEMP_CHN[TEMP_CHN$Months==CharNumb[i],6]),sep=""))
}
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(PREC_CHN[PREC_CHN$Months==CharNumb[i],5])," ",sd(PREC_CHN[PREC_CHN$Months==CharNumb[i],5]),sep=""))
}

for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(SOILM_COL[SOILM_COL$Months==CharNumb[i],5])," ",sd(SOILM_COL[SOILM_COL$Months==CharNumb[i],5]),sep=""))
}
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(TEMP_COL[TEMP_COL$Months==CharNumb[i],4])," ",sd(TEMP_COL[TEMP_COL$Months==CharNumb[i],4]),sep=""))
}
for(i in 1:length(CharNumb)){
  print(paste(i," ",mean(PREC_COL[PREC_COL$Months==CharNumb[i],4])," ",sd(PREC_COL[PREC_COL$Months==CharNumb[i],4]),sep=""))
}
#### COMBINACION DE VARIABLES ####

VARIABLES_BR = cbind(1:12,NDVI_BR_Monthly_Mean,EVI_BR_Monthly_Mean,NDWI_BR_Monthly_Mean,
                      SOILM_BR_Monthly_Mean,TEMP_BR_Monthly_Mean,PREC_BR_Monthly_Mean)
VARIABLES_BR = data.frame(VARIABLES_BR)
colnames(VARIABLES_BR) = c("Month","NDVI","EVI","NDWI","SOILM","TEMP","PREC")
VARIABLES_BR$COUNTRY = c(rep("BRAZIL",nrow(VARIABLES_BR)))
write.table(VARIABLES_BR, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/VARIABLES_BR.csv", sep=";",dec=".")
COR_BR = cor(VARIABLES_BR[,2:7],method = "pearson")
corrplot(COR_BR, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

VARIABLES_CHN = cbind(1:12,NDVI_CHN_Monthly_Mean,EVI_CHN_Monthly_Mean,NDWI_CHN_Monthly_Mean,
                  SOILM_CHN_Monthly_Mean,TEMP_CHN_Monthly_Mean,PREC_CHN_Monthly_Mean)
VARIABLES_CHN = data.frame(VARIABLES_CHN)
colnames(VARIABLES_CHN) = c("Month","NDVI","EVI","NDWI","SOILM","TEMP","PREC")
VARIABLES_CHN$COUNTRY = c(rep("CHINA",nrow(VARIABLES_CHN)))
write.table(VARIABLES_CHN, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/VARIABLES_CHN.csv", sep=";",dec=".")
COR_CHN = cor(VARIABLES_CHN[,2:7],method = "pearson")
corrplot(COR_CHN, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

VARIABLES_COL = cbind(1:12,NDVI_COL_Monthly_Mean,EVI_COL_Monthly_Mean,NDWI_COL_Monthly_Mean,
                      SOILM_COL_Monthly_Mean,TEMP_COL_Monthly_Mean,PREC_COL_Monthly_Mean)
VARIABLES_COL = data.frame(VARIABLES_COL)
colnames(VARIABLES_COL) = c("Month","NDVI","EVI","NDWI","SOILM","TEMP","PREC")
VARIABLES_COL$COUNTRY = c(rep("COLOMBIA",nrow(VARIABLES_COL)))
write.table(VARIABLES_COL, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/VARIABLES_COL.csv", sep=";",dec=".")
COR_COL = cor(VARIABLES_COL[,2:7],method = "pearson")
corrplot(COR_COL, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

par(mfrow=c(1,3))
corrplot(COR_BR, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,cl.cex = 1)
corrplot(COR_CHN, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot(COR_COL, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

VARIABLES = dplyr::bind_rows(VARIABLES_BR,VARIABLES_CHN,VARIABLES_COL)
VARIABLES$MONTH = month.name[VARIABLES$Month]
write.table(VARIABLES, file="H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/PCA/VARIABLES.csv", sep=";",dec=".")

#### PCA ####

PCA_BR = prcomp(VARIABLES_BR[,2:7], center = TRUE, scale. = TRUE)
summary(PCA_BR)
str(PCA_BR)
ggbiplot(PCA_BR,labels=VARIABLES_BR$Month)

PCA_CHN = prcomp(VARIABLES_CHN[,2:7], center = TRUE, scale. = TRUE)
summary(PCA_CHN)
str(PCA_CHN)
ggbiplot(PCA_CHN,labels=VARIABLES_BR$Month)

PCA_COL = prcomp(VARIABLES_COL[,2:7], center = TRUE, scale. = TRUE)
summary(PCA_COL)
str(PCA_COL)
ggbiplot(PCA_COL,labels=VARIABLES_BR$Month)

PCA = prcomp(VARIABLES[,2:7], center = TRUE, scale. = TRUE)
summary(PCA)
str(PCA)
ggbiplot(PCA,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=TRUE,labels=VARIABLES$Month, groups = VARIABLES$COUNTRY, lwd =3)

#---------------------------------------------------------------------------------------------------------------------#
# GRAFICOS DE LINEAS #

VARIABLES_Resumen = melt(VARIABLES[,c(2,3,4,8,9)],id=c("COUNTRY","MONTH"))
VARIABLES_Resumen2 = melt(VARIABLES[,c(5,6,7,8,9)],id=c("COUNTRY","MONTH"))
VARIABLES_Resumen2$MONTH = factor(VARIABLES_Resumen2$MONTH, levels = month.name)
VARIABLES_Resumen$MONTH = factor(VARIABLES_Resumen$MONTH, levels = month.name)

PNDVI = ggplot(VARIABLES, aes(x=MONTH,y=NDVI,color=COUNTRY ,group=COUNTRY, shape=COUNTRY)) + geom_line(size=1.2,aes(linetype=COUNTRY))+ geom_point(size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Months")+ylab("NDVI")+theme(legend.title=element_blank())
plot(PNDVI)
PEVI = ggplot(VARIABLES, aes(x=MONTH,y=EVI,color=COUNTRY ,group=COUNTRY)) + geom_line(size=1,aes(linetype=COUNTRY))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Months")+ylab("EVI")
plot(PEVI)
PNDWI = ggplot(VARIABLES, aes(x=MONTH,y=NDWI,color=COUNTRY ,group=COUNTRY)) + geom_line(size=1,aes(linetype=COUNTRY))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Months")+ylab("NDWI")
plot(PNDWI)
PSOILM = ggplot(VARIABLES, aes(x=MONTH,y=SOILM,color=COUNTRY ,group=COUNTRY)) + geom_line(size=1,aes(linetype=COUNTRY))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Months")+ylab("Soil Moisture - 10 cm")
plot(PSOILM)
PTEMP = ggplot(VARIABLES, aes(x=MONTH,y=TEMP,color=COUNTRY ,group=COUNTRY)) + geom_line(size=1,aes(linetype=COUNTRY))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Months")+ylab("Temperature (°K)")
plot(PTEMP)
PPREC = ggplot(VARIABLES, aes(x=MONTH,y=PREC,color=COUNTRY ,group=COUNTRY)) + geom_line(size=1,aes(linetype=COUNTRY))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Months")+ylab("Precipitation (mm)")
plot(PPREC)

SPI = ggplot(VARIABLES_Resumen, aes(x=MONTH, y=value)) + geom_point(aes(color=variable)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("")+
  theme(legend.title=element_blank())
SPI + facet_grid(variable ~ COUNTRY)

EnviVar = ggplot(VARIABLES_Resumen2, aes(x=MONTH, y=value)) + geom_point(aes(color=variable)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("")+
  theme(legend.title=element_blank())
EnviVar + facet_grid(variable~COUNTRY,scales = "free")

SOILM_BR2 = SOILM_BR[,c(5,7)]
SOILM_BR2$COUNTRY = "BRAZIL"
SOILM_BR2$VAR = "SOIL M."
SOILM_CHN2 = SOILM_CHN[,c(5,7)]
SOILM_CHN2$COUNTRY = "CHINA"
SOILM_CHN2$VAR = "SOIL M."
SOILM_COL2 = SOILM_COL[,c(5,7)]
SOILM_COL2$COUNTRY = "COLOMBIA"
SOILM_COL2$VAR = "SOIL M."

PREC_BR2 = PREC_BR[,c(5,7)]
PREC_BR2$COUNTRY = "BRAZIL"
PREC_BR2$VAR = "PREC."
PREC_CHN2 = PREC_CHN[,c(5,7)]
PREC_CHN2$COUNTRY = "CHINA"
PREC_CHN2$VAR = "PREC."
PREC_COL2 = PREC_COL[,c(4,7)]
PREC_COL2$COUNTRY = "COLOMBIA"
PREC_COL2$VAR = "PREC."

TEMP_BR2 = TEMP_BR[,c(5,7)]
TEMP_BR2$COUNTRY = "BRAZIL"
TEMP_BR2$VAR = "TEMP."
TEMP_CHN2 = TEMP_CHN[,c(6,8)]
TEMP_CHN2$COUNTRY = "CHINA"
TEMP_CHN2$VAR = "TEMP."
TEMP_CHN2 = TEMP_COL[,c(4,7)]
TEMP_COL2$COUNTRY = "COLOMBIA"
TEMP_COL2$VAR = "TEMP."

VARIABLES_Resumen3 = rbind(SOILM_BR2,SOILM_CHN2,SOILM_COL2,PREC_BR2,PREC_CHN2,PREC_COL2,TEMP_BR2,TEMP_CHN2,TEMP_COL2)

M = as.numeric(VARIABLES_Resumen3$Months)
M = factor(M,labels = month.name)
M = data.frame(M)
VARIABLES_Resumen3 = cbind(VARIABLES_Resumen3,M)

EnviVar2 = ggplot(VARIABLES_Resumen3, aes(x=M, y=value)) + geom_boxplot(aes(color=VAR)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("")+
  theme(legend.title=element_blank())

EnviVar2 + facet_grid(VAR~COUNTRY,scales = "free")


NDVI_BR2 = NDVI_BR
NDVI_BR2$COUNTRY = "BRAZIL"
NDVI_BR2 = NDVI_BR2[,c(2,9,10,11,12)]
NDVI_CHN2 = NDVI_CHN
NDVI_CHN2$COUNTRY= "CHINA"
NDVI_CHN2 = NDVI_CHN2[,c(2,9,10,11,12)]
NDVI_COL2 = NDVI_COL
NDVI_COL2$COUNTRY = "COLOMBIA"
NDVI_COL2 = NDVI_COL2[,c(2,9,10,11,12)]

VARIABLES_Resumen4 = rbind(NDVI_BR2,NDVI_CHN2,NDVI_COL2)
VARIABLES_Resumen4 = reshape2::melt(VARIABLES_Resumen4,id=c("Mes","COUNTRY"))
M2 = VARIABLES_Resumen4$Mes
M2 = factor(M2,labels = month.name)
M2 = data.frame(M2)
VARIABLES_Resumen4 = cbind(VARIABLES_Resumen4,M2)

EnviVar3 = ggplot(VARIABLES_Resumen4, aes(x=M2, y=value)) + geom_boxplot(aes(color=variable)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+xlab("")+ylab("")+
  theme(legend.title=element_blank())
EnviVar3 + facet_grid(variable~COUNTRY,scales = "free")

###########GRAFICOS PARA INDICES DE VEGETACIÓN TOTALES ####################

IndicesBrasil = cbind(NDVI_T_BRASIL,EVI_T_BRASIL[,2],NDWI_T_BRASIL[,2])
colnames(IndicesBrasil) = c('doy','NDVI_mean','EVI_mean', 'NDWI_mean')
IndicesBrasil = reshape2::melt(IndicesBrasil, id = 'doy')
modelBrasil = lm(IndicesBrasil$value ~ IndicesBrasil$variable + poly(IndicesBrasil$doy,15))
modelBrasil2 = predict(modelBrasil, data.frame(x= IndicesBrasil$doy), interval = 'confidence', level =0.99)
modelBrasil3 = cbind(IndicesBrasil,modelBrasil2)

IndicesChina = cbind(NDVI_T_CHINA,EVI_T_CHINA[,2],NDWI_T_CHINA[,2])
colnames(IndicesChina) = c('doy','NDVI_mean','EVI_mean', 'NDWI_mean')
IndicesChina$NDWI_mean = (IndicesChina$NDWI_mean)+0.20
IndicesChina = reshape2::melt(IndicesChina, id = 'doy')
modelChina = lm(IndicesChina$value ~ IndicesChina$variable + poly(IndicesChina$doy,20))
modelChina2 = predict(modelChina, data.frame(x= IndicesChina$doy), interval = 'confidence', level =0.99)
modelChina3 = cbind(IndicesChina,modelChina2)

IndicesColombia = cbind(NDVI_T_COLOMBIA,EVI_T_COLOMBIA[,2],NDWI_T_COLOMBIA[,2])
colnames(IndicesColombia) = c('doy','NDVI_mean','EVI_mean', 'NDWI_mean')
IndicesColombia = reshape2::melt(IndicesColombia, id = 'doy')


ggplot(modelBrasil3, aes(x=doy, y=fit, group = variable, linetype=variable)) +
  geom_point(aes(y=value, group = variable))+
  geom_line(size=1, show.legend = NA)+ 
  geom_ribbon( aes(ymin = lwr, ymax = upr, color = NULL), alpha = .15)+
  ylab('Vegetation index') + theme_classic()+
  theme(legend.position = "none") 
ggplot(modelBrasil3, aes(x=doy, y=value, group = variable,color=variable, linetype=variable)) + geom_line(size=1.0)

ggplot(modelChina3, aes(x=doy, y=fit, group = variable,color=variable, linetype=variable)) +
  geom_point(aes(y=value, group = variable, shape=variable))+
  geom_line(size=1.5, show.legend = NA)+ 
  geom_ribbon( aes(ymin = lwr, ymax = upr, color = NULL), alpha = .15)+
  ylab('Vegetation index') + theme_classic()+
  theme(legend.position = "none") 

ggplot(modelChina3, aes(x=doy, y=value, group = variable,color=variable, linetype=variable)) + geom_line(size=1.0)

#####################################################################################################################


SVI_Chn_whittaker = read.csv2('H:/TESIS_DOCTORADO_2016/ARTICULOS/1_ANALISIS_DE_DATOS/APLICACION_WEB_R/DataSet_VegIndex/CHINA/SVI_Total.csv', sep=";")
NDVI_Chn_whittaker = whittaker(SVI_Chn_whittaker$NDVI_mean, lambda = 10000, d=10)
NDWI_Chn_whittaker = whittaker(SVI_Chn_whittaker$NDWI_mean, lambda = 10000, d=10)

plot(SVI_Chn_whittaker$doy,NDVI_Chn_whittaker)
lines(SVI_Chn_whittaker$doy,NDWI_Chn_whittaker)

#############################GRAFICOS PARA INDICES MODIS#############################################################

SVI_graph_Br = ggplot(SVI_Br_,aes(x=doy))+
  geom_line(aes(y=EVI_mean/10000),linetype='dashed',size=1.5)+
  geom_line(aes(y=NDVI_mean/10000),size=1.5)+
  geom_line(aes(y = NDWI_mean),linetype='dotdash',size=1.5)+
  ylab("Spectral vegetation indexes")+
  theme(text = element_text(size = 20))
   
plot(SVI_graph_Br)

SVI_graph_Cn = ggplot(SVI_Cn_,aes(x=doy))+
  geom_line(aes(y=EVI_mean/10000),linetype='dashed',size=1.5)+
  geom_line(aes(y=NDVI_mean/10000),size=1.5)+
  geom_line(aes(y = NDWI_mean),linetype='dotdash',size=1.5)+
  ylab("Spectral vegetation indexes")+
  theme(text = element_text(size = 20))

plot(SVI_graph_Cn)

SVI_graph_Co = ggplot(SVI_Co_,aes(x=doy))+
  geom_line(aes(y=EVI_mean/10000),linetype='dashed',size=1.5)+
  geom_line(aes(y=NDVI_mean/10000),size=1.5)+
  geom_line(aes(y = NDWI_mean),linetype='dotdash',size=1.5)+
  ylab("Spectral vegetation indexes")+
  theme(text = element_text(size = 20))

plot(SVI_graph_Co)

SOILM_BR_Monthly_Mean2 = data.frame(SOILM_BR_Monthly_Mean2, MM = month.name)
colnames(SOILM_BR_Monthly_Mean2)= c("SoilMoisture","mm")
SOILM_graph_Br = ggplot(SOILM_BR_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=SoilMoisture),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Soil moisture'~(m^3/m^3)))+
  xlab('Months')
plot(SOILM_graph_Br)

SOILM_CHN_Monthly_Mean2 = data.frame(SOILM_CHN_Monthly_Mean2, MM = month.name)
colnames(SOILM_CHN_Monthly_Mean2)= c("SoilMoisture","mm")
SOILM_CHN_Monthly_Mean2[9,1]= 0.291
SOILM_CHN_Monthly_Mean2[10,1]= 0.289
SOILM_graph_Cn = ggplot(SOILM_CHN_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=SoilMoisture),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Soil moisture'~(m^3/m^3)))+
  xlab('Months')
plot(SOILM_graph_Cn)

SOILM_COL_Monthly_Mean2 = data.frame(SOILM_COL_Monthly_Mean2, MM = month.name)
colnames(SOILM_COL_Monthly_Mean2)= c("SoilMoisture","mm")
SOILM_graph_Co = ggplot(SOILM_COL_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=SoilMoisture),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Soil moisture'~(m^3/m^3)))+
  xlab('Months')
plot(SOILM_graph_Co)


TEMP_BR_Monthly_Mean2 = data.frame(TEMP_BR_Monthly_Mean2, MM = month.name)
colnames(TEMP_BR_Monthly_Mean2)= c("Temperature","mm")
TEMP_graph_Br = ggplot(TEMP_BR_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=SoilMoisture-273.15),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Temperature'~ (''^'o'*'C')))+
  xlab('Months')
plot(TEMP_graph_Br)

TEMP_CHN_Monthly_Mean2 = data.frame(TEMP_CHN_Monthly_Mean2, MM = month.name)
colnames(TEMP_CHN_Monthly_Mean2)= c("SoilMoisture","mm")
TEMP_graph_Cn = ggplot(TEMP_CHN_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=SoilMoisture-273.15),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Temperature'~ (''^'o'*'C')))+
  xlab('Months')
plot(TEMP_graph_Cn)

TEMP_COL_Monthly_Mean2 = data.frame(TEMP_COL_Monthly_Mean2, MM = month.name)
colnames(TEMP_COL_Monthly_Mean2)= c("SoilMoisture","mm")
TEMP_graph_Co = ggplot(TEMP_COL_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=SoilMoisture-273.15),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Temperature'~ (''^'o'*'C')))+
  xlab('Months')
plot(TEMP_graph_Co)

CampoAlegre = data.frame(mm=month.name,
                         TM = c(32.8,33.0,32.7,32.3,32.5,33.2,33.7,34.5,34.6,32.8,31.5,31.9),
                         tm = c(22.3,22.4,22.5,22.4,22.3,22.2,22.5,22.9,23.0,22.4,22.2,22.1))
CampoAlegre$TMean = (CampoAlegre$TM+CampoAlegre$tm)/2
TEMP_graph_CampoAlegre = ggplot(CampoAlegre,aes(x=mm, group=1))+
  geom_line(aes(y=TM),linetype='dashed',size=1.5)+
  geom_line(aes(y=TMean),size=1.5)+
  geom_line(aes(y=tm),linetype='dotdash',size=1.5)+
  scale_x_discrete(limits=month.name) +
  ylab(bquote('Temperature'~ (''^'o'*'C')))+
  xlab('Months')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 20))
plot(TEMP_graph_CampoAlegre)

Urugaiana = data.frame(mm=month.name,
                         TM = c(32.8,32.1,29.6,25.2,22.1,19.2,19.0,21.2,22.5,25.0,28.8,31.6),
                         tm = c(19.9,19.7,17.8,14.1,11.7,10.0,8.8,9.7,11.6,13.9,16.0,18.3))
Urugaiana$TMean = (Urugaiana$TM+Urugaiana$tm)/2
TEMP_graph_Urugaiana = ggplot(Urugaiana,aes(x=mm, group=1))+
  geom_line(aes(y=TM),linetype='dashed',size=1.5)+
  geom_line(aes(y=TMean),size=1.5)+
  geom_line(aes(y=tm),linetype='dotdash',size=1.5)+
  scale_x_discrete(limits=month.name) +
  ylab(bquote('Temperature'~ (''^'o'*'C')))+
  xlab('Months')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 20))
plot(TEMP_graph_Urugaiana)

Hunan = data.frame(mm=month.name,
                       TM = c(6.9,8.9,14.3,20.2,25.3,29.1,32.6,32.2,27.6,21.8,15.8,9.8),
                       tm = c(1.2,3.0,7.5,13.3,18.6,22.6,26.0,25.3,20.6,14.7,9.2,3.6))
Hunan$TMean = (Hunan$TM+Hunan$tm)/2
TEMP_graph_Hunan = ggplot(Hunan,aes(x=mm, group=1))+
  geom_line(aes(y=TM),linetype='dashed',size=1.5)+
  geom_line(aes(y=TMean),size=1.5)+
  geom_line(aes(y=tm),linetype='dotdash',size=1.5)+
  scale_x_discrete(limits=month.name) +
  ylab(bquote('Temperature'~ (''^'o'*'C')))+
  xlab('Months')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),text = element_text(size = 20))
plot(TEMP_graph_Hunan)

PREC_BR_Monthly_Mean2 = data.frame(PREC_BR_Monthly_Mean2, MM = month.name)
colnames(PREC_BR_Monthly_Mean2)= c("Prec","mm")
PREC_BR_Monthly_Mean2[2,1] = 140
PREC_BR_Monthly_Mean2[11,1] = 165
PREC_graph_Br = ggplot(PREC_BR_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=Prec),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Precipitation'~ ('mm')))+
  xlab('Months')
plot(PREC_graph_Br)

PREC_CHN_Monthly_Mean2 = data.frame(PREC_CHN_Monthly_Mean2, MM = month.name)
colnames(PREC_CHN_Monthly_Mean2)= c("Prec","mm")
PREC_graph_Cn = ggplot(PREC_CHN_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=Prec),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Precipitation'~ ('mm')))+
  xlab('Months')
plot(PREC_graph_Cn)

PREC_COL_Monthly_Mean2 = data.frame(PREC_COL_Monthly_Mean2, MM = month.name)
colnames(PREC_COL_Monthly_Mean2)= c("Prec","mm")
PREC_graph_Co = ggplot(PREC_COL_Monthly_Mean2, aes(x=mm, group=1))+
  geom_line(aes(y=Prec),size=1.5)+ 
  scale_x_discrete(limits = month.name)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), text = element_text(size = 20))+
  ylab(bquote('Precipitation'~ ('mm')))+
  xlab('Months')
plot(PREC_graph_Co)

VarMerged_Br = cbind(Urugaiana,PREC_BR_Monthly_Mean2[,1],SOILM_BR_Monthly_Mean2[,1])
VarMerged_Br = cbind(VarMerged_Br, SVI_Br_[seq(1,23,by = 2),2:4])
colnames(VarMerged_Br) = c("Months","T. Max.", "T. Min", "T. Mean", "Prec", "Soil M.","EVI", "NDVI", "NDWI")
Correlation_Br = cor(VarMerged_Br[,2:9],method = "pearson")
corrplot(Correlation_Br, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
pca_Br = prcomp(VarMerged_Br[,2:7], center = TRUE, scale. = TRUE)
summary(pca_Br)
str(pca_Br)
ggbiplot(pca_Br,labels=VarMerged_Br$Months)

VarMerged_Cn = cbind(Hunan,PREC_CHN_Monthly_Mean2[,1],SOILM_CHN_Monthly_Mean2[,1])
VarMerged_Cn = cbind(VarMerged_Cn, SVI_Cn_[seq(1,23,by = 2),2:4])
colnames(VarMerged_Cn) = c("Months","T. Max.", "T. Min", "T. Mean", "Prec", "Soil M.","EVI", "NDVI", "NDWI")
Correlation_Cn = cor(VarMerged_Cn[,2:9],method = "pearson")
corrplot(Correlation_Cn, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
pca_Cn = prcomp(VarMerged_Cn[,2:7], center = TRUE, scale. = TRUE)
summary(pca_Cn)
str(pca_Cn)
ggbiplot(pca_Cn,labels=VarMerged_Cn$Months)

VarMerged_Co = cbind(Hunan,PREC_COL_Monthly_Mean2[,1],SOILM_COL_Monthly_Mean2[,1])
VarMerged_Co = cbind(VarMerged_Co, SVI_Co_[seq(1,23,by = 2),2:4])
colnames(VarMerged_Co) = c("Months","T. Max.", "T. Min", "T. Mean", "Prec", "Soil M.","EVI", "NDVI", "NDWI")
Correlation_Co = cor(VarMerged_Co[,2:9],method = "pearson")
corrplot(Correlation_Co, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
pca_Co = prcomp(VarMerged_Co[,2:7], center = TRUE, scale. = TRUE)
summary(pca_Co)
str(pca_Co)
ggbiplot(pca_Co,labels=VarMerged_Co$Months)

