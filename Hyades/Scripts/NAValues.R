library(dplyr)
#library(tidyverse)
library(lubridate)
library(ggplot2)

#################################################################################################
SeparateDates = function(DataSet, DateColName){
  DataSet[[paste(DateColName)]] = as.Date(DataSet[[paste(DateColName)]], format="%Y-%m-%d") #Ajustar fechas
  DataSet = DataSet %>% dplyr::mutate(Years= lubridate::year(DataSet[[paste(DateColName)]]), #Convertir a años, meses y días
                                                  Months = lubridate::month(DataSet[[paste(DateColName)]]),
                                                  Days = lubridate::day(DataSet[[paste(DateColName)]]))
  return(DataSet)
}
#################################################################################################
#AN?LISIS POR CLUSTER

SeparateByCluster = function(DataSet, NumberOfCluster){
  if(NumberOfCluster == 0){
    DataSet
  }else{
  DataSet = dplyr::filter(DataSet, cluster == NumberOfCluster) }# Separar datos por cluster
  return(DataSet)
}

#################################################################################################
#AN?LISIS POR ESTACION

SeparatedBySeason = function(DataSet, Season1, Season2, Season3, Season4){
  SM1 = switch(paste(Season1[1]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM2 = switch(paste(Season1[2]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM3 = switch(paste(Season1[3]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM4 = switch(paste(Season2[1]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM5 = switch(paste(Season2[2]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM6 = switch(paste(Season2[3]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM7 = switch(paste(Season3[1]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM8 = switch(paste(Season3[2]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM9 = switch(paste(Season3[3]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM10 = switch(paste(Season4[1]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM11 = switch(paste(Season4[2]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  SM12 = switch(paste(Season4[3]), "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "Octuber"=10, "November"=11, "December"=12)
  WinterQ = dplyr::filter(DataSet, Months %in% c(SM1,SM2,SM3))
  SpringQ = dplyr::filter(DataSet, Months %in% c(SM4,SM5,SM6))
  SummerQ = dplyr::filter(DataSet, Months %in% c(SM7,SM8,SM9))
  AutumnQ = dplyr::filter(DataSet, Months %in% c(SM10,SM11,SM12))
  TheSeasons = list(S1=WinterQ,S2=SpringQ,S3=SummerQ,S4=AutumnQ)
  return(TheSeasons)
}

#################################################################################################
#AN?LISIS POR MES
SeparatedByMonth = function(DataSet){
  January = dplyr::filter(DataSet, Months==1)
  February = dplyr::filter(DataSet, Months==2)
  March = dplyr::filter(DataSet, Months==3)
  April = dplyr::filter(DataSet, Months==4)
  May = dplyr::filter(DataSet, Months==5)
  June = dplyr::filter(DataSet, Months==6)
  July = dplyr::filter(DataSet, Months==7)
  August = dplyr::filter(DataSet, Months==8)
  September = dplyr::filter(DataSet, Months==9)
  Octuber = dplyr::filter(DataSet, Months==10)
  November = dplyr::filter(DataSet, Months==11)
  December = dplyr::filter(DataSet, Months==12)
  return(list(January=January, February=February, March=March, April=April, May=May, June=June, July=July, 
              August=August, September=September, Octuber=Octuber,November=November, December=December))
}






