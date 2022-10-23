
#if (!require("h2o")) install.packages("h2o")
library(h2o)
#if (!require("caTools")) install.packages("caTools")
library(caTools)
#if (!require("ggfortify")) install.packages("ggfortify")
library(ggfortify)

#Aquisión de los datos
#Datos = read.csv2(file.choose())
#DATOS = Datos

#####Incorporacion de los días, meses y años#####
SepararDates = function(DATOS, ColFecha){
  DATOS[[ColFecha]] = as.Date(DATOS[[ColFecha]], "%Y-%m-%d")
  DATOS["Days"] = substr(DATOS[[ColFecha]],9,10)
  DATOS["Days"] = as.numeric(DATOS[["Days"]])
  DATOS["Months"] = substr(DATOS[[ColFecha]],6,7)
  DATOS["Months"] = as.numeric(DATOS[["Months"]])
  DATOS["Years"] = substr(DATOS[[ColFecha]],1,4)
  DATOS["Years"] = as.numeric(DATOS[["Years"]])
  DATOS["MonthsN"] = as.character(DATOS[["Months"]])
  for(i in 1:12){
    Meses = month.name
    DATOS[["MonthsN"]] = replace(DATOS[["MonthsN"]],which(DATOS[["MonthsN"]]==paste(i)),paste(Meses[i]))
  }
  DATOS[["MonthsN"]] = as.factor(DATOS[["MonthsN"]])
  return(DATOS)
}

######Llenar los valores ausentes con su media#####

PreprararData = function(DATOS, Predictores, Predictando, ColMeses, FracEntrenar, SplitVar){
  MeanDATOS = tapply(DATOS[[Predictando]],DATOS[[ColMeses]], mean, na.rm=TRUE) #Llenar NAs con la media/mes
  for(i in 1:12){
    DATOS[which(is.na(DATOS[[Predictando]]) & DATOS[[ColMeses]] == names(MeanDATOS)[i]), Predictando ] = 
      MeanDATOS[[i]]
  }
  EscalarDatos = function(Datos){ #Escalar mis datos
    MinVal = min(Datos)
    MaxVal = max(Datos)
    DatosEscalados = (Datos-MinVal)/(MaxVal-MinVal)
    return(DatosEscalados)
  }
  RangoMM = function(Datos){ #Rango de minimos y maximos de mis datos
    MinVal = apply(Datos, 2, min)
    MaxVal = apply(Datos, 2, max)
    Rango = rbind(Min=MinVal, Max = MaxVal)
    rownames(Rango) = c("Min","Max")
    return(Rango)
  }
  
  set.seed(10000) #Entrenar mi RNA
  SpliT <- sample.split(DATOS[[SplitVar]], SplitRatio = FracEntrenar) #80% de datos para el entrenamiento considarando en num. clusters
  DATOS_train <- subset(DATOS, SpliT == TRUE)
  RDATOS_train = RangoMM(DATOS_train[,c(Predictores, Predictando)])
  DATOS_train = EscalarDatos(DATOS_train[,c(Predictores, Predictando)])
  
  DATOS_test <- subset(DATOS, SpliT == FALSE)
  RDATOS_test = RangoMM(DATOS_test[,c(Predictores, Predictando)])
  DATOS_test = EscalarDatos(DATOS_test[,c(Predictores, Predictando)])
  
  table(DATOS_train$cluster)
  table(DATOS_test$cluster)
  
  DATOS_PREPARADOS = list(Data = DATOS, Trained = DATOS_train, Tested = DATOS_test, MinMaxTrain = RDATOS_train, MinMaxTest = RDATOS_test)
  
  return(DATOS_PREPARADOS)
}

#####Aplicar Red Neuronal#####


RedNeuronalArt = function(DATOS, DATOS_Pr, Predictores, Predictando, MinMax ,Entrenados, Testeados ,h1, h2, 
                          repeticiones, ActFunct){
  h2o.init(nthreads = -1) #Inicializar librería H2O el parámetro -1 permitira hacer uso de todos los cpu
  NN = h2o.deeplearning(y = Predictando, #------------------------> 0
                        training_frame = as.h2o(Entrenados),
                        activation = ActFunct,
                        hidden = c(h1, h2),
                        epochs = repeticiones,
                        train_samples_per_iteration = -2) #Crear red
  prob_pred = h2o.predict(NN, newdata = as.h2o(Testeados)) #------------------------> 1
  Testeados[["Predictions"]] = as.vector(prob_pred$predict)
  #----------------------------------------------------------------------------------------------#
  EscalarDatos = function(Datos){ #Escalar mis datos
    MinVal = min(Datos)
    MaxVal = max(Datos)
    DatosEscalados = (Datos-MinVal)/(MaxVal-MinVal)
    return(DatosEscalados)
  }
  DesEscalarDatos = function(DatosEscalados,MinVal,MaxVal){ #Des-escalar mis datos
    Datos = (DatosEscalados * (MaxVal-MinVal))+ MinVal
    return(Datos)
  }
  #-----------------------------------------------------------------------------------------------#
  Result = DesEscalarDatos(Testeados[,c(Predictando,"Predictions")], MinMax[1,Predictando], MinMax[2,Predictando])
  ErrorMedCuad = 1/nrow(Result) * sum((Result[[Predictando]]-Result[["Predictions"]])^2)
  ErrorMedCuad = format(ErrorMedCuad, nsmall = 5)
  RaizErrorMedCuad = sqrt(1/nrow(Result) * sum((Result[[Predictando]]-Result[["Predictions"]])^2))
  RaizErrorMedCuad = format(RaizErrorMedCuad, nsmall = 5)
  
  #Escalar datos nuevos a predecir
  DATOS_Pr = EscalarDatos(DATOS_Pr[,c(Predictores, Predictando)])
  final_pred = h2o.predict(NN, newdata = as.h2o(DATOS_Pr[,c(Predictores, Predictando)])) #-------------------> 2
  DATOS[["Filled"]] = as.vector(final_pred$predict)
  DATOS[,c("Filled")] = DesEscalarDatos(DATOS[,c("Filled")], MinMax[1,Predictando], MinMax[2,Predictando])
  DATOS[!is.na(DATOS[[Predictando]]), "Filled"] = DATOS[!is.na(DATOS[[Predictando]]), "value"]
  
  ResultadoFinal = list(PredTest = Result, DataFilled = DATOS, MeanSqError = ErrorMedCuad, RootMeanSqError = RaizErrorMedCuad)
  
  return(ResultadoFinal)
  #h2o.shutdown(prompt = FALSE)
}


##################################################################################################################

#DATOS_separados = SepararDates(D1, "Fechas")

#DATOS_preparados = PreprararData(DATOS_separados, c("x","y","Heights", "cluster", "Months", "Years"), "value", "MonthsN", 0.7, "MonthsN")

#DATOS_analizados = RedNeuronalArt(DATOS_separados,DATOS_preparados$Data,c("x","y","Heights", "cluster", "Months", "Years"), "value", DATOS_preparados$MinMaxTest, DATOS_preparados$Trained, DATOS_preparados$Tested,10,10,100000)
