library(ggplot2)

#---------------------------------------------#

N = data.frame(c(1:70))
Alpha02 = data.frame(c(0.9,	0.68377,	0.56481,	0.49265,	0.44698,	0.41037,	0.38148,	0.35831,	0.3391,	0.3226,	0.30829,	0.29577,	0.2847,	0.27481,	0.26588,	0.25778,	0.25039,	0.2436,	0.23735,	0.23156,	0.22617,	0.22115,	0.21645,	0.21205,	0.2079,	0.20399,	0.2003,	0.1968,	0.19348,	0.19032,	0.18732,	0.18445,	0.18171,	0.17909,	0.17659,	0.17418,	0.17188,	0.16966,	0.16753,	0.16547,	0.16349,	0.16158,	0.15974,	0.15796,	0.15623,	0.15457,	0.15295,	0.15139,	0.14987,	0.1484,	0.14697,	0.14558,	0.14423,	0.14292,	0.14164,	0.1404,	0.13919,	0.13801,	0.13686,	0.13573,	0.13464,	0.13357,	0.13253,	0.13151,	0.13052,	0.12954,	0.12859,	0.12766,	0.12675,	0.12586))
Alpha01 = data.frame(c(0.95,	0.77639,	0.63604,	0.56522,	0.50945,	0.46799,	0.43607,	0.40962,	0.38746,	0.36866,	0.35242,	0.33815,	0.32549,	0.31417,	0.30397,	0.29472,	0.28627,	0.27851,	0.27136,	0.26473,	0.25858,	0.25283,	0.24746,	0.24242,	0.23768,	0.2332,	0.22898,	0.22497,	0.22117,	0.21756,	0.21412,	0.21085,	0.20771,	0.20472,	0.20185,	0.1991,	0.19646,	0.19392,	0.19148,	0.18913,	0.18687,	0.18468,	0.18257,	0.18053,	0.17856,	0.17665,	0.17481,	0.17302,	0.17128,	0.16959,	0.16796,	0.16637,	0.16483,	0.16332,	0.16186,	0.16044,	0.15906,	0.15771,	0.15639,	0.15511,	0.15385,	0.15263,	0.15144,	0.15027,	0.14913,	0.14802,	0.14693,	0.14587,	0.14483,	0.14381))
Alpha005 = data.frame(c(0.975,	0.84189,	0.7076,	0.62394,	0.56328,	0.51926,	0.48342,	0.45427,	0.43001,	0.40925,	0.39122,	0.37543,	0.36143,	0.3489,	0.3376,	0.32733,	0.31796,	0.30936,	0.30143,	0.29408,	0.28724,	0.28087,	0.2749,	0.26931,	0.26404,	0.25907,	0.25438,	0.24993,	0.24571,	0.2417,	0.23788,	0.23424,	0.23076,	0.22743,	0.22425,	0.22119,	0.21826,	0.21544,	0.21273,	0.21012,	0.2076,	0.20517,	0.20283,	0.20056,	0.19837,	0.19625,	0.1942,	0.19221,	0.19028,	0.18841,	0.18659,	0.18482,	0.18311,	0.18144,	0.17981,	0.17823,	0.17669,	0.17519,	0.17373,	0.17231,	0.17091,	0.16956,	0.16823,	0.16693,	0.16567,	0.16443,	0.16322,	0.16204,	0.16088,	0.15975))
Alpha002 = data.frame(c(0.99,	0.9,	0.78456,	0.68887,	0.62718,	0.57741,	0.53844,	0.50654,	0.4796,	0.45662,	0.4367,	0.41918,	0.40362,	0.3897,	0.37713,	0.36571,	0.35528,	0.34569,	0.33685,	0.32866,	0.32104,	0.31394,	0.30728,	0.30104,	0.29516,	0.28962,	0.28438,	0.27942,	0.27471,	0.27023,	0.26596,	0.26189,	0.25801,	0.25429,	0.25073,	0.24732,	0.24404,	0.24089,	0.23786,	0.23494,	0.23213,	0.22941,	0.22679,	0.22426,	0.22181,	0.21944,	0.21715,	0.21493,	0.21277,	0.21068,	0.20864,	0.20667,	0.20475,	0.20289,	0.20107,	0.1993,	0.19758,	0.1959,	0.19427,	0.19267,	0.19112,	0.1896,	0.18812,	0.18667,	0.18525,	0.18387,	0.18252,	0.18119,	0.1799,	0.17863))
Alpha001 = data.frame(c(0.995,	0.92929,	0.829,	0.73424,	0.66853,	0.61661,	0.57581,	0.54179,	0.51332,	0.48893,	0.4677,	0.44905,	0.43247,	0.41762,	0.4042,	0.39201,	0.38086,	0.37062,	0.36117,	0.35241,	0.34427,	0.33666,	0.32954,	0.32286,	0.31657,	0.31064,	0.30502,	0.29971,	0.29466,	0.28987,	0.2853,	0.28094,	0.27677,	0.27279,	0.26897,	0.26532,	0.2618,	0.25843,	0.25518,	0.25205,	0.24904,	0.24613,	0.24332,	0.2406,	0.23798,	0.23544,	0.23298,	0.23059,	0.22828,	0.22604,	0.22386,	0.22174,	0.21968,	0.21768,	0.21574,	0.21384,	0.21199,	0.21019,	0.20844,	0.20673,	0.20506,	0.20343,	0.20184,	0.20029,	0.19877,	0.19729,	0.19584,	0.19442,	0.19303,	0.19167))

KolmogorovSmirnovTable= cbind(N,Alpha02,Alpha01,Alpha005,Alpha002,Alpha001)
colnames(KolmogorovSmirnovTable) = c("n","0.2","0.1","0.05","0.02","0.01")
#---------------------------------------------#

##### Upload Data #####

#Data = read.csv2(file.choose())

##### Prepare Data #####

DataMax = function(Data, ColDate, Val){
  Data[[ColDate]] = as.Date(Data[[ColDate]], "%Y-%m-%d")
  Data["Days"] = substr(Data[[ColDate]],9,10)
  Data["Days"] = as.numeric(Data[["Days"]])
  Data["Months"] = substr(Data[[ColDate]],6,7)
  Data["Months"] = as.numeric(Data[["Months"]])
  Data["Years"] = substr(Data[[ColDate]],1,4)
  Data["Years"] = as.numeric(Data[["Years"]])
  Data["MonthsN"] = as.character(Data[["Months"]])
  for(i in 1:12){
    Meses = month.name
    Data[["MonthsN"]] = replace(Data[["MonthsN"]],which(Data[["MonthsN"]]==paste(i)),paste(Meses[i]))
  }
  Data[["MonthsN"]] = as.factor(Data[["MonthsN"]])
  Maxi = tapply(Data[[Val]], Data[["Years"]], max)
  YYYY = unique(Data["Years"])
  DMax= data.frame(YYYY,Maxi)
  colnames(DMax) = c("Years", "MaxValue")
  rownames(DMax) = c(1:nrow(DMax))
  return(DMax)
}

DataMin = function(Data, ColDate, Val){
  Data[[ColDate]] = as.Date(Data[[ColDate]], "%Y-%m-%d")
  Data["Days"] = substr(Data[[ColDate]],9,10)
  Data["Days"] = as.numeric(Data[["Days"]])
  Data["Months"] = substr(Data[[ColDate]],6,7)
  Data["Months"] = as.numeric(Data[["Months"]])
  Data["Years"] = substr(Data[[ColDate]],1,4)
  Data["Years"] = as.numeric(Data[["Years"]])
  Data["MonthsN"] = as.character(Data[["Months"]])
  for(i in 1:12){
    Meses = month.name
    Data[["MonthsN"]] = replace(Data[["MonthsN"]],which(Data[["MonthsN"]]==paste(i)),paste(Meses[i]))
  }
  Data[["MonthsN"]] = as.factor(Data[["MonthsN"]])
  Mini = tapply(Data[[Val]], Data[["Years"]], min)
  YYYY = unique(Data["Years"])
  DMin= data.frame(YYYY,Mini)
  colnames(DMin) = c("Years", "MinValue")
  rownames(DMin) = c(1:nrow(DMin))
  return(DMin)
}
##### Scale Parameter (alpha) #####
AlphaFq = function(DataM){
  Alpha = (sqrt(6) * sd(DataM, na.rm = TRUE))/pi
  return(Alpha)
}

##### Location Parameter (U) #####

UFq = function(DataM, Alpha){
  DataMean = mean(DataM, na.rm=TRUE)
  U_ = DataMean - 0.5772*Alpha 
  return(U_)
}

##### Shape Parameter (k) #####

##### EVI #####

EVIMax = function(DataM,Alpha,U_){
  DataM = tapply(DataM[,"Years"], DataM[,"MaxValue"], sort)
  DataM = data.frame(DataM)
  DataM[["MaxValue"]] = as.numeric(rownames(DataM))
  colnames(DataM) = c("Years","MaxValue")
  y = (DataM[,"MaxValue"] - U_)/Alpha
  EVI = exp(-exp(-y))
  EVI_DF = data.frame(DataM,EVI,y)
  colnames(EVI_DF) = c("Years","MaxSortedData", "EVI_Fx","yt")
  rownames(EVI_DF) = c(1:nrow(EVI_DF))
  return(EVI_DF)
}

EVIMin = function(DataM,Alpha,U_){
  #DataM = tapply(DataM[,"Years"], DataM[,"MinValue"], sort, decreasing=TRUE)
  DataM = DataM[order(DataM[["MinValue"]], decreasing = TRUE),]
  #DataM = data.frame(DataM)
  #DataM[["MinValue"]] = as.numeric(rownames(DataM))
  colnames(DataM) = c("Years","MinValue")
  y = (DataM[,"MinValue"] - U_)/Alpha
  EVI = exp(-exp(-y))
  EVI_DF = data.frame(DataM,EVI,y)
  colnames(EVI_DF) = c("Years","MinSortedData", "EVI_Fx","yt")
  rownames(EVI_DF) = c(1:nrow(EVI_DF))
  return(EVI_DF)
}

EVI_RP = function(EVI){
  idx = which(EVI[["yt"]]>=-10)#By defaukt gt 0
  EVI = EVI[c(idx),]
  EVI[["PR"]] = 1/(1-exp(-exp(-EVI[["yt"]])))
  return(EVI)
}

##### Goodness Of Fit Test #####

GoodnessOfFitTestKolmogorov = function(EVI, SignificanceLevel){
  n = c(1:nrow(EVI))
  N = nrow(EVI)
  EVI_Fn = n/(N+1)
  EVI_Fn = data.frame(EVI_Fn)
  colnames(EVI_Fn) = c("EVI_Fn")
  EVI = cbind(EVI,EVI_Fn)
  D_ = max(abs(EVI[["EVI_Fx"]]-EVI[["EVI_Fn"]]))
  
  if(N<=70){
    D_t = KolmogorovSmirnovTable[N,SignificanceLevel]
  } else{
    if(SignificanceLevel=="0.2"){
      D_t = 1.07/sqrt(N)
    } else if(SignificanceLevel=="0.1"){
      D_t = 1.22/sqrt(N)
    } else if(SignificanceLevel=="0.05") {
      D_t = 1.36/sqrt(N)
    } else if(SignificanceLevel=="0.02") {
      D_t = 1.52/sqrt(N)
    }  else if(SignificanceLevel=="0.01"){
      D_t = 1.63/sqrt(N)
    }
  }
  
  if(D_<D_t){
    cat("D = ", D_," < ", "Dt = " ,D_t, " : Accepted")
  } else{
    cat("D = " ,D_," > ", "Dt = ", D_t, " : Not accepted")
  }
}


R2 = function(EVI){
  n = c(1:nrow(EVI))
  N = nrow(EVI)
  EVI_Fn = n/(N+1)
  EVI_Fn = data.frame(EVI_Fn)
  colnames(EVI_Fn) = c("EVI_Fn")
  EVI = cbind(EVI,EVI_Fn)
  R2 = cor(EVI[["EVI_Fn"]],EVI[["EVI_Fx"]],method = "pearson")
  R2 = (R2)^2
  #return(R2)
  cat("R2 = ", R2)
}

##### Estimation #####

MaxValByT = function(ReturnPeriod,Alpha,U_){
  yt = -log(log(ReturnPeriod/(ReturnPeriod-1))) 
  xt = U_ + Alpha*yt
  #Indica el valor de ... que sea igualado o excedido en un periodo de retorno determinado
  return(xt)
}

POcur = function(xt,Alpha,U_){
  yt = (xt-U_)/Alpha
  P = exp(-exp(-yt))
  #Indica la probabilidad de que un evento X sea menor o igual a xt
  #P(X<=xt)
  return(P)
}
RetP = function(POcur){
  # P(X>=xt) = 1-P
  # T = 1-(1/P)
  # Indica el tiempo de ocurrencias de X >= xt
  RP = 1/(1-POcur)
  return(RP)
}

POcurUnaVez = function(xt,Alpha,U_,N){
  yt = (xt-U_)/Alpha
  P = 1- exp(-exp(-yt))
  RP = 1/(P)
  # Indica la probabilidad de que el evento P(X>=xt) ocurra al menos una vez en N años
  #P_1 = 1-(1-P)^N
  P_1 = 1-(1-(1/RP))^N
  return(P_1)
}

XtConYt = function(yt,Alpha,U_){
  xt = (yt*Alpha) + U_
  # Me da el valor de xt segun el yt indicado
  return(xt)
}

XtConPOcur = function(POcur,Alpha,U_){
  yt = -log(log(1/POcur))
  xt = U_ + Alpha*yt
  # Me indica el valor de xt de acuerdo a su probabilidad de ocurrencia P(X<=xt)
  return(xt)
}

XtConPR = function(PR,Alpha,U_){
  yt = -log(log(PR/(PR-1)))
  xt = U_ + Alpha*yt
  # Me indica el valor de xt de acuerdo a su periodo de retorno
  return(xt)
}
#---------------------------------------------#

#DataMaxi = DataMax(Data, "Fechas", "value")
#Valor_A = AlphaFq(DataMaxi$MaxValue)
#Valor_U = UFq(DataMaxi$MaxValue, Valor_A)
#Fx_EVI = EVI(DataMaxi, Valor_A, Valor_U)
#Fx_EVI_PR = EVI_RP(Fx_EVI)
#GoodnessOfFitTestKolmogorov(Fx_EVI, "0.05")
#R2(Fx_EVI)


#MaxValByT(60,Valor_A,Valor_U)
#POcur(600,Valor_A, Valor_U)
#RetP(POcur(500,Valor_A, Valor_U))
#POcurUnaVez(600,Valor_A, Valor_U, 10)

#XtConPOcur(0.25,Valor_A,Valor_U)
#XtConPR(5,Valor_A,Valor_U)

#Yend1= XtConYt(0,Valor_A, Valor_U)

#ggplot(Fx_EVI, aes(x=MaxSortedData, y=EVI_Fx)) + geom_line() #Probability of ocurrence
#ggplot(Fx_EVI, aes(x=yt, y=MaxSortedData)) + geom_line() + 
#  geom_segment(aes(x=0, y=0, xend=0, yend=Yend1), color="red",linetype="dashed")+
#  geom_segment(aes(x=min(yt), y=Yend1, xend=0, yend=Yend1), color="red",linetype="dashed") # with yt
#ggplot(Fx_EVI_PR, aes(x=PR, y=MaxSortedData)) + geom_line() #Return periods
#ggplot(DataMaxi, aes(MaxValue)) +  geom_histogram(binwidth = 5, aes(y=..density..)) + geom_density(position = "stack")

#plot(Fx_EVI$MaxSortedData,Fx_EVI$EVI_Fx) 
#hist(Data$value)
#plot(Fx_EVI$yt,Fx_EVI$MaxSortedData)

#---------------------------------------------#
#DataMini = DataMin(Data, "Fechas", "value")
#Valor_A2 = AlphaFq(DataMini$MinValue)
#Valor_U2 = UFq(DataMini$MinValue, Valor_A)
#Fx_EVI_min = EVIMin(DataMini, Valor_A2, Valor_U2)
#EVI_RP(Fx_EVI_min)
