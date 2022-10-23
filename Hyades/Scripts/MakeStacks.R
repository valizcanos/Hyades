SetOfRasters = list.files(choose.dir(), pattern = "_Cutted.tif$", full.names = TRUE)
StacksRaster = stack(SetOfRasters)
nlayers(StacksRaster)
length(values(StacksRaster[[1]]))

Fechas = gsub(SetOfRasters, pattern = "_Cutted.tif$", replacement = "")
Fechas = substr(Fechas, nchar(Fechas)-9,nchar(Fechas))
Fechas = rep(Fechas, length(values(StacksRaster[[1]])))
Fechas = sort(Fechas, decreasing = FALSE)
Fechas = data.frame(Fechas)

RasterDF = melt(as.data.frame(StacksRaster[[1:nlayers(StacksRaster)]], row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE), id=c("x","y"))
RasterDF = RasterDF[,-c(3)]
RasterDF = cbind(Fechas,RasterDF)

