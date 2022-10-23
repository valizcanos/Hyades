my_packages = as.data.frame(installed.packages()[ , c(1, 3:4)]) 
my_packages

Path = choose.dir()

setwd(Path)

FilesList = list.files()

for (i in 1:length(FilesList)){
  pathFiles = paste(Path,"\\",FilesList[i],sep="")
  install.packages(pathFiles, repos = NULL, type = "source")
}


