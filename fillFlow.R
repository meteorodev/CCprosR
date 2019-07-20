#this script fillFlow base en random number into a normal distribution
######## libraries Section #########
library(readr)

#@param a = value to verified if is leap-year

isbisiesto <- function(a){
  if((a %% 4 == 0 ) & (a %% 100 != 0) | (a %% 400 == 0 )){
    return(c(31,29,31,30,31,30,31,31,30,31,30,31))
  }
  return(c(31,28,31,30,31,30,31,31,30,31,30,31)) 
}
frame <- data.frame()
#function to compute the normal for fill a value
fillNormal <- function(filePAth) {
  #read data 
  frame <- read.delim(filePAth, header = TRUE, sep = ";", dec = ".", encoding="UTF-8")
  
  
  meanData <- c()
  sdData <- c()
  medianData <- c()
  # first compute by every day the mean sd and q50 median
  for(d in seq(1,31)){
    #print(frame[c(d+3)])
    dataday <- frame[c(d+3)]
    mt <- c(mean (dataday[[1]], na.rm = TRUE ))
    st <- c(sd(dataday[[1]], na.rm = TRUE ))
    mnt <- c(median(dataday[[1]], na.rm = TRUE ))
    meanData <- c(meanData,mt)
    sdData <- c(sdData,st)
    medianData <- c(medianData,mnt)
  }
  cdate <- c()
  cflow <- c()
  
  for (i in seq(1, nrow(frame))) {
    mf <- frame[ i , 2 ]
    af <- frame[ i , 3 ]
    bis <- isbisiesto(af)
    #print(paste(af,"-",mf,"-",bis[mf]))
    #print(paste("mes a tratar ",to,"  ",mf))
    
    for (j in seq( 1, bis[mf])) {
      if(is.na(frame[i,j+3])){
        dfil <- abs(rnorm(1,mean = meanData[j],sd = sdData[j]))
        #print(paste(frame[i,2],"/",frame[i,3],"/",j," -- ",frame[i,j+3],"  --  ",dfil))
        cdate <- c(cdate,paste(j,"/",mf,"/",af,sep = ""))
        #print(paste(j,"/",mf,"/",af,sep = ""))
        cflow <- c(cflow,dfil)
      }else{
        cdate <- c(cdate,paste(j,"/",mf,"/",af,sep = ""))
        cflow <- c(cflow,frame[i,j+3])
      }
    }
  }
  cdate <- as.Date(cdate,format="%d/%m/%Y")
  estacion <-data.frame(format(cdate, "%d/%m/%Y"),cflow)
  names(estacion) <- c("date","flow(m3/s)")
  return(estacion)
}


##### funcion general para llamas a las demas funciones 

ejecutar <- function(rutEnt,rutSal){
  ### lista los archivos de la entrada
  setwd(rutEnt)
  fileLis <- list.files(pattern = ".csv")
  #station
  # filePAth <- paste(rutEnt,fileLis[1],sep = "")
  # print(filePAth)
  # dataWrite <- fillNormal(filePAth)
  
  for (f in fileLis) {
    filePAth <- paste(rutEnt,f,sep = "")
    print(filePAth)
    dataWrite <- fillNormal(filePAth)
    #plot(dataWrite$`flow(m3/s)`,type = "l")
    write_delim(dataWrite,paste(rutSal,f,sep = ""),delim = ",",col_names = T)
  }
  
}
##ruta de entrada de datos
rutEnt <-"/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/caudales/"
##Ruta de salida dedatos
rutSal <-"/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/caudalesHydrobid/"

ejecutar(rutEnt = rutEnt, rutSal = rutSal)


