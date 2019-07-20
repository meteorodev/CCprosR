#### este script mensualiza los archivos que estan en formato hydrobid
##### para los datso de precipitacion se tranforma de cn a mm 
library(readr)

#@param a = value to verified if is leap-year
isbisiesto <- function(a){
  if((a %% 4 == 0 ) & (a %% 100 != 0) | (a %% 400 == 0 )){
    return(c(31,29,31,30,31,30,31,31,30,31,30,31))
  }
  return(c(31,28,31,30,31,30,31,31,30,31,30,31)) 
}

# @param dataframe en formato hydrobid
# @param oper = 1 , 1 suma acumula para RR, 2 promedio para temp
toMounth <- function(frame, oper = 1){
  frame[,1] <- as.Date(frame[,1],format="%d/%m/%Y")
  fi <- frame[1,1]
  ff <- frame[nrow(frame),1]
  fin = 0
  mensual <- data.frame(years=c(),ene=c(),feb=c(),mar=c(),abr=c(),may=c(),jun=c(),jul=c(),ago=c(),sep=c(),oct=c(),nov=c(),dic=c())
  year <- c()
  for(a in seq(1960,1993)){
    ds <- isbisiesto(a)
    #print(a)
    newrow <- data.frame(years=c(a),ene=c(0),feb=c(0),mar=c(0),abr=c(0),may=c(0),jun=c(0),jul=c(0),ago=c(0),sep=c(0),oct=c(0),nov=c(0),dic=c(0))
    #print(newrow)
    for (m in seq(1,12)) {
      ini <- fin + 1
      fin <- fin + ds[m]
      mes <- frame[ini:fin,]
      if(oper == 1){
        newrow[1, m+1 ] <- sum(mes[,2]) * 10
      }
      if(oper == 2 ){
        newrow[1, m+1 ] <- mean(mes[,2])
      }
    }
    mensual <- rbind(mensual,newrow)
    
  }
  #temp <- fechas[(fechas >  as.Date("1960-01-05") ) & (fechas < as.Date("1960-01-31") )]
  return(mensual)
}

#### genera archivos tabulares desde  los archivos en formato hydrobid
writeMounth <-function(rutEnt,rutSal, oper=1){
  setwd(rutEnt)
  fileLis <- list.files(pattern = ".csv")
  for (f in fileLis) {
    filePAth <- paste(rutEnt,f,sep = "")
    print(paste("procesando ",filePAth,sep = " -> "))
    frame <- read.delim(filePAth, header = TRUE, sep = ",", dec = ".", encoding="UTF-8")
    dataWrite <- toMounth(frame = frame, oper = oper)
    #plot(dataWrite$`flow(m3/s)`,type = "l")
    #print(paste(rutSal,f,sep = ""))
    write_delim(dataWrite,paste(rutSal,f,sep = ""),delim = ";",col_names = T)
  }
  #filePAth <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMin/P1.csv"
  #frame <- read.delim(filePAth, header = TRUE, sep = ",", dec = ".", encoding="UTF-8")
  #mensual <- toMounth(frame)
  #print(mensual)
  
}

#source('/media/darwin/Misdatos/Desarrollo/R/republicaDom/fillFlow.R')

#source('/media/darwin/Misdatos/Desarrollo/R/republicaDom/dataRTFormat.R')

##precipitacion
#writeMounth("/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/Precipitacion/", "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/MensualTab/Precipitacion/",1)
### temperatura maxima
#writeMounth("/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMax/","/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/MensualTab/TempMax/",2)
### temperatura minima
#writeMounth("/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMin/","/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/MensualTab/TempMin/",2)
### temperatura media
#writeMounth("/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMed/","/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/MensualTab/TempMed/",2)
### caudales 
writeMounth("/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/caudalesHydrobid/","/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/MensualTab/Caudal/",2)
