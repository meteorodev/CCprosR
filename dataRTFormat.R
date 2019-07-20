#Script de transformacion de datos tabularea a formato hydrobid
## seccion de librerias 

library(readr)
library(readxl)


#@param a = value to verified if is leap-year


toDay <- function(file, rutSal,variable,estaciones){
  
  dates <- seq.Date(from = as.Date("1960/1/1"), to = as.Date("1993/12/31"), by = "days")
  dates 
  frame <- read.delim(file,header = T, sep =" ") 
  for (i in seq(1,nrow(frame))) {
    datarow <- as.numeric(as.vector(frame[i,]))
    if(variable == 1){## variable sde precipitacion se debe trasnformar multiplicando po 0.1
      datarow = datarow * 0.1
    }
    df <- data.frame(format(dates, "%d/%m/%Y"),datarow)
    names(df) <- c("date","value")
    write_delim(x=df,path =paste(rutSal,estaciones[i,1],".csv",sep ="" ),delim = ",",col_names = T)
  }
  
}

TemMedia <- function(rutMax, rutMin, rutSal , estaciones){
  
  for (i in seq(1,nrow(estaciones))) {
    frameMax <- read.delim(paste(rutMax,"/",estaciones[i,1],".csv",sep = ""),header = T, sep =",") 
    frameMin <- read.delim(paste(rutMin,"/",estaciones[i,1],".csv",sep = ""),header = T, sep =",") 
    valor <- (frameMax[,2] + frameMin[,2])/2
    df <- data.frame(frameMin[,1],valor)
    names(df) <- c("date","value")
    write_delim(x=df,path =paste(rutSal,estaciones[i,1],".csv",sep ="" ),delim = ",",col_names = T)
    print(head(df,5))
  }
  
}

#### call functions 

  estaciones <- read_excel("/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/Estaciones_final_código.xls")
  #######Precipitacion 
  rutaEn <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/stations_precip_dia_1960_1993.txt"
  rutSal <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/Precipitacion/"
  estascio <- toDay(rutaEn,rutSal,variable = 1,estaciones = estaciones)
  
  #####temperatura maxima
  rutaEn <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/stations_tmax_dia_1960_1993.txt"
  rutSal <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMax/"
  estascio <- toDay(rutaEn,rutSal,variable = 2,estaciones = estaciones)
  
  #####temperatura minima 
  rutaEn <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/stations_tmin_dia_1960_1993.txt"
  rutSal <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMin/"
  estascio <- toDay(rutaEn,rutSal,variable = 2,estaciones = estaciones)
  
  ####Calculo de la media
  rutMax <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMax/"
  rutMin <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMin/"
  rutSal <- "/media/darwin/Misdatos/Desarrollo/R/republicaDom/data/Presente/TempMed/"
  TemMedia(rutMax,rutMin,rutSal,estaciones)