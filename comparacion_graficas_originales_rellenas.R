library(climatol)
#Graficas comparativas originales vs rellenas
#Define el directorio de trabajo
setwd("C:/R/climatol/Homogenizacion_climatol/ts/")


#Lista estaciones
indice<-read.table("ts_1975-2018.est")


lista.estaciones<-c("Guija","Finca los Andes","Candelaria de la frontera", "Planes de Montecristo", "Santa Ana",
                    "Chorrera del Guayabo", "Sensuntepeque", "Cerron Grande", "Cojutepeque", "Nueva Concepcion",
                    "La Palma", "Las Pilas", "Ahuachapan", "La Hachadura", "San Andres", "Chiltiuipan", "San Miguel",
                    "La Union", "Ilopango", "Acajutla", "Los Naranjos", "Santiago de Maria", "Puente Cuscatlan",
                    "San Francisco Gotera", "Perquin")

#Extraer nombres de estaciones
#for (i in c(1:25)) {
#  lista.estaciones[1,i]<-indice[i,5]
#}

#*************************************************************************************
#Define el directorio de trabajo
setwd("C:/R/climatol/Homogenizacion_climatol/ts/")

#Carga los datos trabajados
load("ts_1975-2018.rda")

#Carga los datos crudos (Originales) en un data frame
datos_crudos<-data.frame(dat)
names(datos_crudos)<-lista.estaciones

#Carga los datos rellenos en un data frame
datos_rellenos<-data.frame(dah)
names(datos_rellenos)<-lista.estaciones

#which.min(datos_crudos$Ilopango)

#datos_rellenos[14807,19]<-246.

for(i in c(1:25)){

  nombre.grafica<-paste("comparacion_ts_",lista.estaciones[i],".jpg",sep="")
  
  jpeg(nombre.grafica, width = 1200, height = 800)
  par(mfrow=c(2,1))
  #Plot los datos crudos
  a<-ts(datos_crudos[i], frequency=365,start = 1975)
  
  nombre.grafica1<-paste(lista.estaciones[i],".jpg",sep="")
  texto1=paste("Serie de tiempo original de temperatura media (C), ",lista.estaciones[i],sep="")
  plot(a, main=texto1, xlab="", ylab="Temperatura",col="red")#, ylim=c(5,33))
  
  #Plot los datos rellenos
  b<-ts(datos_rellenos[i], frequency=365,start = 1975)

  texto2=paste("Serie de tiempo rellena de temperatura media (C), ",lista.estaciones[i],sep="")
  plot(b, main=texto2, xlab="", ylab="Temperatura",col="maroon")#, ylim=c(5,33))
  dev.off()
}

#*************************************************************************************



#*************************************************************************************
#Define el directorio de trabajo
setwd("C:/R/climatol/Homogenizacion_climatol/tmax/")

#Carga los datos trabajados
load("tmax_1975-2018.rda")

#Carga los datos crudos (Originales) en un data frame
datos_crudos<-data.frame(dat)
names(datos_crudos)<-lista.estaciones

#Carga los datos rellenos en un data frame
datos_rellenos<-data.frame(dah)
names(datos_rellenos)<-lista.estaciones

for(i in c(1:25)){
  
  nombre.grafica<-paste("comparacion_tmax_",lista.estaciones[i],".jpg",sep="")
  
  jpeg(nombre.grafica, width = 1200, height = 800)
  par(mfrow=c(2,1))
  #Plot los datos crudos
  a<-ts(datos_crudos[i], frequency=365,start = 1975)
  
  nombre.grafica1<-paste(lista.estaciones[i],".jpg",sep="")
  texto1=paste("Serie de tiempo original de temperatura maxima (C), ",lista.estaciones[i],sep="")
  plot(a, main=texto1, xlab="", ylab="Temperatura",col="red")#, ylim=c(5,33))
  
  #Plot los datos rellenos
  b<-ts(datos_rellenos[i], frequency=365,start = 1975)
  
  texto2=paste("Serie de tiempo rellena de de temperatura maxima (C), ",lista.estaciones[i],sep="")
  plot(b, main=texto2, xlab="", ylab="Temperatura",col="navyblue")#, ylim=c(5,33))
  dev.off()
}

#*************************************************************************************



#*************************************************************************************
#Define el directorio de trabajo
setwd("C:/R/climatol/Homogenizacion_climatol/tmin/")

#Carga los datos trabajados
load("tmin_1975-2018.rda")

#Carga los datos crudos (Originales) en un data frame
datos_crudos<-data.frame(dat)
names(datos_crudos)<-lista.estaciones

#Carga los datos rellenos en un data frame
datos_rellenos<-data.frame(dah)
names(datos_rellenos)<-lista.estaciones

for(i in c(1:25)){
  
  nombre.grafica<-paste("comparacion_tmin_",lista.estaciones[i],".jpg",sep="")
  
  jpeg(nombre.grafica, width = 1200, height = 800)
  par(mfrow=c(2,1))
  #Plot los datos crudos
  a<-ts(datos_crudos[i], frequency=365,start = 1975)
  
  nombre.grafica1<-paste(lista.estaciones[i],".jpg",sep="")
  texto1=paste("Serie de tiempo original de temperatura maxima (C), ",lista.estaciones[i],sep="")
  plot(a, main=texto1, xlab="", ylab="Temperatura",col="red")#, ylim=c(5,33))
  
  #Plot los datos rellenos
  b<-ts(datos_rellenos[i], frequency=365,start = 1975)
  
  texto2=paste("Serie de tiempo rellena de de temperatura maxima (C), ",lista.estaciones[i],sep="")
  plot(b, main=texto2, xlab="", ylab="Temperatura",col="cyan")#, ylim=c(5,33))
  dev.off()
}

#*************************************************************************************




#*************************************************************************************
#Define el directorio de trabajo
setwd("C:/R/climatol/Homogenizacion_climatol/pd/")

#Carga los datos trabajados
load("pd_1975-2018.rda")

#Carga los datos crudos (Originales) en un data frame
datos_crudos<-data.frame(dat)
names(datos_crudos)<-lista.estaciones

#Carga los datos rellenos en un data frame
datos_rellenos<-data.frame(dah)
names(datos_rellenos)<-lista.estaciones

for(i in c(1:25)){
  
  nombre.grafica<-paste("comparacion_pd_",lista.estaciones[i],".jpg",sep="")
  
  jpeg(nombre.grafica, width = 1200, height = 800)
  par(mfrow=c(2,1))
  #Plot los datos crudos
  a<-ts(datos_crudos[i], frequency=365,start = 1975)
  
  nombre.grafica1<-paste(lista.estaciones[i],".jpg",sep="")
  texto1=paste("Serie de tiempo original de precipitacion diaria, ",lista.estaciones[i],sep="")
  plot(a, main=texto1, xlab="", ylab="Precipitacion (mm)",col="red")#, ylim=c(5,33))
  
  #Plot los datos rellenos
  b<-ts(datos_rellenos[i], frequency=365,start = 1975)
  
  texto2=paste("Serie de tiempo rellena de precipitacion diaria, ",lista.estaciones[i],sep="")
  plot(b, main=texto2, xlab="", ylab="Precipitacion (mm)",col="green")#, ylim=c(5,33))
  dev.off()
}

#*************************************************************************************


