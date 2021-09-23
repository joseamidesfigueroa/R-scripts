#Establezco mi directorio de trabajo
setwd("C:/R/datos_esa")

#Cargo los datos tal cual me los da la consulta
ts_raw <- read.csv('datos_ts.txt',sep = "",skip = "1", header = TRUE)
#ts_raw2 <- read.table('datos_ts.txt',sep = "",skip = "1", header = TRUE)

#Extraigo el índice de la estación
indice <- substr(ts_raw$A.15,1,4)

#Lista Estaciones
lista_estaciones <- cbind('A-15','A-18','A-27','A-31','A-37','B-01','B-06','B-10','C-09','G-03','G-04','G-13','H-08','H-14','L-04','L-27','M-24','N-02','S-10','T-06','T-24',
                          'U-06','V-09','Z-02','Z-03')

#Extraigo la fecha
#fecha <- as.Date(substr(ts_raw$X1981.01.0121.3,1,10))
anio <- substr(ts_raw$X1981.01.0121.3,1,4)
mes <- substr(ts_raw$X1981.01.0121.3,6,7)
dia <- substr(ts_raw$X1981.01.0121.3,9,10)

fecha <- paste(anio,mes,dia,sep = "")

#Extraigo el dato de temperatura.
temp <- substr(ts_raw$X1981.01.0121.3,11,15)

#Uno las 3 columnas en un data frame
datos_temperatura <- data.frame(indice,fecha,temp)


A15_temp <- data.frame(subset(datos_temperatura$temp, datos_temperatura$indice==lista_estaciones[1], 
              select=datos_temperatura$temp))

A15_fecha <- data.frame(subset(datos_temperatura$fecha, datos_temperatura$indice==lista_estaciones[1], 
                         select=datos_temperatura$fecha))

A15 <- cbind(A15_fecha,A15_temp)
names(A15)<-cbind("fecha","temp")

write.table(A15,file = "A15_ts.txt",quote = FALSE, sep = "\t",row.names = FALSE)
