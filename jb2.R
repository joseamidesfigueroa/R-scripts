#Script para mi amor caribe
#Nunca seras molestia corazón.

#Define el escritorio de trabajo.
setwd("C:/R/")

#Abre los datos para trabajar como un dataframe, el archivo que tienes que preparar es un csv normal, por cada columna hay datos y se guardan en un dataframe para
#Trabajarlo en R. OJO: debe estar en el directorio de trabajo que esta definido en la linea 4.
datos<-as.data.frame(read.table("datosJB2.csv",sep=","))

fecha1<-paste(datos$V1,datos$V2,sep="-")

fechas<-as.POSIXct(fecha1,format="%d/%m/%Y-%H:%M")

#Define el nombre del archivo de la grafica que se va a generar, se guardara en el directorio de trabajo, sera un jpg
nombre.grafica<-paste("comparacion_perfiladores.jpg",sep="")

#Comienza a construir la grafica, el tamaño tu lo defines como quieras. Aqui el comando "par" define una grafica de dos filas y una columna.
jpeg(nombre.grafica, width = 1200, height = 800, res=100)
par(mfrow=c(3,1))

#****************************************************************************************************************************************************
#Genera una serie de tiempo con los datos del perfilador A
#a<-ts(datos$V2, frequency=365,start = 1975)

#Define el titulo de la grafica para el perfilador A
texto1=paste("Comparación de datos de viento entre perfilador A",sep="")

#Genera la grafica
#plot(a, main=texto1, xlab="", ylab="Velocidad del viento (Km/h)",col="red")#, ylim=c(5,33))
plot(fechas,datos$V3, type = "l", ylab="Velocidad del viento (Km/h)", col="red", main=texto1, xlab = fechas )

#****************************************************************************************************************************************************
#Define una serie de tiempo con los datos del perfilador B 
#b<-ts(datos$V3, frequency=365,start = 1975)

#Define el titulo para la grafica del perfilador B
texto2=paste("Comparación de datos de viento entre perfilador B",sep="")

#Genera la grafica del perfilador B
#plot(b, main=texto2, xlab="", ylab="Velocidad del viento en (Km/h)",col="maroon")#, ylim=c(5,33))
plot(fechas,datos$V4, type = "l", ylab="Velocidad del viento (Km/h)", col="blue", main=texto2, xlab = "" )

#****************************************************************************************************************************************************

#Define una serie de tiempo con los datos del diferencia entre A y B
#c<-ts(datos$V4, frequency=365, start = 1975)

#Define el titulo para la grafica del perfilador B
texto3=paste("Diferencia entre perfiladores A y B",sep="")

#Genera la grafica del perfilador B
#plot(c, main=texto3, xlab="", ylab="Velocidad del viento en (Km/h)",col="maroon")#, ylim=c(5,33))
plot(fechas,datos$V5, type = "l", ylab="Velocidad del viento (Km/h)", col="maroon", main=texto3, xlab = "" )

dev.off()