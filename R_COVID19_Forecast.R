#Script para aplicar los resultados de la modelación numérica regional con el WRF-Clima3 para calcular el factor de reproducción potencial
#Del COVID-19

#Viernes 20 de marzo de 2020

#Autor del script: Juan José Amides Figueroa Urbano, El Salvador.  joseamidesfigueroa@gmail.com 
#La reproducción total o parcial de este script esta permitida siempre y cuando se mencione la fuente original. 

#Enlace al paper: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3551767


#Los archivos rasters de pronostico de las variables meteorológicas utilizadas aqui corresponden a los resultados de la modelación numérica regional
#para la región realizada con el modelo WRF con condiciones iniciales del modelo climático CFS V2, la resolución espacial del modelo es de 15KM
#Es decir un espaciamiento de 15KM entre puntos de malla en la horizontal.

#Mas información sobre los resultados de modelación numérica aqui: http://centroclima.org/clima/clima3/ 

#Datos importantes sobre los archivos geotiff usados y producidos en este script: 
#Las longitudes son todas positivas, es decir que se debe cambiar el CRS para georeferenciar bien los mismos.
#Mas información al respecto: https://gis.stackexchange.com/questions/109733/plotting-layer-given-with-longitudes-0-360-to-180-180 

#Datos sobre la extensión del dominio utilizado:
#Latitud Norte: 24.01998
#Latitud Sur: 5.876589
#Longitud extremo oeste: 263.4459 o -96.5541
#Longitud extremo este: 293.0129 o -66.9871

#Es importante tomar estos datos a la hora de utilizar los archivos rasters para no tener problemas con la georeferenciación.

#Carga las librerias a usar
library(raster)
library(rgdal)

#Establece el escritorio de trabajo
setwd("C:/Users/Amides/Desktop/temp")

#Establece el mapa a usar
#Carga el mapa para toda la región
mapa_ca <- readOGR(dsn=path.expand("C:/R/shape"),layer="TM_WORLD_BORDERS-0.3")

#Carga el mapa departamental para El Salvador
mapa_esa <- readOGR(dsn=path.expand("C:/R/shape"),layer="ESA_CA_wgs84")

#Prepara los datos raster para ser usados en R como promedios de 5 dias. en el caso de la temperatura los datos son diarios por lo que hay que
#Hay que promediarlos primero

###########################################################################################################################

#Temperatura
#---------------------------------------------------------------------------------->
d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_1.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_2.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_3.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_4.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_5.tif")

#Promedio de la pentada 1
temperatura_promedio_p1<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_6.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_7.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_8.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_9.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_10.tif")

#Promedio de la pentada 2
temperatura_promedio_p2<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_11.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_12.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_13.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_14.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_15.tif")

#Promedio de la pentada 3
temperatura_promedio_p3<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_16.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_17.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_18.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_19.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_20.tif")

#Promedio de la pentada 4
temperatura_promedio_p4<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_16.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_17.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_18.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_19.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_20.tif")

#Promedio de la pentada 5
temperatura_promedio_p5<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_21.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_22.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_23.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_24.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_25.tif")

#Promedio de la pentada 6
temperatura_promedio_p6<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_26.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_27.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_28.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_29.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/temp/temperatura_promedio_dia_30.tif")

#Promedio de la pentada 7
temperatura_promedio_p7<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

#---------------------------------------------------------------------------------->
#Define la extensión de cada promedio realizado

extent(temperatura_promedio_p1)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(temperatura_promedio_p2)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(temperatura_promedio_p3)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(temperatura_promedio_p4)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(temperatura_promedio_p5)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(temperatura_promedio_p6)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(temperatura_promedio_p7)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
#---------------------------------------------------------------------------------->

#########################################################################################################################
#########################################################################################################################
#Humedad
#---------------------------------------------------------------------------------->
d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_1.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_2.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_3.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_4.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_5.tif")

#Promedio de la pentada 1
humedad_promedio_p1<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_6.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_7.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_8.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_9.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_10.tif")

#Promedio de la pentada 2
humedad_promedio_p2<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_11.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_12.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_13.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_14.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_15.tif")

#Promedio de la pentada 3
humedad_promedio_p3<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_16.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_17.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_18.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_19.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_20.tif")

#Promedio de la pentada 4
humedad_promedio_p4<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_16.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_17.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_18.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_19.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_20.tif")

#Promedio de la pentada 5
humedad_promedio_p5<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_21.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_22.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_23.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_24.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_25.tif")

#Promedio de la pentada 6
humedad_promedio_p6<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

d01<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_26.tif")
d02<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_27.tif")
d03<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_28.tif")
d04<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_29.tif")
d05<-raster("C:/Users/Amides/Desktop/temp/hume/humedad_promedio_dia_30.tif")

#Promedio de la pentada 7
humedad_promedio_p7<-(d01+d02+d03+d04+d05)/5
#---------------------------------------------------------------------------------->

#---------------------------------------------------------------------------------->
#Define la extensión de cada promedio realizado

extent(humedad_promedio_p1)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(humedad_promedio_p2)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(humedad_promedio_p3)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(humedad_promedio_p4)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(humedad_promedio_p5)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(humedad_promedio_p6)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
extent(humedad_promedio_p7)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
#---------------------------------------------------------------------------------->

#########################################################################################################################
#########################################################################################################################

#Ahora, con las temperaturas y humedad relativa promedio cada 5 dias procedemos a calcular la relacion.

#Genera listas para los bucles
#---------------------------------------------------------------------------------->
lista.temperatura<-c(temperatura_promedio_p1,temperatura_promedio_p2,temperatura_promedio_p3,temperatura_promedio_p4,
                     temperatura_promedio_p5,temperatura_promedio_p6,temperatura_promedio_p7)

lista.humedad<-c(humedad_promedio_p1,humedad_promedio_p2,humedad_promedio_p3,humedad_promedio_p3,humedad_promedio_p4,
                  humedad_promedio_p5,humedad_promedio_p6,humedad_promedio_p7)

lista.fechas<-c("promedio del 1 al 5 de abril 2020", "promedio del 6 al 10 de abril 2020", "promedio del 11 al 15 de abril 2020",
                "promedio del 16 al 20 de abril 2020", "promedio del 21 al 25 de abril 2020", "promedio del 25 al 30 de abril 2020")
#---------------------------------------------------------------------------------->

#Con 
#Bucle para calcular el potencial tanto en forma gráfica como en formato raster
#---------------------------------------------------------------------------------->
for (i in c(1:7)) {
  
  #Calcula el primer termino de la ecuación
  a<-0.0383*lista.temperatura[[i]]
  
  #Calcula el segundo termino de la ecuación
  b<-0.0224*lista.humedad[[i]]
  
  #Calcula el factor de
  R<-(3.968-a-b)
  nombre<-paste("R_",i,".tif",sep = "")
  nombre_esa.png<-paste("R_esa",i,".png",sep = "")
  nombre_ca.png<-paste("R_ca",i,".png",sep = "")
  
  rf<-writeRaster(R, filename=nombre, overwrite=TRUE)
  
  png(nombre_esa.png, pointsize=13, res = 150, width = 1800, height = 1350)
  plot(R,legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
       main="Numero de reproduccion efectiva del COVID-19 estimada", sub=lista.fechas[i])
  plot(mapa_esa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
  dev.off()
  
  png(nombre_ca.png, pointsize=13, res = 150, width = 1800, height = 1350)
  plot(R,legend=TRUE, main="Numero de reproduccion efectiva del COVID-19 estimada", sub=lista.fechas[i])
  plot(mapa_ca,add=TRUE)
  dev.off()
}



