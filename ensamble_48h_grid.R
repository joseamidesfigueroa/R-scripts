#GENERA PRONOSTICO DE LLUVIA ACUMULA MENSUAL AJUSTADA CON ERROR PROMEDIO MENSUAL DEL MODELO VS CHIRPS (LINEA BASE)
#POR JUAN JOSE AMIDES FIGUEROA URBANO , EL SALVADOR, 12 DE NOVIEMBRE 2018. -- joseamidesfigueroa@gmail.com

#------------------------------------------------------------------------------------------------------------
#Carga librerias necesarias
library(raster)
library(rgdal)
library(rgeos)
library(crs)
library(sp)
library(gglot2)
library(xlsx)

setwd("C:/R")

#Carga shapefile
#mapa <- readOGR(dsn=path.expand("D:/R/shape"),layer="ESA_CA_wgs84")

#carga geotiff de lluvia
lluvia_24_A<-raster("C:/R/Conjunto/06/A/lluvia_24h.tif")
lluvia_24_B<-raster("C:/R/Conjunto/06/B/lluvia_24h.tif")
lluvia_24_C<-raster("C:/R/Conjunto/06/C/lluvia_24h.tif")
lluvia_24_D<-raster("C:/R/Conjunto/06/D/lluvia_24h.tif")
lluvia_24_E<-raster("C:/R/Conjunto/06/E/lluvia_24h.tif")
lluvia_24_F<-raster("C:/R/Conjunto/06/F/lluvia_24h.tif")
lluvia_24_G<-raster("C:/R/Conjunto/06/G/lluvia_24h.tif")

########################################################################################################
########################################################################################################
########################################################################################################
#*******************************************************************************************************
# Realiza el seteo de proyección y extensión de cada raster.
#*******************************************************************************************************
#Define la extension del raster y cambia a valores negativos la longitud. A
extent(lluvia_24_A)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_A) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. B
extent(lluvia_24_B)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_B) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. C
extent(lluvia_24_C)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_C) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. D
extent(lluvia_24_D)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_D) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. E
extent(lluvia_24_E)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_E) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. F
extent(lluvia_24_F)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_F) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Define la extension del raster y cambia a valores negativos la longitud. G
extent(lluvia_24_G)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24_G) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#*******************************************************************************************************

#*******************************************************************************************************
#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER DUMMY)
#m<-matrix(runif(0),3,3)
dummy<-lluvia_24_A
extent(dummy)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy,lluvia_24_A, method="bilinear")
dummy<-dummy*0
#------------------------------------------------------------------------------------------------------------
#*******************************************************************************************************


#*******************************************************************************************************
#Declara una función para evaluar un condicional
#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}
#*******************************************************************************************************



#*******************************************************************************************************
#*******************************************************************************************************
#Calculo de ocurrencias según condicionales.
#Probabilidad de ocurrencia para cada modelo (Pesos iguales)
cte<-0.14285714285714285714285714285714


#*******************************************************************************************************
#Probabilidad de más de 10mm
dummy1<-Con((lluvia_24_A>10),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>10),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>10),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>10),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>10),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>10),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>10),cte+dummy,0+dummy)
prob_10mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 20mm
dummy1<-Con((lluvia_24_A>20),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>20),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>20),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>20),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>20),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>20),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>20),cte+dummy,0+dummy)
prob_20mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 30mm
dummy1<-Con((lluvia_24_A>30),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>30),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>30),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>30),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>30),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>30),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>30),cte+dummy,0+dummy)
prob_30mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 40mm
dummy1<-Con((lluvia_24_A>40),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>40),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>40),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>40),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>40),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>40),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>40),cte+dummy,0+dummy)
prob_40mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 50mm
dummy1<-Con((lluvia_24_A>50),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>50),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>50),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>50),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>50),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>50),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>50),cte+dummy,0+dummy)
prob_50mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 60mm
dummy1<-Con((lluvia_24_A>60),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>60),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>60),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>60),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>60),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>60),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>60),cte+dummy,0+dummy)
prob_60mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 70mm
dummy1<-Con((lluvia_24_A>70),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>70),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>70),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>70),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>70),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>70),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>70),cte+dummy,0+dummy)
prob_70mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 80mm
dummy1<-Con((lluvia_24_A>80),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>80),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>80),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>80),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>80),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>80),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>80),cte+dummy,0+dummy)
prob_80mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 90mm
dummy1<-Con((lluvia_24_A>90),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>90),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>90),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>90),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>90),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>90),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>90),cte+dummy,0+dummy)
prob_90mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 100mm
dummy1<-Con((lluvia_24_A>100),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>100),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>100),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>100),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>100),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>100),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>100),cte+dummy,0+dummy)
prob_100mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 150mm
dummy1<-Con((lluvia_24_A>150),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>150),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>150),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>150),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>150),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>150),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>150),cte+dummy,0+dummy)
prob_150mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 200mm
dummy1<-Con((lluvia_24_A>200),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>200),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>200),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>200),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>200),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>200),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>200),cte+dummy,0+dummy)
prob_200mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 300mm
dummy1<-Con((lluvia_24_A>300),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>300),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>300),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>300),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>300),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>300),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>300),cte+dummy,0+dummy)
prob_300mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 500mm
dummy1<-Con((lluvia_24_A>500),cte+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>500),cte+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>500),cte+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>500),cte+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>500),cte+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>500),cte+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>500),cte+dummy,0+dummy)
prob_500mm_24h<-sum(dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7)
#*******************************************************************************************************

#*******************************************************************************************************
rf<-writeRaster(prob_10mm_24h, filename="prob_10mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_20mm_24h, filename="prob_20mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_30mm_24h, filename="prob_30mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_40mm_24h, filename="prob_40mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_50mm_24h, filename="prob_50mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_60mm_24h, filename="prob_60mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_70mm_24h, filename="prob_70mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_80mm_24h, filename="prob_80mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_90mm_24h, filename="prob_90mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_100mm_24h, filename="prob_100mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_150mm_24h, filename="prob_150mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_200mm_24h, filename="prob_200mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_300mm_24h, filename="prob_300mm_24h.tif", overwrite=TRUE)
rf<-writeRaster(prob_500mm_24h, filename="prob_500mm_24h.tif", overwrite=TRUE)










