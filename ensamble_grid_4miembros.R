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
library(plotly)

setwd("C:/R")

#Carga shapefile
mapa <- readOGR(dsn=path.expand("D:/R/shape"),layer="ESA_CA_wgs84")

#carga geotiff de lluvia
lluvia_24_A<-raster("C:/R/Conjunto/A/lluvia_24h.tif")
lluvia_24_B<-raster("C:/R/Conjunto/B/lluvia_24h.tif")
lluvia_24_C<-raster("C:/R/Conjunto/C/lluvia_24h.tif")
lluvia_24_D<-raster("C:/R/Conjunto/D/lluvia_24h.tif")
lluvia_24_E<-raster("C:/R/Conjunto/E/lluvia_24h.tif")
lluvia_24_F<-raster("C:/R/Conjunto/F/lluvia_24h.tif")
lluvia_24_G<-raster("C:/R/Conjunto/G/lluvia_24h.tif")

#carga geotiff de lluvia 48H
lluvia_48_A<-raster("C:/R/Conjunto/A/lluvia_48h.tif")
lluvia_48_B<-raster("C:/R/Conjunto/B/lluvia_48h.tif")
lluvia_48_C<-raster("C:/R/Conjunto/C/lluvia_48h.tif")
lluvia_48_D<-raster("C:/R/Conjunto/D/lluvia_48h.tif")
lluvia_48_E<-raster("C:/R/Conjunto/E/lluvia_48h.tif")
lluvia_48_F<-raster("C:/R/Conjunto/F/lluvia_48h.tif")
lluvia_48_G<-raster("C:/R/Conjunto/G/lluvia_48h.tif")

#carga geotiff de lluvia 72H
lluvia_72_A<-raster("C:/R/Conjunto/A/lluvia_72h.tif")
lluvia_72_B<-raster("C:/R/Conjunto/B/lluvia_72h.tif")
lluvia_72_C<-raster("C:/R/Conjunto/C/lluvia_72h.tif")
lluvia_72_D<-raster("C:/R/Conjunto/D/lluvia_72h.tif")
lluvia_72_E<-raster("C:/R/Conjunto/E/lluvia_72h.tif")
lluvia_72_F<-raster("C:/R/Conjunto/F/lluvia_72h.tif")
lluvia_72_G<-raster("C:/R/Conjunto/G/lluvia_72h.tif")

#Calcula el total de los 3 días
total_A<-sum(lluvia_24_A,lluvia_48_A,lluvia_72_A)
total_B<-sum(lluvia_24_B,lluvia_48_B,lluvia_72_B)
total_C<-sum(lluvia_24_C,lluvia_48_C,lluvia_72_C)
total_D<-sum(lluvia_24_D,lluvia_48_D,lluvia_72_D)
total_E<-sum(lluvia_24_E,lluvia_48_E,lluvia_72_E)
total_F<-sum(lluvia_24_F,lluvia_48_F,lluvia_72_F)
total_G<-sum(lluvia_24_G,lluvia_48_G,lluvia_72_G)



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
#Define la extension del raster y cambia a valores negativos la longitud. A
extent(lluvia_48_A)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_A) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. B
extent(lluvia_48_B)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_B) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. C
extent(lluvia_48_C)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_C) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. D
extent(lluvia_48_D)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_D) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. E
extent(lluvia_48_E)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_E) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. F
extent(lluvia_48_F)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_F) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. G
extent(lluvia_48_G)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_48_G) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#*******************************************************************************************************

#*******************************************************************************************************
#Define la extension del raster y cambia a valores negativos la longitud. A
extent(lluvia_72_A)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_A) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. B
extent(lluvia_72_B)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_B) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. C
extent(lluvia_72_C)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_C) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. D
extent(lluvia_72_D)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_D) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. E
extent(lluvia_72_E)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_E) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. F
extent(lluvia_72_F)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_F) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. G
extent(lluvia_72_G)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_72_G) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#*******************************************************************************************************

#*******************************************************************************************************
#Define la extension del raster y cambia a valores negativos la longitud. A
extent(total_A)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_A) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. B
extent(total_B)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_B) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. C
extent(total_C)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_C) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. D
extent(total_D)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_D) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. E
extent(total_E)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_E) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. F
extent(total_F)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_F) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Define la extension del raster y cambia a valores negativos la longitud. G
extent(total_G)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(total_G) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
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


#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy1)
#m<-matrix(runif(0),3,3)
dummy1<-lluvia_24_A
extent(dummy1)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy1) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy1,lluvia_24_A, method="bilinear")
dummy1<-dummy1*0
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy2)
#m<-matrix(runif(0),3,3)
dummy2<-lluvia_24_A
extent(dummy2)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy2) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy2,lluvia_24_A, method="bilinear")
dummy2<-dummy2*0
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy3)
#m<-matrix(runif(0),3,3)
dummy3<-lluvia_24_A
extent(dummy3)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy3) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy3,lluvia_24_A, method="bilinear")
dummy3<-dummy3*0
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy4)
#m<-matrix(runif(0),3,3)
dummy4<-lluvia_24_A
extent(dummy4)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy4) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy4,lluvia_24_A, method="bilinear")
dummy4<-dummy4*0
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy5)
#m<-matrix(runif(0),3,3)
dummy5<-lluvia_24_A
extent(dummy5)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy5) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy5,lluvia_24_A, method="bilinear")
dummy5<-dummy5*0
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy6)
#m<-matrix(runif(0),3,3)
dummy6<-lluvia_24_A
extent(dummy6)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy6) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy6,lluvia_24_A, method="bilinear")
dummy6<-dummy6*0
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER dummy7)
#m<-matrix(runif(0),3,3)
dummy7<-lluvia_24_A
extent(dummy7)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(dummy7) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
resample(dummy7,lluvia_24_A, method="bilinear")
dummy7<-dummy7*0
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
cte2<-0.25
#*******************************************************************************************************



##############################################################################################################
##############################################################################################################
#*******************************************************************************************************
#------------------------------------------------------------------------------ 24 horas
#Probabilidad de más de 10mm
dummy1=dummy1

dummy1<-Con((lluvia_24_A>=10),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=10),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=10),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=10),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=10),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=10),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=10),cte2+dummy,0+dummy)
prob_10mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 20mm
dummy1<-Con((lluvia_24_A>=20),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=20),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=20),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=20),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=20),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=20),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=20),cte2+dummy,0+dummy)
prob_20mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 30mm
dummy1<-Con((lluvia_24_A>=30),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=30),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=30),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=30),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=30),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=30),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=30),cte2+dummy,0+dummy)
prob_30mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 40mm
dummy1<-Con((lluvia_24_A>=40),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=40),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=40),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=40),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=40),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=40),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=40),cte2+dummy,0+dummy)
prob_40mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 50mm
dummy1<-Con((lluvia_24_A>=50),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=50),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=50),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=50),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=50),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=50),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=50),cte2+dummy,0+dummy)
prob_50mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 60mm
dummy1<-Con((lluvia_24_A>=60),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=60),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=60),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=60),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=60),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=60),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=60),cte2+dummy,0+dummy)
prob_60mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 70mm
dummy1<-Con((lluvia_24_A>=70),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=70),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=70),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=70),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=70),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=70),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=70),cte2+dummy,0+dummy)
prob_70mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 80mm
dummy1<-Con((lluvia_24_A>=80),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=80),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=80),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=80),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=80),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=80),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=80),cte2+dummy,0+dummy)
prob_80mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 90mm
dummy1<-Con((lluvia_24_A>=90),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=90),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=90),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=90),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=90),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=90),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=90),cte2+dummy,0+dummy)
prob_90mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 100mm
dummy1<-Con((lluvia_24_A>=100),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=100),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=100),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=100),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=100),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=100),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=100),cte2+dummy,0+dummy)
prob_100mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 150mm
dummy1<-Con((lluvia_24_A>=150),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=150),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=150),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=150),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=150),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=150),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=150),cte2+dummy,0+dummy)
prob_150mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 200mm
dummy1<-Con((lluvia_24_A>=200),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=200),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=200),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=200),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=200),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=200),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=200),cte2+dummy,0+dummy)
prob_200mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 300mm
dummy1<-Con((lluvia_24_A>=300),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=300),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=300),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=300),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=300),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=300),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=300),cte2+dummy,0+dummy)
prob_300mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 500mm
dummy1<-Con((lluvia_24_A>=500),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_24_B>=500),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_24_C>=500),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_24_D>=500),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_24_E>=500),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_24_F>=500),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_24_G>=500),cte2+dummy,0+dummy)
prob_500mm_24h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#*******************************************************************************************************
#------------------------------------------------------------------------------ 48 horas
#Probabilidad de más de 10mm
dummy1<-Con((lluvia_48_A>=10),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=10),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=10),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=10),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=10),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=10),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=10),cte2+dummy,0+dummy)
prob_10mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 20mm
dummy1<-Con((lluvia_48_A>=20),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=20),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=20),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=20),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=20),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=20),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=20),cte2+dummy,0+dummy)
prob_20mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 30mm
dummy1<-Con((lluvia_48_A>=30),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=30),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=30),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=30),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=30),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=30),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=30),cte2+dummy,0+dummy)
prob_30mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 40mm
dummy1<-Con((lluvia_48_A>=40),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=40),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=40),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=40),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=40),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=40),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=40),cte2+dummy,0+dummy)
prob_40mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 50mm
dummy1<-Con((lluvia_48_A>=50),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=50),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=50),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=50),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=50),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=50),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=50),cte2+dummy,0+dummy)
prob_50mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 60mm
dummy1<-Con((lluvia_48_A>=60),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=60),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=60),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=60),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=60),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=60),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=60),cte2+dummy,0+dummy)
prob_60mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 70mm
dummy1<-Con((lluvia_48_A>=70),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=70),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=70),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=70),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=70),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=70),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=70),cte2+dummy,0+dummy)
prob_70mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 80mm
dummy1<-Con((lluvia_48_A>=80),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=80),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=80),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=80),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=80),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=80),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=80),cte2+dummy,0+dummy)
prob_80mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 90mm
dummy1<-Con((lluvia_48_A>=90),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=90),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=90),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=90),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=90),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=90),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=90),cte2+dummy,0+dummy)
prob_90mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 100mm
dummy1<-Con((lluvia_48_A>=100),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=100),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=100),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=100),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=100),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=100),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=100),cte2+dummy,0+dummy)
prob_100mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 150mm
dummy1<-Con((lluvia_48_A>=150),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=150),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=150),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=150),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=150),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=150),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=150),cte2+dummy,0+dummy)
prob_150mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 200mm
dummy1<-Con((lluvia_48_A>=200),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=200),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=200),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=200),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=200),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=200),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=200),cte2+dummy,0+dummy)
prob_200mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 300mm
dummy1<-Con((lluvia_48_A>=300),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=300),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=300),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=300),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=300),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=300),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=300),cte2+dummy,0+dummy)
prob_300mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 500mm
dummy1<-Con((lluvia_48_A>=500),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_48_B>=500),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_48_C>=500),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_48_D>=500),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_48_E>=500),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_48_F>=500),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_48_G>=500),cte2+dummy,0+dummy)
prob_500mm_48h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#*******************************************************************************************************
#------------------------------------------------------------------------------ 72 horas
#Probabilidad de más de 10mm
dummy1<-Con((lluvia_72_A>=10),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=10),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=10),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=10),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=10),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=10),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=10),cte2+dummy,0+dummy)
prob_10mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 20mm
dummy1<-Con((lluvia_72_A>=20),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=20),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=20),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=20),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=20),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=20),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=20),cte2+dummy,0+dummy)
prob_20mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 30mm
dummy1<-Con((lluvia_72_A>=30),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=30),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=30),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=30),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=30),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=30),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=30),cte2+dummy,0+dummy)
prob_30mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 40mm
dummy1<-Con((lluvia_72_A>=40),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=40),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=40),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=40),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=40),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=40),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=40),cte2+dummy,0+dummy)
prob_40mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 50mm
dummy1<-Con((lluvia_72_A>=50),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=50),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=50),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=50),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=50),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=50),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=50),cte2+dummy,0+dummy)
prob_50mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 60mm
dummy1<-Con((lluvia_72_A>=60),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=60),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=60),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=60),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=60),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=60),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=60),cte2+dummy,0+dummy)
prob_60mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 70mm
dummy1<-Con((lluvia_72_A>=70),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=70),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=70),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=70),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=70),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=70),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=70),cte2+dummy,0+dummy)
prob_70mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 80mm
dummy1<-Con((lluvia_72_A>=80),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=80),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=80),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=80),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=80),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=80),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=80),cte2+dummy,0+dummy)
prob_80mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 90mm
dummy1<-Con((lluvia_72_A>=90),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=90),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=90),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=90),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=90),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=90),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=90),cte2+dummy,0+dummy)
prob_90mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 100mm
dummy1<-Con((lluvia_72_A>=100),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=100),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=100),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=100),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=100),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=100),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=100),cte2+dummy,0+dummy)
prob_100mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 150mm
dummy1<-Con((lluvia_72_A>=150),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=150),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=150),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=150),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=150),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=150),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=150),cte2+dummy,0+dummy)
prob_150mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 200mm
dummy1<-Con((lluvia_72_A>=200),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=200),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=200),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=200),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=200),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=200),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=200),cte2+dummy,0+dummy)
prob_200mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 300mm
dummy1<-Con((lluvia_72_A>=300),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=300),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=300),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=300),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=300),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=300),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=300),cte2+dummy,0+dummy)
prob_300mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 500mm
dummy1<-Con((lluvia_72_A>=500),cte2+dummy,0+dummy)
dummy2<-Con((lluvia_72_B>=500),cte2+dummy,0+dummy)
dummy3<-Con((lluvia_72_C>=500),cte2+dummy,0+dummy)
dummy4<-Con((lluvia_72_D>=500),cte2+dummy,0+dummy)
dummy5<-Con((lluvia_72_E>=500),cte2+dummy,0+dummy)
dummy6<-Con((lluvia_72_F>=500),cte2+dummy,0+dummy)
dummy7<-Con((lluvia_72_G>=500),cte2+dummy,0+dummy)
prob_500mm_72h<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************
##############################################################################################################
##############################################################################################################

##############################################################################################################
##############################################################################################################
#*******************************************************************************************************
#------------------------------------------------------------------------------ 3 días
#Probabilidad de más de 10mm
dummy1<-Con((total_A>=10),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=10),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=10),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=10),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=10),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=10),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=10),cte2+dummy,0+dummy)
prob_10mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 20mm
dummy1<-Con((total_A>=20),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=20),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=20),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=20),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=20),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=20),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=20),cte2+dummy,0+dummy)
prob_20mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 30mm
dummy1<-Con((total_A>=30),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=30),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=30),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=30),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=30),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=30),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=30),cte2+dummy,0+dummy)
prob_30mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 40mm
dummy1<-Con((total_A>=40),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=40),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=40),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=40),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=40),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=40),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=40),cte2+dummy,0+dummy)
prob_40mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 50mm
dummy1<-Con((total_A>=50),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=50),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=50),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=50),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=50),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=50),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=50),cte2+dummy,0+dummy)
prob_50mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 60mm
dummy1<-Con((total_A>=60),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=60),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=60),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=60),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=60),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=60),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=60),cte2+dummy,0+dummy)
prob_60mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 70mm
dummy1<-Con((total_A>=70),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=70),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=70),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=70),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=70),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=70),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=70),cte2+dummy,0+dummy)
prob_70mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 80mm
dummy1<-Con((total_A>=80),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=80),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=80),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=80),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=80),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=80),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=80),cte2+dummy,0+dummy)
prob_80mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 90mm
dummy1<-Con((total_A>=90),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=90),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=90),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=90),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=90),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=90),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=90),cte2+dummy,0+dummy)
prob_90mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 100mm
dummy1<-Con((total_A>=100),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=100),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=100),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=100),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=100),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=100),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=100),cte2+dummy,0+dummy)
prob_100mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 150mm
dummy1<-Con((total_A>=150),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=150),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=150),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=150),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=150),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=150),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=150),cte2+dummy,0+dummy)
prob_150mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 200mm
dummy1<-Con((total_A>=200),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=200),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=200),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=200),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=200),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=200),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=200),cte2+dummy,0+dummy)
prob_200mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 300mm
dummy1<-Con((total_A>=300),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=300),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=300),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=300),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=300),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=300),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=300),cte2+dummy,0+dummy)
prob_300mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************

#*******************************************************************************************************
#Probabilidad de más de 500mm
dummy1<-Con((total_A>=500),cte2+dummy,0+dummy)
dummy2<-Con((total_B>=500),cte2+dummy,0+dummy)
dummy3<-Con((total_C>=500),cte2+dummy,0+dummy)
dummy4<-Con((total_D>=500),cte2+dummy,0+dummy)
dummy5<-Con((total_E>=500),cte2+dummy,0+dummy)
dummy6<-Con((total_F>=500),cte2+dummy,0+dummy)
dummy7<-Con((total_G>=500),cte2+dummy,0+dummy)
prob_500mm_total<-sum(dummy1,dummy2,dummy3,dummy4)
#*******************************************************************************************************
##############################################################################################################
##############################################################################################################





##############################################################################################################
##############################################################################################################
#******************************************************************************************************* 24 horas
rf<-writeRaster(prob_10mm_24h, filename="prob_10mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_10mm_24h.jpg", width = 1060, height = 745)
plot(prob_10mm_24h,main="Probabilidad lluvia >= 10mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

rf<-writeRaster(prob_20mm_24h, filename="prob_20mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_20mm_24h.jpg", width = 1060, height = 745)
plot(prob_20mm_24h,main="Probabilidad lluvia >= 20mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_30mm_24h, filename="prob_30mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_30mm_24h.jpg", width = 1060, height = 745)
plot(prob_30mm_24h,main="Probabilidad lluvia >= 30mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_40mm_24h, filename="prob_40mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_40mm_24h.jpg", width = 1060, height = 745)
plot(prob_40mm_24h,main="Probabilidad lluvia >= 40mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_50mm_24h, filename="prob_50mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_50mm_24h.jpg", width = 1060, height = 745)
plot(prob_50mm_24h,main="Probabilidad lluvia >= 50mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_60mm_24h, filename="prob_60mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_60mm_24h.jpg", width = 1060, height = 745)
plot(prob_60mm_24h,main="Probabilidad lluvia >= 60mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_70mm_24h, filename="prob_70mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_70mm_24h.jpg", width = 1060, height = 745)
plot(prob_70mm_24h,main="Probabilidad lluvia >= 70mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_80mm_24h, filename="prob_80mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_80mm_24h.jpg", width = 1060, height = 745)
plot(prob_80mm_24h,main="Probabilidad lluvia >= 80mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_90mm_24h, filename="prob_90mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_90mm_24h.jpg", width = 1060, height = 745)
plot(prob_90mm_24h,main="Probabilidad lluvia >= 90mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_100mm_24h, filename="prob_100mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_100mm_24h.jpg", width = 1060, height = 745)
plot(prob_100mm_24h,main="Probabilidad lluvia >= 100mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_150mm_24h, filename="prob_150mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_150mm_24h.jpg", width = 1060, height = 745)
plot(prob_150mm_24h,main="Probabilidad lluvia >= 150mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_200mm_24h, filename="prob_200mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_200mm_24h.jpg", width = 1060, height = 745)
plot(prob_200mm_24h,main="Probabilidad lluvia >= 200mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_300mm_24h, filename="prob_300mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_300mm_24h.jpg", width = 1060, height = 745)
plot(prob_300mm_24h,main="Probabilidad lluvia >= 300mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_500mm_24h, filename="prob_500mm_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_500mm_24h.jpg", width = 1060, height = 745)
plot(prob_500mm_24h,main="Probabilidad lluvia >= 500mm en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
##############################################################################################################
##############################################################################################################



##############################################################################################################
##############################################################################################################
#******************************************************************************************************* 48 horas
rf<-writeRaster(prob_10mm_48h, filename="prob_10mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_10mm_48h.jpg", width = 1060, height = 745)
plot(prob_10mm_48h,main="Probabilidad lluvia >= 10mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

rf<-writeRaster(prob_20mm_48h, filename="prob_20mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_20mm_48h.jpg", width = 1060, height = 745)
plot(prob_20mm_48h,main="Probabilidad lluvia >= 20mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_30mm_48h, filename="prob_30mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_30mm_48h.jpg", width = 1060, height = 745)
plot(prob_30mm_48h,main="Probabilidad lluvia >= 30mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_40mm_48h, filename="prob_40mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_40mm_48h.jpg", width = 1060, height = 745)
plot(prob_40mm_48h,main="Probabilidad lluvia >= 40mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_50mm_48h, filename="prob_50mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_50mm_48h.jpg", width = 1060, height = 745)
plot(prob_50mm_48h,main="Probabilidad lluvia >= 50mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_60mm_48h, filename="prob_60mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_60mm_48h.jpg", width = 1060, height = 745)
plot(prob_60mm_48h,main="Probabilidad lluvia >= 60mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_70mm_48h, filename="prob_70mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_70mm_48h.jpg", width = 1060, height = 745)
plot(prob_70mm_48h,main="Probabilidad lluvia >= 70mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_80mm_48h, filename="prob_80mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_80mm_48h.jpg", width = 1060, height = 745)
plot(prob_80mm_48h,main="Probabilidad lluvia >= 80mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_90mm_48h, filename="prob_90mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_90mm_48h.jpg", width = 1060, height = 745)
plot(prob_90mm_48h,main="Probabilidad lluvia >= 90mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_100mm_48h, filename="prob_100mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_100mm_48h.jpg", width = 1060, height = 745)
plot(prob_100mm_48h,main="Probabilidad lluvia >= 100mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_150mm_48h, filename="prob_150mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_150mm_48h.jpg", width = 1060, height = 745)
plot(prob_150mm_48h,main="Probabilidad lluvia >= 150mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_200mm_48h, filename="prob_200mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_200mm_48h.jpg", width = 1060, height = 745)
plot(prob_200mm_48h,main="Probabilidad lluvia >= 200mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_300mm_48h, filename="prob_300mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_300mm_48h.jpg", width = 1060, height = 745)
plot(prob_300mm_48h,main="Probabilidad lluvia >= 300mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_500mm_48h, filename="prob_500mm_48h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_500mm_48h.jpg", width = 1060, height = 745)
plot(prob_500mm_48h,main="Probabilidad lluvia >= 500mm en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
##############################################################################################################
##############################################################################################################


##############################################################################################################
##############################################################################################################
#******************************************************************************************************* 72 horas
rf<-writeRaster(prob_10mm_72h, filename="prob_10mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_10mm_72h.jpg", width = 1060, height = 745)
plot(prob_10mm_72h,main="Probabilidad lluvia >= 10mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

rf<-writeRaster(prob_20mm_72h, filename="prob_20mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_20mm_72h.jpg", width = 1060, height = 745)
plot(prob_20mm_72h,main="Probabilidad lluvia >= 20mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_30mm_72h, filename="prob_30mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_30mm_72h.jpg", width = 1060, height = 745)
plot(prob_30mm_72h,main="Probabilidad lluvia >= 30mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_40mm_72h, filename="prob_40mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_40mm_72h.jpg", width = 1060, height = 745)
plot(prob_40mm_72h,main="Probabilidad lluvia >= 40mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_50mm_72h, filename="prob_50mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_50mm_72h.jpg", width = 1060, height = 745)
plot(prob_50mm_72h,main="Probabilidad lluvia >= 50mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_60mm_72h, filename="prob_60mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_60mm_72h.jpg", width = 1060, height = 745)
plot(prob_60mm_72h,main="Probabilidad lluvia >= 60mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_70mm_72h, filename="prob_70mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_70mm_72h.jpg", width = 1060, height = 745)
plot(prob_70mm_72h,main="Probabilidad lluvia >= 70mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_80mm_72h, filename="prob_80mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_80mm_72h.jpg", width = 1060, height = 745)
plot(prob_80mm_72h,main="Probabilidad lluvia >= 80mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_90mm_72h, filename="prob_90mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_90mm_72h.jpg", width = 1060, height = 745)
plot(prob_90mm_72h,main="Probabilidad lluvia >= 90mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_100mm_72h, filename="prob_100mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_100mm_72h.jpg", width = 1060, height = 745)
plot(prob_100mm_72h,main="Probabilidad lluvia >= 100mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_150mm_72h, filename="prob_150mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_150mm_72h.jpg", width = 1060, height = 745)
plot(prob_150mm_72h,main="Probabilidad lluvia >= 150mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_200mm_72h, filename="prob_200mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_200mm_72h.jpg", width = 1060, height = 745)
plot(prob_200mm_72h,main="Probabilidad lluvia >= 200mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_300mm_72h, filename="prob_300mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_300mm_72h.jpg", width = 1060, height = 745)
plot(prob_300mm_72h,main="Probabilidad lluvia >= 300mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_500mm_72h, filename="prob_500mm_72h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_500mm_72h.jpg", width = 1060, height = 745)
plot(prob_500mm_72h,main="Probabilidad lluvia >= 500mm en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
##############################################################################################################
##############################################################################################################




##############################################################################################################
##############################################################################################################
#******************************************************************************************************* Total
rf<-writeRaster(prob_10mm_total, filename="prob_10mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_10mm_total.jpg", width = 1060, height = 745)
plot(prob_10mm_total,main="Probabilidad lluvia >= 10mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

rf<-writeRaster(prob_20mm_total, filename="prob_20mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_20mm_total.jpg", width = 1060, height = 745)
plot(prob_20mm_total,main="Probabilidad lluvia >= 20mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_30mm_total, filename="prob_30mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_30mm_total.jpg", width = 1060, height = 745)
plot(prob_30mm_total,main="Probabilidad lluvia >= 30mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_40mm_total, filename="prob_40mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_40mm_total.jpg", width = 1060, height = 745)
plot(prob_40mm_total,main="Probabilidad lluvia >= 40mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_50mm_total, filename="prob_50mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_50mm_total.jpg", width = 1060, height = 745)
plot(prob_50mm_total,main="Probabilidad lluvia >= 50mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_60mm_total, filename="prob_60mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_60mm_total.jpg", width = 1060, height = 745)
plot(prob_60mm_total,main="Probabilidad lluvia >= 60mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_70mm_total, filename="prob_70mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_70mm_total.jpg", width = 1060, height = 745)
plot(prob_70mm_total,main="Probabilidad lluvia >= 70mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_80mm_total, filename="prob_80mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_80mm_total.jpg", width = 1060, height = 745)
plot(prob_80mm_total,main="Probabilidad lluvia >= 80mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_90mm_total, filename="prob_90mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_90mm_total.jpg", width = 1060, height = 745)
plot(prob_90mm_total,main="Probabilidad lluvia >= 90mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_100mm_total, filename="prob_100mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_100mm_total.jpg", width = 1060, height = 745)
plot(prob_100mm_total,main="Probabilidad lluvia >= 100mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_150mm_total, filename="prob_150mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_150mm_total.jpg", width = 1060, height = 745)
plot(prob_150mm_total,main="Probabilidad lluvia >= 150mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_200mm_total, filename="prob_200mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_200mm_total.jpg", width = 1060, height = 745)
plot(prob_200mm_total,main="Probabilidad lluvia >= 200mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_300mm_total, filename="prob_300mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_300mm_total.jpg", width = 1060, height = 745)
plot(prob_300mm_total,main="Probabilidad lluvia >= 300mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
rf<-writeRaster(prob_500mm_total, filename="prob_500mm_total.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
jpeg("prob_500mm_total.jpg", width = 1060, height = 745)
plot(prob_500mm_total,main="Probabilidad lluvia >= 500mm para los 3 dias", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=FALSE)
legend("topright",legend = c("Probabilidad de 0 a 20%","Probabilidad de 21 a 40%","Probabilidad de 41 a 60%",
                             "Probabilidad de 61 a 80%","Probabilidad de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->
##############################################################################################################
##############################################################################################################





##############################################################################################################
##############################################################################################################


#----------- Pasa el raster a contornos
#contorno<-rasterToContour(prob_10mm_24h)
#class(contorno)
#plot(contorno)
#------------------------------------------>