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

#Carga shapefile
mapa <- readOGR(dsn=path.expand("D:/R/shape"),layer="ESA_CA_wgs84")

#carga geotiff de lluvia
lluvia_24_A<-raster("C:/R/Conjunto/06/A/lluvia_24h.tif")
lluvia_24_B<-raster("C:/R/Conjunto/06/B/lluvia_24h.tif")
lluvia_24_C<-raster("C:/R/Conjunto/06/C/lluvia_24h.tif")
lluvia_24_D<-raster("C:/R/Conjunto/06/D/lluvia_24h.tif")
lluvia_24_E<-raster("C:/R/Conjunto/06/E/lluvia_24h.tif")
lluvia_24_F<-raster("C:/R/Conjunto/06/F/lluvia_24h.tif")
lluvia_24_G<-raster("C:/R/Conjunto/06/G/lluvia_24h.tif")


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


#Definie las latitudes
lat<-seq(12.62895,14.97795,0.53)

length(lat)

#Define las longitudes
lon<-seq(-90.414790909,-87.309790909,0.7)
length(lon)


#Extracción de lluvia por puntos lat lon

registro_A<-matrix(NA,5,5)
#Longitud fija
for (i in 1:5){
    #Latitud fija
    for (j in 1:5){
      punto<-cbind(lon[i],lat[j])
      registro_A[i,j]<-extract(lluvia_24_A,punto, method='bilinear')
      
    }
}
registro_A

reja<-cbind(lon,registro_A)

latid<-c(NA,lat)
latid

reja2<-rbind(latid,reja)
pronostico<-data.frame(reja2)
pronostico[2,1]

plot(pronostico[2,2],pronostico[3,3])


#************************************************************************
#************************************************************************
#Define la estación del aeropuerto de Ilopango.
Ilopango<-cbind(-89.118313,13.699318)

#Extrae datos
MSSS_A<-extract(lluvia_24_A,Ilopango, method='bilinear')
MSSS_B<-extract(lluvia_24_B,Ilopango, method='bilinear')
MSSS_C<-extract(lluvia_24_C,Ilopango, method='bilinear')
MSSS_D<-extract(lluvia_24_D,Ilopango, method='bilinear')
MSSS_E<-extract(lluvia_24_E,Ilopango, method='bilinear')
MSSS_F<-extract(lluvia_24_F,Ilopango, method='bilinear')
MSSS_G<-extract(lluvia_24_G,Ilopango, method='bilinear')

#Calcula los percentiles de los geotiff

matriz_lluvia_A<-as.matrix(lluvia_24_A)
matriz_lluvia_B<-as.matrix(lluvia_24_B)


#-------------------------------------------------------------------------------
#Percentiles por punto
conjunto<-c(MSSS_A,MSSS_B,MSSS_C,MSSS_D,MSSS_E,MSSS_F,MSSS_G)

percentiles<-c()
percentiles<-quantile(conjunto,c(.10,.20,.30,.40,.50,.60,.70,.80,.90,1))
percentiles
mean(conjunto)
sd(conjunto)
#-------------------------------------------------------------------------------



#******************************************************************************************
#--------- Establece la probabilidad de 10mm
#******************************************************************************************
probabilidad_10mm<-0
contador<-0
if(MSSS_A>=10){
  contador=contador+1
}
if(MSSS_B>=10){
  contador=contador+1
}
if(MSSS_C>=10){
  contador=contador+1
}
if(MSSS_D>=10){
  contador=contador+1
}
if(MSSS_E>=10){
  contador=contador+1
}
if(MSSS_F>=10){
  contador=contador+1
}
if(MSSS_G>=10){
  contador=contador+1
}
probabilidad_10mm<-contador/7
probabilidad_10mm
#******************************************************************************************
#******************************************************************************************








#-------------------------------------------------------------------------------
#Percentiles por grid

percentiles_grid<-quantile(matriz_conjunto_grid,c(.10,.20,.30,.40,.50,.60,.70,.80,.90,1))
percentiles_grid
raster_percentiles<-raster(percentiles_grid)



#******************************************************************************************
#******************************************************************************************




#******************************************************************************************
#******************************************************************************************
#Grafica el raster.
plot(lluvia_24_A,main="Lluvia en 24 horas",xlim=c(-90.3,-87.5),ylim=c(13.1,14.7))

plot(suma,main="Suma de lluvia",xlim=c(-90.3,-87.5),ylim=c(13.1,14.7))



#Grafica el mapa
plot(mapa,add = TRUE,xlim=c(-90.3,-87.5),ylim=c(13.1,14.7))
plot 