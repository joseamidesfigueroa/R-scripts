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

time<-Sys.time()
texto<-"Ciclo 00Z, 4 Miembros"

#Carga shapefile
mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="ESA_CA_wgs84")

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



##############################################################################################################
##############################################################################################################
#Realiza el apilado de los rasters

conjunto_rasters_24h<-stack(lluvia_24_A,lluvia_24_B,lluvia_24_C,lluvia_24_D,lluvia_24_E,lluvia_24_F,lluvia_24_G)
conjunto_rasters_48h<-stack(lluvia_48_A,lluvia_48_B,lluvia_48_C,lluvia_48_D,lluvia_48_E,lluvia_48_F,lluvia_48_G)

#Para 72 horas y tres días solo se toman los primeros 4 miembros del conjunto
conjunto_rasters_72h<-stack(lluvia_72_A,lluvia_72_B,lluvia_72_C,lluvia_72_D)
conjunto_rasters_total<-stack(total_A,total_B,total_C,total_D)
##############################################################################################################
##############################################################################################################


##############################################################################################################
##############################################################################################################
# Declaración de funciones

#Funcion para calcular la moda
mode <- function(x) {
  ux <- unique(x)
  ux=ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}
##############################################################################################################
##############################################################################################################



##############################################################################################################
##############################################################################################################
#*******************************************************************************************************
#------------------------------------------------------------------------------ 24 horas
#Calucla la moda para los rasters
moda_24h = calc(conjunto_rasters_24h,fun = mode)
#Calcula la desviación estandard de los rasters
desviacion_standar_24h<-calc(conjunto_rasters_24h,sd)

#*******************************************************************************************************
#------------------------------------------------------------------------------ 48 horas
#Calucla la moda para los rasters
moda_48h = calc(conjunto_rasters_48h,fun = mode)
#Calcula la desviación estandard de los rasters
desviacion_standar_48h<-calc(conjunto_rasters_48h,sd)

#*******************************************************************************************************
#------------------------------------------------------------------------------ 72 horas
#Calucla la moda para los rasters
moda_72h = calc(conjunto_rasters_72h,fun = mode)
#Calcula la desviación estandard de los rasters
desviacion_standar_72h<-calc(conjunto_rasters_72h,sd)

#*******************************************************************************************************
#------------------------------------------------------------------------------ 3 días
#Calucla la moda para los rasters
moda_total = calc(conjunto_rasters_total,fun = mode)
#Calcula la desviación estandard de los rasters
desviacion_standar_total<-calc(conjunto_rasters_total,sd)

#*******************************************************************************************************
##############################################################################################################
##############################################################################################################





##############################################################################################################
##############################################################################################################
#******************************************************************************************************* 24 horas
#******************************************************************************************** MODA

rf<-writeRaster(moda_24h, filename="moda_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
#Moda
jpeg("moda_24h.jpg", width = 1060, height = 745)
plot(moda_24h,main="Moda de precipitación en mm \n conjunto modelos WRF en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#9bf88b","#0000ff","#ffff00","#ff8000","#ff0000","#ff00ff"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("Moda de 0 a 20%","Moda de 21 a 40%","Moda de 41 a 60%",
#                             "Moda de 61 a 80%","Moda de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

#--------------- Exporta como imagenes
#Moda
rf<-writeRaster(moda_48h, filename="moda_48h.tif", overwrite=TRUE)
jpeg("moda_48h.jpg", width = 1060, height = 745)
plot(moda_48h,main="Moda de precipitación en mm \n conjunto modelos WRF en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#9bf88b","#0000ff","#ffff00","#ff8000","#ff0000","#ff00ff"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("Moda de 0 a 20%","Moda de 21 a 40%","Moda de 41 a 60%",
#                             "Moda de 61 a 80%","Moda de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

#--------------- Exporta como imagenes
#Moda
rf<-writeRaster(moda_72h, filename="moda_72h.tif", overwrite=TRUE)
jpeg("moda_72h.jpg", width = 1060, height = 745)
plot(moda_72h,main="Moda de precipitación en mm \n conjunto modelos WRF en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#9bf88b","#0000ff","#ffff00","#ff8000","#ff0000","#ff00ff"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("Moda de 0 a 20%","Moda de 21 a 40%","Moda de 41 a 60%",
#                             "Moda de 61 a 80%","Moda de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->


#--------------- Exporta como imagenes
#Moda
rf<-writeRaster(moda_total, filename="moda_total.tif", overwrite=TRUE)
jpeg("moda_total.jpg", width = 1060, height = 745)
plot(moda_total,main="Moda de precipitación en mm \n conjunto modelos WRF en 3 días", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#9bf88b","#0000ff","#ffff00","#ff8000","#ff0000","#ff00ff"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("Moda de 0 a 20%","Moda de 21 a 40%","Moda de 41 a 60%",
#                             "Moda de 61 a 80%","Moda de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

#******************************************************************************************** DESVIACION STD

rf<-writeRaster(desviacion_standar_24h, filename="desviacion_standar_24h.tif", overwrite=TRUE)
#--------------- Exporta como imagenes
#desviacion_standar
jpeg("desviacion_standar_24h.jpg", width = 1060, height = 745)
plot(desviacion_standar_24h,main="desviacion_standar de precipitación en mm \n conjunto modelos WRF en 24 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("desviacion_standar de 0 a 20%","desviacion_standar de 21 a 40%","desviacion_standar de 41 a 60%",
#                             "desviacion_standar de 61 a 80%","desviacion_standar de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

#--------------- Exporta como imagenes
#desviacion_standar
rf<-writeRaster(desviacion_standar_48h, filename="desviacion_standar_48h.tif", overwrite=TRUE)
jpeg("desviacion_standar_48h.jpg", width = 1060, height = 745)
plot(desviacion_standar_48h,main="desviacion_standar de precipitación en mm \n conjunto modelos WRF en 48 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("desviacion_standar de 0 a 20%","desviacion_standar de 21 a 40%","desviacion_standar de 41 a 60%",
#                             "desviacion_standar de 61 a 80%","desviacion_standar de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()
#------------------------------------->

#--------------- Exporta como imagenes
#desviacion_standar
rf<-writeRaster(desviacion_standar_72h, filename="desviacion_standar_72h.tif", overwrite=TRUE)
jpeg("desviacion_standar_72h.jpg", width = 1060, height = 745)
plot(desviacion_standar_72h,main="desviacion_standar de precipitación en mm \n conjunto modelos WRF en 72 horas", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("desviacion_standar de 0 a 20%","desviacion_standar de 21 a 40%","desviacion_standar de 41 a 60%",
#                             "desviacion_standar de 61 a 80%","desviacion_standar de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
mtext(time,side=3)
dev.off()
#------------------------------------->


#--------------- Exporta como imagenes
#desviacion_standar
rf<-writeRaster(desviacion_standar_total, filename="desviacion_standar_total.tif", overwrite=TRUE)
jpeg("desviacion_standar_total.jpg", width = 1060, height = 745)
plot(desviacion_standar_total,main="desviacion_standar de precipitación en mm \n conjunto modelos WRF en 3 días", xlim=c(-90.2,-87.7),ylim=c(13.1,14.7),
     col=c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"),alpha=TRUE,interpolate=TRUE,
     ylab="Latitud",xlab="Longitud",legend=TRUE)
#legend("topright",legend = c("desviacion_standar de 0 a 20%","desviacion_standar de 21 a 40%","desviacion_standar de 41 a 60%",
#                             "desviacion_standar de 61 a 80%","desviacion_standar de 81 a 100%"),fill = c("#FFFFFF00","#006633","#FFCC33","#FF0000","#660000"))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
mtext(time,side=4)
mtext(texto,side=3)
dev.off()
#------------------------------------->

##############################################################################################################
##############################################################################################################

tipo<-"4 miembros"
tipo
time