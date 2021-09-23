#GENERA PRONOSTICO DE LLUVIA ACUMULA MENSUAL AJUSTADA CON ERROR PROMEDIO MENSUAL DEL MODELO VS CHIRPS (LINEA BASE)
#POR JUAN JOSE AMIDES FIGUEROA URBANO , EL SALVADOR, 29 de noviembre 2019. -- joseamidesfigueroa@gmail.com

#------------------------------------------------------------------------------------------------------------
#Carga librerias necesarias
library(raster)
library(rgdal)
library(rgeos)
library(crs)
library(sp)
library(RColorBrewer)
library(ggplot2)

#Define algunas variables para ser usadas
setwd("C:/R/")
dir.entrada <- ("C:/R/datos_entrada/")
mapa <- readOGR(dsn=path.expand("C:/R/shape"),layer="ESA_CA_wgs84")
mapa2 <- readOGR(dsn=path.expand("C:/R/shape"),layer="TM_WORLD_BORDERS-0.3")

#Declara los dï¿½?as a usar
dias <- c("_dia_1.tif","_dia_2.tif","_dia_3.tif","_dia_4.tif","_dia_5.tif","_dia_6.tif","_dia_7.tif","_dia_8.tif","_dia_9.tif","_dia_10.tif",
          "_dia_11.tif","_dia_12.tif","_dia_13.tif","_dia_14.tif","_dia_15.tif","_dia_16.tif","_dia_17.tif","_dia_18.tif","_dia_19.tif","_dia_20.tif",
          "_dia_21.tif","_dia_22.tif","_dia_23.tif","_dia_24.tif","_dia_25.tif","_dia_26.tif","_dia_27.tif","_dia_28.tif","_dia_29.tif","_dia_30.tif","_dia_31.tif")

#Declaro las variables a sustituir cada mes

input_mes_actual <-"10"
name_mes_actual <-"octubre de 2021"
name_condiciones_iniciales <- "15 junio 2021"

#Declara la cantidad de dias a usar
longitud <- seq(1:30)

#------------------------------------------------------------------------------------------------------------
#Crea un raster con el area efectiva de trabajo (RASTER DUMMY)
m<-matrix(runif(400),20,20)
area_efectiva<-raster(m)
extent(area_efectiva)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(area_efectiva) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#------------------------------------------------------------------------------------------------------------
#Lee los raster de trabajo
raw_acum<-raster(paste("C:/R/datos_entrada/acum_general_",input_mes_actual,".tif",sep = ""))
desv<-raster(paste("C:/R/desv/",input_mes_actual,".tif",sep = ""))

chirps_prom<-raster(paste("C:/R/CHIRPS_PROMEDIO/CHIRPS_",input_mes_actual,"_PROM.tif",sep = ""))
q33_raw<-raster(paste("C:/R/climatologia_chirps_1981_2010/",input_mes_actual,"_q33_1981_2010.tif",sep = ""))
q33<-crop(q33_raw,area_efectiva)
q66_raw<-raster(paste("C:/R/climatologia_chirps_1981_2010/",input_mes_actual,"_q66_1981_2010.tif",sep = ""))
q66<-crop(q66_raw,area_efectiva)
prom_raw<-raster(paste("C:/R/climatologia_chirps_1981_2010/",input_mes_actual,"_promedio_1981_2010.tif",sep = ""))
prom<-crop(prom_raw,area_efectiva)

#------------------------------------------------------------------------------------------------------------
#Define la extensi?n del raster y cambia a valores negativos la longitud (RASTER LLUVIA ACUMULADA 'CRUDA')
extent(raw_acum)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
projection(raw_acum) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
acum<-crop(raw_acum,area_efectiva)
acum_resampled<-resample(acum,q33)
acum_croped<-mask(acum_resampled,q33)

extent(desv)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(desv) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
desv<-crop(desv,area_efectiva)
desv<-resample(desv,q33)

chirps_prom<-resample(chirps_prom,q33)

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Calcula la anomalia contra la climatolog?a CHIRPS
anom<-acum_croped-chirps_prom

#------------------------------------------------------------------------------------------------------------
#Genera un raster para guardar el pronostico ajustado (RASTER VACIO PARA GUARDAR EL RESULTADO DEL AJUSTE)
pronostico_ajustado<-q33
pronostico_ajustado<-pronostico_ajustado*0

#------------------------------------------------------------------------------------------------------------
#Calcula la diferencia entre el raster de pronostico recortado y la desviacion promedio

#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}

pronostico_ajustado<-Con(desv>0,acum_croped-desv,acum_croped+desv)


#Evalua si el resultado del ajuste es negativo, si lo fuere se deja el pron?stico sin ajuste.
#acum_ajustada_final=as.matrix(Con(acum_ajustada<0,acum_ajustada_final<-mat_acum,acum_ajustada_final<-acum_ajustada))
acum_ajustada_final=Con(pronostico_ajustado<0,acum_ajustada_final<-acum_croped,acum_ajustada_final<-pronostico_ajustado)

pronostico_ajustado<-acum_ajustada_final
#plot(acum_ajustada_final)

extent(pronostico_ajustado)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(pronostico_ajustado) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot(pronostico_ajustado)

#------------------------------------------------------------------------------------------------------------
#Calcula la escenarios
dummy1<-q33
dummy1<-dummy1*0
abajo<-dummy1
abajo<-Con(pronostico_ajustado[]<=q33[],dummy1-1,dummy1+0)

dummy2<-q33
dummy2<-dummy2*0
arriba<-dummy2
arriba<-Con(pronostico_ajustado[]>=q66[],dummy2+1,dummy2+0)

escenarios<-q33
escenarios<-escenarios*0
escenarios<-abajo+arriba

rf<-writeRaster(escenarios, filename="C:/R/resultados/escenario.tif", overwrite=TRUE)

anomalia<-resample(pronostico_ajustado,prom)-prom
anomalia[anomalia <= -500]<-(-500)
anomalia[anomalia >= 500]<-500

rf<-writeRaster(anomalia, filename="C:/R/resultados/anomalia.tif", overwrite=TRUE)



anomalia_porcentual2<-resample((pronostico_ajustado*100),prom)/prom
anomalia_porcentual<-anomalia_porcentual2
anomalia_porcentual[anomalia_porcentual2 <=-300]<-(-300)
anomalia_porcentual[anomalia_porcentual2 >=300]<-300

rf<-writeRaster(anomalia_porcentual, filename="C:/R/resultados/anomalia_porcentual.tif", overwrite=TRUE)

##############################################################################################################
##############################################################################################################
##############################################################################################################

#------------------------------------------------------------------------------------------------------------
#Definici?n de estaciones para extraer datos de precipitaci?n:  
Estaciones <-list(E01_Guija<-cbind(-89.46999,14.229286),E02_Los_Andes<-cbind(-89.628326000000001,13.874305000000000),E03_Candelaria_Frontera<-cbind(-89.651657999999998,14.120964000000001),
                     E04_Montecristo<-cbind(-89.359988999999999,14.399279000000000),E05_Santa_Ana<-cbind(-89.548320000000004,13.982635999999999),
                     E06_Chorrea_Guayabo<-cbind(-89.548320000000004,13.987634000000000),E07_Sensuntepeque<-cbind(-88.646636000000001,13.870975000000000),
                     E08_Cerron_Grande<-cbind(-88.926643999999996,13.93764000000000),E09_Cojutepeque<-cbind(-88.926643999999996,13.720983000000000),
                     E10_Nueva_Concepcion<-cbind(-89.289987999999994,14.125966000000000),E11_La_Palma<-cbind(-89.161653999999999,14.292623000000001),
                     E12_Las_Pilas<-cbind(-89.088307999999998,14.374274000000000),E13_Ahuachapan<-cbind(-89.859994999999998,13.944305999999999),
                     E14_La_Hachadura<-cbind(-90.089990000000000,13.860979000000000),E15_San_Andres<-cbind(-89.406654000000003,13.809312000000000),
                     E16_Chiltiupan<-cbind(-89.469984999999994,13.597661000000000),E17_San_Miguel<-cbind(-88.158293999999998,13.439336000000001),
                     E18_La_Union<-cbind(-87.826779000000002,13.333684000000000),E19_Ilopango<-cbind(-89.118313,13.699318),E20_Acajutla<-cbind(-89.833333999999994,13.574325999999999),
                     E21_Los_Naranjos<-cbind(-89.674995999999993,13.875971000000000),E22_Santiago_Maria<-cbind(-88.471638999999996,13.485992000000000),
                     E23_Puente_Cuscatlan<-cbind(-88.593303000000006,13.602655000000000),E24_San_Francisco_Gotera<-cbind(-88.106623999999996,13.697651000000000),
                     E25_Perquin<-cbind(-88.162499999999994,13.960889000000000))

#------------------------------------------------------------------------------------------------------------
#Extrae el dato de pronostico por estaci?n del dato "crudo" del modelo:

lluvia_cruda<-list()
for (i in seq(1:25) ){
  lluvia_cruda[i] <- (extract(acum_croped,Estaciones[[i]],method='bilinear'))
}

#------------------------------------------------------------------------------------------------------------
#Extrae las desviaciones por cada estacion

desviaciones <-list()
for (i in seq(1:25) ){
  desviaciones[i] <- (extract(desv,Estaciones[[i]],method='bilinear'))
}

#------------------------------------------------------------------------------------------------------------
#Extrae el dato de pronostico por estaci?n del dato ajustado del modelo por linea base

ajustados <-list()
for (i in seq(1:25) ){
  ajustados[i] <- (extract(pronostico_ajustado,Estaciones[[i]],method='bilinear'))
}

#------------------------------------------------------------------------------------------------------------
#Crea vector con la lluvia por estacion

mat_Lluvia_mensual_cruda<-matrix(lluvia_cruda,nrow = 1,ncol = 25)
mat_Lluvia_mensual_ajustada<-matrix(ajustados,nrow = 1,ncol = 25)
mat_desviacion_promedio<-matrix(desviaciones,nrow = 1,ncol = 25)

#------------------------------------------------------------------------------------------------------------
#Escribe los valores en un CSV
write.table(mat_Lluvia_mensual_cruda, file="C:/R/resultados/lluvia_mensual_raw.csv",row.names = FALSE, col.names = FALSE, sep =",")
write.table(mat_Lluvia_mensual_ajustada, file="C:/R/resultados/lluvia_mensual_ajustada.csv",row.names = FALSE, col.names = FALSE, sep =",")
write.table(mat_desviacion_promedio, file="C:/R/resultados/desviacion_promedio.csv",row.names = FALSE, col.names = FALSE, sep =",")

#------------------------------------------------------------------------------------------------------------
#Escribe los valores de modelo ajustado en un raster
rf<-writeRaster(raw_acum, filename="C:/R/resultados/raw_pronostico.tif", overwrite=TRUE)
rf<-writeRaster(acum_ajustada_final, filename="C:/R/resultados/pronostico_ajustado.tif", overwrite=TRUE)
rf<-writeRaster(anom, filename="C:/R/resultados/anomalias_precipitacion.tif", overwrite=TRUE)


##############################################################################################################################################################
#------------------------------------------------------------------------------------------------------------
#Genera lluvia acumulada cada 5 dias




#------------------------------------------------------------------------------------------------------------
#Carga los rasters de cumulos a usar

lista_lluvia_cumulos <-list()

for (i in longitud) {
  lista_lluvia_cumulos <-paste(dir.entrada,"acumulado_lluvia_cumulos",dias,sep = "")
}

acumulado_cumulos <-calc(stack(lista_lluvia_cumulos), fun = sum)
extent(acumulado_cumulos)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
acumulado_cumulos <-resample(acumulado_cumulos,q33)
extent(acumulado_cumulos)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
projection(acumulado_cumulos) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#------------------------------------------------------------------------------------------------------------
#Carga los rasters de cumulos a usar

lista_lluvia_microfisica <-list()

for (i in longitud) {
  lista_lluvia_microfisica <-paste(dir.entrada,"acumulado_lluvia_microfisica",dias,sep = "")
}

acumulado_microfisica <-calc(stack(lista_lluvia_microfisica), fun = sum)
extent(acumulado_microfisica)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
acumulado_microfisica <-resample(acumulado_microfisica,q33)
extent(acumulado_microfisica)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
projection(acumulado_microfisica) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#------------------------------------------------------------------------------------------------------------
#Exporta los rasters de acumulados

rf<-writeRaster(acumulado_cumulos, filename="C:/R/resultados/precipitacion_general_cumulos.tif", overwrite=TRUE)
rf<-writeRaster(acumulado_microfisica, filename="C:/R/resultados/precipitacion_general_microfisica.tif", overwrite=TRUE)

#------------------------------------------------------------------------------------------------------------
#Carga los rasters de temperatura promedio

lista_temperatura_promedio <-list()

for (i in longitud) {
  lista_temperatura_promedio <-paste(dir.entrada,"temperatura_promedio",dias,sep = "")
}

temperatura_promedio_mensual <-calc(stack(lista_temperatura_promedio), fun = mean)
extent(temperatura_promedio_mensual)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_promedio_mensual<-crop(temperatura_promedio_mensual,area_efectiva)
temperatura_promedio_mensual <-resample(temperatura_promedio_mensual,q33)
extent(temperatura_promedio_mensual)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_promedio_mensual) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

rf<-writeRaster(temperatura_promedio_mensual, filename="C:/R/resultados/temperatura_promedio_mensual.tif", overwrite=TRUE)
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Carga los rasters de temperatura mï¿½?nima

lista_temperatura_minima <-list()
for (i in longitud) {
  lista_temperatura_minima <-paste(dir.entrada,"temperatura_minima",dias,sep = "")
}

#Temperatura mï¿½?nima promedio
temperatura_minima_promedio_mensual <-calc(stack(lista_temperatura_minima), fun = mean)
extent(temperatura_minima_promedio_mensual)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_minima_promedio_mensual <-crop(temperatura_minima_promedio_mensual,area_efectiva)
temperatura_minima_promedio_mensual <-resample(temperatura_minima_promedio_mensual,q33)
extent(temperatura_minima_promedio_mensual)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_minima_promedio_mensual) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf<-writeRaster(temperatura_minima_promedio_mensual, filename="C:/R/resultados/temperatura_minima_promedio_mensual.tif", overwrite=TRUE)

#Temperatura mï¿½?nima absoluta
temperatura_minima_absoluta <-calc(stack(lista_temperatura_minima), fun = min)
extent(temperatura_minima_absoluta)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_minima_absoluta <-crop(temperatura_minima_absoluta,area_efectiva)
temperatura_minima_absoluta <-resample(temperatura_minima_absoluta,q33)
extent(temperatura_minima_absoluta)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_minima_absoluta) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf<-writeRaster(temperatura_minima_absoluta, filename="C:/R/resultados/temperatura_minima_absoluta.tif", overwrite=TRUE)

#Temperatura mÃ¡xima absoluta
temperatura_minima_maxima_absoluta <-calc(stack(lista_temperatura_minima), fun = max)
extent(temperatura_minima_maxima_absoluta)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_minima_maxima_absoluta <-crop(temperatura_minima_maxima_absoluta,area_efectiva)
temperatura_minima_maxima_absoluta <-resample(temperatura_minima_maxima_absoluta,q33)
extent(temperatura_minima_maxima_absoluta)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_minima_maxima_absoluta) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf<-writeRaster(temperatura_minima_maxima_absoluta, filename="C:/R/resultados/temperatura_minima_maxima_absoluta.tif", overwrite=TRUE)

#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
#Carga los rasters de temperatura mÃ¡xima

lista_temperatura_maxima <-list()
for (i in longitud) {
  lista_temperatura_maxima <-paste(dir.entrada,"temperatura_maxima",dias,sep = "")
}

#Temperatura maxima promedio
temperatura_maxima_promedio_mensual <-calc(stack(lista_temperatura_maxima), fun = mean)
extent(temperatura_maxima_promedio_mensual)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_maxima_promedio_mensual <-crop(temperatura_maxima_promedio_mensual,area_efectiva)
temperatura_maxima_promedio_mensual <-resample(temperatura_maxima_promedio_mensual,q33)
extent(temperatura_maxima_promedio_mensual)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_maxima_promedio_mensual) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf<-writeRaster(temperatura_maxima_promedio_mensual, filename="C:/R/resultados/temperatura_maxima_promedio_mensual.tif", overwrite=TRUE)

#Temperatura maxima absoluta
temperatura_maxima_absoluta <-calc(stack(lista_temperatura_maxima), fun = max)
extent(temperatura_maxima_absoluta)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_maxima_absoluta <-crop(temperatura_maxima_absoluta,area_efectiva)
temperatura_maxima_absoluta <-resample(temperatura_maxima_absoluta,q33)
extent(temperatura_maxima_absoluta)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_maxima_absoluta) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf<-writeRaster(temperatura_maxima_absoluta, filename="C:/R/resultados/temperatura_maxima_absoluta.tif", overwrite=TRUE)

#Temperatura minima mÃ¡xima absoluta
temperatura_maxima_minima_absoluta <-calc(stack(lista_temperatura_maxima), fun = min)
extent(temperatura_maxima_minima_absoluta)<-c(263.4459-360,293.0129-360,5.876589,24.01998)
temperatura_maxima_minima_absoluta <-crop(temperatura_maxima_minima_absoluta,area_efectiva)
temperatura_maxima_minima_absoluta <-resample(temperatura_maxima_minima_absoluta,q33)
extent(temperatura_maxima_minima_absoluta)<-c(264.2-360,291.8855-360,6.849999,23.38065)
projection(temperatura_maxima_minima_absoluta) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rf<-writeRaster(temperatura_maxima_minima_absoluta, filename="C:/R/resultados/temperatura_maxima_minima_absoluta.tif", overwrite=TRUE)

#------------------------------------------------------------------------------------------------------------


##############################################################################################################
#Genera imagenes de pronÃ³stico regional
##############################################################################################################

#Define paletas de colores
paleta.anomalias<-colorRampPalette(c("#a51a01","#f8a203","#feed7c","#806d5a","#aa9988","#dfd9d2","#ffffff",
                                     "#ccffcc","#99ff99","#33cc33","#66ccff","#0066ff","#ccccff","#cc99ff"))
paleta<-colorRampPalette(c("#B18904","#F3F781","#04B404"))
paleta.errores<-colorRampPalette(c("maroon","red","coral","white","cadetblue1","cyan","blue"))
paleta.temperaturas <-colorRampPalette(c("aliceblue","cadetblue1","cornflowerblue","gold","orange","orangered1","red"))
intervalos<-c(-1,0,1)

#Genera paleta de colores
paleta.colores<-colorRampPalette(c("#FFFFFF00","#9bf88b","#0000ff","#ffff00","#ff8000","#ff0000","#ff00ff"))

jpeg("C:/R/resultados/wrf.jpg", width = 2000, height = 1400)
plot(acum_resampled,main=paste("Precipitacion en mm para el mes de ",name_mes_actual," \n WRF Clima - CI",name_condiciones_iniciales,sep = ""), xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     col=paleta.colores(12),alpha=TRUE,interpolate=FALSE,ylab="Latitud",xlab="Longitud",legend=TRUE)
plot(mapa,add = TRUE, xlim=c(-90.2,-87.6),ylim=c(12.9,14.7))
dev.off()


jpeg("C:/R/resultados/wrf_ajustado.jpg", width = 2000, height = 1400)
plot(pronostico_ajustado,main=paste("Precipitacion en mm para el mes de ",name_mes_actual," \n WRF Clima - CI ",name_condiciones_iniciales,sep = ""), xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     col=paleta.colores(12),alpha=TRUE,interpolate=FALSE,ylab="Latitud",xlab="Longitud",legend=TRUE)
plot(mapa,add = TRUE, xlim=c(-90.2,-87.6),ylim=c(12.9,14.7))
dev.off()

#Exporta como imagen  ------------ Fisica de cumulos
jpeg("C:/R/resultados/wrf_cumulos.jpg", width = 2000, height = 1400)
plot(acumulado_cumulos,main=paste("Precipitacion en mm para el mes de ",name_mes_actual," \n WRF Clima - CI ",name_condiciones_iniciales," - Fisica de cumulos",sep=""), xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     col=paleta.colores(14),alpha=TRUE,interpolate=FALSE,ylab="Latitud",xlab="Longitud",legend=TRUE)
plot(mapa,add = TRUE, xlim=c(-90.2,-87.6),ylim=c(12.9,14.7))
dev.off()

#Exporta como imagen  ------------ Microfisica
jpeg("C:/R/resultados/wrf_microfisica.jpg", width = 2000, height = 1400)
plot(acumulado_microfisica,main=paste("Precipitacion en mm para el mes de ",name_mes_actual," \n WRF Clima - CI ",name_condiciones_iniciales," - Microfisica",sep=""), xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     col=paleta.colores(14),alpha=TRUE,interpolate=FALSE,ylab="Latitud",xlab="Longitud",legend=TRUE)
plot(mapa,add = TRUE, xlim=c(-90.2,-87.6),ylim=c(12.9,14.7))
dev.off()

##############################################################################################################


##############################################################################################################
#Genera imagenes de escenarios regionales
##############################################################################################################
png("C:/R/resultados/escenario.png", pointsize=13, res = 150, width = 2000, height = 1400)
plot(escenarios, col=paleta(3),legend=FALSE,ylim=c(6.8,23.3),xlim(-93,-68),breaks=c(-1,-0.9999,0.9999,1),
     main=paste("Escenarios WRF-Clima3 -",name_mes_actual," - CI ",name_condiciones_iniciales,sep=""))
legend("topright",legend = c("A","N","B"),fill = c("#04B404","#F3F781","#B18904"))
plot(mapa2,add=TRUE,xlim=c(-95.8,-68.1145),ylim=c(6.8,23.3))
dev.off()

png("C:/R/resultados/anomalia.png", pointsize=13, res = 150, width = 2000, height = 1400)
plot(anomalia, col=paleta.anomalias(20),legend=TRUE,ylim=c(6.8,23.3),xlim(264.2-360,291.8855-360),
     main=paste("Anomalia (mm) WRF-Clima3 vs CHIRPS ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""),breaks = seq(from = -500, to = 500, by = 50))
plot(mapa2,add=TRUE,xlim=c(-95.8,-68.1145),ylim=c(6.8,23.3))
dev.off()

png("C:/R/resultados/anomalia_porcentual.png", pointsize=13, res = 150, width = 2000, height = 1400)
plot(anomalia_porcentual, col=paleta.errores(12),legend=TRUE,ylim=c(6.8,23.3),xlim(264.2-360,291.8855-360),
     main=paste("Anomalia porcentual WRF-Clima3 vs CHIRPS ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""),breaks = seq(from = -300, to = 300, by = 50))
plot(mapa2,add=TRUE,xlim=c(-95.8,-68.1145),ylim=c(6.8,23.3))
dev.off()

##############################################################################################################

##############################################################################################################
#Genera imagenes de escenarios El Salvador
##############################################################################################################
png("C:/R/resultados/escenario_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(escenarios, col=paleta(3),legend=FALSE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),breaks=c(-1,-0.9999,0.9999,1),
     main=paste("Escenarios WRF-Clima3 ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""))
legend("topright",legend = c("A","N","B"),fill = c("#04B404","#F3F781","#B18904"))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/anomalia_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(anomalia, col=paleta.errores(20),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Anomalia (mm) WRF-Clima3 vs CHIRPS ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""),breaks = seq(from = -500, to = 500, by = 50))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/anomalia_porcentual_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(anomalia_porcentual, col=paleta.errores(12),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Anomalia porcentual WRF-Clima3 vs CHIRPS ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""),breaks = seq(from = -300, to = 300, by = 50))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

##############################################################################################################

##############################################################################################################
#Genera imagenes de Temperaturas El Salvador
##############################################################################################################

png("C:/R/resultados/temperatura_promedio_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_promedio_mensual, col=paleta.temperaturas(14),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura promedio °C WRF-Clima3 ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/temperatura_maxima_promedio_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_maxima_promedio_mensual, col=paleta.temperaturas(14),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura maxima promedio °C WRF-Clima3 ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/temperatura_minima_promedio_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_minima_promedio_mensual, col=paleta.temperaturas(14),xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura minima promedio °C WRF-Clima3 ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/temperatura_maxima_absoluta_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_maxima_absoluta, col=paleta.temperaturas(14),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura maxima absoluta °C WRF-Clima3 ",name_mes_actual,"- CI ",name_condiciones_iniciales,sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/temperatura_minima_absoluta_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_minima_absoluta, col=paleta.temperaturas(14),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura minima absoluta °C WRF-Clima3 ",name_mes_actual," - CI 15 febrero 2021",sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/temperatura_minima_maxima_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_minima_maxima_absoluta, col=paleta.temperaturas(14),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura minima maxima absoluta °C WRF-Clima3 ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

png("C:/R/resultados/temperatura_mÃ¡xima_minima_esa.png", pointsize=13, res = 150, width = 1800, height = 1350)
plot(temperatura_maxima_minima_absoluta, col=paleta.temperaturas(14),legend=TRUE,xlim=c(-90.2,-87.6),ylim=c(12.9,14.7),
     main=paste("Temperatura maxima minima absoluta °C WRF-Clima3 ",name_mes_actual," - CI ",name_condiciones_iniciales,sep = ""))
plot(mapa,add=TRUE,xlim=c(-90.2,-87.7),ylim=c(13.1,14.7))
dev.off()

##############################################################################################################