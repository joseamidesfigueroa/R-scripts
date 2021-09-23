library(raster)
library(akima)
library(rgdal)
library(gridExtra)
library(grid)
library(png)


#Seteo el directorio de trabajo
setwd("/home/arw/evaluacion")

ciclo<-"Ciclo 12Z. \n"
texto<-"Evaluación WRF vs Hidroestimador. Período evaluado de las ultimas 24horas entre\n 7:00 AM del día 1 a 7:00AM del día 2\n\n Evaluación realizada el día 2 con fecha:\n"
time<-Sys.time()
texto_resumen<-"\n Resumen:\n"

#Defino estaciones
Santa_Ana<-cbind(-89.548320000000004,13.982635999999999)
Ilopango<-cbind(-89.118313,13.699318)
San_Miguel<-cbind(-88.158293999999998,13.439336000000001)

#Cargo el mapa 
mapa <- readOGR(dsn=path.expand("/home/arw/shape"),layer="ESA_CA_wgs84")

########################################################################################
#Leo los datos xyz del hidroestimador del CAFFG
#xyz<-read.table("/home/arw/evaluacion/hydroestimador.xyz",header = F,skip = 1)

#Genero latitudes y longitudes desde el archivo
#x<-coordinates(xyz)[,1]
#y<-coordinates(xyz)[,2]
#x<-xyz$V1
#y<-xyz$V2

#Guardo el dato de precipitaci?n desde el archivo
#z<-xyz$V3

#Genero una interpolaci?n 
#r1<-interp(x,y,z)

#Genero el raster con los datos interpolados y asigno extensi?n y proyecci?n
he_CAFFG<-raster("hydroestimador.grd")
extent(he_CAFFG)<-c(-94,-74,4,20)
#projection(he_CAFFG) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
########################################################################################

########################################################################################
#Carga datos WRF A
lluvia_24h_A<-raster("lluvia_24h_A.tif")
extent(lluvia_24h_A)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
projection(lluvia_24h_A) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Carga datos WRF B
lluvia_24h_B<-raster("lluvia_24h_B.tif")
extent(lluvia_24h_B)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
projection(lluvia_24h_B) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Carga datos WRF C
lluvia_24h_Ca<-raster("lluvia_24h_C.tif")
extent(lluvia_24h_Ca)<-c(269.585209091-360,272.690209091-360,12.62895,14.97795)
projection(lluvia_24h_Ca) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
lluvia_24h_C<-resample(lluvia_24h_Ca,lluvia_24h_A)

#Carga datos WRF D
lluvia_24h_D<-raster("lluvia_24h_D.tif")
extent(lluvia_24h_D)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
projection(lluvia_24h_D) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Carga datos WRF E
lluvia_24h_E<-raster("lluvia_24h_E.tif")
extent(lluvia_24h_E)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
projection(lluvia_24h_E) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Carga datos WRF F
lluvia_24h_F<-raster("lluvia_24h_F.tif")
extent(lluvia_24h_F)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
projection(lluvia_24h_F) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Carga datos WRF G
lluvia_24h_G<-raster("lluvia_24h_G.tif")
extent(lluvia_24h_G)<-c(269.5991045454544519-360,272.6782775471975242-360,12.6275776326056857,14.9644500000000011)
projection(lluvia_24h_G) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#Carga datos WRF EURO
lluvia_24h_EURO<-raster("lluvia_24h_EURO.tif")
extent(lluvia_24h_EURO)<-c(269.3881120689655404-360,272.6515547361652807-360,12.3727395401548232,15.1973999999999965)
lluvia_24h_EURO<-resample(lluvia_24h_EURO,lluvia_24h_A)
projection(lluvia_24h_EURO) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
########################################################################################

#*******************************************************************************************************
#Declara una funci?n para evaluar un condicional
#Funcion condicional
Con=function(condition, trueValue, falseValue){
  return(condition * trueValue + (!condition)*falseValue)}
#*******************************************************************************************************

#*******************************************************************************************************
#Funcion para añadir imagen
#---------------------------------------------------------------------------------------
addImg <- function(
  obj, # an image file imported as an array (e.g. png::readPNG, jpeg::readJPEG)
  x = NULL, # mid x coordinate for image
  y = NULL, # mid y coordinate for image
  width = NULL, # width of image (in x coordinate units)
  interpolate = TRUE # (passed to graphics::rasterImage) A logical vector (or scalar) indicating whether to apply linear interpolation to the image when drawing. 
){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr # A vector of the form c(x1, x2, y1, y2) giving the extremes of the user coordinates of the plotting region
  PIN <- par()$pin # The current plot dimensions, (width, height), in inches
  DIM <- dim(obj) # number of x-y pixels for the image
  ARp <- DIM[1]/DIM[2] # pixel aspect ratio (y/x)
  WIDi <- width/(USR[2]-USR[1])*PIN[1] # convert width units to inches
  HEIi <- WIDi * ARp # height in inches
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) # height in units
  rasterImage(image = obj, 
              xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), 
              interpolate = interpolate)
}
#---------------------------------------------------------------------------------------
#*******************************************************************************************************


#*******************************************************************************************************
#Define las paletas de colores
#----------------------------------------------------------------------------------------->
paleta.colores<-colorRampPalette(c("#ffffff00","cornflowerblue","#68ff06","#effe08","#ff7000","#ff1f00","#710f02","#710f02","magenta"))
paleta.errores<-colorRampPalette(c("red","white","green"))
hit<-colorRampPalette(c("red","white","green"))
nohit<-colorRampPalette(c("green","white","red"))

intervalos<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150)
intervalos_80<-c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80)
intervalos_errores<-c(-150,-125,-100,-75,-50,-25,0,25,50,75,100,125,150)
#----------------------------------------------------------------------------------------->
#*******************************************************************************************************

########################################################################################
#-------------------------------------------------------------> Valores del hidroestimador
#Recorta rasters
hidroestimador_intermedio<-crop(he_CAFFG,lluvia_24h_A)
#wrf<-crop(lluvia_24h_A,cph_grid)
hidroestimador<-resample(hidroestimador_intermedio,lluvia_24h_A)
maximo_hidroestimador<- round(maxValue(hidroestimador),digits = 2)

acum_hidro_Santa_Ana<-round(extract(hidroestimador,Santa_Ana, method='bilinear'),digits = 2)
acum_hidro_Ilopango<-round(extract(hidroestimador,Ilopango, method='bilinear'),digits = 2)
acum_hidro_San_Miguel<-round(extract(hidroestimador,San_Miguel, method='bilinear'),digits = 2)
########################################################################################

################################################################################################################################
#-------------------------------------------------------------> Evalua miembro A

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_A<-lluvia_24h_A
#wrf_A<-resample(lluvia_24h_A,hidroestimador)

diferencia_A<-wrf_A-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_A<-wrf_A
aciertos_A[]<-0
aciertos_A<-Con(abs(diferencia_A)<=20,aciertos_A+1,aciertos_A+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_A<-wrf_A
perdidos_A[]<-0
perdidos_A<-Con(abs(diferencia_A)>20,perdidos_A+1,perdidos_A+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_A<-wrf_A
falsa_alarma_A[]<-0
falsa_alarma_A<-Con((wrf_A>20 & (abs(diferencia_A)<=20)),falsa_alarma_A+1,falsa_alarma_A+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_A<-wrf_A
falso_correcto_A[]<-0
falso_correcto_A<-Con((wrf_A==0 & (hidroestimador==0)),falso_correcto_A+1,falso_correcto_A+0)

#Estadisticos básicos

sumario<-summary(values(wrf_A))
max_wrf_A<-sumario[6]
maximo_wrf_A<-round(max_wrf_A,digits = 2)


acum_wrf_A_Santa_Ana<-round(extract(wrf_A,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_A_Ilopango<-round(extract(wrf_A,Ilopango, method='bilinear'),digits = 2)

acum_wrf_A_San_Miguel<-round(extract(wrf_A,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_A<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_A<-c(maximo_hidroestimador,maximo_wrf_A,acum_hidro_Santa_Ana,acum_wrf_A_Santa_Ana,acum_hidro_Ilopango,acum_wrf_A_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_A_San_Miguel)

resumen_general_A<-data.frame(cbind(titulos_resumen_A,resumen_A))
colnames(resumen_general_A)<-c("Variable a evaluar","Valor")
rownames(resumen_general_A)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_A.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_A)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_A<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_A.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_A, filename="/home/arw/evaluacion/resultados/wrf_A.tif", overwrite=TRUE)
plot(wrf_A,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_A", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_A, filename="/home/arw/evaluacion/resultados/aciertos_A.tif", overwrite=TRUE)
plot(aciertos_A,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_A, filename="/home/arw/evaluacion/resultados/perdidos_A.tif", overwrite=TRUE)
plot(perdidos_A,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_A, filename="/home/arw/evaluacion/resultados/falsa_alarma_A.tif", overwrite=TRUE)
plot(falsa_alarma_A,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_A, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_A,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_A, filename="/home/arw/evaluacion/resultados/diferencia_A.tif", overwrite=TRUE)
plot(diferencia_A,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
##text()
imagen_de_tabla_A<-"/home/arw/evaluacion/resultados/tabla_A.png"
pic<-readPNG(imagen_de_tabla_A)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(0.5,0.9,paste(ciclo,texto,time),cex=0.8,col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_A), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_A)
#grid.table(resumen_general_A)
#grafico1<-grid.table(resumen_general_A)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_A<-median(RMSE_CAFFG_A[])
#media_RMSE_CAFFG_A<-mean(RMSE_CAFFG_A[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_A<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_A<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_A","RMSE_CPH_A","SD_CAFFG_A","SD_CPH_A","ERR_CAFFG_A","ERR_CPH_A","Hit_Rate_CAFFG_A","Hit_Rate_CAFFG_A")
#estadisticas_A<-cbind(summary(RMSE_CAFFG_A),summary(RMSE_CPH_A),summary(desv_CAFFG_A),summary(desv_CPH_A),summary(error_CAFFG_A),
                      #summary(error_CPH_A),summary(hit_rate_CAFFG_A),summary(hit_rate_CPH_A))
#resumen_estadisticas_A<-rbind(nombres,estadisticas_A)

#------------------------------------------------------------->
################################################################################################################################


################################################################################################################################
#-------------------------------------------------------------> Evalua miembro B

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_B<-lluvia_24h_B
#wrf_B<-resample(lluvia_24h_B,hidroestimador)

diferencia_B<-wrf_B-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_B<-wrf_B
aciertos_B[]<-0
aciertos_B<-Con(abs(diferencia_B)<=20,aciertos_B+1,aciertos_B+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_B<-wrf_B
perdidos_B[]<-0
perdidos_B<-Con(abs(diferencia_B)>20,perdidos_B+1,perdidos_B+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_Alarma_B<-wrf_B
falsa_Alarma_B[]<-0
falsa_Alarma_B<-Con((wrf_B>20 & (abs(diferencia_B)<=20)),falsa_Alarma_B+1,falsa_Alarma_B+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_B<-wrf_B
falso_correcto_B[]<-0
falso_correcto_B<-Con((wrf_B==0 & (hidroestimador==0)),falso_correcto_B+1,falso_correcto_B+0)

#Estadisticos básicos

sumario<-summary(values(wrf_B))
max_wrf_B<-sumario[6]
maximo_wrf_B<-round(max_wrf_B,digits = 2)


acum_wrf_B_Santa_Ana<-round(extract(wrf_B,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_B_Ilopango<-round(extract(wrf_B,Ilopango, method='bilinear'),digits = 2)

acum_wrf_B_San_Miguel<-round(extract(wrf_B,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_B<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_B<-c(maximo_hidroestimador,maximo_wrf_B,acum_hidro_Santa_Ana,acum_wrf_B_Santa_Ana,acum_hidro_Ilopango,acum_wrf_B_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_B_San_Miguel)

resumen_general_B<-data.frame(cbind(titulos_resumen_B,resumen_B))
colnames(resumen_general_B)<-c("Variable a evaluar","Valor")
rownames(resumen_general_B)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_B.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_B)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_B.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_B, filename="/home/arw/evaluacion/resultados/wrf_B.tif", overwrite=TRUE)
plot(wrf_B,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_B", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_B, filename="/home/arw/evaluacion/resultados/aciertos_B.tif", overwrite=TRUE)
plot(aciertos_B,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_B, filename="/home/arw/evaluacion/resultados/perdidos_B.tif", overwrite=TRUE)
plot(perdidos_B,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_Alarma_B, filename="/home/arw/evaluacion/resultados/falsa_Alarma_B.tif", overwrite=TRUE)
plot(falsa_Alarma_B,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_B, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_B,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_B, filename="/home/arw/evaluacion/resultados/diferencia_B.tif", overwrite=TRUE)
plot(diferencia_B,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_B<-"/home/arw/evaluacion/resultados/tabla_B.png"
pic<-readPNG(imagen_de_tabla_B)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_B)
#grid.table(resumen_general_B)
#grafico1<-grid.table(resumen_general_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################

################################################################################################################################
#-------------------------------------------------------------> Evalua miembro C

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_C<-lluvia_24h_C
#wrf_C<-resample(lluvia_24h_B,hidroestimador)

diferencia_C<-wrf_C-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_C<-wrf_C
aciertos_C[]<-0
aciertos_C<-Con(abs(diferencia_C)<=20,aciertos_C+1,aciertos_C+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_C<-wrf_C
perdidos_C[]<-0
perdidos_C<-Con(abs(diferencia_C)>20,perdidos_C+1,perdidos_C+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_C<-wrf_C
falsa_alarma_C[]<-0
falsa_alarma_C<-Con((wrf_C>20 & (abs(diferencia_C)<=20)),falsa_alarma_C+1,falsa_alarma_C+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_C<-wrf_C
falso_correcto_C[]<-0
falso_correcto_C<-Con((wrf_C==0 & (hidroestimador==0)),falso_correcto_C+1,falso_correcto_C+0)

#Estadisticos básicos

sumario<-summary(values(wrf_C))
max_wrf_C<-sumario[6]
maximo_wrf_C<-round(max_wrf_C,digits = 2)


acum_wrf_C_Santa_Ana<-round(extract(wrf_C,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_C_Ilopango<-round(extract(wrf_C,Ilopango, method='bilinear'),digits = 2)

acum_wrf_C_San_Miguel<-round(extract(wrf_C,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_C<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_C<-c(maximo_hidroestimador,maximo_wrf_C,acum_hidro_Santa_Ana,acum_wrf_C_Santa_Ana,acum_hidro_Ilopango,acum_wrf_C_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_C_San_Miguel)

resumen_general_C<-data.frame(cbind(titulos_resumen_C,resumen_C))
colnames(resumen_general_C)<-c("Variable a evaluar","Valor")
rownames(resumen_general_C)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_C.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_C)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_C.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_C, filename="/home/arw/evaluacion/resultados/wrf_C.tif", overwrite=TRUE)
plot(wrf_C,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_C", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_C, filename="/home/arw/evaluacion/resultados/aciertos_C.tif", overwrite=TRUE)
plot(aciertos_C,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_C, filename="/home/arw/evaluacion/resultados/perdidos_C.tif", overwrite=TRUE)
plot(perdidos_C,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_C, filename="/home/arw/evaluacion/resultados/falsa_alarma_C.tif", overwrite=TRUE)
plot(falsa_alarma_C,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_C, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_C,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_C, filename="/home/arw/evaluacion/resultados/diferencia_C.tif", overwrite=TRUE)
plot(diferencia_C,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_C<-"/home/arw/evaluacion/resultados/tabla_C.png"
pic<-readPNG(imagen_de_tabla_C)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_B)
#grid.table(resumen_general_B)
#grafico1<-grid.table(resumen_general_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################


################################################################################################################################
#-------------------------------------------------------------> Evalua miembro D

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_D<-lluvia_24h_D
#wrf_D<-resample(lluvia_24h_B,hidroestimador)

diferencia_D<-wrf_D-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_D<-wrf_D
aciertos_D[]<-0
aciertos_D<-Con(abs(diferencia_D)<=20,aciertos_D+1,aciertos_D+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_D<-wrf_D
perdidos_D[]<-0
perdidos_D<-Con(abs(diferencia_D)>20,perdidos_D+1,perdidos_D+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_D<-wrf_D
falsa_alarma_D[]<-0
falsa_alarma_D<-Con((wrf_D>20 & (abs(diferencia_D)<=20)),falsa_alarma_D+1,falsa_alarma_D+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_D<-wrf_D
falso_correcto_D[]<-0
falso_correcto_D<-Con((wrf_D==0 & (hidroestimador==0)),falso_correcto_D+1,falso_correcto_D+0)

#Estadisticos básicos

sumario<-summary(values(wrf_D))
max_wrf_D<-sumario[6]
maximo_wrf_D<-round(max_wrf_D,digits = 2)


acum_wrf_D_Santa_Ana<-round(extract(wrf_D,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_D_Ilopango<-round(extract(wrf_D,Ilopango, method='bilinear'),digits = 2)

acum_wrf_D_San_Miguel<-round(extract(wrf_D,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_D<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_D<-c(maximo_hidroestimador,maximo_wrf_D,acum_hidro_Santa_Ana,acum_wrf_D_Santa_Ana,acum_hidro_Ilopango,acum_wrf_D_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_D_San_Miguel)

resumen_general_D<-data.frame(cbind(titulos_resumen_D,resumen_D))
colnames(resumen_general_D)<-c("Variable a evaluar","Valor")
rownames(resumen_general_D)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_D.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_D)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_D.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_D, filename="/home/arw/evaluacion/resultados/wrf_D.tif", overwrite=TRUE)
plot(wrf_D,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_D", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_D, filename="/home/arw/evaluacion/resultados/aciertos_D.tif", overwrite=TRUE)
plot(aciertos_D,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_D, filename="/home/arw/evaluacion/resultados/perdidos_D.tif", overwrite=TRUE)
plot(perdidos_D,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_D, filename="/home/arw/evaluacion/resultados/falsa_alarma_D.tif", overwrite=TRUE)
plot(falsa_alarma_D,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_D, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_D,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_D, filename="/home/arw/evaluacion/resultados/diferencia_D.tif", overwrite=TRUE)
plot(diferencia_D,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_D<-"/home/arw/evaluacion/resultados/tabla_D.png"
pic<-readPNG(imagen_de_tabla_D)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_B)
#grid.table(resumen_general_B)
#grafico1<-grid.table(resumen_general_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################

################################################################################################################################
#-------------------------------------------------------------> Evalua miembro E

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_E<-lluvia_24h_E
#wrf_E<-resample(lluvia_24h_B,hidroestimador)

diferencia_E<-wrf_E-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_E<-wrf_E
aciertos_E[]<-0
aciertos_E<-Con(abs(diferencia_E)<=20,aciertos_E+1,aciertos_E+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_E<-wrf_E
perdidos_E[]<-0
perdidos_E<-Con(abs(diferencia_E)>20,perdidos_E+1,perdidos_E+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_E<-wrf_E
falsa_alarma_E[]<-0
falsa_alarma_E<-Con((wrf_E>20 & (abs(diferencia_E)<=20)),falsa_alarma_E+1,falsa_alarma_E+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_E<-wrf_E
falso_correcto_E[]<-0
falso_correcto_E<-Con((wrf_E==0 & (hidroestimador==0)),falso_correcto_E+1,falso_correcto_E+0)

#Estadisticos básicos

sumario<-summary(values(wrf_E))
max_wrf_E<-sumario[6]
maximo_wrf_E<-round(max_wrf_E,digits = 2)


acum_wrf_E_Santa_Ana<-round(extract(wrf_E,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_E_Ilopango<-round(extract(wrf_E,Ilopango, method='bilinear'),digits = 2)

acum_wrf_E_San_Miguel<-round(extract(wrf_E,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_E<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_E<-c(maximo_hidroestimador,maximo_wrf_E,acum_hidro_Santa_Ana,acum_wrf_E_Santa_Ana,acum_hidro_Ilopango,acum_wrf_E_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_E_San_Miguel)

resumen_general_E<-data.frame(cbind(titulos_resumen_E,resumen_E))
colnames(resumen_general_E)<-c("Variable a evaluar","Valor")
rownames(resumen_general_E)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_E.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_E)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_E.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_E, filename="/home/arw/evaluacion/resultados/wrf_E.tif", overwrite=TRUE)
plot(wrf_E,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_E", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_E, filename="/home/arw/evaluacion/resultados/aciertos_E.tif", overwrite=TRUE)
plot(aciertos_E,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_E, filename="/home/arw/evaluacion/resultados/perdidos_E.tif", overwrite=TRUE)
plot(perdidos_E,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_E, filename="/home/arw/evaluacion/resultados/falsa_alarma_E.tif", overwrite=TRUE)
plot(falsa_alarma_E,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_E, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_E,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_E, filename="/home/arw/evaluacion/resultados/diferencia_E.tif", overwrite=TRUE)
plot(diferencia_E,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_E<-"/home/arw/evaluacion/resultados/tabla_E.png"
pic<-readPNG(imagen_de_tabla_E)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_B)
#grid.table(resumen_general_B)
#grafico1<-grid.table(resumen_general_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################

################################################################################################################################
#-------------------------------------------------------------> Evalua miembro F

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_F<-lluvia_24h_F
#wrf_F<-resample(lluvia_24h_B,hidroestimador)

diferencia_F<-wrf_F-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_F<-wrf_F
aciertos_F[]<-0
aciertos_F<-Con(abs(diferencia_F)<=20,aciertos_F+1,aciertos_F+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_F<-wrf_F
perdidos_F[]<-0
perdidos_F<-Con(abs(diferencia_F)>20,perdidos_F+1,perdidos_F+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_F<-wrf_F
falsa_alarma_F[]<-0
falsa_alarma_F<-Con((wrf_F>20 & (abs(diferencia_F)<=20)),falsa_alarma_F+1,falsa_alarma_F+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_F<-wrf_F
falso_correcto_F[]<-0
falso_correcto_F<-Con((wrf_F==0 & (hidroestimador==0)),falso_correcto_F+1,falso_correcto_F+0)

#Estadisticos básicos

sumario<-summary(values(wrf_F))
max_wrf_F<-sumario[6]
maximo_wrf_F<-round(max_wrf_F,digits = 2)


acum_wrf_F_Santa_Ana<-round(extract(wrf_F,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_F_Ilopango<-round(extract(wrf_F,Ilopango, method='bilinear'),digits = 2)

acum_wrf_F_San_Miguel<-round(extract(wrf_F,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_F<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_F<-c(maximo_hidroestimador,maximo_wrf_F,acum_hidro_Santa_Ana,acum_wrf_F_Santa_Ana,acum_hidro_Ilopango,acum_wrf_F_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_F_San_Miguel)

resumen_general_F<-data.frame(cbind(titulos_resumen_F,resumen_F))
colnames(resumen_general_F)<-c("Variable a evaluar","Valor")
rownames(resumen_general_F)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_F.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_F)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_F.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_F, filename="/home/arw/evaluacion/resultados/wrf_F.tif", overwrite=TRUE)
plot(wrf_F,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_F", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_F, filename="/home/arw/evaluacion/resultados/aciertos_F.tif", overwrite=TRUE)
plot(aciertos_F,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_F, filename="/home/arw/evaluacion/resultados/perdidos_F.tif", overwrite=TRUE)
plot(perdidos_F,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_F, filename="/home/arw/evaluacion/resultados/falsa_alarma_F.tif", overwrite=TRUE)
plot(falsa_alarma_F,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_F, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_F,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_F, filename="/home/arw/evaluacion/resultados/diferencia_F.tif", overwrite=TRUE)
plot(diferencia_F,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_F<-"/home/arw/evaluacion/resultados/tabla_F.png"
pic<-readPNG(imagen_de_tabla_F)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_B)
#grid.table(resumen_general_B)
#grafico1<-grid.table(resumen_general_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################

################################################################################################################################
#-------------------------------------------------------------> Evalua miembro G

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_G<-lluvia_24h_G
#wrf_G<-resample(lluvia_24h_B,hidroestimador)

diferencia_G<-wrf_G-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_G<-wrf_G
aciertos_G[]<-0
aciertos_G<-Con(abs(diferencia_G)<=20,aciertos_G+1,aciertos_G+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_G<-wrf_G
perdidos_G[]<-0
perdidos_G<-Con(abs(diferencia_G)>20,perdidos_G+1,perdidos_G+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_G<-wrf_G
falsa_alarma_G[]<-0
falsa_alarma_G<-Con((wrf_G>20 & (abs(diferencia_G)<=20)),falsa_alarma_G+1,falsa_alarma_G+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_G<-wrf_G
falso_correcto_G[]<-0
falso_correcto_G<-Con((wrf_G==0 & (hidroestimador==0)),falso_correcto_G+1,falso_correcto_G+0)

#Estadisticos básicos

sumario<-summary(values(wrf_G))
max_wrf_G<-sumario[6]
maximo_wrf_G<-round(max_wrf_G,digits = 2)


acum_wrf_G_Santa_Ana<-round(extract(wrf_G,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_G_Ilopango<-round(extract(wrf_G,Ilopango, method='bilinear'),digits = 2)

acum_wrf_G_San_Miguel<-round(extract(wrf_G,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_G<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_G<-c(maximo_hidroestimador,maximo_wrf_G,acum_hidro_Santa_Ana,acum_wrf_G_Santa_Ana,acum_hidro_Ilopango,acum_wrf_G_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_G_San_Miguel)

resumen_general_G<-data.frame(cbind(titulos_resumen_G,resumen_G))
colnames(resumen_general_G)<-c("Variable a evaluar","Valor")
rownames(resumen_general_G)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_G.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_general_G)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_WRF_G.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_G, filename="/home/arw/evaluacion/resultados/wrf_G.tif", overwrite=TRUE)
plot(wrf_G,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="WRF_G", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_G, filename="/home/arw/evaluacion/resultados/aciertos_G.tif", overwrite=TRUE)
plot(aciertos_G,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_G, filename="/home/arw/evaluacion/resultados/perdidos_G.tif", overwrite=TRUE)
plot(perdidos_G,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_G, filename="/home/arw/evaluacion/resultados/falsa_alarma_G.tif", overwrite=TRUE)
plot(falsa_alarma_G,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_G, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_G,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_G, filename="/home/arw/evaluacion/resultados/diferencia_G.tif", overwrite=TRUE)
plot(diferencia_G,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_G<-"/home/arw/evaluacion/resultados/tabla_G.png"
pic<-readPNG(imagen_de_tabla_G)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_general_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_general_B)
#grid.table(resumen_general_B)
#grafico1<-grid.table(resumen_general_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_grid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################


################################################################################################################################
#-------------------------------------------------------------> Evalua miembro EURO

#Calcula la diferencia entre el valor pronosticado y el valor registrado por hidroestimador
wrf_EURO<-lluvia_24h_EURO
#wrf_EURO<-resample(lluvia_24h_B,hidroestimador)

diferencia_EURO<-wrf_EURO-hidroestimador

#Calcula los aciertos
#--------------- CAFFG
aciertos_EURO<-wrf_EURO
aciertos_EURO[]<-0
aciertos_EURO<-Con(abs(diferencia_EURO)<=20,aciertos_EURO+1,aciertos_EURO+0)

#Calcula los perdidos
#--------------- CAFFG
perdidos_EURO<-wrf_EURO
perdidos_EURO[]<-0
perdidos_EURO<-Con(abs(diferencia_EURO)>20,perdidos_EURO+1,perdidos_EURO+0)

#Calcula las falsas alarmas
#--------------- CAFFG
falsa_alarma_EURO<-wrf_EURO
falsa_alarma_EURO[]<-0
falsa_alarma_EURO<-Con((wrf_EURO>20 & (abs(diferencia_EURO)<=20)),falsa_alarma_EURO+1,falsa_alarma_EURO+0)

#Calcula las falsos positivo
#--------------- CAFFG
falso_correcto_EURO<-wrf_EURO
falso_correcto_EURO[]<-0
falso_correcto_EURO<-Con((wrf_EURO==0 & (hidroestimador==0)),falso_correcto_EURO+1,falso_correcto_EURO+0)

#Estadisticos básicos

sumario<-summary(values(wrf_EURO))
max_wrf_EURO<-sumario[6]
maximo_wrf_EURO<-round(max_wrf_EURO,digits = 2)


acum_wrf_EURO_Santa_Ana<-round(extract(wrf_EURO,Santa_Ana, method='bilinear'),digits = 2)

acum_wrf_EURO_Ilopango<-round(extract(wrf_EURO,Ilopango, method='bilinear'),digits = 2)

acum_wrf_EURO_San_Miguel<-round(extract(wrf_EURO,San_Miguel, method='bilinear'),digits = 2)

titulos_resumen_EURO<-c("Máximo Hidroestimador\n","Máximo WRF","Hidroestimador Santa Ana",
                     "WRF Santa Ana", "Hidroestimador Ilopango", "WRF Ilopango",
                     "Hidroestimador San Miguel", "WRF San Miguel")
resumen_EURO<-c(maximo_hidroestimador,maximo_wrf_EURO,acum_hidro_Santa_Ana,acum_wrf_EURO_Santa_Ana,acum_hidro_Ilopango,acum_wrf_EURO_Ilopango,
             acum_hidro_San_Miguel,acum_wrf_EURO_San_Miguel)

resumen_EUROeneral_EURO<-data.frame(cbind(titulos_resumen_EURO,resumen_EURO))
colnames(resumen_EUROeneral_EURO)<-c("Variable a evaluar","Valor")
rownames(resumen_EUROeneral_EURO)<-c("1","2","3","4","5","6","7","8")

png("/home/arw/evaluacion/resultados/tabla_EURO.png", pointsize=12, res = 150, width = 1000, height = 1000)
ss<-tableGrob(resumen_EUROeneral_EURO)
grid.arrange(ss)
dev.off()

#------------------------------------------------------------------------------------------------------->
#Calcula el error cuadratico medio
#RMSE_CAFFG_B<-sqrt(mean(diferencia1^2))
#Graficos miembro A

jpeg("/home/arw/evaluacion/resultados/Evaluacion_wrf_EURO.jpg", pointsize=12, res = 150, width = 2000, height = 2000)
par(mfrow=c(4,2),mai=c(1,1,1,1))

par(mar = rep(2, 4))
rf<-writeRaster(hidroestimador, filename="/home/arw/evaluacion/resultados/hidroestimador.tif", overwrite=TRUE)
plot(hidroestimador,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="Hidroestimador", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(wrf_EURO, filename="/home/arw/evaluacion/resultados/wrf_EURO.tif", overwrite=TRUE)
plot(wrf_EURO,xlim=c(-90.2,-85.7),ylim=c(12.9,14.5),main="wrf_EURO", width=5, heigth=5,col=paleta.colores(30),alpha=TRUE,interpolate=FALSE,
     ylab="Latitud",xlab="Longitud",breaks=intervalos,legend=TRUE)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(aciertos_EURO, filename="/home/arw/evaluacion/resultados/aciertos_EURO.tif", overwrite=TRUE)
plot(aciertos_EURO,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Aciertos",legend=FALSE)
legend("topright",legend = c("Acierto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(perdidos_EURO, filename="/home/arw/evaluacion/resultados/perdidos_EURO.tif", overwrite=TRUE)
plot(perdidos_EURO,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Perdidos",legend=FALSE)
legend("topright",legend = c("Perdido","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falsa_alarma_EURO, filename="/home/arw/evaluacion/resultados/falsa_alarma_EURO.tif", overwrite=TRUE)
plot(falsa_alarma_EURO,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falsa Alarma",legend=FALSE)
legend("topright",legend = c("Falsa Alarma","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(falso_correcto_EURO, filename="/home/arw/evaluacion/resultados/falso_correcto.tif", overwrite=TRUE)
plot(falso_correcto_EURO,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Falso Correcto",legend=FALSE)
legend("topright",legend = c("Falso Correcto","N/A"),fill = c("green","white"))
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

par(mar = rep(2, 4))
rf<-writeRaster(diferencia_EURO, filename="/home/arw/evaluacion/resultados/diferencia_EURO.tif", overwrite=TRUE)
plot(diferencia_EURO,xlim=c(-90.2,-87.7),ylim=c(12.9,14.5),main="Diferencia en mm",col=paleta.errores(12),breaks=intervalos_errores)
par(mar = rep(2, 4))
plot(mapa,add = TRUE, xlim=c(-90.2,-87.7),ylim=c(12.9,14.5))

#Genera un gráfico vacio
par(mar = rep(2, 4))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', main="Evaluación modelo WRF vs Hidroestimador")
#text()
imagen_de_tabla_EURO<-"/home/arw/evaluacion/resultados/tabla_EURO.png"
pic<-readPNG(imagen_de_tabla_EURO)
addImg(pic,x=0.5,y=0.35,width = 0.85)

#Puedo poner texto dentro del espacio del gráfico vacio.
text(x = 0.5, y = 0.9, paste(ciclo,texto,time), cex = 0.8, col = "black")
#text(x = 0.5, y = 0.5, paste(resumen_EUROeneral_B), cex = 1, col = "black")
#Genera un gráfico con una tabla
#plot.table(resumen_EUROeneral_B)
#grid.table(resumen_EUROeneral_B)
#grafico1<-grid.table(resumen_EUROeneral_B)
#grid.arrange(grafico1)
dev.off()
#----------------------------------------------------------------------------------------->

#mediana_RMSE_CAFFG_B<-median(RMSE_CAFFG_B[])
#media_RMSE_CAFFG_B<-mean(RMSE_CAFFG_B[])

#Calcula la desviaci?n estandar 
#desv_CAFFG_B<-calc(stack(wrf1,hidroestimador),sd)
#desv_CPH_B<-calc(stack(wrf2,cph_EUROrid),sd)

#nombres<-c("RMSE_CAFFG_B","RMSE_CPH_B","SD_CAFFG_B","SD_CPH_B","ERR_CAFFG_B","ERR_CPH_B","Hit_Rate_CAFFG_B","Hit_Rate_CAFFG_B")
#estadisticas_B<-cbind(summary(RMSE_CAFFG_B),summary(RMSE_CPH_B),summary(desv_CAFFG_B),summary(desv_CPH_B),summary(error_CAFFG_B),
#summary(error_CPH_B),summary(hit_rate_CAFFG_B),summary(hit_rate_CPH_B))
#resumen_estadisticas_B<-rbind(nombres,estadisticas_B)

#------------------------------------------------------------->
################################################################################################################################
